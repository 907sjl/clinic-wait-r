# Referral wait times
# An R script to create visualizations of specialty clinic referral conversion rates for presentations.
# https://907sjl.github.io/

library(lubridate)
library(tidyverse)
library(myedwtools)
library(svglite)
library(ggplot2)
library(scales)
library(viridis)
library(RColorBrewer)

refresh_data <- function(end_date, window_size) {
  # Returns referral source data over a time window.
  #
  # Args:
  #   end_date - The last date in the time window
  #   window_size - The number of months in the time window
  #
  # Returns:
  #   A list of named data frames:
  #     "referrals" - Source data of referrals sent to clinics over a variable time window that are not cancelled,
  #                   rejected, or closed without being seen.
  #     "volume" - Source data of all referrals sent to clinics over a 12-month time window.
  
  referral_sql <- paste(
    "SELECT 
         a.ReferralID
         , cast(a.ReferralWrittenDTS AS date) AS WrittenDT 
         , cast(a.ReferralSentDTS AS date) AS SentDT
         , cast(a.ReferralSeenDTS] AS date) AS ReferralSeenDT
         , cast(a.AppointmentDTS AS date) AS ClinicCheckInDT
         , a.FrontLineClinicNM 
         , a.ReferralStatus
         , a.ReferralPriority
         , a.ReferralSeenOrCheckInFLG
         , a.DaysUntilPatientSeenOrCheckInAMT 
     FROM ExampleDB.ExampleSchema.ExampleReportingView a
     WHERE 
         a.[Reporting Date 90 Day Lag] >= dateadd(day, -",  window_size, ", '", end_date ,"')
         and a.[Reporting Date 90 Day Lag] < '", end_date ,"' 
         and a.DaysUntilPatientSeenOrCheckInAMT >= 0
         and a.ReferralAgedFLG = 1 
         and a.FrontLineClinicNM NOT IN ('Occupational Therapy',
                                         'Physical Therapy', 
                                         'Speech Pathology')", sep="")
  
  conn <- edw.open.connection(prod = FALSE)
  referral_dta <- edw.fetch(conn, sql = referral_sql)
  edw.close.connection(conn)

  volume_sql <- paste(
    "SELECT 
         a.ReferralID
         , cast(a.ReferralSentDTS AS date) AS SentDT
         , a.FrontLineClinicNM
         , a.ReferralStatus
         , a.ReferralClosedWithoutBeingSeenFLG
         , a.ReferralRejectedFLG
         , a.ReferralCanceledFLG
         , a.ReferralPriority
     FROM ExampleDB.ExampleSchema.ExampleReportingView a
     WHERE 
         a.ReferralSentDTS >= dateadd(month, -12, '", end_date ,"')
         and a.ReferralSentDTS < '", end_date ,"'
         and a.FrontLineClinicNM NOT IN ('Occupational Therapy',
                                         'Physical Therapy', 
                                         'Speech Pathology')", sep="")
  
  conn <- edw.open.connection(prod = FALSE)
  volume_dta <- edw.fetch(conn, sql = volume_sql)
  edw.close.connection(conn)
  
  return(list("referrals"=referral_dta,
              "volume"=volume_dta
             )
        )
} 

reverse_quantile <- function(wait_value, days_v) {
  # Returns the associated quantile for the number of days that referrals waited to be seen.
  #
  # Args:
  #  wait_value - The specific number of days that referrals waited to be seen.
  #  days_v - A vector with the number of days that referrals waited to be seen.
  #
  # Returns:
  #  A decimal number value for the quantile
  pop_size = length(days_v)
  included_size = length(which(days_v <= wait_value))
  return(included_size / pop_size)
}

count_by_days <- function(wait_value, days_v) {
  # Returns a scalar value count of records prior to and including a specific day milestone value. Used to
  # calculate referral conversion rates by day.
  #
  # Args:
  #   wait_value - The milestone value to look for
  #   days_v - A vector of milestone values for each record
  #
  # Returns: A scalar whole value count of records with a matching number of days
  return(length(which(days_v <= wait_value)))
}

count_at_days <- function(wait_value, days_v) {
  # Returns a scalar value count of records with a specific day milestone value. Used to count referrals
  # by day.
  #
  # Args:
  #   wait_value - The milestone value to look for
  #   days_v - A vector of milestone values for each record
  #
  # Returns: A scalar whole value count of records with a matching number of days
  return(length(which(days_v == wait_value)))
}

get_clinic_days_v <- function(clinic, tbl) {
  # Returns a vector of the number of days to see each referred patient for every referral sent to a clinic. Used
  # to calulcate performance curves of referral conversion rates by day.
  #
  # Args:
  #   clinic - The name of the clinic to return data for
  #   tbl - The table with referral data
  #
  # Returns: A vector of days until each referral was seen (or NaN)
  return((tbl %>%
          filter(FrontLineClinicNM == clinic))$DaysUntilPatientSeenOrCheckInAMT)
}

count_at_month <- function(at_month, months_v) {
  # Returns a scalar value count of records with a month value. Used to count referrals
  # by month.
  #
  # Args:
  #   at_month - The month value to look for
  #   months_v - A vector of month values for each record
  #
  # Returns: A scalar whole value count of records with a matching month
  return(length(which(months_v == at_month)))
}

get_month_starts_v <- function(priority, tbl) {
  # Returns a vector of all month starting date values for all referrals in table order. Used to count
  # referrals by month.
  #
  # Args:
  #   priority - Either "Urgent" or "Routine" used as a filter
  #   tbl - A table of referral data with a Month_Start column
  #
  # Returns: A vector of month starting dates
  return((tbl %>%
          filter(ReferralPriority == priority))$Month_Start)
}

get_data_point_vjust <- function(var_value) {
  # A configurable vertical justification amount for visuals
  #
  # Args:
  #   var_value - The value in the visual
  #
  # Returns:
  #   A decimal number value
  if (var_value < 0) return(1.8)
  else return(-1)
}

create_performance_visuals <- function(source_tbl, file_label, end_date, window_size) {
  # Transforms the source data into referral conversion rates by clinic and exports
  # visualizations as SVG files.
  #
  # Args:
  #   source_tbl - The source data for referral conversions to appointments
  #   file_label - Either "Urgent" or "Routine" to be used as a filter
  #   end_date - The last date in the time window for reporting
  #   window_size - The number of months in the time window

  # Inits
  months_included <- ceiling(window_size / 31.0)
  end_date_label <- strftime(end_date-1, "%B %d, %Y")
  end_date_file_suffix <- strftime(end_date-1, "%Y_%m_%d")
  file_name_suffix <- paste(window_size, "_", end_date_file_suffix ,".svg", sep="")
  
  # Transformations

  ## Create summary table by day milestones from which to attach aggregates
  overall_days_v = source_tbl$DaysUntilPatientSeenOrCheckInAMT
  if (file_label == "Urgent") {
    day_milestones <- seq(7, 30)
    last_milestone <- 30
  } else {
    day_milestones <- seq(10, 90, length.out=17)
    last_milestone <- 90
  }
  days_tbl <- tibble(day_milestones)

  ## Calculate aggregates for each milestone
  overall_days_tbl <- days_tbl %>%
    rowwise() %>% 
    mutate("Pct"=reverse_quantile(day_milestones, overall_days_v),
           "Count_Rolling"=count_by_days(day_milestones, overall_days_v),
           "Count"=count_at_days(day_milestones, overall_days_v) 
          )
  
  ## Create summary table by clinic and milestone
  clinics_v <- sort(unique(source_tbl$FrontLineClinicNM))
  clinics_tbl <- tibble(clinics_v)
  clinic_days_tbl <- cross_join(clinics_tbl, days_tbl)
  clinic_days_tbl <- clinic_days_tbl %>% 
    rowwise() %>% 
    mutate("Pct"=reverse_quantile(day_milestones, get_clinic_days_v(clinics_v, source_tbl)),
           "Count"=count_by_days(day_milestones, get_clinic_days_v(clinics_v, source_tbl))
    )
  
  ## Create a clinic name category column that sorts by # of referrals seen
  temp_tbl <- clinic_days_tbl %>% 
    filter(day_milestones == last_milestone) %>%
    arrange(Pct)
  clinic_days_tbl <- clinic_days_tbl %>% 
    mutate(Clinic=factor(clinics_v, levels=temp_tbl$clinics_v, ordered=TRUE))
  
  ## Create summary table by clinic
  clinic_summary_tbl <- source_tbl %>% 
    group_by(FrontLineClinicNM) %>% 
    summarize("Referrals"=n(),
              "Seen"=sum(SeenIn90DaysFLG),
              "DaysTo50pct"=quantile(DaysUntilPatientSeenOrCheckInAMT, probs=0.5),
              "DaysTo60pct"=quantile(DaysUntilPatientSeenOrCheckInAMT, probs=0.6),
              "DaysTo70pct"=quantile(DaysUntilPatientSeenOrCheckInAMT, probs=0.7),
              "DaysTo80pct"=quantile(DaysUntilPatientSeenOrCheckInAMT, probs=0.8),
              "DaysTo90pct"=quantile(DaysUntilPatientSeenOrCheckInAMT, probs=0.9),
    ) 
  clinic_summary_tbl <- clinic_summary_tbl %>% mutate(SeenPct=Seen/Referrals*100.0)
  clinic_summary_tbl <- clinic_summary_tbl %>% mutate(Clinic=factor(FrontLineClinicNM))

  ## Create a long pivot of clinics and quantiles with associated data for scatter plots
  clinic_pivot <- clinic_summary_tbl %>%
    pivot_longer(cols = !c(FrontLineClinicNM, Clinic, Referrals, Seen, SeenPct)) %>%
    mutate(quantile = as.integer(str_extract(pattern = '\\d{2}', string = name))) %>% 
    rename(days=value)

  ## Create a data frame from the pivot with only records for unique clinics in descending order of volume
  clinic_plot_frame <- clinic_pivot %>% 
    distinct(Clinic, .keep_all = T) %>% 
    arrange(desc(Referrals))
  clinic_plot_max_days <- max(clinic_pivot$days)
  clinic_plot_colors=factor(clinic_pivot$quantile)
  
  
  # Plots
  
  ## Overall line chart of day milestones and % seen
  count_factor <- max(overall_days_tbl$Count)
  p <- ggplot(overall_days_tbl, aes(x=day_milestones, y=Pct, fill=day_milestones, color=day_milestones)) +
    ### Columns for volume using secondary axis
    geom_col(aes(y=Count/count_factor), color="#AFAFAF") +
    geom_line(aes(y=Pct), size=1.25, color="#696969") + 
    geom_point(aes(y=Pct), size=2.25, color="#AF4949") + 
    geom_text(aes(label=Count, y=0.00), color="#494949", size=4, vjust=-1.0) +
    geom_text(aes(label=scales::percent(round(Pct,2), accuracy=1), y=Pct), color="#494949", size=3.75, vjust=-1.5) +
    scale_fill_viridis(option="turbo", direction=1, begin=0.2, end=0.8, alpha=0.7) +
    scale_y_continuous(breaks=round(seq(0, 1.0, length.out=11), 2), limits=c(0, 1), labels=scales::percent) +
    scale_x_continuous(breaks=day_milestones) +
    labs(title=paste("Overall Days to See ", file_label, " Referrals", sep=""),
         subtitle=paste("Previous ", months_included, " months ending ", end_date_label, sep=""),
         y="% of Referrals",
         x="# of Days") +
    theme_minimal() + 
      theme(
        axis.title = element_text(size=11, color="#696969"),
        axis.text = element_text(size = 12, color="#696969"),
        title=element_text(size=14, color="#696969"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "none"
    )
  ggsave(plot=p, width=svg_width, height=svg_height, units="in", file=paste(tolower(file_label), "_seen_curve_by_days_", file_name_suffix, sep=""))
  
  ## Day milestones and % seen by clinic in a waffle chart ordered by % seen
  brewer_pal <- brewer.pal(9, "Blues")
  blues_pal <- colorRampPalette(brewer_pal[1:5], bias=0.5)(50)
  p <- ggplot(clinic_days_tbl, aes(x=day_milestones, y=Clinic, fill=Pct, color=Pct)) +
    geom_tile(size=0.75, color="#696969", alpha=0.8) + 
    geom_text(aes(label=scales::percent(round(Pct,2), accuracy=1)), size=3.5, color="#494949") + 
    scale_fill_viridis(option="turbo", direction=1, begin=0.2, end=0.8, alpha=0.8) +
    scale_fill_gradient2(low="white", mid="white", high=blues_pal, limits=c(0.5, 1), midpoint=0.4, na.value="white") +
    scale_x_continuous(breaks=day_milestones) +
    scale_y_discrete(limits = levels(clinic_days_tbl$Clinic)) + 
    labs(title=paste("Days to See ", file_label, " Referrals by Clinic", sep=""),
         subtitle=paste("Previous ", months_included, " months ending ", end_date_label, sep=""),
         y="Clinic",
         x="# of Days") +
    theme_minimal() + 
    theme(
      axis.title = element_text(size=11, color="#696969"),
      axis.text = element_text(size = 12, color="#696969"),
      title=element_text(size=14, color="#696969"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 12, color="#696969"),
      axis.text.y = element_text(size = 12, color="#696969", margin=margin(r=-15)),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      legend.position = "none"
    )
  ggsave(plot=p, width=13.5, height=svg_height, units="in", file=paste(tolower(file_label), "_seen_waffle_by_days_and_clinic_", file_name_suffix, sep=""))

  ## The days to each quantile by clinic in a dumbbell type scatter plot
  quantile_colors <- c("#313695", "#4575B4", "#74ADD1", "#FDAE61", "#D73027")
  p <- ggplot(clinic_plot_frame, aes(x=days, y=Clinic)) + 
    labs(
      title=paste("Days to See ", file_label, " Referrals by Clinic ", sep=""),
      subtitle=paste("Previous ", months_included, " months ending ", end_date_label, sep=""),
      x="# of Days",
      y="Clinic") +
    scale_x_continuous(breaks=round(seq(0, clinic_plot_max_days, length.out=21))) +
    scale_y_discrete(limits = rev(clinic_plot_frame$Clinic)) + 
  
    ### Add points at days to each quantile
    annotate("segment", y=0, yend=25.75, x=90, color="#006633", linetype=2) + 
    annotate("text", x=87-(clinic_plot_max_days/100.0), y=25.5, label="90d", color="#006633") + 
    geom_segment(data=clinic_summary_tbl, aes(x=DaysTo50pct, xend=DaysTo90pct, y=Clinic), color="#696969", size=1) + 
    geom_point(data=clinic_pivot, aes(x=days, y=Clinic, color=clinic_plot_colors), size=3, shape=16) + 
    scale_color_manual(values=quantile_colors, name="% of Referrals") +
    theme_minimal() + 
    theme(
      axis.title = element_text(size=11, color="#696969"),
      axis.text = element_text(size = 12, color="#696969"),
      title=element_text(size=14, color="#696969"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      legend.text = element_text(size=11),
      legend.title = element_text(size=12)
    ) + 
    coord_cartesian(clip="off")
  ggsave(plot=p, width=13.5, height=svg_height, units="in", file=paste(tolower(file_label), "_seen_dumbells_by_clinic_", file_name_suffix, sep=""))
  
  ## Plot the volume by clinic
  total_referrals = sum(clinic_summary_tbl$Referrals)
  max_referrals = max(clinic_summary_tbl$Referrals)
  p <- ggplot(clinic_summary_tbl, aes(x=Referrals, y=reorder(Clinic, Referrals))) + 
    labs(
      title=paste("# of ", file_label, " Referrals by Clinic with % of Total", sep=""),
      subtitle=paste("Previous ", months_included, " months ending ", end_date_label, sep=""),
      x="# of Referrals",
      y="Clinic") +
    scale_x_continuous(breaks=round(seq(0, max(clinic_summary_tbl$Referrals), length.out=10)), label=comma) +
    geom_segment(aes(x=0, xend=Referrals), color="#696969", size=1.75) + 
    geom_point(aes(x=Referrals), color="#4575B4", size=3.5, shape=16) + 
    geom_label(aes(label=paste(round(Referrals/total_referrals*100.0), "%", sep=""), x=max_referrals/30*-1), color="#494949", hjust=0.5, vjust=0.6, size=3.5) + 
    theme_minimal() + 
    theme(
      axis.title = element_text(size=11, color="#696969"),
      axis.text.x = element_text(size = 12, color="#696969"),
      axis.text.y = element_text(size = 12, color="#696969", margin=margin(r=-10)),
      title=element_text(size=14, color="#696969"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      legend.text = element_text(size=11),
      legend.title = element_text(size=12)
    ) 
  ggsave(plot=p, width=svg_width, height=svg_height, units="in", file=paste(tolower(file_label), "_volume_by_clinic_", file_name_suffix, sep=""))
}


# MAIN 

## Get data
end_date <- as.Date('2023-10-01') 
window_size <- 364
source_dta <- refresh_data(end_date, window_size) 
referral_dta <- source_dta$referrals
referral_tbl <- as_tibble(referral_dta)
volume_dta <- source_dta$volume
volume_tbl <- as_tibble(volume_dta)


## Transform data

### Calculate seen or not seen indicators
referral_tbl <- referral_tbl %>% 
  mutate(SeenIn90DaysFLG=ifelse(((ReferralSeenOrCheckInFLG==1) & (DaysUntilPatientSeenOrCheckInAMT<91)),
                                1, 0))

### Urgent referral source
urgent_referral_tbl <- referral_tbl %>%
  filter(ReferralPriority == 'Urgent')

### Routine referral source
routine_referral_tbl <- referral_tbl %>%
  filter(ReferralPriority == 'Routine')

### Create table of volume measurements by month
volume_tbl <- volume_tbl %>% 
  mutate("Month_Start"=make_date(year=year(SentDT), month=month(SentDT), day=1))
months_v <- sort(unique(volume_tbl$Month_Start))
months_tbl <- tibble(months_v)
months_tbl <- months_tbl %>%
  rowwise() %>%
  mutate("Urgent_Count"=count_at_month(months_v, get_month_starts_v('Urgent', volume_tbl)),
         "Routine_Count"=count_at_month(months_v, get_month_starts_v('Routine', volume_tbl)))

### Calculate annualized average monthly volumes and then monthly variance
urgent_annual_avg <- length(get_month_starts_v('Urgent', volume_tbl)) / 12.0
routine_annual_avg <- length(get_month_starts_v('Routine', volume_tbl)) / 12.0 
months_tbl <- months_tbl %>%
  rowwise() %>%
  mutate("Urgent_Var"=Urgent_Count-urgent_annual_avg,
         "Routine_Var"=Routine_Count-routine_annual_avg
  )
months_tbl <- months_tbl %>%
  rowwise() %>%
  mutate("Urgent_Var_Pct"=Urgent_Var / urgent_annual_avg,
         "Routine_Var_Pct"=Routine_Var / routine_annual_avg,
         "Urgent_Vjust"=get_data_point_vjust(Urgent_Var),
         "Routine_Vjust"=get_data_point_vjust(Routine_Var)
  )


## Visualize data
dpi <- 300
svg_width <- 3150/dpi
svg_height <- 1772/dpi

### Conversion rate performance visuals for urgent and routine referrals
create_performance_visuals(urgent_referral_tbl, "Urgent", end_date, window_size)
create_performance_visuals(routine_referral_tbl, "Routine", end_date, window_size)

### Plot urgent monthly referral volume 
urgent_zero_annotation = paste(round(urgent_annual_avg), "/mth", sep="")
urgent_full_annotation = paste("x-axis represents the average of ",
                               round(urgent_annual_avg),
                               " urgent referrals per month", sep="")
p <- ggplot(months_tbl, aes(x=months_v, y=Urgent_Var_Pct)) + 
  geom_segment(aes(y=0, yend=Urgent_Var_Pct), color="#696969", size=1) + 
  geom_line(aes(y=0), size=1.25, color="#696969") + 
  geom_point(color="#4575B4", size=3.5, shape=16) + 
  geom_text(aes(label=Urgent_Count, vjust=Urgent_Vjust), size=3.5, color="#494949") + 
  annotate("text", x=min(months_v), y=0, label=urgent_zero_annotation, color="#494949", vjust=-1, hjust=-0.25) + 
  annotate("text", x=min(months_v), y=-0.24, label=urgent_full_annotation, color="#494949", hjust=-.05) + 
  scale_y_continuous(breaks=round(seq(-0.25, 0.25, length.out=11), 2), labels=scales::percent, limits=c(-0.25, 0.25)) +
  scale_x_date(breaks=months_v, labels=date_format("%Y-%b")) +
  labs(title="Monthly Urgent Referral Volume with Over/Under %",
       x="Month",
       y="% Over/Under Average") +
  theme_minimal() + 
  theme(
    axis.title = element_text(size=11, color="#696969"),
    axis.text = element_text(size = 12, color="#696969"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    title=element_text(size=14, color="#696969"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "none"
  )
ggsave(plot=p, width=svg_width, height=svg_height, units="in", file="urgent_monthly_volume.svg")

### Plot routine monthly referral volume 
routine_zero_annotation = paste(format(round(routine_annual_avg), big.mark=","), "/mth", sep="")
routine_full_annotation = paste("x-axis represents the average of ",
                                format(round(routine_annual_avg), big.mark=","),
                                " routine referrals per month", sep="")
p <- ggplot(months_tbl, aes(x=months_v, y=Routine_Var_Pct)) + 
  geom_segment(aes(y=0, yend=Routine_Var_Pct), color="#696969", size=1) + 
  geom_line(aes(y=0), size=1.25, color="#696969") + 
  geom_point(color="#4575B4", size=3.5, shape=16) + 
  geom_text(aes(label=format(Routine_Count, big.mark=","), vjust=Routine_Vjust), size=3.5, color="#494949") + 
  annotate("text", x=min(months_v), y=0, label=routine_zero_annotation, color="#494949", vjust=-1, hjust=-0.25) + 
  annotate("text", x=min(months_v), y=-0.24, label=routine_full_annotation, color="#494949", hjust=-.05) + 
  scale_y_continuous(breaks=round(seq(-0.25, 0.25, length.out=11), 2), labels=scales::percent, limits=c(-0.25, 0.25)) +
  scale_x_date(breaks=months_v, labels=date_format("%Y-%b")) +
  labs(title="Monthly Routine Referral Volume with Over/Under %",
       x="Month",
       y="% Over/Under Average") +
  theme_minimal() + 
  theme(
    axis.title = element_text(size=11, color="#696969"),
    axis.text = element_text(size = 12, color="#696969"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    title=element_text(size=14, color="#696969"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "none"
  )
ggsave(plot=p, width=svg_width, height=svg_height, units="in", file="routine_monthly_volume.svg")
