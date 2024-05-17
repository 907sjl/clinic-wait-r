< [Portfolio](https://907sjl.github.io) | [GitHub Repository](https://github.com/907sjl/clinic-wait-r)    

## Background
One way to measure access to care is timeliness. Long delays to see a healthcare provider can speak to availability issues, either a lack of resources or inefficiencies 
that result in a less than optimal conversion of referrals into appointments. Long delays can also speak to accommodation issues or accessibility issues if patients have 
difficulty attending their scheduled appointments. This project is an example of creating visuals that can be embedded in reports or presentations that communicate the 
rate at which specialty clinics are seeing patients who are referred.    

## Technology Stack
This is a simple R script that uses the ggplot library to export visuals as Scalable Vector Graphics files. SVG files can be imported into PowerPoint slide decks, HTML 
documents, or Word documents.     

R is a free and widely known statistical programming language used for data analysis and visualization. RStudio is a development environment for R that enables analysts to 
work with datasets and quickly create visualizations.    

Scalable Vector Graphics (SVG) files are instructions for drawing images and text without storing a raster of pixels. Images are usually drawn in real time while viewing. 
SVG images can be scaled to fit a slide or report page without pixelation.    

## Application Overview    
This R script visualizes the rates that clinics see referred patients over a window of time. This window can be changed by modifying variables within the script. This 
script is designed to extract the referral data from an enterprise data warehouse with SQL views that support this analysis. However, the example visuals in this example 
contain synthetic data imported from Comma Separated Variable files that was imported into RStudio instead of using the data warehouse.    

Various visuals illustrate overall referral conversion rates over time, conversion rates over time by clinic, and the volume of referrals for each clinic. Each visual 
is described below.    

## Data Sources
All data in these reports is fabricated and does not represent any real healthcare organization. The synthetic data was loaded into RStudio from these Comma Separated 
Variable files: 
- The **referral_tbl.csv** file loads records of referrals and processing dates for referrals that were not rejected, cancelled, or administratively closed. These 
represent referrals that are expected to be converted into patient appointments. 
- The **volume_tbl.csv** file loads records of every referral sent to each clinic and is used for visualizations of overall volume.    

Both files represent datasets that would have been loaded into R using SQL and an enterprise data warehouse.    
   
## Visuals    
The R script loads the source data, transforms it into reporting level information, and generates these visuals within SVG files.    

### Overall conversion rate curve    
<img src="images/urgent_seen_curve_by_days_364_2023_09_30.svg?raw=true" alt="SVG image: overall rate curve by day"/>    

The rate curve is a line chart showing the percent of referrals seen by days waited. All referrals are aged 90 days from the date that they were originally sent 
to the clinic and then reported. This is to create a consistent denominator population of referrals were every referral had the same opportunity to be scheduled 
and the patient seen within a reasonable time. The wait times for reach referral are calculated from the date when the referral was first sent.    

Bars behind the line show the relative number of referrals for each day of wait. This creates a histogram displaying the weighting of referrals across the 
curve.    

### Clinic conversion rate dumbbells    
<img src="images/urgent_seen_dumbells_by_clinic_364_2023_09_30.svg?raw=true" alt="SVG image: dumbbell chart by clinic"/>    

A dumbbell style scatter plot breaks out the rate curves by clinic. Each clinic on the y-axis has data points for the 50th, 60th, 70th, 80th, and 90th quantiles 
with a line connecting them to more readily communicate the range of wait times between those quantiles. An annotation calls attention to the 90-day wait time 
point on the x-axis as a visual indication of where the reasonable wait time range ends.    

This visual places every clinic on one chart while still allowing the viewer to readily see the relative performance of one compared to another. The next visual 
provides more precise information.    

### Clinic conversion rate waffle    
<img src="images/urgent_seen_waffle_by_days_and_clinic_364_2023_09_30.svg?raw=true" alt="SVG image: waffle chart by clinic"/>    

This chart provides more precise numerical data at the expense of readability. It is a crosstab table with clinics on the y-axis and days of wait time on the x-axis. 
Table cells at the intersection of each display the associated quantile of referrals. Blue-tone shading highlights the quantile range starting at 50% with darker 
shading for higher percentages. The clinics are sorted in descending order of overall performance so that the shading guides the eye from the best performers to the 
worst.    

### Referral volume by clinic    
<img src="images/urgent_volume_by_clinic_364_2023_09_30.svg?raw=true" alt="SVG image: chart of referral volumes by clinic"/>    

Horizontal lines illustrate the number of referrals in the denominators for each clinic. Each clinic's percent of the grand total is displayed by a label next to 
the clinic name. This visual provides some context as to which clinic are moving the overall conversion rates and which clinics are not. It also shows clinics 
with the fewest referrals, hinting at which clinic percentages are most likely to be affected by outliers.    

### Overall referral volume by month    
<img src="images/urgent_monthly_volume.svg?raw=true" alt="SVG image: chart of referral volumes by month"/>    

A lollipop style chart displays the number of referrals sent per month in terms of their variance from the annualized average. The y-axis is the percent of variance from the 
average. The exact number of referrals each month is displayed in a data label. This view shows both the magnitude of referrals that require clinic staff to decide on 
a disposition, and the magnitude of the variation in that volume month-to-month.    

< [Portfolio](https://907sjl.github.io) | [GitHub Repository](https://github.com/907sjl/clinic-wait-r)    

<p style="font-size:11px">Page template forked from <a href="https://github.com/evanca/quick-portfolio">evanca</a></p>
<!-- Remove above link if you don't want to attribute -->
