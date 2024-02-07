Lab 05 - Data Wrangling
================
Yinuo Zhao
Feb 07, 2024

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(tidyr)
```

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lat <- met$lat / 1000
met$lon <- met$lon / 1000

met$wind.sp <- met$wind.sp / 10
met$temp <- met$temp / 10
met$dew.point <- met$dew.point / 10
met$atm.press <- met$atm.press / 10

met$relative_humidity <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_dt <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )

head(met_dt[, list(USAFID, WBAN, STATE)], n = 4)
```

    ##    USAFID  WBAN STATE
    ## 1: 690150 93121    CA
    ## 2: 690150 93121    CA
    ## 3: 690150 93121    CA
    ## 4: 690150 93121    CA

``` r
dim(met_dt)
```

    ## [1] 2569423      34

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
temp_median <- quantile(met_dt$temp, probs = 0.5, na.rm = TRUE)
wind_speed_median <- quantile(met_dt$wind.sp, probs = 0.5, na.rm = TRUE)
pressure_median <- quantile(met_dt$atm.press, probs = 0.5, na.rm = TRUE)

stations_median <- (met_dt %>% group_by(USAFID) %>% 
                           summarise(median_temp = median(temp, na.rm = TRUE),
                                     median_wind_speed = median(wind.sp, na.rm = TRUE),
                                     median_pressure = median(atm.press, na.rm = TRUE)))

equal_stations_temp_median <- 
  stations_median$USAFID[which(stations_median$median_temp == temp_median)]
equal_stations_wind_speed_median <- 
  stations_median$USAFID[which(stations_median$median_wind_speed == wind_speed_median)]
equal_stations_pressure_median <- 
  stations_median$USAFID[which(stations_median$median_pressure == pressure_median)]

common_stations <- intersect(intersect(equal_stations_temp_median, equal_stations_wind_speed_median), equal_stations_pressure_median)

print(common_stations)
```

    ## [1] 723119

Station 723119 is the only coincide station.

Next identify the stations have these median values.

``` r
# From previous part, 723119 is the only coincide station

desired_station <- met_dt[met_dt$USAFID == 723119]
print(head(desired_station[, c("STATE", "LAT", "LON")], 1))
```

    ##    STATE    LAT    LON
    ## 1:    SC 34.849 -82.35

This station is in the state of SC with latitude 34.849 and longitude
-82.35.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
states_median <- met_dt %>% 
  group_by(STATE) %>% 
  summarise(state_median_temp =  median(temp, na.rm = TRUE),
            state_median_wind_speed = median(wind.sp, na.rm = TRUE))
stations_median2 <- met_dt %>% 
  group_by(USAFID) %>% 
  summarise(station_median_temp = median(temp, na.rm = TRUE),
            station_median_wind_speed = median(wind.sp, na.rm = TRUE),
            STATE = max(STATE))

merged_df <- merge(x = states_median, 
                   y = stations_median2, 
                   by.x  = "STATE", 
                   by.y  = "STATE", 
                   all.x = TRUE, 
                   all.y = TRUE)

merged_df$temp_diff <- merged_df$station_median_temp - merged_df$state_median_temp
merged_df$wind_speed_diff <- merged_df$station_median_wind_speed - merged_df$state_median_wind_speed
merged_df$euclidean <- sqrt(merged_df$temp_diff**2 + merged_df$wind_speed_diff**2)

merged_df_by_state <- merged_df %>% 
  group_by(STATE) %>% 
  arrange(euclidean) %>% 
  slice(1)
closest_stations <- merged_df_by_state[, c("STATE", "USAFID", "euclidean")]
closest_stations
```

    ## # A tibble: 48 × 3
    ## # Groups:   STATE [48]
    ##    STATE USAFID euclidean
    ##    <chr>  <int>     <dbl>
    ##  1 AL    720265     0.200
    ##  2 AR    722188     0.100
    ##  3 AZ    722728     0.5  
    ##  4 CA    722950     0.300
    ##  5 CO    724695     0.100
    ##  6 CT    725040     0.5  
    ##  7 DE    724093     0.200
    ##  8 FL    722024     0    
    ##  9 GA    722255     0    
    ## 10 IA    722097     0    
    ## # ℹ 38 more rows

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
state_geo_median <- stations %>% group_by(STATE) %>% summarise(median_lat = median(LAT, na.rm = TRUE),
                                                               median_long = median(LON, na.rm = TRUE))

state_closest_station <- merge(x = closest_stations, 
                   y = stations, 
                   by.x  = "USAFID", 
                   by.y  = "USAF", 
                   all.x = TRUE, 
                   all.y = FALSE)

library(leaflet)

l <- leaflet(state_geo_median) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(data = state_geo_median, 
                   lat = ~median_lat, lng = ~median_long, 
                   opacity = 0.2, fillOpacity = 0.5, radius = 2, color = "blue",
                   group = "Geograohic Median of States") %>% 
  addCircleMarkers(data = state_closest_station, 
                   lat = ~LAT, lng = ~LON, 
                   opacity = 0.2, fillOpacity = 0.5, radius = 2, color = "red",
                   group = "Closest Station Base on Euclidean Distance of Temperature and Wind Speed") %>%   
  addLayersControl(overlayGroups = c("Geograohic Median of States", 
                                     "Closest Station on Euclidean Distance of Temperature and Wind Speed"),
    options = layersControlOptions(collapsed = FALSE)
  )

# uncomment the code below to see the map
l
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
met_dt <- met_dt %>% mutate(elev_category = case_when(
  elev < 93 ~ "low",
  elev >= 93 & elev < 401 ~ "mid",
  elev >= 401 ~ "high")
)

summary_table <- met_dt %>% 
  group_by(STATE, elev_category) %>% 
  summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop") 

summary_table <- spread(summary_table, key = elev_category, value = avg_temp)
colnames(summary_table) <- c("State", "Low Elevation", "Mid Elevation", "High Elevation")

# uncomment the code below to see the summary table
#kable(summary_table, 
#      caption = "Average Temperature by Elevation Category for Each #State") %>%
#  kable_styling(full_width = FALSE)
```

Note: kable produces HTML output but the output of this rmd file is not
html. Therefore it cannot be knitted.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
filtered_data <- stations_median %>%
  filter(median_pressure >= 1000 & median_pressure <= 1020)

scatterplot <- ggplot(filtered_data, aes(x = median_pressure, y = median_temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = "blue") + # Add smooth line
  labs(x = "Atmospheric Pressure", y = "Temperature",
       title = "Temperature vs Atmospheric Pressure with Regression Lines") +
  theme_linedraw()

scatterplot
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
linear_model <- lm(median_temp ~ median_pressure, data = filtered_data)
plot(linear_model)
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = median_temp ~ median_pressure, data = filtered_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3696  -2.6173   0.1844   2.3012  11.6394 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     1175.93898   58.53149   20.09   <2e-16 ***
    ## median_pressure   -1.14162    0.05785  -19.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.762 on 1083 degrees of freedom
    ## Multiple R-squared:  0.2645, Adjusted R-squared:  0.2638 
    ## F-statistic: 389.4 on 1 and 1083 DF,  p-value: < 2.2e-16

``` r
spline_model <- gam(median_temp ~ s(median_wind_speed, bs = "cs"), data = filtered_data)
plot(spline_model)
```

![](README_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## median_temp ~ s(median_wind_speed, bs = "cs")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.9391     0.1297   161.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                       edf Ref.df     F p-value    
    ## s(median_wind_speed) 3.03      9 6.845  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0529   Deviance explained = 5.56%
    ## GCV = 18.295  Scale est. = 18.227    n = 1083

The linear model has Adjusted R-squared of 0.2638 while the spine model
has 0.0529. This means that the linear model explains a larger
proportion of the observed data. Therefore, the linear model is best fit
among the two.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
