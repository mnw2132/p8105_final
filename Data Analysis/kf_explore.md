Exploratory Analysis
================
Kaleb J. Frierson
2024-11-23

Loading packages:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Copied and pasted `final_data.csv` from data preparation folder into
data analysis folder and am reading it into this document as `ticks`.

``` r
ticks = read_csv("final_data.csv")
```

    ## Rows: 147 Columns: 26
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): name, county_centroid, abbrev, swis, nysp_zone, nyc, geometry
    ## dbl  (16): year, total_ticks_collected, tick_population_density, ticks_teste...
    ## lgl   (2): dos_ll, dosll_date
    ## date  (1): datemod
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

I want a count of counties with complete data for each year:

``` r
summary_by_year = 
  ticks |> 
  group_by(year) |> 
  summarize(non_missing_counties = sum(!is.na(total_ticks_collected)))

summary_by_year
```

    ## # A tibble: 4 × 2
    ##    year non_missing_counties
    ##   <dbl>                <int>
    ## 1  2019                   43
    ## 2  2020                   50
    ## 3  2021                   45
    ## 4    NA                    0

I also want to know which couties have non zero values for all three
years of data collection:

``` r
all_year_data = 
  ticks |> 
  filter(year %in% c(2019, 2020, 2021)) |>  
  group_by(name) |>                        
  summarize(
    years_with_data = sum(!is.na(total_ticks_collected)),  
    all_years_present = all(year %in% c(2019, 2020, 2021)) 
  ) |> 
  filter(years_with_data == 3 & all_years_present) |>      
  select(name)                                          

all_year_data
```

    ## # A tibble: 36 × 1
    ##    name      
    ##    <chr>     
    ##  1 Albany    
    ##  2 Allegany  
    ##  3 Cayuga    
    ##  4 Chautauqua
    ##  5 Clinton   
    ##  6 Columbia  
    ##  7 Delaware  
    ##  8 Dutchess  
    ##  9 Erie      
    ## 10 Franklin  
    ## # ℹ 26 more rows

I think it would be reasonable to restrict the dataset to the 36
counties with data across three years. This would allow us to
incorporate a column for tmax for each county in each year and
potentially assess any correlation between temperature increases and
abundance.
