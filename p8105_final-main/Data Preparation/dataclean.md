Analytic Dataset
================
Sarahy Martinez
2024-11-16

This document for cleaning and tidying the data.

``` r
library(tidyverse)
library(rvest)
library(sf)
library(purrr)
```

``` r
nineteen_twentyone_county_lyme = read_csv("new_york_county_lyme.csv") %>% 
  janitor::clean_names() %>% 
  select(county_name, event_count,percent_rate ) %>% 
  rename(NAME = "county_name") %>% 
  mutate( data = "lyme")

print(nineteen_twentyone_county_lyme)
```

    ## # A tibble: 62 × 4
    ##    NAME      event_count percent_rate data 
    ##    <chr>           <dbl>        <dbl> <chr>
    ##  1 Queens            336          4.9 lyme 
    ##  2 Saratoga          494         70.8 lyme 
    ##  3 Greene            612        427.  lyme 
    ##  4 Niagara            10          1.6 lyme 
    ##  5 Allegany           59         42.3 lyme 
    ##  6 Bronx              78          1.8 lyme 
    ##  7 Ulster            657        122.  lyme 
    ##  8 Jefferson         170         49.7 lyme 
    ##  9 Orange            878         75.1 lyme 
    ## 10 Wayne             120         44.2 lyme 
    ## # ℹ 52 more rows

Tidy deer tick data

``` r
tick_surveillance = read_csv("./Deer_Tick_Surveillance__Adults__Oct_to_Dec__excluding_Powassan_virus__Beginning_2008_20241116.csv") %>% 
  janitor::clean_names() %>% 
  filter(year %in% 2019:2021 ) %>% 
  rename(NAME = "county") %>% 
  select(-total_sites_visited, -school_districts_2016_shp, -local_waterfront_revitalization_program_lwrp_communities, -new_york_zip_codes, -counties, -nys_senate_districts, -year) %>% 
mutate(data = "ticks")


print(tick_surveillance)
```

    ## # A tibble: 138 × 10
    ##    NAME       total_ticks_collected tick_population_density total_tested
    ##    <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1 Albany                       366                    85.8          168
    ##  2 Chautauqua                   199                    36.7          107
    ##  3 Dutchess                     438                    66.8           51
    ##  4 Warren                        88                    13.8           87
    ##  5 Albany                       114                    37.2          103
    ##  6 Chemung                      271                   257.            50
    ##  7 Delaware                      75                    21.9           69
    ##  8 Tompkins                     131                    52.4           44
    ##  9 Oneida                       157                    78.5           50
    ## 10 Seneca                        51                    35.5           50
    ## # ℹ 128 more rows
    ## # ℹ 6 more variables: b_burgdorferi_percent <dbl>,
    ## #   a_phagocytophilum_percent <dbl>, b_microti_percent <dbl>,
    ## #   b_miyamotoi_percent <dbl>, county_centroid <chr>, data <chr>

Read in shp file for NY counties

``` r
ny_county =  st_read("./Counties.shp") 
```

    ## Reading layer `Counties' from data source 
    ##   `C:\Users\sarah\OneDrive\Documents\Data Science\p8105_final\Data Preparation\Counties.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 62 features and 17 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 105571.4 ymin: 4480943 xmax: 779932.1 ymax: 4985476
    ## Projected CRS: NAD83 / UTM zone 18N

``` r
print(ny_county)
```

    ## Simple feature collection with 62 features and 17 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 105571.4 ymin: 4480943 xmax: 779932.1 ymax: 4985476
    ## Projected CRS: NAD83 / UTM zone 18N
    ## First 10 features:
    ##           NAME ABBREV GNIS_ID FIPS_CODE   SWIS   NYSP_ZONE POP1990 POP2000
    ## 1       Albany   ALBA  974099     36001 010000        East  292594  294565
    ## 2     Allegany   ALLE  974100     36003 020000        West   50470   49927
    ## 3        Bronx   BRON  974101     36005 600000 Long Island 1203789 1332650
    ## 4       Broome   BROO  974102     36007 030000     Central  212160  200536
    ## 5  Cattaraugus   CATT  974103     36009 040000        West   84234   83955
    ## 6       Cayuga   CAYU  974104     36011 050000     Central   82313   81963
    ## 7   Chautauqua   CHAU  974105     36013 060000        West  141895  139750
    ## 8      Chemung   CHEM  974106     36015 070000     Central   95195   91070
    ## 9     Chenango   CHEN  974107     36017 080000     Central   51768   51401
    ## 10     Clinton   CLIN  974108     36019 090000        East   85969   79894
    ##    POP2010 POP2020 DOS_LL DOSLL_DATE NYC CALC_SQ_MI    DATEMOD Shape_Leng
    ## 1   304204  314848   <NA> -001-11-30   N  532.79178 2017-11-10  166077.83
    ## 2    48946   46456   <NA> -001-11-30   N 1035.20913 2019-04-26  210499.34
    ## 3  1385108 1472654   <NA> -001-11-30   Y   57.47215 2019-10-04   57253.86
    ## 4   200600  198683   <NA> -001-11-30   N  715.28747 2019-04-26  227933.33
    ## 5    80317   77042   <NA> -001-11-30   N 1324.30922 2019-04-26  276084.51
    ## 6    80026   76248   <NA> -001-11-30   N  881.82350 2018-07-18  334039.80
    ## 7   134905  127657   <NA> -001-11-30   N 1507.79455 2019-04-26  247508.47
    ## 8    88830   84148   <NA> -001-11-30   N  410.95932 2019-04-26  146916.78
    ## 9    50477   47220   <NA> -001-11-30   N  897.81864 2018-10-03  226955.16
    ## 10   82128   79843   <NA> -001-11-30   N 1116.81373 2018-12-07  235243.35
    ##    Shape_Area                       geometry
    ## 1  1379924372 MULTIPOLYGON (((605729 4737...
    ## 2  2681179340 MULTIPOLYGON (((229573.9 47...
    ## 3   148852180 MULTIPOLYGON (((595540.7 45...
    ## 4  1852586030 MULTIPOLYGON (((428899.3 46...
    ## 5  3429945130 MULTIPOLYGON (((169747.3 47...
    ## 6  2283912393 MULTIPOLYGON (((369644.2 47...
    ## 7  3905169964 MULTIPOLYGON (((161319.8 47...
    ## 8  1064379743 MULTIPOLYGON (((353386.9 46...
    ## 9  2325339613 MULTIPOLYGON (((464936.4 47...
    ## 10 2892534278 MULTIPOLYGON (((629506.7 49...

Merging the data sets

``` r
# merging the excel sheets first 

NY_lyme_tick = full_join(tick_surveillance, nineteen_twentyone_county_lyme, by = "NAME") %>% 
  select(-data.x, -data.y)

print(NY_lyme_tick)
```

    ## # A tibble: 147 × 11
    ##    NAME       total_ticks_collected tick_population_density total_tested
    ##    <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1 Albany                       366                    85.8          168
    ##  2 Chautauqua                   199                    36.7          107
    ##  3 Dutchess                     438                    66.8           51
    ##  4 Warren                        88                    13.8           87
    ##  5 Albany                       114                    37.2          103
    ##  6 Chemung                      271                   257.            50
    ##  7 Delaware                      75                    21.9           69
    ##  8 Tompkins                     131                    52.4           44
    ##  9 Oneida                       157                    78.5           50
    ## 10 Seneca                        51                    35.5           50
    ## # ℹ 137 more rows
    ## # ℹ 7 more variables: b_burgdorferi_percent <dbl>,
    ## #   a_phagocytophilum_percent <dbl>, b_microti_percent <dbl>,
    ## #   b_miyamotoi_percent <dbl>, county_centroid <chr>, event_count <dbl>,
    ## #   percent_rate <dbl>

``` r
# now merging the shape file with excel file 

NY_lyme_tick_county = NY_lyme_tick %>%
  left_join(ny_county, by = "NAME") %>% 
  janitor::clean_names()
# didn't delete the other parts from abbrev and later because they are part of the shapefile and if I delete I have to convert it to a dataframe making the shapefile no longer spatial. 

print(NY_lyme_tick_county)
```

    ## # A tibble: 147 × 28
    ##    name       total_ticks_collected tick_population_density total_tested
    ##    <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1 Albany                       366                    85.8          168
    ##  2 Chautauqua                   199                    36.7          107
    ##  3 Dutchess                     438                    66.8           51
    ##  4 Warren                        88                    13.8           87
    ##  5 Albany                       114                    37.2          103
    ##  6 Chemung                      271                   257.            50
    ##  7 Delaware                      75                    21.9           69
    ##  8 Tompkins                     131                    52.4           44
    ##  9 Oneida                       157                    78.5           50
    ## 10 Seneca                        51                    35.5           50
    ## # ℹ 137 more rows
    ## # ℹ 24 more variables: b_burgdorferi_percent <dbl>,
    ## #   a_phagocytophilum_percent <dbl>, b_microti_percent <dbl>,
    ## #   b_miyamotoi_percent <dbl>, county_centroid <chr>, event_count <dbl>,
    ## #   percent_rate <dbl>, abbrev <chr>, gnis_id <chr>, fips_code <chr>,
    ## #   swis <chr>, nysp_zone <chr>, pop1990 <dbl>, pop2000 <dbl>, pop2010 <dbl>,
    ## #   pop2020 <dbl>, dos_ll <chr>, dosll_date <date>, nyc <chr>, …
