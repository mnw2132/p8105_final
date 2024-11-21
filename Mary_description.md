Cleaning and Description of Analytic dataset
================
2024-11-21

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
  select(-total_sites_visited, -school_districts_2016_shp, -local_waterfront_revitalization_program_lwrp_communities, -new_york_zip_codes, -counties, -nys_senate_districts, -a_phagocytophilum_percent, -b_microti_percent, -b_miyamotoi_percent) %>% 
mutate(data = "ticks")


print(tick_surveillance)
```

    ## # A tibble: 138 × 8
    ##     year NAME       total_ticks_collected tick_population_density total_tested
    ##    <dbl> <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1  2021 Albany                       366                    85.8          168
    ##  2  2021 Chautauqua                   199                    36.7          107
    ##  3  2021 Dutchess                     438                    66.8           51
    ##  4  2021 Warren                        88                    13.8           87
    ##  5  2020 Albany                       114                    37.2          103
    ##  6  2020 Chemung                      271                   257.            50
    ##  7  2020 Delaware                      75                    21.9           69
    ##  8  2020 Tompkins                     131                    52.4           44
    ##  9  2020 Oneida                       157                    78.5           50
    ## 10  2020 Seneca                        51                    35.5           50
    ## # ℹ 128 more rows
    ## # ℹ 3 more variables: b_burgdorferi_percent <dbl>, county_centroid <chr>,
    ## #   data <chr>

Read in shp file for NY counties

``` r
ny_county =  st_read("./Counties.shp")  %>%
  select(-DOS_LL, -DOSLL_DATE)
```

    ## Reading layer `Counties' from data source 
    ##   `/Users/nickywilliams/Desktop/Data Science/p8105_final/Counties.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 62 features and 17 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 105571.4 ymin: 4480943 xmax: 779932.1 ymax: 4985476
    ## Projected CRS: NAD83 / UTM zone 18N

``` r
print(ny_county)
```

    ## Simple feature collection with 62 features and 15 fields
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
    ##    POP2010 POP2020 NYC CALC_SQ_MI    DATEMOD Shape_Leng Shape_Area
    ## 1   304204  314848   N  532.79178 2017-11-10  166077.83 1379924372
    ## 2    48946   46456   N 1035.20913 2019-04-26  210499.34 2681179340
    ## 3  1385108 1472654   Y   57.47215 2019-10-04   57253.86  148852180
    ## 4   200600  198683   N  715.28747 2019-04-26  227933.33 1852586030
    ## 5    80317   77042   N 1324.30922 2019-04-26  276084.51 3429945130
    ## 6    80026   76248   N  881.82350 2018-07-18  334039.80 2283912393
    ## 7   134905  127657   N 1507.79455 2019-04-26  247508.47 3905169964
    ## 8    88830   84148   N  410.95932 2019-04-26  146916.78 1064379743
    ## 9    50477   47220   N  897.81864 2018-10-03  226955.16 2325339613
    ## 10   82128   79843   N 1116.81373 2018-12-07  235243.35 2892534278
    ##                          geometry
    ## 1  MULTIPOLYGON (((605729 4737...
    ## 2  MULTIPOLYGON (((229573.9 47...
    ## 3  MULTIPOLYGON (((595540.7 45...
    ## 4  MULTIPOLYGON (((428899.3 46...
    ## 5  MULTIPOLYGON (((169747.3 47...
    ## 6  MULTIPOLYGON (((369644.2 47...
    ## 7  MULTIPOLYGON (((161319.8 47...
    ## 8  MULTIPOLYGON (((353386.9 46...
    ## 9  MULTIPOLYGON (((464936.4 47...
    ## 10 MULTIPOLYGON (((629506.7 49...

Merging the data sets

``` r
# merging the excel sheets first 

NY_lyme_tick = full_join(tick_surveillance, nineteen_twentyone_county_lyme, by = "NAME") %>% 
  select(-data.x, -data.y)

print(NY_lyme_tick)
```

    ## # A tibble: 147 × 9
    ##     year NAME       total_ticks_collected tick_population_density total_tested
    ##    <dbl> <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1  2021 Albany                       366                    85.8          168
    ##  2  2021 Chautauqua                   199                    36.7          107
    ##  3  2021 Dutchess                     438                    66.8           51
    ##  4  2021 Warren                        88                    13.8           87
    ##  5  2020 Albany                       114                    37.2          103
    ##  6  2020 Chemung                      271                   257.            50
    ##  7  2020 Delaware                      75                    21.9           69
    ##  8  2020 Tompkins                     131                    52.4           44
    ##  9  2020 Oneida                       157                    78.5           50
    ## 10  2020 Seneca                        51                    35.5           50
    ## # ℹ 137 more rows
    ## # ℹ 4 more variables: b_burgdorferi_percent <dbl>, county_centroid <chr>,
    ## #   event_count <dbl>, percent_rate <dbl>

``` r
# now merging the shape file with excel file 

NY_lyme_tick_county = NY_lyme_tick %>%
  left_join(ny_county, by = "NAME") %>% 
  janitor::clean_names()
# didn't delete the other parts from abbrev and later because they are part of the shapefile and if I delete I have to convert it to a dataframe making the shapefile no longer spatial. 

print(NY_lyme_tick_county)
```

    ## # A tibble: 147 × 24
    ##     year name       total_ticks_collected tick_population_density total_tested
    ##    <dbl> <chr>                      <dbl>                   <dbl>        <dbl>
    ##  1  2021 Albany                       366                    85.8          168
    ##  2  2021 Chautauqua                   199                    36.7          107
    ##  3  2021 Dutchess                     438                    66.8           51
    ##  4  2021 Warren                        88                    13.8           87
    ##  5  2020 Albany                       114                    37.2          103
    ##  6  2020 Chemung                      271                   257.            50
    ##  7  2020 Delaware                      75                    21.9           69
    ##  8  2020 Tompkins                     131                    52.4           44
    ##  9  2020 Oneida                       157                    78.5           50
    ## 10  2020 Seneca                        51                    35.5           50
    ## # ℹ 137 more rows
    ## # ℹ 19 more variables: b_burgdorferi_percent <dbl>, county_centroid <chr>,
    ## #   event_count <dbl>, percent_rate <dbl>, abbrev <chr>, gnis_id <chr>,
    ## #   fips_code <chr>, swis <chr>, nysp_zone <chr>, pop1990 <dbl>, pop2000 <dbl>,
    ## #   pop2010 <dbl>, pop2020 <dbl>, nyc <chr>, calc_sq_mi <dbl>, datemod <date>,
    ## #   shape_leng <dbl>, shape_area <dbl>, geometry <MULTIPOLYGON [m]>

``` r
summary(NY_lyme_tick_county)
```

    ##       year          name           total_ticks_collected
    ##  Min.   :2019   Length:147         Min.   :   1.0       
    ##  1st Qu.:2019   Class :character   1st Qu.:  82.0       
    ##  Median :2020   Mode  :character   Median : 129.0       
    ##  Mean   :2020                      Mean   : 220.5       
    ##  3rd Qu.:2021                      3rd Qu.: 226.0       
    ##  Max.   :2021                      Max.   :1893.0       
    ##  NA's   :9                         NA's   :9            
    ##  tick_population_density  total_tested    b_burgdorferi_percent
    ##  Min.   :  0.25          Min.   :  1.00   Min.   : 0.00        
    ##  1st Qu.: 27.94          1st Qu.: 50.00   1st Qu.:48.00        
    ##  Median : 45.17          Median : 50.50   Median :54.15        
    ##  Mean   : 56.61          Mean   : 96.04   Mean   :53.72        
    ##  3rd Qu.: 76.07          3rd Qu.:114.25   3rd Qu.:60.98        
    ##  Max.   :256.90          Max.   :500.00   Max.   :80.00        
    ##  NA's   :9               NA's   :9        NA's   :9            
    ##  county_centroid     event_count       percent_rate       abbrev         
    ##  Length:147         Min.   :   3.00   Min.   :  1.60   Length:147        
    ##  Class :character   1st Qu.:  95.75   1st Qu.: 25.30   Class :character  
    ##  Mode  :character   Median : 242.00   Median : 60.30   Mode  :character  
    ##                     Mean   : 348.34   Mean   : 93.86                     
    ##                     3rd Qu.: 493.00   3rd Qu.:130.30                     
    ##                     Max.   :1269.00   Max.   :446.80                     
    ##                     NA's   :3         NA's   :3                          
    ##    gnis_id           fips_code             swis            nysp_zone        
    ##  Length:147         Length:147         Length:147         Length:147        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     pop1990           pop2000           pop2010           pop2020       
    ##  Min.   :   5279   Min.   :   5379   Min.   :   4836   Min.   :   5107  
    ##  1st Qu.:  51981   1st Qu.:  51401   1st Qu.:  51599   1st Qu.:  49532  
    ##  Median :  89123   Median :  93765   Median :  93772   Median :  91283  
    ##  Mean   : 239097   Mean   : 249278   Mean   : 255168   Mean   : 263303  
    ##  3rd Qu.: 220756   3rd Qu.: 219846   3rd Qu.: 219607   3rd Qu.: 232125  
    ##  Max.   :2300664   Max.   :2465326   Max.   :2504700   Max.   :2736074  
    ##  NA's   :2         NA's   :2         NA's   :2         NA's   :2        
    ##      nyc              calc_sq_mi         datemod             shape_leng    
    ##  Length:147         Min.   :  33.73   Min.   :2017-10-25   Min.   : 57254  
    ##  Class :character   1st Qu.: 532.79   1st Qu.:2018-02-12   1st Qu.:176097  
    ##  Mode  :character   Median : 837.15   Median :2018-11-01   Median :235243  
    ##                     Mean   : 937.57   Mean   :2018-11-07   Mean   :230237  
    ##                     3rd Qu.:1257.21   3rd Qu.:2019-04-26   3rd Qu.:280626  
    ##                     Max.   :2818.74   Max.   :2020-05-19   Max.   :390792  
    ##                     NA's   :2         NA's   :7            NA's   :2       
    ##    shape_area                 geometry  
    ##  Min.   :8.735e+07   MULTIPOLYGON :147  
    ##  1st Qu.:1.380e+09   epsg:26918   :  0  
    ##  Median :2.168e+09   +proj=utm ...:  0  
    ##  Mean   :2.428e+09                      
    ##  3rd Qu.:3.256e+09                      
    ##  Max.   :7.301e+09                      
    ##  NA's   :2

# Description of data

The data set includes 147 observations and 147 variables. Of the
variables, all are `numeric` except for those that are characters (name,
county_centroid, swis, nysp_zone, dos_ll, nyc) and multipolygon
(geometry). These contain information about tick surveillance within New
York counties, including the number of ticks collected, the number
tested for Lyme disease, and the tick population density. The dataset
also includes demographic and spatial data of human populations within
each county. The information can help identify the high-risk areas and
guide public health interventions.
