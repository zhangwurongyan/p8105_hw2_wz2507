p8105\_hw2\_wz2507
================
Wurongyan Zhang
9/25/2019

## Problem 1

``` r
library(readxl)
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.2.1 ─

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
trash <- read_excel("data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 1, range = "A2:N408") %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>%
  mutate(
    sports_balls = as.integer( sports_balls))
```

``` r
trash18 <- read_excel("data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 3, range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  mutate(Year = "2018") %>%
  mutate(month = month.name) %>% 
  drop_na()  
```

``` r
trash17 <- read_excel("data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 4, range = "A2:B14")%>% 
  janitor::clean_names() %>% 
  mutate(month = month.name) %>% 
  mutate(Year = "2017") 
```

``` r
precip1718 <- as.tibble(rbind(trash17, trash18))
```

    ## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

Be sure to note the number of observations in both resulting datasets,
and give examples of key variables. For available data, what was the
total precipitation in 2018? What was the median number of sports balls
in a dumpster in
2017.

``` r
ball17 <- tibble(x = pull(trash, year ), y = pull(trash, sports_balls )) %>% 
  filter(x == "2017")
```

The number of observations in Mr.Trash Wheel data set is 344 with 14
variables and in precipatation data set is 19 with 3 variables. key
variables?? The total precipatation in 2018 is 23.5. The median of
sports ball is 8.

## Problem 2

``` r
pols <- read_csv("data/pols-month.csv") %>% 
  separate(mon, into = c("year", "month", "day")) %>%
  mutate(month = as.integer(month)) %>% 
  mutate(month = month.name[month]) %>% 
  mutate(prez_dem = recode(prez_dem, "1" = "democrat", "0" = "republican")) %>% 
  mutate(president =  prez_dem) %>% 
  select(-prez_dem, -prez_gop) %>% 
  select(-day)%>% 
  mutate(year = as.integer(year))
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
snp <- read_csv("data/snp.csv") %>% 
  separate(date, into = c("month", "day", "year")) %>%
  mutate(month = as.integer(month)) %>%
  arrange(year, month) %>% 
  select(year, month, everything()) %>% 
  mutate(month = month.name[month]) %>% 
  mutate(year = as.integer(year))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
une <- read_csv("data/unemployment.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(jan:dec,
               names_to = "month", 
               values_to = "unemployment rate") %>%
  mutate(month = factor(month, labels = month.name))%>%
  arrange(year, month) %>% 
  select(year, month, everything())
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
join = full_join(pols, une)
```

    ## Joining, by = c("year", "month")

    ## Warning: Column `month` joining character vector and factor, coercing into
    ## character vector

``` r
all = full_join(join, snp)
```

    ## Joining, by = c("year", "month")

Explain briefly what each dataset contained, and describe the resulting
dataset (e.g. give the dimension, range of years, and names of key
variables).

The dimension of the data set is 828 entries with 12?? total columns(if
we delete prez\_gop, prez\_dem and day). The range of years is 1947 to
2015. However, pols data starts from 1947, snp data starts from 1950 and
une data starts from 1947. The names of key variables are –

for pols data: mon: date of the count rep\_dem: the number of democratic
representatives on the associated date gov\_gop: the number of
republican governors on the associated date sen\_gop: the number of
republican senators on the associated date rep\_gop: the number of
republican representatives on the associated date gov\_dem: the number
of democratic governors on the associated date sen\_dem: the number of
democratic senators on the associated date president: indicator of
whether the president was democratic or republican

for snp data: year: the date of the observation month: the month of the
observation day: the day of the observation close: the closing values of
the S\&P stock index on the associated date

For unemployment data: year: the year of the measurements on that row
month: the month of the measurements on that row each month: percentage
of unemployment in each month of the associated year rate: unemployment
rate

## Problem 3

``` r
first <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
```

``` r
baby <- read_csv("data/Popular_Baby_Names.csv", skip = 1,col_names =c("birth_year", "gender", "race", "first_name", "count","rank")) %>% 
  mutate(
    first_name = str_to_lower(first_name), first_name = first(first_name)) %>%
  mutate(
    gender = str_to_lower(gender), race = str_to_lower(race)) %>% 
  mutate(race = replace(race, race == "asian and paci", "asian and pacific islander"), 
         race = replace(race,race == "black non hisp","black non hispanic"), 
         race = replace(race,race == "white non hisp","white non hispanic")) %>% 
  distinct()
```

    ## Parsed with column specification:
    ## cols(
    ##   birth_year = col_double(),
    ##   gender = col_character(),
    ##   race = col_character(),
    ##   first_name = col_character(),
    ##   count = col_double(),
    ##   rank = col_double()
    ## )

most\_popular\_male\_names \<- names\[pull(names,gender)==‘male’,\] %\>%
group\_by(birth\_yr,race) %\>% slice(which.min(rank))

``` r
olivia = baby[pull(baby, first_name) == "Olivia",] %>% 
  arrange(birth_year)
```

``` r
pivot_wider(
  olivia, id_cols = "birth_year", names_from = "race", values_from = "rank"
)
```

    ## # A tibble: 6 x 5
    ##   birth_year `asian and pacifi… `black non hispa… hispanic `white non hisp…
    ##        <dbl>              <dbl>             <dbl>    <dbl>            <dbl>
    ## 1       2011                  4                10       18                2
    ## 2       2012                  3                 8       22                4
    ## 3       2013                  3                 6       22                1
    ## 4       2014                  1                 8       16                1
    ## 5       2015                  1                 4       16                1
    ## 6       2016                  1                 8       13                1

``` r
male_name = baby[pull(baby, gender) == "male",] %>% 
  group_by(birth_year, race) %>% 
  slice(which.min(rank))

pivot_wider(
  male_name, id_cols = "birth_year", names_from = "race", values_from = "first_name"
)
```

    ## # A tibble: 6 x 5
    ## # Groups:   birth_year [6]
    ##   birth_year `asian and pacifi… `black non hispa… hispanic `white non hisp…
    ##        <dbl> <chr>              <chr>             <chr>    <chr>           
    ## 1       2011 Ethan              Jayden            Jayden   Michael         
    ## 2       2012 Ryan               Jayden            Jayden   Joseph          
    ## 3       2013 Jayden             Ethan             Jayden   David           
    ## 4       2014 Jayden             Ethan             Liam     Joseph          
    ## 5       2015 Jayden             Noah              Liam     David           
    ## 6       2016 Ethan              Noah              Liam     Joseph

``` r
male = baby[pull(baby, gender) == "male",] 

white = male[pull(male, race) == "white non hispanic",] 
white16 = white[pull(white, birth_year) == "2016",]
```

``` r
library(ggplot2)
white16 %>% 
  ggplot(aes(x = rank, y = count )) + geom_point() 
```

![](hw2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
