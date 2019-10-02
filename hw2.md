p8105\_hw2\_wz2507
================
Wurongyan Zhang
9/25/2019

## Problem 1

``` r
library(readxl)
library(tidyverse)
library(ggplot2)
```

``` r
# create the data set
trash <- read_excel("data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 1, range = "A2:N408") %>% 
  janitor::clean_names() %>% #clean names
  drop_na(dumpster) %>% #drop non dumpster data
  mutate(
    sports_balls = as.integer( sports_balls)) 
#round the number to integers
```

``` r
# create precipatation data for 2018
trash18 <- read_excel("data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 3, range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  mutate(Year = "2018") %>% #change the year to 2018
  mutate(month = month.name) %>% #change to full month names
  drop_na()  #omit rows without data
```

``` r
# create precipatation data for 2018
trash17 <- read_excel("data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 4, range = "A2:B14")%>% 
  janitor::clean_names() %>% 
  mutate(month = month.name) %>% #change to full month names
  mutate(Year = "2017") #change the year to 2018
```

``` r
#combine precipatation data for 2017 and 2018
precip1718 <- as.tibble(rbind(trash17, trash18))
```

``` r
ball17 <- tibble(x = pull(trash, year ), y = pull(trash, sports_balls )) %>% 
  filter(x == "2017")
```

Comment: The number of observations in Mr.Trash Wheel data set is 344
with 14 variables and in precipatation data set is 19 with 3 variables.
key variables?? The total precipatation in 2018 is 23.5. The median of
sports ball is 8. The key variables for trash includes dumpter
number(dumpster), data of collection, amount of total
litter(weight\_tons, volume\_cubic\_yards) and litter
type(plastic\_bottles, polystyrene, cigarette\_butts, glass\_bottles,
grocery\_bags, chip\_bags, sports\_balls, etc.). For the precipatation
data, it includes month, year and the total precipitation amounts.

## Problem 2

``` r
# create pols data set
pols <- read_csv("data/pols-month.csv") %>% 
  #convert the date into year, month and day
  separate(mon, into = c("year", "month", "day")) %>% 
  #change month into full name
  mutate(month = as.integer(month)) %>% 
  mutate(month = month.name[month]) %>% 
  #specify if the presidwent is democrat or republican
  mutate(prez_dem = recode(prez_dem, "1" = "democrat", "0" = "republican")) %>% 
  mutate(president =  prez_dem) %>% 
  #delete prez_dem, prez_gop and day
  select(-prez_dem, -prez_gop) %>% 
  select(-day)%>% 
  mutate(year = as.integer(year))
```

``` r
# create snp data set
snp <- read_csv("data/snp.csv") %>% 
  #convert the date into year, month and day
  separate(date, into = c("month", "day", "year")) %>%
  #change month into full name
  mutate(month = as.integer(month)) %>%
  #arrange the order by year, month and others
  arrange(year, month) %>% 
  select(year, month, everything()) %>% 
  mutate(month = month.name[month]) %>% 
  mutate(year = as.integer(year))
```

``` r
#create unemployment data set
une <- read_csv("data/unemployment.csv") %>% 
  janitor::clean_names() %>% 
  # change month to a column
  pivot_longer(jan:dec,
               names_to = "month", 
               values_to = "unemployment rate") %>%
  #change month into full name
  mutate(month = factor(month, labels = month.name))%>%
  #arrange the data by year, month and others
  arrange(year, month) %>% 
  select(year, month, everything())
```

``` r
#combine those three data sets
join = full_join(pols, une)
```

    ## Warning: Column `month` joining character vector and factor, coercing into
    ## character vector

``` r
all = full_join(join, snp)
```

Comments: The dimension of the data set after combination is 828 entries
with 12 total columns(if we delete prez\_gop, prez\_dem and day). The
range of years is 1947 to 2015. However, pols data starts from 1947, snp
data starts from 1950 and une data starts from 1947. Therefore, the
combined data range from 1947 to 2015. The names of key variables are –

for pols data:

year: year of the count

month: month of the count of the year

rep\_dem: the number of democratic representatives on the associated
date

gov\_gop: the number of republican governors on the associated date

sen\_gop: the number of republican senators on the associated date

rep\_gop: the number of republican representatives on the associated
date

gov\_dem: the number of democratic governors on the associated date

sen\_dem: the number of democratic senators on the associated date

president: indicator of whether the president was democratic or
republican

for snp data:

year: the date of the observation

month: the month of the observation

day: the day of the observation

close: the closing values of the S\&P stock index on the associated date

for unemployment data:

year: the year of the measurements on that row

month: the month of the measurements on that row

rate: unemployment rate

## Problem 3

``` r
# create the function of capital letter of the first names
first <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
```

``` r
# create the data set
# change column names
baby <- read_csv("data/Popular_Baby_Names.csv", skip = 1,col_names =c("birth_year", "gender", "race", "first_name", "count","rank")) %>% 
  # change the first name into capital letter of the first character with rest of lower case
  mutate(
    first_name = str_to_lower(first_name), first_name = first(first_name)) %>%
  # lower the case for gender
  # unify race names
  mutate(
    gender = str_to_lower(gender), race = str_to_lower(race)) %>% 
  mutate(race = replace(race, race == "asian and paci", "asian and pacific islander"), 
         race = replace(race,race == "black non hisp","black non hispanic"), 
         race = replace(race,race == "white non hisp","white non hispanic")) %>% 
  #delete duplicate rows
  distinct()
```

# i.

``` r
# rank of Olivia
# create the data set, pull out data for Olivia
olivia = baby[pull(baby, first_name) == "Olivia",] %>% 
  # arrange the set by birth year
  arrange(birth_year)
```

``` r
# create the table
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

# ii.

``` r
# rank the male children 
male_name = baby[pull(baby, gender) == "male",] %>% 
  group_by(birth_year, race) %>% 
  slice(which.min(rank))
# create the table
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

# iii.

``` r
# pull the data of male, white born in 2016
male = baby[pull(baby, gender) == "male",] 

white = male[pull(male, race) == "white non hispanic",] 
white16 = white[pull(white, birth_year) == "2016",]
```

``` r
# create the ggplot

white16 %>% 
  ggplot(aes(x = rank, y = count )) + geom_point() 
```

![](hw2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
