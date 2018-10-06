2017 White House Employee Salary Analysis - Gender pay gap
================
Amin Yakubu
10/6/2018

Loading tidyverse package and setting global options

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_bw() + theme(legend.position = "bottom"))
```

``` r
library(tidyverse)
```

Reading and cleaning the dataset

``` r
wh_pay = read_csv("./data//white_house_2017_salaries_clean.csv") %>% 
  select(-(starts_with("X")))
```

    ## Warning: Missing column names filled in: 'X7' [7], 'X8' [8], 'X9' [9],
    ## 'X10' [10], 'X11' [11]

    ## Parsed with column specification:
    ## cols(
    ##   NAME = col_character(),
    ##   STATUS = col_character(),
    ##   SALARY = col_integer(),
    ##   `PAY BASIS` = col_character(),
    ##   TITLE = col_character(),
    ##   GENDER = col_character(),
    ##   X7 = col_character(),
    ##   X8 = col_character(),
    ##   X9 = col_character(),
    ##   X10 = col_character(),
    ##   X11 = col_character()
    ## )

In 2017, the White House spent a total of $ 35766744 on the salaries of 377 employees.

Below is a table showing the top 10 earners

``` r
wh_pay %>% 
  top_n(1, SALARY) %>% 
  arrange(desc(SALARY)) %>% 
  knitr::kable()
```

| NAME           | STATUS   |  SALARY| PAY BASIS | TITLE                 | GENDER |
|:---------------|:---------|-------:|:----------|:----------------------|:-------|
| House, Mark S. | Detailee |  187100| Per Annum | SENIOR POLICY ADVISOR | M      |

``` r
wh_pay %>% 
  filter(SALARY > 100000) %>% 
  group_by(GENDER) %>% 
  summarize(NUMBER = n()) %>% 
  knitr::kable()
```

| GENDER |  NUMBER|
|:-------|-------:|
| F      |      51|
| M      |     102|

Of the 153 staff earning more than $100,000 only 51 of them are female.

``` r
wh_pay %>% 
  filter(SALARY < 100000) %>% 
  group_by(GENDER) %>% 
  summarize(NUMBER = n()) %>% 
  knitr::kable()
```

| GENDER |  NUMBER|
|:-------|-------:|
| F      |     115|
| M      |     109|

Of the remaining 224 that earn less than $100,000, 115 of them are female. This include Ivanka Trump and her husband, Jared Kusner, and Reed Cordish all of whom have a salary of 0. Gary Cohn, millionare former CEO of Goldman Sachs has a salary of 30,000.

``` r
wh_pay %>%
  group_by(GENDER) %>%
  summarize(n = n())
```

    ## # A tibble: 2 x 2
    ##   GENDER     n
    ##   <chr>  <int>
    ## 1 F        166
    ## 2 M        211

``` r
wh_pay %>% 
  group_by(TITLE) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
```

    ## # A tibble: 68 x 2
    ##    TITLE                                  n
    ##    <chr>                              <int>
    ##  1 SPECIAL ASSISTANT TO THE PRESIDENT    77
    ##  2 DEPUTY ASSISTANT TO THE PRESIDENT     28
    ##  3 ASSISTANT TO THE PRESIDENT            26
    ##  4 DIRECTOR                              24
    ##  5 EXECUTIVE ASSISTANT                   20
    ##  6 ASSOCIATE DIRECTOR                    18
    ##  7 STAFF ASSISTANT                       18
    ##  8 ASSOCIATE COUNSEL TO THE PRESIDENT    13
    ##  9 INFORMATION SERVICES OPERATOR         12
    ## 10 DEPUTY DIRECTOR                       11
    ## # ... with 58 more rows

``` r
wh_pay %>% 
  group_by(GENDER) %>% 
  summarize(n = n(),
            mean = mean(SALARY),
            median = median(SALARY)) 
```

    ## # A tibble: 2 x 4
    ##   GENDER     n    mean median
    ##   <chr>  <int>   <dbl>  <dbl>
    ## 1 F        166  84886.  77000
    ## 2 M        211 102729.  95000

``` r
wh_pay %>%
  group_by(GENDER) %>%
  summarize(n = n(),
            mean = mean(SALARY),
            median = median(SALARY))  %>%
  ggplot(aes(x = GENDER, y = mean, fill = GENDER)) + 
  geom_col(width = 0.5) 
```

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-1.png" width="90%" />

``` r
wh_pay %>%
  group_by(GENDER) %>%
  summarize(n = n(),
            mean = mean(SALARY),
            median = median(SALARY))  %>%
  ggplot(aes(x = GENDER, y = median, fill = GENDER)) + 
  geom_col(width = 0.5) 
```

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-2.png" width="90%" />

``` r
ggplot(wh_pay, aes(x = GENDER, y = SALARY, fill = GENDER)) +
  geom_boxplot()
```

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-3.png" width="90%" />

``` r
ggplot(wh_pay, aes(x = STATUS, fill = GENDER)) + 
  geom_bar(width = 0.5)
```

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-4.png" width="90%" />

``` r
ggplot(wh_pay, aes(x = SALARY, fill = GENDER)) + 
  geom_density(alpha = .4, adjust = .5, color = "blue")
```

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-5.png" width="90%" />

``` r
ggplot(wh_pay, aes(x = SALARY, fill = GENDER)) + 
  geom_histogram(position = "dodge") 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="2017_white_house_salary_analysis_files/figure-markdown_github/unnamed-chunk-8-6.png" width="90%" />
