
## International Border Closure Policies Due to COVID-19

**Plots created by:** [Thuy
Nguyen](https://www.linkedin.com/in/nguyendata/)

### Introduction

“The COVID Border Accountability Project (COBAP) provides a dataset of
\>1000 policies systematized to reflect a complete timeline of new
country-level restrictions on movement across international borders”.

Our full dataset is hosted on Harvard Dataverse here:
<https://doi.org/10.7910/DVN/U6DJAC>

### Read in data

``` r
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(forcats)
```

``` r
d <- rio::import(here::here("data", "policy_list.csv")) %>% 
  select(1:23) %>% janitor::clean_names() 
# convert to date format, select needed columns
data <- d %>% 
  mutate(start_asdate = lubridate::mdy(d$start_date), # date format mm_dd_yyyy
         end_asdate = lubridate::mdy(d$end_date)) %>% 
  select(c(country_name, iso3, policy_type, air, land, sea, start_asdate, end_asdate)) %>%   drop_na()
# data for the line chart
 pd_line <- data %>% 
  select(country_name, iso3, start_asdate) %>% 
  add_count(week = floor_date(start_asdate, "week")) %>%   #count in week interval
  arrange(start_asdate) %>% drop_na()
```

#### Plot 1: Overall policies

``` r
p_allpolicies <- pd_line %>% 
  ggplot(aes(week, n)) +
  geom_vline(xintercept = as.Date("2020-03-11"), linetype = "dotted", color = "orange") +
   geom_text(label = "Pandemic declared (March 11)", 
             size = 5, color = "orange", hjust = -0.2,
             x = as.Date("2020-02-26"), y = 230) + 
  geom_point(aes(week, n), size = 6, shape = 21, fill = "#3E80B6", color = "#3E80B6") +
  geom_line(color = "#3E80B6", size = 1) +
  
  #show values inside the points
  geom_text(aes(label = n), color = "white", size = 3) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
 
  # First country to adop a boder closure American\Samoa; 
    annotate(geom = "text", x = as.Date("2020-01-15"), y = .5,
                label = "American\nSamoa", vjust = -1.5, hjust = 0.9, color = "#FD69B3") +
    annotate("segment", x = as.Date("2020-01-01"), xend = as.Date("2020-01-01"), 
               y = 25, yend = 3, colour = "#3E80B6", size=.5, alpha = .5) +   
  
  #countries adopted policies during the second week 2020-Jan-26 to end of 2020-Feb-01
   annotate(geom = "text", x = as.Date("2020-01-26"), y = .5,
                label = "Mozambique\nSingapore\n", vjust = -4, hjust = 0.95, color = "#FD69B3") +
  annotate(geom = "text", x = as.Date("2020-01-26"), y = .5,
                label = "Israel\nItaly\nPakistan\nRussia\nPalau\nMongolia", vjust = -1, hjust = 0, color = "#FD69B3") +
  annotate("segment", x = as.Date("2020-01-26"), xend = as.Date("2020-01-26"), 
               y = 115, yend = 10, colour = "#3E80B6", size=.5, alpha = .5) +
  
  # additional info about the dataset - total policies 
  annotate(geom = "text", x = as.Date("2020-11-01"), y = 170,
                label = "#Policies coded", fontface = "bold", color = "white") +
  annotate(geom = "text", x = as.Date("2021-01-20"), y = 170,
                label = "1368",  fontface = "bold", size = 10, color = "orange") +
    # about the dataset - total countries
  annotate(geom = "text", x = as.Date("2020-11-01"), y = 150,
                label = "#Countries covered", fontface = "bold", color = "white") +
  annotate(geom = "text", x = as.Date("2021-01-20"), y = 150,
                label = "196", fontface = "bold", size = 10, color = "orange") +
  # about the dataset - complete policies 
  annotate(geom = "text", x = as.Date("2020-10-25"), y = 130,
                label = "#Complete closure policies", fontface = "bold", color = "white") +
  annotate(geom = "text", x = as.Date("2021-01-20"), y = 130,
                label = "432", fontface = "bold", size = 10, color = "orange") +
  # labs and theme
  labs(x = "",
       y = "Number of Policies Issued",
       title = "Number of Border Closure Policies Adopted Worldwide Due to COVID-19", 
       caption = "Source: Covid Border Accountability Project (COBAP)") +
  theme(
    axis.title.y = element_text(colour = "white", size = 12),
    axis.text.y = element_text(color="white",face = "bold", size = 12),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(colour = "white", size = 18),
    plot.subtitle = element_text(colour = "white"),
    plot.caption = element_text(colour = "white"),
    plot.background = element_rect(fill = "#193853"),
    panel.background = element_rect(fill = "#193853"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
p_allpolicies
```

![](README_files/figure-gfm/plot%201-1.png)<!-- -->

#### Plot 2: Countries with high number of border closure policies issued

``` r
# Countries issued more than 10 policies
more10policies <- data %>% 
  group_by(country_name) %>% 
  count() %>% filter(n > 10) %>% arrange(desc(n)) %>% 
  mutate(country_name = as.factor(country_name)) %>% 
  mutate(country_name = fct_relevel(country_name, "Cocos (Keeling) Islands", "Romania", "Latvia", "Cyprus", "Finland", "Germany", "American Samoa", "Austria", "Aruba", "Myanmar", "Brazil", "Curaçao", "Nepal", "Seychelles", "Spain"))

  #plot
p_more10 <- more10policies %>%   
ggplot(aes(country_name, n)) +
  geom_segment(aes(x = country_name, xend = country_name, y = 0, yend = n), color = "skyblue", size = 2) +
  geom_point(color = "orange", size = 6) +
  labs(title = "Countries Issued the Most Border Closure Policies",
    subtitle = "Related to COVID-19, Jan-2020 to Apr-2021",
    x = "",
    y = "Number of Policy Issued",
    caption = "Source: Covid Border Accountability Project (COBAP)") +
  coord_flip() +
  theme(
    axis.title.x = element_text(colour = "white", size = 15),
    axis.text.y = element_text(color="white", 
                           size=14),
    axis.text.x = element_text(face="bold", color="white", 
                           size=14),
    plot.title = element_text(colour = "white", size = 18),
    plot.subtitle = element_text(colour = "white"),
    plot.caption = element_text(colour = "white"),
    plot.background = element_rect(fill = "#1B2547"),
    panel.background = element_rect(fill = "#1B2547"),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
p_more10
```

![](README_files/figure-gfm/plot2-1.png)<!-- -->

#### [Authors of the dataset](https://doi.org/10.7910/DVN/U6DJAC):

Shiraef, Mary A.; Weiss, Mark A.; Hirst, Cora; Walker, Bryn; Nguyen,
Thuy; Kline, Camilla; Bhaskaran, Aadya; Beling, Elizabeth; Mattar,
Layth; Amme, Matthew; Shum, Maggie; Sweere, Johanna; Brantley, Susanna;
Schenoni, Luis; Lewis-Beck, Colin; Selvaraj, Yashwini; Jackson,
Cayleigh; Lazar, Nikolas; Musetti, Rachel; Naseer, Sarah; Taylor, Noah;
Gradie, Amalia; Yu, William, 2020, “The COVID Border Accountability
Project (COBAP): Mapping Travel and Immigration Policy Responses to
COVID-19”, <https://doi.org/10.7910/DVN/U6DJAC>, Harvard Dataverse, V21,
UNF:6:51bUOj0lSRrMwups3A7RCg== \[fileUNF\]
