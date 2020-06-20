remove(list = objects())

library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)

dat <- read_csv("14100287.csv")

dat2 <- dat %>%
  filter(Statistics == "Estimate",
         `Data type` == "Seasonally adjusted",
         GEO == "Canada",
         Sex == "Both sexes", 
         `Age group` == "15 years and over") %>%
  select(REF_DATE, 
         GEO, 
         `Labour force characteristics`, 
         Sex,
         `Age group`,
         VALUE) %>% 
  pivot_wider(names_from = `Labour force characteristics`,
              values_from = VALUE)  %>% 
  mutate(REF_DATE = ymd(REF_DATE, truncated = 1))

ggplot(data = dat2, 
       mapping = aes(x= REF_DATE,y= `Unemployment rate`)) + 
  geom_line()
