# Use R to produce pictures used in the textbook
library(tidyverse)
library(ggplot2)

# Picture 1: Supply and demand graph to be used as the icon
# in the "economics" box
supplydemand <- tibble(Q=1:20)
supplydemand$PS <- supplydemand$Q
supplydemand$PD <- 21 - supplydemand$Q

sd_plot <- ggplot(data=supplydemand,
              mapping=aes(x=Q)) + 
  geom_line(mapping=aes(y=PS),
            colour="#EB6E1F",
            size=3) + 
  geom_line(mapping=aes(y=PD),
            colour="#002D62",
            size=3) +
  labs(x="Q",
       y="P") +
  annotate("text",
           x=20,
           y=19,
           label="S",
           col="#EB6E1F") +
  annotate("text",
           x=20,
           y=2,
           label="D",
           col="#002D62") +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(size=2,
                               arrow=arrow(type='open',
                                           length = unit(10,'pt'))))
show(sd_plot)

png(file="economics.png",bg="transparent",height=390,width=600)
show(sd_plot)
dev.off()


# Picture 2: Histogram of Canadian unemployment rate, to be
# used as the logo.

EmpData <- read_csv("sampledata/EmploymentData.csv") %>%
  mutate(MonthYr = as.Date(MonthYr, "%m/%d/%Y")) %>% 
  mutate(UnempPct = 100*UnempRate) %>% 
  mutate(LFPPct = 100*LFPRate)

emp_hist <- ggplot(data = EmpData,
       aes(x = UnempPct)) + 
  geom_histogram(binwidth = 0.5,
                 fill = "#002D62") +
  geom_density() +
  annotate("text",
           x=11,
           y=90,
           label="Introductory",
           col="#EB6E1F",
           size=15) +
  annotate("text",
           x=11,
           y=75,
           label="Statistics for",
           col="#EB6E1F",
           size=15) +
  annotate("text",
           x=11,
           y=60,
           label="Economics",
           col="#EB6E1F",
           size=15) +
  labs(title = "Unemployment rate",
       subtitle = paste("January 1976 - January 2021 (",
                        nrow(EmpData),
                        " months)",
                        sep = "",
                        collapse = ""),
       caption = "Source: Statistics Canada, Labour Force Survey",
       tag = "Canada") +
  xlab("Unemployment rate, %") +
  ylab("Count")

show(emp_hist)

png(file="logo.png",bg="transparent")
show(emp_hist)
dev.off()


