library("tidyverse")

# The code below is almost the same as in the textbook chapter, with
# one major exception: when you execute R commands in the console window,
# results of calculations are displayed automatically.  When you execute
# R commands in a script, the results are only displayed if you explicitly
# ask for them displayed (using the print, cat or show functions).

##### READ IN AND CLEAN DATA
EmpData <- read_csv("sampledata/EmploymentData.csv")
#EmpData <- read_csv("https://bookdown.org/bkrauth/BOOK/sampledata/EmploymentData.csv")
# This is equivalent to names(EmpData)
EmpData %>% names() %>% print()
# This is equivalent to sqrt(2)
2 %>% sqrt() %>% print()
# This is equivalent to cat(sqrt(2)," is the square root of 2")
2 %>% sqrt() %>% cat(" is the square root of 2") %>% print()
# Make permanent changes to EmpData
# Change MonthYr to date format
# Add UnempPct and LFPPct
EmpData <- EmpData %>%
  mutate(MonthYr = as.Date(MonthYr, "%m/%d/%Y")) %>% 
  mutate(UnempPct = 100*UnempRate) %>% 
  mutate(LFPPct = 100*LFPRate)
print(EmpData)
# This will give all of the observations with unemployment rates over 12.5%

##### SHOW DATA
# This will take out all variables except a few
# This will sort the rows by unemployment rate
cat("Observations with unemployment > 12.5% \n")
EmpData %>% 
  filter(UnempPct > 12.5) %>%
  select(MonthYr, UnempPct, LFPPct, PrimeMinister) %>%
  arrange(UnempPct)  %>% print()
# This is what the same code looks like without the pipe
print(arrange(select(filter(EmpData,UnempPct>12.5),MonthYr,UnempPct,LFPPct,PrimeMinister),UnempPct))

##### ANALYZE DATA
# Summary statistics
cat("Summary statistics for EmpData \n")
summary(EmpData)  %>% print()
# Mean of a single variable
cat("mean of UnempPct = ", mean(EmpData$UnempPct),"\n")
# VAR calculates the sample variance
cat("variance of UnempPct = ", var(EmpData$UnempPct), "\n")
# SD calculates the standard deviation
cat("standard deviation of UnempPct = ", sd(EmpData$UnempPct), "\n")
# MEDIAN calculates the sample median
cat("median of UnempPct = ", median(EmpData$UnempPct), "\n")
# AnnPopGrowth has NA values
cat("mean of AnnPopGrowth = ", mean(EmpData$AnnPopGrowth),"\n")
cat("mean of AnnPopGrowth (dropping NA values) = ", mean(EmpData$AnnPopGrowth, na.rm = TRUE),"\n")
# Mean of each column
cat("Mean of each column \n")
EmpData %>%
  select(where(is.numeric)) %>%
  lapply(mean, na.rm = TRUE) %>% print()
# Standard deviation of each column
cat("Standard deviation of each column \n")
EmpData %>%
  select(where(is.numeric)) %>%
  lapply(sd, na.rm = TRUE) %>% print()
# COUNT creates a frequency table for discrete variables
cat("Simple frequency table of PrimeMinister \n")
EmpData %>% 
  count(PrimeMinister) %>% print()
# COUNT and CUT_INTERVAL create a binned frequency table
cat("Binned frequency table of UnempPct \n")
EmpData %>% 
  count(cut_interval(UnempPct, 6)) %>% print()

##### CREATE GRAPHS
# Histogram
p1 <- ggplot(data = EmpData,
       aes(x = UnempPct)) + 
  geom_histogram(binwidth = 0.5,
                 fill = "blue") +
  geom_density() +
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
# Time series (line) graph
p2 <- ggplot(data = EmpData,
       aes(x = MonthYr)) + 
  geom_line(aes(y = UnempPct),
            col = "blue") +
  geom_text(x = as.Date("1/1/2000", "%m/%d/%Y"),
            y = 15,
            label = "Unemployment",col="blue") +
  geom_line(aes(y = LFPPct),
            col = "red") +
  geom_text(x = as.Date("1/1/2000", "%m/%d/%Y"),
            y = 60,
            label = "LFP",
            col = "red") +
  labs(title = "Unemployment and LFP rates",
      subtitle = paste("January 1976 - January 2021 (",
                     nrow(EmpData),
                     " months)",
                     sep = "",
                     collapse = ""),
      caption = "Source: Statistics Canada, Labour Force Survey",
      tag = "Canada") +
  xlab("") +
  ylab("Percent")  

# This will display the histogram
show(p1)
# This will display the time series graph
show(p2)
