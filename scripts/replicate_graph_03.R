# This file can be used to replicate figure 5
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(haven)
library(tidyr)
library(sqldf)

# Read the dta
age_comp_pop <- read_dta(here::here("inputs/data/agecomp-seer.dta"))

# Only include useful columns
age_comp_pop <- subset(age_comp_pop, select = -c(fem1519, fem2034, fem3544, stname))

# group by and find the sum of all columns

age_comp_pop_grouped <- group_by(age_comp_pop, year) %>% summarise(across(everything(), sum))

# generate vector of column names that start with fem
age_comp_pop_grouped_start_with_fem <-
  select(age_comp_pop_grouped, starts_with("fem"))

fem_col_name <- colnames(age_comp_pop_grouped_start_with_fem)

# transform wide to long
age_comp_pop_long <- gather(age_comp_pop_grouped, mage, fem, fem_col_name)

# generate new columns
age_comp_pop_long$mage <- gsub("^.{0,3}", "", age_comp_pop_long$mage)

# transform it into a numeric value
age_comp_pop_long$mage <- as.numeric(as.character(age_comp_pop_long$mage))

age_comp_pop_long$cohort <- with(age_comp_pop_long, year - mage)
age_comp_pop_long$cohort2 <- ''

# assign different numbers based on years
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1968 & age_comp_pop_long$cohort <= 1972] <- 1
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1973 & age_comp_pop_long$cohort <= 1977] <- 2
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1978 & age_comp_pop_long$cohort <= 1982] <- 3
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1983 & age_comp_pop_long$cohort <= 1987] <- 4
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1988 & age_comp_pop_long$cohort <= 1992] <- 5
age_comp_pop_long$cohort2[age_comp_pop_long$cohort >= 1993 & age_comp_pop_long$cohort <= 1997] <- 6
age_comp_pop_long <- subset(age_comp_pop_long, cohort2 != '')

names(age_comp_pop_long)[names(age_comp_pop_long) == 'fem'] <- 'pop'

age_comp_pop_long_group <- group_by(age_comp_pop_long, cohort2, mage) %>% summarise(pop_sum = sum(pop))

# read data files
cohort_analysis <- read_dta(here::here("inputs/data/nchs_cohort_analysis.dta"))

# merge two file together
merged_data <- merge(cohort_analysis, age_comp_pop_long_group, by = c("cohort2", "mage"))

# define new columns
merged_data$brate <- with(merged_data, numbirth/pop_sum*1000)

data_with_only_cohort_and_brate <- subset(merged_data, select = c(cohort2, brate, mage))

data_with_only_cohort_and_brate <- data_with_only_cohort_and_brate[order(data_with_only_cohort_and_brate$cohort2, data_with_only_cohort_and_brate$mage),]

# accmulate all the values
data_with_only_cohort_and_brate$brate <- ave(data_with_only_cohort_and_brate$brate, data_with_only_cohort_and_brate$cohort2, FUN=cumsum)

data_with_only_cohort_and_brate_grouped <- group_by(data_with_only_cohort_and_brate, cohort2, mage) %>% summarise(cum_brate = sum(brate) / 1000)

names(data_with_only_cohort_and_brate_grouped)[names(data_with_only_cohort_and_brate_grouped) == 'cohort2'] <- 'cohort'

final_data_wide_format <- spread(data_with_only_cohort_and_brate_grouped, key = cohort, value = cum_brate,  sep = '')

# drop all na value
data_with_cohort1 <- drop_na(final_data_wide_format, cohort1)
data_with_cohort2 <- drop_na(final_data_wide_format, cohort2)
data_with_cohort3 <- drop_na(final_data_wide_format, cohort3)
data_with_cohort4 <- drop_na(final_data_wide_format, cohort4)
data_with_cohort5 <- drop_na(final_data_wide_format, cohort5)
data_with_cohort6 <- drop_na(final_data_wide_format, cohort6)

# draw the plot
ggplot() +
  geom_line(data = data_with_cohort1, aes(x = mage, y = cohort1, color = "1968-1972"))+
  geom_line(data = data_with_cohort2, aes(x = mage, y = cohort2, color = "1973–1977")) +
  geom_line(data = data_with_cohort3, aes(x = mage, y = cohort3, color = "1977–1982")) +
  geom_line(data = data_with_cohort4, aes(x = mage, y = cohort4, color = "1983–1987")) +
  geom_line(data = data_with_cohort5, aes(x = mage, y = cohort5, color = "1988–1992")) +
  geom_line(data = data_with_cohort6, aes(x = mage, y = cohort6, color = "1993–1997")) +
  scale_color_manual(name = "Birth Cohorts", 
                     values = c("1968-1972" = "green", 
                                "1973–1977" = "blue", 
                                "1977–1982" = "orange", 
                                "1983–1987" = "gray",
                                "1988–1992" = "yellow",
                                "1993–1997" = "lightblue"
                     )) +
  xlab("Mother's Age") + ylab("Children ever born") + 
  ggtitle("Children Ever Born by Mother’s Age and Birth Cohort")








