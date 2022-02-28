#### load the libraies we need ####
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)

# import the csv
US_birth_date_from_1985_to_2020 <- read_csv(here::here("inputs/data/Birthrate.csv"))

# Plot the graph
ggplot(data=US_birth_date_from_1985_to_2020, aes(x=`year`, y=`brate_all`, group=1)) +
  geom_line()+
  geom_point() +
  ylim(50, 80) +
  xlab("Year") + ylab("Births per 1,000 women age 15-44")

