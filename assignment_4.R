library(tidyverse)

polls_adjusted <- read_csv("covid_approval_polls_adjusted.csv")

#1
#a
polls_adjusted <- polls_adjusted |>
  group_by(subject) |>
  mutate(average_approval = mean(approve_adjusted, na.rm = TRUE))

#b
hist(polls_adjusted$approve_adjusted, xlab = "Approval", ylab= "Count")
theme_minimal()

#c