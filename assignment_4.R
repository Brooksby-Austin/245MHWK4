library(tidyverse)

polls_adjusted <- read_csv("covid_approval_polls_adjusted.csv")

#1
polls_adjusted <- polls_adjusted |>
  filter(!is.na(modeldate)) |>
  group_by(subject) |>
  mutate(average_approval = mean(approve_adjusted, na.rm = TRUE))
  
ggplot(data = polls_adjusted, aes(x = approve_adjusted, fill = subject)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = average_approval), linetype = "dashed") +
  labs(title = "American approval of Biden and Trump’s response to coronavirus",
       subtitle = "From 2020 to 2022",
       x = "Approval", y = "Count", fill = "President") +
  facet_wrap(~subject) +
  scale_fill_manual(values = c("#008FD5", "#FF2700")) +
  theme_minimal() +
  theme(legend.position = "bottom")

#2
polls_adjusted <- polls_adjusted |>
  mutate(end_date = mdy(enddate), approve_fraction = approve_adjusted/100) |>
  filter(party == "R"| party == "D"| party == "I")

ggplot(data = polls_adjusted, aes(x = end_date, y = approve_fraction, color = party)) + 
  labs(title = "Approval of President's Handling of Covid−19 Pandemic", 
       subtitle = "From 2020 to 2022",
       x = "", y = "") +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700")) +
  geom_vline(aes(xintercept = as.Date("2021-01-20")), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
