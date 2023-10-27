library(tidyverse)

polls_adjusted <- read_csv("covid_approval_polls_adjusted.csv")

us_standardizer <- function(x){
  replacement_string <- x
  for(word in replacement_string){
    if (str_to_lower(word) %in% c("united states of america", "united states", "us","usa")){
      replacement_string[which(replacement_string==word)] <- "United States"
    }
  }
  return(replacement_string)
}

#1
polls_adjusted <- polls_adjusted |>
  filter(!is.na(modeldate)) |>
  group_by(subject) |>
  mutate(average_approval = mean(approve_adjusted, na.rm = TRUE))
  
approve_hist <- ggplot(data = polls_adjusted, aes(x = approve_adjusted, fill = subject)) + 
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

approve_plot <- ggplot(data = polls_adjusted, aes(x = end_date, y = approve_fraction, color = party)) + 
  labs(title = "Approval of President's Handling of Covid−19 Pandemic", 
       subtitle = "From 2020 to 2022",
       x = "", y = "") +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700")) +
  geom_vline(aes(xintercept = as.Date("2021-01-20")), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

#Part 2
gtrend = read_csv("gtrend_data_clean.csv")

gtrend_plot <- ggplot(data = gtrend, aes(x = date, y = inflation/10)) + 
  labs(x = "Date", y = "") +
  geom_line(aes(y = inflation/10, color = "search for the word inflation")) +
  geom_line(aes(y = value, color = "inflation rate")) +
  geom_vline(aes(xintercept = as.Date("2020-03-01")), linetype = "dashed") +
  annotate("text", x=as.Date("2020-04-01"), y=10, label="Covid-19", hjust = 0) + 
  scale_color_manual(name = "", values = c("inflation rate" = "black", "search for the word inflation" = "red")) +
  facet_wrap(~geo)

#Part 3
#a
world <- ggplot2::map_data("world") |> filter(region != "Antarctica")

#b
forest_area_16 <- read_csv("forest_area.csv") |> filter(year == 2016)

#c
differences <- lubridate::setdiff(world$region, forest_area_16$entity)

#e
world <- world |> mutate(region = us_standardizer(region))

#f
world_area <- world |> left_join(forest_area_16, by= c("region" = "entity"))

#g
world_area_graph <- world_area %>%
  ggplot(aes(x = long, y = lat, group = group, fill = forest_area)) + ## edit
  geom_polygon(color = "black", linewidth = 0.09) + ## do not edit
  scale_fill_gradient(low = "yellow", high = "dark green") + ## do not edit
  ggthemes::theme_map() + ## do not edit
  labs(fill = "% Global Forest Area (2016)") + ## edit
  theme(legend.background=element_rect(fill = alpha("white", 0.5))) ## do not edit

print(approve_hist)
print(approve_plot)
print(gtrend_plot)
print(world_area_graph)
