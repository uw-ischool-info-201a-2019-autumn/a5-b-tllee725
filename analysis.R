
library(dplyr)
library(knitr)
library(tidyr)
library(leaflet)
library(ggplot2)

df <- read.csv(file = "data/shootings-2018.csv", stringsAsFactors = FALSE)

total_events <- nrow(df)

lives_lost <- sum(df$num_killed)

impacted_city <- df %>%
  summarize(most_impacted_city = max(city))

impacted_state <- df %>%
  summarize(most_impacted_state = max(state))

average_lives <- df %>%
  summarize(average_lives_lost = mean(num_killed))

city_analysis <- df %>%
  group_by(city) %>%
  summarize(
    mean_killed_per_city = mean(num_killed),
    mean_injured_per_city = mean(num_injured)
  ) %>%
  arrange(desc(mean_killed_per_city))

city_analysis_table <- knitr::kable(city_analysis,
  caption = "Average Number of People Injured & Killed Per City"
)

date_of_event <- df %>%
  summarize(date[max(num_killed)])

address <- df %>%
  summarize(address[max(num_killed)])

city <- df %>%
  summarize(city[max(num_killed)])

state <- df %>%
  summarize(state[max(num_killed)])

number_killed <- df %>%
  summarize(num_killed[max(num_killed)])

number_injured <- df %>%
  summarize(num_injured[max(num_killed)])

date2 <- paste("Date: ", (df$date))
location <- paste("Address: ", (df$address), ", ",
  (df$city), ", ", (df$state),
  sep = ""
)

total_number_killed <- paste(
  "Number of People Killed: ",
  (df$num_killed)
)
total_number_injured <- paste(
  "Number of People Injured: ",
  (df$num_injured)
)
details <- paste(
  sep = "\n", location, "<br>",
  total_number_killed, "<br>",
  total_number_injured
)

df <- df %>%
  mutate(total = df$num_killed + df$num_injured)

map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = df$long, lat = df$lat, radius = df$total,
    label = lapply(details, htmltools::HTML)
  )


map2 <- ggplot(data = df) +
  geom_point(mapping = aes(x = df$num_killed, y = df$num_injured)) +
  geom_smooth(
    mapping = aes(x = df$num_killed, y = df$num_injured),
    method = "lm", formula = y ~ x, se = FALSE
  )
