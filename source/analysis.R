library(tidyverse)

incarceration <- read.csv(file = "../data/incarceration_trends.csv", stringsAsFactors = FALSE)

incarceration_country <- incarceration %>% 
  filter(year >= "1990", year <= "2016") %>% 
  group_by(year) %>%
  summarize(total_pop = sum(total_pop),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
            complete_prison = total_jail_pop + total_prison_pop,
            complete_black_pop = sum(black_prison_pop + black_jail_pop, na.rm = TRUE),
            complete_white_pop = sum(white_prison_pop + white_jail_pop, na.rm = TRUE))

incarceration_wa <- incarceration %>% 
  filter(year >= "1990", year <= "2016") %>% 
  filter(state == "WA") %>% 
  group_by(year) %>%
  summarize(total_pop = sum(total_pop),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
            complete_prison = total_jail_pop + total_prison_pop,
            complete_black_pop = sum(black_prison_pop + black_jail_pop, na.rm = TRUE),
            complete_white_pop = sum(white_prison_pop + white_jail_pop, na.rm = TRUE))

incarceration_2016 <- incarceration %>% 
  filter(year == "2016") %>% 
  select(county_name,
         fips,
         state,
         total_pop,
         total_jail_pop, 
         total_prison_pop, 
         black_male_prison_pop, 
         white_male_prison_pop,
         black_jail_pop) %>% 
  mutate(complete_incarceration = total_jail_pop + total_prison_pop) %>% 
  mutate(complete_proportion = complete_incarceration/total_pop*100) %>% 
  mutate(black_proportion = black_jail_pop/total_jail_pop*100)

# This year had the highest total jail population in Washington
max_jail_year <- incarceration_wa %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(year)

# with a jail population of
max_jail_population <- incarceration_wa %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(total_jail_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# This year had the highest population of people both in prison and in jail in Washington
complete_max_year <- incarceration_wa %>% 
  mutate(complete_incarceration = total_jail_pop + total_prison_pop) %>% 
  filter(complete_incarceration == max(complete_incarceration)) %>% 
  pull(year)

# with a complete incarcerated population of
complete_max_population <- incarceration_wa %>% 
  mutate(complete_incarceration = total_jail_pop + total_prison_pop) %>% 
  filter(complete_incarceration == max(complete_incarceration)) %>% 
  pull(complete_incarceration) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# In 2016, this county has the highest population of people in jail
max_jail_county <- incarceration_2016 %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(county_name)

# with this number of people
max_jail_county_people <- incarceration_2016 %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(total_jail_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# This county has the highest proportion of people in jail in 2016
max_jail_proportion_county <- incarceration_2016 %>% 
  filter(complete_proportion == max(complete_proportion)) %>% 
  pull(county_name)

# Number of Black people in jail, 2016
hard_numbers_2016_black <- incarceration_country %>% 
  filter(year == "2016") %>% 
  filter(complete_black_pop == max(complete_black_pop)) %>% 
  pull(complete_black_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Number of White people in jail, 2016
hard_numbers_2016_white <- incarceration_country %>% 
  filter(year == "2016") %>% 
  filter(complete_white_pop == max(complete_white_pop)) %>% 
  pull(complete_white_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Chart one

trend_colors <- c("#355C7D", "#107016", "#F8B195")

time_trend_chart <- ggplot(incarceration_wa,
                           aes(x = year)) +
  geom_line(aes(y = complete_prison,
                color = "Total Incarcerated"),
            size = 1.2,) +
  geom_line(aes(y = complete_black_pop,
                color = "Black"),
            size = 1.2,) +
  geom_line(aes(y = complete_white_pop,
                color = "White"),
            size = 1.2,) +
  scale_color_manual(values = trend_colors) +
  labs(title = "Number of People Incarcerated In Washington by Year",
       x = "Year",
       y = "Number of prisoners",
       color = "Race")


# chart two

comparison_data <- incarceration_wa %>% 
  rename(Black = complete_black_pop, White = complete_white_pop) %>%
  select(year, Black, White) %>%
  gather(key = Race, value = population, -year)

comparison_colors <- c("#355C7D", "#F8B195")

variable_comparison <- ggplot(comparison_data) +
  geom_col(mapping = aes(x = year, 
                         y = population, 
                         fill = Race), 
           position = "dodge") +
  scale_fill_manual(values = comparison_colors) +
  labs(title = "Total Prison Population by Race per Year",
       subtitle = "in Washington",
       x = "Year",
       y = "Prison Population Count")


# map

# Minimalist theme for maps, found in textbook
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

map_data <- incarceration_2016 %>%
  filter(state == "WA") %>%
  mutate(county = tolower(str_remove_all(county_name," County"))) %>%
  select(county, black_proportion)


complete_wa_map_data <- map_data("county","washington") %>%
  rename(county = subregion) %>% 
  left_join(map_data, by = "county")

wa_map <- ggplot(complete_wa_map_data) +
  geom_polygon(mapping = aes(x = long, 
                             y = lat, 
                             group = group, 
                             fill = black_proportion),
               color = "white", 
               size = 0.2) +
  coord_map() +
  scale_fill_gradient("% of Black People in Jail", 
                      low = "#e6e6f7", 
                      high = "#61151d") +
  blank_theme +
  labs(title = "Pecentage of Black People Jailed in Washington by County")