# 
# 
# This scripts ......
#
# Joy Horng, Mar 2020

df_2019 <- read_clean_data() %>% 
  mutate(day = factor(day, c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))) %>% 
  filter(year == 2019)

week_of_day_df <- df_2019 %>% 
  group_by(day, week_of_year) %>% 
  summarise(child_participants = sum(ChildParticipants),
            adult_participants = sum(AdultParticipants),
            n = n()) %>% 
  mutate(avg_child_participants = child_participants / n,
         avg_adult_participants = adult_participants / n) %>% 
  drop_na() %>% 
  arrange(week_of_year, day)

heatmap_child <- week_of_day_df %>% 
  mutate(text = paste0("day: ", day, "\n", "week of year: ", week_of_year, "\n", "Child participants: ", round(avg_child_participants))) %>% 
  filter(avg_child_participants != 0) %>% 
  select(day, week_of_year, avg_child_participants, text) %>% 
  ggplot(aes(x = day, y = week_of_year, fill = round(avg_child_participants), text = text)) + 
  geom_tile() +
  scale_fill_gradient(low = nous_colour("S5"), high = nous_colour("C3"))

heatmap_adult <- week_of_day_df %>% 
  mutate(text = paste0("day: ", day, "\n", "week of year: ", week_of_year, "\n", "Adult participants: ", round(avg_adult_participants))) %>% 
  filter(avg_adult_participants != 0) %>% 
  select(day, week_of_year, avg_adult_participants, text) %>% 
  ggplot(aes(x = day, y = week_of_year, fill = round(avg_adult_participants), text = text)) + 
  geom_tile() +
  scale_fill_gradient(low = nous_colour("S5"), high = nous_colour("C3"))

ggplotly(heatmap_child, tooltip = "text")
ggplotly(heatmap_adult, tooltip = "text")


duration_df <- df_2019 %>% 
  mutate(duration = as.duration(EndTime - StartTime)) %>% 
  group_by(duration) %>% 
  summarise(child_participants = sum(ChildParticipants),
            adult_participants = sum(AdultParticipants),
            n = n()) %>% 
  mutate(avg_child_participants = child_participants / n,
         avg_adult_participants = adult_participants / n) %>% 
  filter(n > 100) %>% # only look at sample size > 100
  arrange(desc(avg_child_participants), desc(avg_adult_participants))

start_hour_df <- df_2019 %>% 
  group_by(start_hour) %>% 
  summarise(child_participants = sum(ChildParticipants),
            adult_participants = sum(AdultParticipants),
            n = n()) %>% 
  mutate(avg_child_participants = child_participants/n,
         avg_adult_participants = adult_participants/n) %>% 
  drop_na() %>% 
  filter(n > 100) %>% # only look at sample size > 100
  arrange(desc(avg_child_participants), desc(avg_adult_participants))
