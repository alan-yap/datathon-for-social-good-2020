# This scripts creates the outputs required in the final submission
# Most of them are wrapped up in functions


# Joy Horng, Mar 2020

create_weekly_participant_df <- function(){
  df_2019 <- read_clean_data() %>% 
  mutate(day = factor(day, c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))) %>% 
  filter(year == 2019)
  
 df_2019 %>% 
  group_by(day, week_of_year) %>% 
  summarise(child_participants = sum(ChildParticipants),
            adult_participants = sum(AdultParticipants),
            n = n()) %>% 
  mutate(avg_child_participants = child_participants / n,
         avg_adult_participants = adult_participants / n) %>% 
  drop_na() %>% 
  arrange(week_of_year, day)
}
  
# --- heatmap_child
plot_heatmap_child <- function(){
  create_weekly_participant_df() %>% 
    mutate(text = paste0("Day: ", day, "\n", "Week of year: ", week_of_year, "\n", "Average child participants: ", round(avg_child_participants))) %>% 
    filter(avg_child_participants != 0) %>% 
    select(day, week_of_year, avg_child_participants, text) %>% 
    ggplot(aes(x = day, y = week_of_year, fill = round(avg_child_participants), text = text)) + 
    geom_raster() +
    scale_fill_gradient(low = "grey", high = "orange") + 
    labs(x = "Day", y = "Week of year", fill = "Average child parcipants\nper activity") + 
    theme(plot.title = element_text(size = 12),
          axis.title = element_text(size = 8),
          legend.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8))
}


# --- headmap_adult
plot_heatmap_adult <- function(){
  create_weekly_participant_df() %>% 
  mutate(text = paste0("Day: ", day, "\n", "Week of year: ", week_of_year, "\n", "Average adult participants: ", round(avg_adult_participants))) %>% 
    filter(avg_adult_participants != 0) %>% 
    select(day, week_of_year, avg_adult_participants, text) %>% 
    ggplot(aes(x = day, y = week_of_year, fill = round(avg_adult_participants), text = text)) + 
    geom_raster() +
    scale_fill_gradient(low = "grey", high = "orange") + 
    labs(x = "Day", y = "Week of year", fill = "Average adult parcipants\nper activity") + 
    theme(plot.title = element_text(size = 12),
          axis.title = element_text(size = 8),
          legend.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8))
}

ggplotly(p, tooltip = "text")

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

# ----- regression model
create_fit_data <- function(){
  read_clean_data() %>% 
  mutate(day = factor(day, c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))) %>% 
  filter(year == 2019) %>% 
  mutate(duration = as.duration(EndTime - StartTime),
         start_hour = as.factor(start_hour),
         day = as.character(day),
         term = case_when(
           week_of_year <= 16  ~ 1,
           week_of_year <= 28  ~ 2,
           week_of_year <= 40  ~ 3,
           week_of_year > 40  ~ 4,
         ),
         term = as.factor(term))
}

pull_fit_result <- function(fit){
  
  header <- colnames(create_fit_data())
  
  regressors <- fit$coefficients %>% names()
  
  tibble(variables = names(fit$coefficients),
         coef = coef(summary(fit))[,1],
         p_value = coef(summary(fit))[,4]
         ) %>% 
    filter(p_value < 0.01 & coef > 0) %>% 
    arrange(desc((coef))) %>% 
    slice(1:8) %>% 
    mutate(variable_types = case_when(
      grepl("ProgrammeName", variables)   ~  "Activity type",
      grepl("start_hour", variables)      ~  "Starting hour of activity",
      grepl("term", variables)            ~  "School term",
      grepl("day", variables)             ~  "Day of week"
      ),
      variable = gsub(paste(header, collapse = "|"), "", variables),
    ) %>% 
    select(variable_types, variable)
  
}

fit_adult <- function(){
  fit_data <- create_fit_data()
  
  fit <- glm(AdultParticipants ~ ProgrammeName + week_of_year + day + start_hour + duration + term + 
               term:day, data = fit_data)
  pull_fit_result(fit)
}

fit_child <- function(){
  fit_data <- create_fit_data()
  
  fit <- glm(ChildParticipants ~ ProgrammeName + week_of_year + day + start_hour + duration + term +  
                     term:day, data = fit_data); summary(fit_data)
  pull_fit_result(fit)
}
