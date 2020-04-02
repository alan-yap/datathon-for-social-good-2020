# This scripts creates the outputs required in the final submission
# Most of them are wrapped up in function


# Joy Horng, Mar 2020


fit_df_2019 <- create_fit_data()

# School volunteers
fit_school_v <- 
  glm(SchoolVolunteers ~ ProgrammeName + week_of_year + day + start_hour + duration + term +  
             term:day, data = fit_df_2019); summary(fit_school_v)
pull_fit_result(fit_school_v)


# Hub volunteers
fit_hub_v <- 
  glm(HubVolunteers ~ ProgrammeName + week_of_year + day + start_hour + duration + term +  
        term:day, data = fit_df_2019); summary(fit_hub_v)
pull_fit_result(fit_hub_v)

# External volunteers
fit_external_v <- 
  glm(ExternalVolunteers ~ ProgrammeName + week_of_year + day + start_hour + duration + term +  
        term:day, data = fit_df_2019); summary(fit_external_v)
pull_fit_result(fit_external_v)

## Volunteer cross tab
fit_df_2019 %>% 
  summarise(total_school_volunteer = sum(SchoolVolunteers),
            total_hub_volunteer = sum(HubVolunteers),
            total_external_volunteer = sum(ExternalVolunteers))
