# Data cleaning for internal data 
# 
# This scripts does the basic data cleaning, including:
# 1. Drop rows with either child participant/adult participant above 90th percentile  
# 2. Drop unimportant columns 
# 3. Create new variables: days of week, school terms (using provided school terms data), month, year
# 4. Impute missing variables
#
# Feb 2020

# ====================== Read data =======================
suppressWarnings(
  hub_raw <- read_csv("1_Data/Internal data track/Datathon - CHA activities 2016-2020.csv"))

# read_csv will return a warning for 151524 parsing failures in three cols where the first two cols should be ignored: 
# "LastModifiedDateTime", "LastModifiedDateTime.1" & "DSS_ClientParticipants"
# 
# There is only one parsing failure in DSS_ClientParticipants: obs 156494 where DSS_ClientParticipants is NA


# A general function to prepare the entire internal data set 
read_clean_data <- function(){
  
 
  
  # ===================== Clean ============================
  
  hub_filtered <- hub_raw %>% 
    #drop unused and duplicated columns
    select(-LastModifiedBy, -LastModifiedDateTime, -Status, -LastModifiedBy.1, -LastModifiedDateTime.1, 
           -IsActive, -Status.1, -StartDate, -X1, -ProgrammeID.1, -ProgrammeCategoryID.1) %>%
    #drop observations with child or adult participant >90th percentile
    filter(ChildParticipants < quantile(ChildParticipants, 0.90) | AdultParticipants < quantile(AdultParticipants, 0.90)) %>% 
    # parse date and time into the correct format and create two new date-time columns Start and End
    mutate(ActivityDate = dmy(ActivityDate)) %>% 
    # determine duration, day, month and year of activity as new distinct columns
    mutate(day = wday(ActivityDate, label = TRUE),
           month = month(ActivityDate),
           year = year(ActivityDate),
           week_of_year = week(ActivityDate),
           start_hour = hour(seconds_to_period(StartTime)))
  
  # For missing volunteer numbers 2016-2018, we first find the average participants to volunteers ratio for each program
  # and assume the total number of volunteers
  # For the breakdown between school, hub and external 
  # We look at the average breakdown by hub
  # Finally we work out an imputed hub-wise activity-wise volunteer numbers
  participant_volunteer_ratio_by_activity <- hub_filtered %>% 
    filter(year == 2019) %>% 
    group_by(ProgrammeID, ProgrammeName, ProgrammeCategoryID) %>% 
    summarise(total_participants = sum(ChildParticipants, AdultParticipants, DSS_ClientParticipants, ReferralParticipants, na.rm = TRUE),
              total_volunteers = sum(HubVolunteers, SchoolVolunteers, ExternalVolunteers, na.rm = TRUE)) %>% 
    mutate(p_v_ratio = total_participants / total_volunteers,
           p_v_ratio = ifelse(is.na(p_v_ratio) | p_v_ratio == Inf, 0, p_v_ratio)) %>% 
    select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, p_v_ratio)
  
  overall_volunteer_breakdown <- hub_filtered %>% 
    filter(year == 2019) %>% 
    group_by() %>% 
    summarise(total_volunteers = sum(SchoolVolunteers, HubVolunteers, ExternalVolunteers, na.rm = TRUE),
              prop_school = sum(SchoolVolunteers) / total_volunteers,
              prop_external = sum(ExternalVolunteers) / total_volunteers,
              prop_hub = sum(HubVolunteers) / total_volunteers)
  
  volunteer_breakdown_by_hub <- hub_filtered %>% 
    filter(year == 2019) %>% 
    group_by(HubRandomID) %>% 
    summarise(total_volunteers = sum(SchoolVolunteers, HubVolunteers, ExternalVolunteers, na.rm = TRUE),
              prop_school = sum(SchoolVolunteers) / total_volunteers,
              prop_external = sum(ExternalVolunteers) / total_volunteers,
              prop_hub = sum(HubVolunteers) / total_volunteers) %>% 
    replace_na(list(prop_school = 0, prop_external = 0, prop_hub = 0)) %>% # for hubs that have never had volunteers in 2019
    select(HubRandomID, prop_external, prop_hub, prop_school) %>% 
    right_join(enframe(unique(hub_filtered$HubRandomID), name = NULL), by = c('HubRandomID' = 'value')) %>% 
    replace_na(list(prop_school = overall_volunteer_breakdown$prop_school, 
                    prop_external = overall_volunteer_breakdown$prop_external, 
                    prop_hub = overall_volunteer_breakdown$prop_hub)) # assume population distribution for the hubs that don't have activities in 2019
  
  # Final cleaned data set
  hub_filtered %>% 
    mutate(imputation = ifelse(year < 2019, 1, 0), # imputation flag
           total_participants = ChildParticipants + AdultParticipants + DSS_ClientParticipants + ReferralParticipants,
           total_volunteers = SchoolVolunteers + HubVolunteers + ExternalVolunteers) %>% 
    left_join(participant_volunteer_ratio_by_activity, by = c('ProgrammeID', 'ProgrammeName', 'ProgrammeCategoryID')) %>% 
    left_join(volunteer_breakdown_by_hub, by = 'HubRandomID') %>% 
    # impute the number of volunteers
    mutate(SchoolVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_school, SchoolVolunteers),
           HubVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_hub, SchoolVolunteers),
           ExternalVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_external, SchoolVolunteers),
           # clean up
           SchoolVolunteers = ifelse(is.na(SchoolVolunteers) | SchoolVolunteers == Inf, 0, round(SchoolVolunteers)),
           HubVolunteers = ifelse(is.na(HubVolunteers) | HubVolunteers == Inf, 0, round(HubVolunteers)),
           ExternalVolunteers = ifelse(is.na(ExternalVolunteers) | ExternalVolunteers == Inf, 0, round(ExternalVolunteers))) %>% 
    # create variables
    mutate(duration = EndTime - StartTime,
           session_flag = ifelse(StartTime == 00:00:00 | EndTime == 00:00:00 | is.na(StartTime),
                                 "Not a session", "Session") # if the start time and end time are both 00:00, or if the time is NA, this is not a session
           ) %>% 
    select(HubRandomID, ProgrammeID, ProgrammeName, ProgrammeCategoryID, ActivityDate, StartTime, EndTime, SchoolVolunteers, HubVolunteers,
           ExternalVolunteers, ChildParticipants, AdultParticipants, DSS_ClientParticipants, ReferralParticipants, EngagedCount,
           day, month, year, week_of_year, start_hour) %>% 
    arrange(ActivityDate, StartTime, EndTime, ProgrammeID, HubRandomID)
  
}