# Data cleaning for internal data 
# 
# This scripts does the basic data cleaning, including:
# 1. Drop rows with either child participant/adult participant above 90th percentile  
# 2. Drop unimportant columns 
# 3. Create new variables: days of week, school terms (using provided school terms data), month, year
# 4. Impute missing variables
#
#
# Alan Yap, Riya Kundu, Joy Horng, Feb 2020

#======================Read data=========================================

source ('setup.R')

suppressWarnings(
hub_raw <- read_csv("1_Data/Internal data track/Datathon - CHA activities 2016-2020.csv"))

# read_csv will return a warning for 151524 parsing failures in three cols where the first two cols should be ignored: 
# "LastModifiedDateTime", "LastModifiedDateTime.1" & "DSS_ClientParticipants"
# 
# There is only one parsing failure in DSS_ClientParticipants: obs 156494 where DSS_ClientParticipants is NA
 
# problems(hub_raw)
# unique((problems(hub_raw))$col)
# check <- problems(hub_raw) %>% filter(col == "DSS_ClientParticipants")

#get all path to school terms excel sheets
all_excel_path <- dir_ls(path = "1_Data/Internal data track/School terms dates/", regexp = "xlsx")

#create function to read all sheets in all school terms excel sheets and store as school_terms

school_terms <- data.frame()

for (x in all_excel_path) {
  
  #get all sheet names
  sheets_to_read <- excel_sheets(as.character(all_excel_path[1]))
  
  #read all sheets, add tabname, and then bind rows
  terms <- bind_rows(lapply(1:length(sheets_to_read),
                            function(i)read_excel(as.character(x),
                                                  sheet = sheets_to_read[i]) %>%
                              mutate(state = sheets_to_read[i])))
  
  school_terms <- bind_rows(school_terms, terms)
  
}

names(school_terms)[5] <- "region"

#### Currently there are errors reading 2019 and 2020 spreadsheets due to date formats in those two files not being compatible. Please rectify


#=====================DATA CLEANING============================================

hub_clean <- hub_raw %>% 
  
  #drop unused and duplicated columns
  select(-LastModifiedBy, -LastModifiedDateTime, -Status, -LastModifiedBy.1, -LastModifiedDateTime.1, -IsActive, -Status.1, -StartDate, -X1, -ProgrammeID.1, -ProgrammeCategoryID.1) %>%
  
  #parse date and time into the correct format and create two new date-time columns Start and End
  mutate(ActivityDate = dmy(ActivityDate)) %>% 
  
  #determine duration, day, month and year of activity as new distinct columns
  mutate(day = wday(ActivityDate, label = TRUE),
         month = month(ActivityDate),
         year = year(ActivityDate),
         week_of_year = week(ActivityDate)) %>% 
  
  #drop observations with child or adult participant >90th percentile
  filter(ChildParticipants < quantile(ChildParticipants, 0.90) | AdultParticipants < quantile(AdultParticipants, 0.90))

#ref data frame for all unique program ID and names
program_data_cube <- hub_clean %>% 
  select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ShortName, ServiceTypeID, CategoryName, ShortName.1) %>% 
  distinct()

#identify most frequent start time, week of year and duration by activity and hub in 2019
# 
# hub_2019 <- hub_clean %>% 
#   filter(year == 2019 & !str_detect(Start, "NA") | !str_detect(End, "NA")) %>% 
#   mutate(Start = ymd_hms(Start),
#          End = ymd_hms(End),
#          duration = time_length(interval(Start, End), "hours")) %>% 
#   separate(Start, c("StartDate", "StartTime"), sep = " ") 
# 
# most_freq_start_time <- hub_2019 %>% 
#   group_by(ProgrammeID, HubRandomID) %>% 
#   count(StartTime) %>% 
#   top_n(1) %>% 
#   #take average if tie
#   select(-n) %>% 
#   summarise(StartTime = mean(times(StartTime)))
#   
# most_freq_week <- hub_2019 %>% 
#   group_by(ProgrammeID, HubRandomID) %>% 
#   count(week_of_year) %>% 
#   top_n(1) %>% 
#   #take average if tie
#   select(-n) %>% 
#   summarise(week_of_year = mean(week_of_year))
# 
# most_freq_duration <- hub_2019 %>% 
#   group_by(ProgrammeID, HubRandomID) %>% 
#   count(duration) %>% 
#   top_n(1) %>% 
#   #take average if tie
#   select(-n) %>% 
#   summarise(duration = mean(duration))

#identify most frequent volunteer counts by activity and hub in 2019

#=====================IMPUTE MISSING VALUES============================================

# For missing start time and end time 2016-2018, we find the most frequent seesion run time by each hub and program in 2019
# And assume it's also run in the same session time in 2016-18
most_frequent_session_time <- hub_clean %>% 
  group_by(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ShortName.1, StartTime, EndTime) %>%
  summarise(n = n()) %>% 
  arrange(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ShortName.1, desc(n)) %>% 
  drop_na() %>% 
  filter(StartTime != 00:00:00 & EndTime != 00:00:00) %>% 
  ungroup() %>% 
  distinct(ProgrammeID, .keep_all = TRUE) %>% 
  rename(imputed_start_time = StartTime,
         imputed_end_time = EndTime) %>% 
  select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ShortName.1, imputed_start_time, imputed_end_time)

# For missing volunteer numbers 2016-2018, we first find the average participants to volunteers ratio for each program
# and assume the total number of volunteers
# For the breakdown between school, hub and external 
# We look at the average breakdown by hub
# Finally we work out an imputed hub-wise activity-wise volunteer numbers
participant_volunteer_ratio_by_activity <- hub_clean %>% 
  filter(year == 2019) %>% 
  group_by(ProgrammeID, ProgrammeName, ProgrammeCategoryID) %>% 
  summarise(total_participants = sum(ChildParticipants, AdultParticipants, DSS_ClientParticipants, ReferralParticipants, na.rm = TRUE),
            total_volunteers = sum(HubVolunteers, SchoolVolunteers, ExternalVolunteers, na.rm = TRUE)) %>% 
  mutate(p_v_ratio = total_participants / total_volunteers,
         p_v_ratio = ifelse(is.na(p_v_ratio) | p_v_ratio == Inf, 0, p_v_ratio)) %>% 
  select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, p_v_ratio)

volunteer_breakdown_by_hub <- hub_clean %>% 
  filter(year == 2019) %>% 
  group_by(HubRandomID) %>% 
  summarise(total_volunteers = sum(SchoolVolunteers, HubVolunteers, ExternalVolunteers, na.rm = TRUE),
            prop_school = sum(SchoolVolunteers) / total_volunteers,
            prop_external = sum(ExternalVolunteers) / total_volunteers,
            prop_hub = sum(HubVolunteers) / total_volunteers) %>% 
  replace_na(list(prop_school = 0, prop_external = 0, prop_hub = 0)) %>% # for hubs that have never had volunteers in 2019
  select(HubRandomID, prop_external, prop_hub, prop_school) %>% 
  right_join(enframe(unique(hub_clean$HubRandomID), name = NULL), by = c('HubRandomID' = 'value')) %>% 
  replace_na(list(prop_school = 0.293, prop_external = 0.377, prop_hub = 0.33)) # assume population distribution for the hubs that don't have activities in 2019

# Final cleaned data set
hub_final <- hub_clean %>% 
  mutate(imputation = ifelse(year < 2019, 1, 0),
         total_participants = ChildParticipants + AdultParticipants + DSS_ClientParticipants + ReferralParticipants,
         total_volunteers = SchoolVolunteers + HubVolunteers + ExternalVolunteers) %>% 
  left_join(most_frequent_session_time, by = c('ProgrammeID', 'ProgrammeName', 'ProgrammeCategoryID', 'ShortName.1')) %>% 
  left_join(participant_volunteer_ratio_by_activity, by = c('ProgrammeID', 'ProgrammeName', 'ProgrammeCategoryID')) %>% 
  left_join(volunteer_breakdown_by_hub, by = 'HubRandomID') %>% 
  mutate(SchoolVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_school, SchoolVolunteers),
         HubVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_hub, SchoolVolunteers),
         ExternalVolunteers = ifelse(imputation == 1, total_participants / p_v_ratio * prop_external, SchoolVolunteers),
         StartTime = ifelse(imputation == 1, imputed_start_time, StartTime),
         EndTime = ifelse(imputation == 1, imputed_end_time, EndTime),
         # clean up
         SchoolVolunteers = ifelse(is.na(SchoolVolunteers) | SchoolVolunteers == Inf, 0, round(SchoolVolunteers)),
         HubVolunteers = ifelse(is.na(HubVolunteers) | HubVolunteers == Inf, 0, round(HubVolunteers)),
         ExternalVolunteers = ifelse(is.na(ExternalVolunteers) | ExternalVolunteers == Inf, 0, round(ExternalVolunteers)),
         start_hour = hour(seconds_to_period(StartTime)),
         StartTime = sprintf("%02i:%02i", hour(seconds_to_period(StartTime)), minute(seconds_to_period(StartTime))),
         EndTime = sprintf("%02i:%02i", hour(seconds_to_period(EndTime)), minute(seconds_to_period(EndTime))),
         StartTime = ifelse(StartTime == "NA:NA", NA, StartTime),
         EndTime = ifelse(EndTime == "NA:NA", NA, EndTime)
         ) %>% 
  select(HubRandomID, ProgrammeID, ProgrammeName, ProgrammeCategoryID, ActivityDate, StartTime, EndTime, SchoolVolunteers, HubVolunteers,
         ExternalVolunteers, ChildParticipants, AdultParticipants, DSS_ClientParticipants, ReferralParticipants, EngagedCount,
         day, month, year, week_of_year, start_hour) %>% 
  arrange(ActivityDate, StartTime, EndTime, ProgrammeID, HubRandomID)
