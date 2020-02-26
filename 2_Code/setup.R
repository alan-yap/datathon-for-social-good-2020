library(tidyverse)
library(lubridate)
library(readxl)


#======================Read data=========================================

hub_raw <- read_csv("1_Data/Internal data track/Datathon - CHA activities 2016-2020.csv")

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

#=====================DATA CLEANING============================================

hub_clean <- hub_raw %>% 
  
  #drop unused and duplicated columns
  select(-LastModifiedBy, -LastModifiedDateTime, -Status, -LastModifiedBy.1, -LastModifiedDateTime.1, -IsActive, -Status.1, -StartDate, -X1, -ProgrammeID.1, -ProgrammeCategoryID.1) %>%
  
  #parse date and time into the correct format and create two new date-time columns Start and End
  mutate(ActivityDate = dmy(ActivityDate),
         EndTime = as.character(EndTime)) %>% 
  unite(Start, c(ActivityDate, StartTime), sep = " ", remove = FALSE) %>% 
  mutate(ActivityDate = as_date(ifelse(EndTime == "00:00:00", ActivityDate + days(1), ActivityDate))) %>% 
  unite(End, c(ActivityDate, EndTime), sep = " ", remove = FALSE) %>% 
  select(-ActivityDate, -StartTime, -EndTime) %>% 
  
  #determine day, month and year of activity as new distinct columns
  mutate(day = wday(Start, label = TRUE),
         month = month(Start),
         year = year(Start)) %>% 
  
  #drop observations with child or adult participant >90th percentile
  filter(ChildParticipants < quantile(ChildParticipants, 0.90) | AdultParticipants < quantile(AdultParticipants, 0.90))
  
#identify most frequent Start time by activity in 2019

freq_start <- hub_clean %>% 
  filter(year == 2019) %>% 
  group_by(ProgrammeID) %>% 
  summarise(start_time = )
  
  
 
  #ref data frame for all unique ID and names
code <- hub_clean %>% 
  select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ShortName, ServiceTypeID, CategoryName, ShortName.1) %>% 
  distinct()
  
  


  
  
  
  
  
  












  