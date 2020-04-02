# Clear documentation of script is important when collaborating in the team!
# 
# This scripts ......
#
# Alan Yap, Mar 2020

clean_df <- read_clean_data()


#===========Program data cube for reference============

program_data_cube <- hub_raw %>% 
  select(ProgrammeID, ProgrammeName, ProgrammeCategoryID, ServiceTypeID, CategoryName, ShortName.1) %>% 
  distinct()


#----------Hub profile------------------

# status for each hub
status <- clean_df %>% 
  group_by(HubRandomID) %>% 
  summarise(earliest_activity_date = min(ActivityDate),
            latest_activity_date = max(ActivityDate)) %>% 
  ungroup() %>% 
  mutate(status_at_dec_19 = ifelse(latest_activity_date < ymd("2019-12-01"), 0, 1),
         years_in_operation = round(time_length(interval(earliest_activity_date, latest_activity_date), "years"), digits = 1)) %>%
  select(-earliest_activity_date, -latest_activity_date)

# top three most common program offered at each hub each year 
common_program <- clean_df %>% 
  group_by(HubRandomID, year) %>% 
  count(ProgrammeName, ProgrammeID) %>% 
  arrange(desc(n)) %>%
  slice(1:3) %>%
  ungroup()


# hub feature dataframe 
hub_feature <- clean_df %>% 
  group_by(HubRandomID, year) %>%
  #attendance features
  summarise(annual_child_prtcpnt = sum(ChildParticipants),
            annual_adult_prtcpnt = sum(AdultParticipants),
            annual_DSS_prtcpnt = sum(DSS_ClientParticipants, na.rm = TRUE),
            annual_referral_prtcpnt = sum(ReferralParticipants),
            #program features          
            annual_program_count = length(unique(ProgrammeID)),
            annual_activity_count = n(),
            #volunteer features
            annual_school_volunteer = sum(SchoolVolunteers),
            annual_hub_volunteer = sum(HubVolunteers),
            annual_external_volunteer = sum(ExternalVolunteers)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  #attendance features
  mutate(annual_prtcpnt_all = sum(annual_child_prtcpnt, annual_adult_prtcpnt, annual_DSS_prtcpnt,
                                  annual_referral_prtcpnt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(avg_mthly_prtcpnt_all = round((annual_prtcpnt_all / 12), digits = 1),
         avg_prtcpnt_per_activity = annual_prtcpnt_all / annual_activity_count,
         #program features
         avg_mthly_activity_all = round((annual_activity_count / 12), digits = 1)) %>% 
  rowwise() %>% 
  #volunteer features
  mutate(annual_volunteer_all = sum(annual_school_volunteer, annual_hub_volunteer, annual_external_volunteer)) %>% 
  ungroup() %>% 
  mutate(avg_mthly_volunteer_all = round((annual_volunteer_all / 12), digits = 1)) %>% 
  group_by(year) %>% 
  #attendance features
  mutate(avg_hub_annual_prtcpnt = mean(annual_prtcpnt_all),
         #program features
         avg_hub_annual_activity_count = mean(annual_activity_count),
         avg_hub_annual_program_count = mean(annual_program_count),
         avg_hub_prtcpnt_per_activity = mean(avg_prtcpnt_per_activity),
         avg_hub_annual_volunteer = mean(annual_volunteer_all)) %>% 
  ungroup() %>% 
  #combine with status dataframe
  left_join(status, by = "HubRandomID") 

# three-year CAGR growth rates 
growth_rates <- hub_feature %>% 
  gather(variable, value, -HubRandomID, -year) %>% 
  spread(year, value) %>% 
  filter(str_detect(variable, "^annual")) %>% 
  group_by(HubRandomID, variable) %>% 
  mutate(three_year_cagr = (`2019` / `2016`) ^ (1 / 3) - 1) %>% 
  select(-`2016`:-`2019`) %>% 
  pivot_wider(names_from = variable,
              names_prefix = "cagr_",
              values_from = three_year_cagr)

hub_feature <- hub_feature %>% 
  left_join(growth_rates, by = "HubRandomID")


# 
# # top 10 hub in terms of annual participant size in 2019
# 
# top_prtcpnt_hub <- hub_feature %>% 
#   filter(year == 2019) %>% 
#   distinct(HubRandomID, annual_prtcpnt_all) %>% 
#   arrange(desc(annual_prtcpnt_all)) %>% 
#   slice(1:10) %>% 
#   pull(HubRandomID)
# 
# # bottom 10 hub in terms of annual participant size in 2019
# 
# bottom_prtcpnt_hub <- hub_feature %>% 
#   filter(year == 2019) %>% 
#   distinct(HubRandomID, annual_prtcpnt_all) %>% 
#   arrange(annual_prtcpnt_all) %>% 
#   slice(1:10) %>% 
#   pull(HubRandomID)
# 
# # averages for plotting horizontal line
# 
# avg_yr_operation <- status %>% 
#   filter(status_at_dec_19 == 1) %>% 
#   group_by(status_at_dec_19) %>% 
#   summarise(avg_yrs_in_operation = mean(years_in_operation)) %>% 
#   mutate(year = 2019) %>% 
#   select(-status_at_dec_19)
# 
# avg_line <- hub_feature %>% 
#   filter(year == 2019) %>% 
#   select(year, avg_hub_annual_prtcpnt, avg_hub_annual_activity_count, avg_hub_annual_program_count,
#          avg_hub_prtcpnt_per_activity, avg_hub_annual_volunteer) %>% 
#   distinct() %>% 
#   left_join(avg_yr_operation, by = "year") %>% 
#   select(-year)

#-------------k-means clustering for hubs----------------------------------

# select a subset of features from hub_feature for clustering analysis. 
# 
# annual program count
# annual activity count
# annual prtcpnt all
# annual volunteer all
# status at dec 19
# years in operation

# scale 2019 data with the above features

cluster_2019 <- scale_data(2019)

# Optimal number of clusters in the data using the elbow method

fviz_nbclust(cluster_2019, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)

# Optimal number of clusters in the data using the silhouette method

fviz_nbclust(cluster_2019, kmeans, method = "silhouette") 

# use 4 clusters. Compute k-means with k = 4

set.seed(123)

km.res <- kmeans(cluster_2019, 4, nstart = 25)

# Display mean of each variable by clusters in a table with conditional formatting

cluster_mean_table_2019 <- aggregate(cluster_2019, by=list(cluster=km.res$cluster), mean) %>% 
  mutate_if(is.numeric, function(x) {
    round(x, 2) 
    
  }) %>% 
  mutate_if(is.numeric, function(x) {
    
    cell_spec(x, "html", bold = TRUE, color = ifelse(x > 0, "green", "red"))
    
  }) %>% 
  kable(format = "html", escape = FALSE, align = "c") %>%
  kable_styling(c("striped", "condensed"), full_width = FALSE)

# assign cluster to data

cluster_2019_final <- cbind(cluster_2019, cluster = km.res$cluster) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  select(HubRandomID = rowname, cluster_19 = cluster) %>% 
  mutate(year = 2019,
         HubRandomID = as.numeric(HubRandomID))

# visualise k-means clusters 

fviz_cluster(km.res, cluster_2019, ellipse.type = "norm", pointsize = 1, labelsize = 8)

fviz_cluster(km.res, cluster_2019,
             palette = "Set2", ggtheme = theme_minimal(), pointsize = 1, labelsize = 8)


# repeat cluster analysis with data from 2016 to 2018 

cluster_2018 <- scale_data(2018)

fviz_nbclust(cluster_2018, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(cluster_2018, kmeans, method = "silhouette") 

# use 4 clusters. Compute k-means with k = 4

set.seed(123)

km.res <- kmeans(cluster_2018, 4, nstart = 25)

cluster_mean_table_2018 <- aggregate(cluster_2018, by=list(cluster=km.res$cluster), mean) %>% 
  mutate_if(is.numeric, function(x) {
    round(x, 2) 
    
  }) %>% 
  mutate_if(is.numeric, function(x) {
    
    cell_spec(x, "html", bold = TRUE, color = ifelse(x > 0, "green", "red"))
    
  }) %>% 
  kable(format = "html", escape = FALSE, align = "c") %>%
  kable_styling(c("striped", "condensed"), full_width = FALSE)

# assign cluster to data

cluster_2018_final <- cbind(cluster_2018, cluster = km.res$cluster) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  select(HubRandomID = rowname, cluster_18 = cluster) %>% 
  mutate(year = 2018,
         HubRandomID = as.numeric(HubRandomID))


# visualise k-means clusters 

fviz_cluster(km.res, cluster_2018, ellipse.type = "norm", pointsize = 1, labelsize = 8)

fviz_cluster(km.res, cluster_2018,
             palette = "Set2", ggtheme = theme_minimal(), pointsize = 1, labelsize = 8)


cluster_2017 <- scale_data(2017)

fviz_nbclust(cluster_2017, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(cluster_2017, kmeans, method = "silhouette") 

# use 4 clusters. Compute k-means with k = 4

set.seed(123)

km.res <- kmeans(cluster_2017, 4, nstart = 25)

cluster_mean_table_2017 <- aggregate(cluster_2017, by=list(cluster=km.res$cluster), mean) %>% 
  mutate_if(is.numeric, function(x) {
    round(x, 2) 
    
  }) %>% 
  mutate_if(is.numeric, function(x) {
    
    cell_spec(x, "html", bold = TRUE, color = ifelse(x > 0, "green", "red"))
    
  }) %>% 
  kable(format = "html", escape = FALSE, align = "c") %>%
  kable_styling(c("striped", "condensed"), full_width = FALSE)

# assign cluster to data

cluster_2017_final <- cbind(cluster_2017, cluster = km.res$cluster) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  select(HubRandomID = rowname, cluster_17 = cluster) %>% 
  mutate(year = 2017,
         HubRandomID = as.numeric(HubRandomID))


# visualise k-means clusters 

fviz_cluster(km.res, cluster_2017, ellipse.type = "norm", pointsize = 1, labelsize = 8)

fviz_cluster(km.res, cluster_2017,
             palette = "Set2", ggtheme = theme_minimal(), pointsize = 1, labelsize = 8)


cluster_2016 <- scale_data(2016)

fviz_nbclust(cluster_2016, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(cluster_2016, kmeans, method = "silhouette") 

# use 4 clusters. Compute k-means with k = 4

set.seed(123)

km.res <- kmeans(cluster_2016, 4, nstart = 25)

cluster_mean_table_2016 <- aggregate(cluster_2016, by=list(cluster=km.res$cluster), mean) %>% 
  mutate_if(is.numeric, function(x) {
    round(x, 2) 
    
  }) %>% 
  mutate_if(is.numeric, function(x) {
    
    cell_spec(x, "html", bold = TRUE, color = ifelse(x > 0, "green", "red"))
    
  }) %>% 
  kable(format = "html", escape = FALSE, align = "c") %>%
  kable_styling(c("striped", "condensed"), full_width = FALSE)

# assign cluster to data

cluster_2016_final <- cbind(cluster_2016, cluster = km.res$cluster) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  select(HubRandomID = rowname, cluster_16 = cluster) %>% 
  mutate(year = 2016,
         HubRandomID = as.numeric(HubRandomID))


# visualise k-means clusters 

fviz_cluster(km.res, cluster_2016, ellipse.type = "norm", pointsize = 1, labelsize = 8)

fviz_cluster(km.res, cluster_2016,
             palette = "Set2", ggtheme = theme_minimal(), pointsize = 1, labelsize = 8)


# assign cluster information to hub_feature and common program

hub_feature_final <- hub_feature %>% 
  left_join(cluster_2019_final, by = c("HubRandomID", "year")) %>%
  left_join(cluster_2018_final, by = c("HubRandomID", "year")) %>% 
  left_join(cluster_2017_final, by = c("HubRandomID", "year")) %>% 
  left_join(cluster_2016_final, by = c("HubRandomID", "year")) 


common_program_final <- common_program %>% 
  left_join(cluster_2019_final, by = c("HubRandomID", "year")) %>%
  left_join(cluster_2018_final, by = c("HubRandomID", "year")) %>% 
  left_join(cluster_2017_final, by = c("HubRandomID", "year")) %>% 
  left_join(cluster_2016_final, by = c("HubRandomID", "year")) 

tmp <- common_program_final %>% filter(year == 2019) %>% group_by(cluster_19, ProgrammeName, ProgrammeID) %>% summarise(total = sum(n))



# 
# #----------Volunteer------------------
# 
hub_feature_final$year <- factor(hub_feature_final$year,
                                 labels = c("2016", "2017", "2018", "2019"))

# Exploratory plots

p_school_volunteer <- hub_feature_final %>%
  select(year, annual_school_volunteer) %>%
  filter(annual_school_volunteer < quantile(annual_school_volunteer, 0.999)) %>%
  ggplot(aes(x = year, y = annual_school_volunteer)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800, 100),
                     limits = c(0, 800))

p_hub_volunteer <- hub_feature_final %>%
  select(year, annual_hub_volunteer) %>%
  filter(annual_hub_volunteer < quantile(annual_hub_volunteer, 0.999)) %>%
  ggplot(aes(x = year, y = annual_hub_volunteer)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1000, 100),
                     limits = c(0, 1000))

p_external_volunteer <- hub_feature_final %>%
  select(year, annual_external_volunteer) %>%
  filter(annual_external_volunteer < quantile(annual_external_volunteer, 0.999)) %>%
  ggplot(aes(x = year, y = annual_external_volunteer)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1000, 100),
                     limits = c(0, 1000))

p_all_volunteer <- hub_feature_final %>%
  select(year, annual_volunteer_all) %>%
  filter(annual_volunteer_all < quantile(annual_volunteer_all, 0.999)) %>%
  ggplot(aes(x = year, y = annual_volunteer_all)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1500, 100),
                     limits = c(0, 1500))

# other plots for final report - messy code and no time to functionalise



top_hubs_prtcpnt_growth <- ggplot(hub_feature, aes(year, annual_prtcpnt_all)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(`cagr_annual_prtcpnt_all`) > 0.15,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual participant count", labels = comma, limits = c(0, 50000)) +
  labs(title = "Top five hubs by annual participants CAGR 2016-2019.") 

top_hubs_volunteer_growth <- ggplot(hub_feature, aes(year, annual_volunteer_all)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(`cagr_annual_volunteer_all`) > 0.2,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual volunteer count", labels = comma, limits = c(0, 1500)) +
  labs(title = "Top five hubs by annual volunteers CAGR 2016-2019.") 

top_hubs_activity_growth <- ggplot(hub_feature, aes(year, annual_activity_count)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(`cagr_annual_activity_count`) > 0.045,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual activity count", labels = comma, limits = c(0, 1600)) +
  labs(title = "Top five hubs by annual activity CAGR 2016-2019.") 

top_hubs_program_growth <- ggplot(hub_feature, aes(year, annual_program_count)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(`cagr_annual_program_count`) > 0.19,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual program count", labels = comma, limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  labs(title = "Top five hubs by annual program CAGR 2016-2019.")

top_hubs_adult_prtcpnt_growth <- ggplot(hub_feature, aes(year, annual_adult_prtcpnt)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(cagr_annual_adult_prtcpnt) > 0.185,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual adult participant count", labels = comma, limits = c(0, 8000)) +
  labs(title = "Top five hubs by annual adult participants CAGR 2016-2019.") 

top_hubs_child_prtcpnt_growth <- ggplot(hub_feature, aes(year, annual_child_prtcpnt)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(cagr_annual_child_prtcpnt) > 0.16,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual child participant count", labels = comma, limits = c(0, 30000)) +
  labs(title = "Top five hubs by annual child participants CAGR 2016-2019.") 

top_hubs_DSS_prtcpnt_growth <- ggplot(hub_feature, aes(year, annual_DSS_prtcpnt)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(cagr_annual_DSS_prtcpnt) > 0.2,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual DSS participant count", labels = comma, limits = c(0, 22000)) +
  labs(title = "Top five hubs by annual DSS participants CAGR 2016-2019.") 


top_hubs_referral_prtcpnt_growth <- ggplot(hub_feature, aes(year, annual_referral_prtcpnt)) +
  geom_line(aes(color = as.factor(HubRandomID))) +
  xlab("Year") +
  gghighlight(max(cagr_annual_referral_prtcpnt) > 0.15,
              max_highlight = 5,
              label_key = HubRandomID) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Annual referral participant count", labels = comma, limits = c(0, 4000)) +
  labs(title = "Top five hubs by annual referral participants CAGR 2016-2019.") 

filtered_data <- hub_feature %>% 
  select(HubRandomID, year, contains("cagr")) %>%
  gather(variable, value, -HubRandomID, -year) %>% 
  filter(str_detect(variable, "prtcpnt") & !str_detect(variable, "all")) 

growth_comparison <- ggplot(filtered_data, aes(variable, value)) +
  geom_boxplot() +
  xlab("Participant segments") +
  scale_y_continuous(name = "CAGR 2016-2019", labels = percent_format(accuracy = 2), limits = c(-1, 2), breaks = seq(-1, 2, 0.2)) +
  labs(title = "Three-year CAGR of annual participant count by participant segments and hubs.")


school_volunteer_highlight <- hub_feature %>% 
  select(HubRandomID, year, annual_school_volunteer) %>% 
  filter(year == 2019) %>% 
  arrange(desc(annual_school_volunteer)) %>% 
  top_n(5) %>% 
  pull(HubRandomID)

hub_highlight <- hub_feature %>% 
  filter(HubRandomID %in% school_volunteer_highlight)

top_hubs_school_volunteer_growth <- ggplot() +
  geom_line(data = hub_feature, aes(year, annual_school_volunteer, group = as.factor(HubRandomID)), colour = "grey") +
  geom_line(data = hub_highlight, aes(year, annual_school_volunteer, colour = as.factor(HubRandomID))) +
  geom_text(data = subset(hub_highlight, year == 2019), aes(label = as.factor(HubRandomID), colour = as.factor(HubRandomID), x = 2019, y = annual_school_volunteer), position = position_jitter()) +
  scale_colour_discrete(guide = 'none') +  
  xlab("Year") +
  scale_y_continuous(name = "Annual school volunteer count", labels = comma, limits = c(0, 600), breaks = seq(0, 600, 50)) +
  labs(title = "Top four hubs by annual school volunteer in 2019.") 



hub_volunteer_highlight <- hub_feature %>% 
  select(HubRandomID, year, annual_hub_volunteer) %>% 
  filter(year == 2019) %>% 
  arrange(desc(annual_hub_volunteer)) %>% 
  top_n(5) %>% 
  pull(HubRandomID)

hub_highlight <- hub_feature %>% 
  filter(HubRandomID %in% hub_volunteer_highlight)

top_hubs_volunteer_growth <- ggplot() +
  geom_line(data = hub_feature, aes(year, annual_hub_volunteer, group = as.factor(HubRandomID)), colour = "grey") +
  geom_line(data = hub_highlight, aes(year, annual_hub_volunteer, colour = as.factor(HubRandomID))) +
  geom_text(data = subset(hub_highlight, year == 2019), aes(label = as.factor(HubRandomID), colour = as.factor(HubRandomID), x = 2019, y = annual_hub_volunteer), position = position_jitter()) +
  scale_colour_discrete(guide = 'none') +  
  xlab("Year") +
  scale_y_continuous(name = "Annual hub volunteer count", labels = comma, limits = c(0, 600), breaks = seq(0, 600, 50)) +
  labs(title = "Top four hubs by annual hub volunteer in 2019.") 



prog_activity_prtcpnt_cluster <- hub_feature_final %>%
  filter(year == 2019) %>%
  select(HubRandomID, annual_program_count, annual_activity_count, annual_prtcpnt_all, cluster_19) %>% 
  mutate(cluster_19 = as.factor(cluster_19)) %>% 
  ggplot(aes(annual_program_count, annual_activity_count, color = cluster_19)) + 
  geom_point(aes(size = annual_prtcpnt_all)) +
  scale_color_manual(name = "Cluster", values = nous_colour(colour = "contrasting")) +
  scale_size_continuous(name = "Annual participant count") +
  theme(legend.position = "bottom") + 
  xlab("Annual program count") +
  ylab("Annual activity count") +
  scale_y_continuous(labels = comma, breaks = seq(0, 1500, 250)) +
  labs(title = "Program, activity and participant count analysis of hubs by clusters.")


