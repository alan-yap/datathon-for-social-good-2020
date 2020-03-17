# Clear documentation of script is important when collaborating in the team!
# 
# This scripts ......
#
# Name, Mar 2020

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
  left_join(status, by = "HubRandomID")



# top 10 hub in terms of annual participant size in 2019

top_prtcpnt_hub <- hub_feature %>% 
  filter(year == 2019) %>% 
  distinct(HubRandomID, annual_prtcpnt_all) %>% 
  arrange(desc(annual_prtcpnt_all)) %>% 
  slice(1:10) %>% 
  pull(HubRandomID)

# bottom 10 hub in terms of annual participant size in 2019

bottom_prtcpnt_hub <- hub_feature %>% 
  filter(year == 2019) %>% 
  distinct(HubRandomID, annual_prtcpnt_all) %>% 
  arrange(annual_prtcpnt_all) %>% 
  slice(1:10) %>% 
  pull(HubRandomID)

# averages for plotting horizontal line

avg_yr_operation <- status %>% 
  filter(status_at_dec_19 == 1) %>% 
  group_by(status_at_dec_19) %>% 
  summarise(avg_yrs_in_operation = mean(years_in_operation)) %>% 
  mutate(year = 2019) %>% 
  select(-status_at_dec_19)

avg_line <- hub_feature %>% 
  filter(year == 2019) %>% 
  select(year, avg_hub_annual_prtcpnt, avg_hub_annual_activity_count, avg_hub_annual_program_count,
         avg_hub_prtcpnt_per_activity, avg_hub_annual_volunteer) %>% 
  distinct() %>% 
  left_join(avg_yr_operation, by = "year") %>% 
  select(-year)

#-------------k-means clustering for hubs----------------------------------

# select a subset of features from hub_feature for clustering analysis. use 2019 observations
# 
# annual program count
# annual activity count
# annual prtcpnt all
# annual volunteer all
# status at dec 19
# years in operation

# scale data

cluster_data <- hub_feature %>% 
  select(HubRandomID, year, annual_program_count, annual_activity_count, annual_prtcpnt_all, annual_volunteer_all, status_at_dec_19, years_in_operation) %>% 
  filter(year == 2019) %>% 
  select(-year) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'HubRandomID') 

scale_data <- scale(cluster_data)

# Optimal number of clusters in the data using the elbow method

fviz_nbclust(scale_data, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)

# Optimal number of clusters in the data using the silhouette method

fviz_nbclust(scale_data, kmeans, method = "silhouette") 

# start with 5 clusters. Compute k-means with k = 5
 
set.seed(123)

km.res <- kmeans(scale_data, 5, nstart = 25)

print(km.res)

# Compute the mean of each variable by clusters 

aggregate(scale_data, by=list(cluster=km.res$cluster), mean)

# assign cluster to original data

cluster_data_final <- cbind(cluster_data, cluster = km.res$cluster)

# visualise k-means clusters 

fviz_cluster(km.res, cluster_data_final, ellipse.type = "norm")

fviz_cluster(km.res, cluster_data_final,
             palette = "Set2", ggtheme = theme_minimal())

# assign cluster information to hub_feature and common program

cluster_tmp <- cluster_data_final %>% 
  rownames_to_column(var = "HubRandomID") %>%
  mutate_if(is.character, as.numeric) %>% 
  select(HubRandomID, cluster) 

hub_feature_final <- cluster_tmp %>% 
  right_join(hub_feature, by = "HubRandomID")

common_program_final <- cluster_tmp %>% 
  right_join(common_program, by = "HubRandomID")

# # visualise k-means clusters by first reducing the number of features using PCA
# # check that variables are highly correlated 
# 
# cormat <- round(cor(cluster_data_final), 2)
# 
# ggcorrplot(cormat, hc.order = TRUE, type = "lower", outline.color = "white")
# 
# # PCA
# # Scale data and identify principal components 
# 
# pr_out <-prcomp(cluster_data_final, center = TRUE, scale = TRUE) 
# 
# summary(pr_out)
# 
# # visualise principal components with a Screeplot and a cumulative PVE plot 
# 
# pr_var <-  pr_out$sdev ^ 2
# 
# pve <- pr_var / sum(pr_var)
# 
# plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
# 
# plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# 
# # interpret the first two principal components before proceeding
# # Rotate loadings
# rot_loading <- varimax(pr_out$rotation[, 2:4])
# rot_loading


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





# ------save all objects for Rmarkdown---------

save.image(file = "all_objects.RData")
