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

# annual and average monthly participant count at each hub each year
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
  ungroup()

# top three most common program offered at each hub each year 
common_program <- clean_df %>% 
  group_by(HubRandomID, year) %>% 
  count(ProgrammeName) %>% 
  arrange(desc(n)) %>%
  slice(1:3) %>%
  ungroup()

# status for each hub
status <- clean_df %>% 
  group_by(HubRandomID) %>% 
  summarise(earliest_activity_date = min(ActivityDate),
            latest_activity_date = max(ActivityDate)) %>% 
  ungroup() %>% 
  mutate(status_at_dec_19 = ifelse(latest_activity_date < ymd("2019-12-01"), "Inactive", "Active"),
         years_in_operation = round(time_length(interval(earliest_activity_date, latest_activity_date), "years"), digits = 1)) %>%
  select(-earliest_activity_date, -latest_activity_date)

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
  filter(status_at_dec_19 == "Active") %>% 
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

#-------------Clustering analysis----------------------------------

# calculate distance of all hub features using a distance metric that can handle mixed data types: Gower distance
# use only 2019 data

# 
# data <- feature_master %>% 
#   filter(year == 2019) %>% 
#   mutate_if(is.character, as.factor)
# 
# # remove hub id and year before clustering 
# 
# gower_dist <- daisy(data[,-1:-2],
#                     metric = "gower",
#                     type = list(logratio = 3))
# 
# # Check attributes to ensure the correct methods are being used
# # (I = interval, N = nominal)
# # Note that despite logratio being called, 
# # the type remains coded as "I"
# 
# summary(gower_dist)
# 
# # Sanity check - Output most similar pair
# 
# gower_mat <- as.matrix(gower_dist)
# 
# data[
#   which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
#         arr.ind = TRUE)[1, ], ]
# 
# # Sanity check - Output most dissimilar pair
# 
# data[
#   which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
#         arr.ind = TRUE)[1, ], ]
# 
# # Determine the optimal number of clusters using the silhouette width metric
# # Calculate silhouette width for many k using PAM
# 
# sil_width <- c(NA)
# 
# for(i in 2:10){
#   
#   pam_fit <- pam(gower_dist,
#                  diss = TRUE,
#                  k = i)
#   
#   sil_width[i] <- pam_fit$silinfo$avg.width
#   
# }
# 
# # Plot sihouette width (higher is better)
# 
# plot(1:10, sil_width,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:10, sil_width)
# 
# # Run summary of 8 clusters 
# 
# pam_fit <- pam(gower_dist, diss = TRUE, k = 8)
# 
# pam_results <- data %>%
#   mutate(cluster = pam_fit$clustering) %>%
#   group_by(cluster) %>%
#   do(the_summary = summary(.))
# 
# pam_results$the_summary
# 
# data[pam_fit$medoids, ]
# 
# # visualise using t-SNE 
# 
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 1)
# 
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering),
#          name = data$HubRandomID)
# 
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))
# 
# #---------Clustering analysis 2--------------------
# 
# data2 <- feature_master %>% 
#   select_if(is.numeric) 
# 
# scale_data2 <- scale(data2[,-1:-2]) 
# 
# data2_final <- data2 %>% 
#   select(HubRandomID, year) %>% 
#   merge(scale_data2)
# 
# #----------Volunteer------------------
# 
# volunteer$year <- factor(volunteer$year,
#                          labels = c("2016", "2017", "2018", "2019"))
# 
# # Exploratory plots
# 
# p_school_volunteer <- volunteer %>%
#   select(year, annual_school_volunteer) %>% 
#   filter(annual_school_volunteer < quantile(annual_school_volunteer, 0.999)) %>% 
#   ggplot(aes(x = year, y = annual_school_volunteer)) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(0, 1000, 100),
#                      limits = c(0, 1000))
# 
# p_hub_volunteer <- volunteer %>%
#   select(year, annual_hub_volunteer) %>% 
#   filter(annual_hub_volunteer < quantile(annual_hub_volunteer, 0.999)) %>% 
#   ggplot(aes(x = year, y = annual_hub_volunteer)) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(0, 1000, 100),
#                      limits = c(0, 1000))
# 
# p_external_volunteer <- volunteer %>%
#   select(year, annual_external_volunteer) %>% 
#   filter(annual_external_volunteer < quantile(annual_external_volunteer, 0.999)) %>% 
#   ggplot(aes(x = year, y = annual_external_volunteer)) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(0, 1000, 100),
#                      limits = c(0, 1000))
# 
# p_all_volunteer <- volunteer %>%
#   select(year, annual_volunteer_all) %>% 
#   filter(annual_volunteer_all < quantile(annual_volunteer_all, 0.999)) %>% 
#   ggplot(aes(x = year, y = annual_volunteer_all)) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(0, 1500, 100),
#                      limits = c(0, 1500))





# ------save all objects for Rmarkdown---------

save.image(file = "all_objects.RData")
