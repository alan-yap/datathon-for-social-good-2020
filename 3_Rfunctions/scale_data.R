
scale_data <- function(year_to_analyse) {

# scale data

cluster_data_scaled <- hub_feature %>% 
  select(HubRandomID, year, annual_program_count, annual_activity_count, annual_prtcpnt_all, annual_volunteer_all, status_at_dec_19, years_in_operation) %>% 
  filter(year == year_to_analyse) %>% 
  select(-year) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'HubRandomID') %>% 
  scale()

cluster_data_scaled

}