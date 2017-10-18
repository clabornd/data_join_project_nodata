source("Auxiliary/requirements.R")
source("Auxiliary/functions.R")
###########Load, Check, Clean#########

jobvite_data <- read_csv(tk_choose.files(caption = "Select Jobvite Data in CSV format"))
nuview_data <- read_csv(tk_choose.files(caption = "Select NuView Data in CSV format"))

#duplicate check, change to datetime, collapse nuview, filter for recent dates
jobvite_data <- clean_jobvite(jobvite_data)
collapsed_nuview <- group_nuview(nuview_data, min(jobvite_data$Candidate_Hire_Date))

collapsed_nuview <- fill_nuview_collapsed(collapsed_nuview, jobvite_data)


#### JOINS ####
nuview_lj_jobvite <- nuview_data %>% 
                    mutate(PosEffDt = as.Date(PosEffDt, format = '%m/%d/%Y')) %>%
                    left_join(collapsed_nuview %>% select(jobvite_id, Pos), by = "Pos") %>% 
                    left_join(jobvite_data, by = c("jobvite_id" = "Application_Id")) %>%
                    filter(PosEffDt >= min(jobvite_data$Candidate_Hire_Date))

jobvite_not_nuview <- jobvite_data %>% 
                      left_join(nuview_lj_jobvite, by = c("Application_Id" = "jobvite_id")) %>% 
                      filter(is.na(Emp)) %>%
                      distinct(Application_Id) %>%
                      left_join(jobvite_data, by = "Application_Id")

nuview_not_jobvite <- nuview_lj_jobvite %>% 
                      filter(is.na(jobvite_id)) %>% 
                      distinct(Emp) %>%
                      left_join(nuview_data, by = "Emp") 


###Check for values that are in jobvite but not nuview
crosscheck <- jobvite_data %>% left_join(nuview_lj_jobvite, by = c("Application_Id" = "jobvite_id"))
missing <- crosscheck %>% filter(is.na(Emp)) %>% select(Application_Id, Candidate_Full_Name, Candidate_Hire_Date, job_jobtype_name, EmlEffDt, Requisition_Country)

###

###Misc Writing
full_data <- nuview_data %>% left_join(jobvite_data, by = c("jobvite_id"="Application_Id"))
collapsed_data <- full_data %>% filter(!is.na(jobvite_id))

write_csv(nuview_lj_jobvite, "Output/nuview_lj_jobvite.csv")
write_csv(jobvite_not_nuview, "Output/jobvite_not_nuview.csv")
write_csv(nuview_not_jobvite, "Output/nuview_not_jobvite.csv")


###TEST BLOCKS###






