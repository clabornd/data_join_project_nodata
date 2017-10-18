source("Auxiliary/requirements.R")
library(survival)
library(h2o)
library(lime)

joined_data <- nuview_lj_jobvite %>% filter(!is.na(jobvite_id))
joined_data['Attrition'] <- 0
joined_data['los'] <- 0

###Group by individual, get survival time.

finaldate <- max(joined_data$EmlEffDt)
initdate <- min(joined_data$EmlEffDt)

joined_data <- joined_data %>% group_by(Emp) %>% mutate(censor = ifelse("Terminated" %in% EmpSts, 1,0)) %>%
               mutate(surv = ifelse(censor == 1, julian(max(EmlEffDt))-julian(initdate), julian(finaldate)-julian(initdate)),
                       los = ifelse(censor == 1, julian(max(EmlEffDt))-julian(min(EmlEffDt)), julian(finaldate)-julian(min(EmlEffDt)))) 
               
emp_surv <- joined_data %>% group_by(Emp) %>% top_n(1,EmlEffDt) %>% filter(los != 0)

time_in <- Surv(emp_surv$los, emp_surv$censor) 

surv_mod1 <- survreg(time_in ~ Candidate_Source, emp_surv)

##TEST BLOCK###
  
joined_data %>% group_by(Emp) %>% select(EmlEfdDt)
