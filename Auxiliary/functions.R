source("Auxiliary/requirements.R")

#split complicated names to compare each part to full name in jobvite
split_and_combine <- function(x){
  paste(unlist(strsplit(x, split = " |\\-")), collapse = "|")
}

group_nuview <- function(nuview_data, truncdate){
  nuview_data %>% group_by(Emp) %>% 
  unique() %>% ungroup() %>%
  mutate(EmlEffDt = as.Date(EmlEffDt, format = '%m/%d/%Y')) %>%
  filter(EmlEffDt >= truncdate) %>%
  group_by(Pos) %>%
  top_n(-1, EmlEffDt) %>% slice(1)
}

clean_jobvite <- function(jobvite_data){
  jobvite_data %>% group_by(Application_Id) %>% 
  unique() %>% 
  ungroup %>%
  mutate(Candidate_Full_Name = iconv(Candidate_Full_Name, to = 'ASCII//TRANSLIT'),
         Candidate_Hire_Date = as.Date(Candidate_Hire_Date)) %>%
  filter(!is.na(Application_Id))
}

###match and populate the jobvite_id field in the nuview data
fill_nuview_collapsed <- function(collapsed_nuview, jobvite_data){

  collapsed_nuview$jobvite_id <- NA
  
  ############
  for(i in 1:nrow(collapsed_nuview)){
    #If after checks, length is zero, there were no matching records.
    if(nrow(jobvite_data %>% 
            filter(grepl(split_and_combine(collapsed_nuview[i,]$FirstName), gsub("[[:punct:]]+", "", Candidate_Full_Name), ignore.case = TRUE) == TRUE &
                   grepl(split_and_combine(collapsed_nuview[i,]$LastName), gsub("[[:punct:]]+", "", Candidate_Full_Name), ignore.case = TRUE) == TRUE &
                   Candidate_Hire_Date <= collapsed_nuview[i,]$EmlEffDt
            ) %>% 
            ###Get most recent job
            filter(Candidate_Hire_Date == max(Candidate_Hire_Date))
    )!= 0){
      #If matches are found append application ID.
      collapsed_nuview[i,]$jobvite_id <- 
        as.integer((jobvite_data %>% 
                      filter(grepl(split_and_combine(collapsed_nuview[i,]$FirstName), gsub("[[:punct:]]+", "", Candidate_Full_Name), ignore.case = TRUE) == TRUE &
                               grepl(split_and_combine(collapsed_nuview[i,]$LastName), gsub("[[:punct:]]+", "", Candidate_Full_Name), ignore.case = TRUE) == TRUE &
                               Candidate_Hire_Date <= collapsed_nuview[i,]$EmlEffDt
                      ) %>% 
                      ###Get most recent job
                      filter(Candidate_Hire_Date == max(Candidate_Hire_Date)) %>%
                      filter(Requisition_Created_On == max(Requisition_Created_On)))$Application_Id
        ) 
      }
    }

  collapsed_nuview
}