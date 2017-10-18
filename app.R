library(ggplot2)
library(shiny)
library(shinyFiles)
source("Auxiliary/requirements.R")
source("Auxiliary/functions.R")


###Setup, creates fields to enter Jobvite and Nuview data, a button to perform the join and download
###the resulting dataframe as a csv file, options to truncate the date, and whether or not to return
###records that do not have a matching jobvite ID.

ui <- fluidPage(
  
  fileInput("jobvite", 
            "Navigate to jobvite file in CSV format", 
            accept = c("text/csv", ".csv")),
  
  fileInput("nuview",
            "Navigate to nuview file in CSV format",
            accept = c("text/csv", ".csv")),
  
  dateInput("truncdate", "Select earliest date from which to truncate data", "2016-01-01",
            min = "1900-01-01", 
            max = "2020-01-01"),
  
  checkboxGroupInput("df_which", "Select Tables to Retrieve", 
                      c("Nuview data with appended position Id's" = "1", 
                        "Nuview records with no corresponding Jobvite record" = "2",
                        "Jobvite records with no corresponding Nuview record" = "3")),
  
  downloadButton("dl", "Join and Download File")
  
  

)

#Joining function
server <- function(input, output){
  
  df_jobvite <- isolate(reactive({
    
    #load data
    jobvite_data <- read_csv(input$jobvite$datapath)
    nuview_data <- read_csv(input$nuview$datapath)
    
    jobvite_data <- clean_jobvite(jobvite_data)
    collapsed_nuview <- group_nuview(nuview_data, input$truncdate)
      
    #Add unique job ID field to nuview data
    collapsed_nuview$jobvite_id <- NA
    
    withProgress(message = "Matching Records...", value = 0,{
      
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
      
        incProgress(1/nrow(collapsed_nuview), detail = paste(round(i/nrow(collapsed_nuview)*100), "% Done"))
    }
      
  #End progress brackets
  })
  
    
  ####JOIN#### 
    
  #useful to always have this dataframe
  nuview_lj_jobvite <- nuview_data %>% 
    mutate(PosEffDt = as.Date(PosEffDt, format = '%m/%d/%Y')) %>%
    left_join(collapsed_nuview %>% select(jobvite_id, Pos), by = "Pos") %>% 
    left_join(jobvite_data, by = c("jobvite_id" = "Application_Id")) %>%
    filter(PosEffDt >= input$truncdate)  
    
  df_list <- list()
  
  #Check which selections were made
  if("1" %in% input$df_which){
    df_list[[length(df_list)+1]] <- nuview_lj_jobvite
    
    names(df_list)[length(df_list)] <- "nuview_lj_jobvite"
  }  
  
  if("2" %in% input$df_which){
    df_list[[length(df_list)+1]] <- nuview_lj_jobvite %>% 
                                    filter(is.na(jobvite_id)) %>% 
                                    distinct(Emp) %>%
                                    left_join(nuview_data, by = "Emp") 
    
    names(df_list)[length(df_list)] <- "nuview_not_jobvite"
  }
    
  if("3" %in% input$df_which){
    df_list[[length(df_list)+1]] <- jobvite_data %>% 
                                    left_join(nuview_lj_jobvite, by = c("Application_Id" = "jobvite_id")) %>% 
                                    filter(is.na(Emp)) %>%
                                    distinct(Application_Id) %>%
                                    left_join(jobvite_data, by = "Application_Id")
    
    names(df_list)[length(df_list)] <- "jobvite_not_nuview"
  }
    
  print(names(df_list))
      
  df_list   
  #
  
  #Isolate and reactive brackets
  }))
  
  
  #download output which calls on isolated join operation
  output$dl <- downloadHandler(
    
    
    filename = function(){
      "tables_download.zip"
    }
    ,
    content = function(file) {
      
      otd <- tempdir()
      write(paste0("TMP = ", getwd()), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
      on.exit(write(paste0("TMP = ", otd, file=file.path(Sys.getenv('R_USER'), '.Renviron'))))
      print(tempdir())
      
      #call on isolated join operation
      listDataFrames <- df_jobvite()
      
      ###### zip up all files returned by df_jobvite() #######
      allFileNames <- NULL
      
      for(i in 1:length(listDataFrames)){
        filename <- paste0(names(listDataFrames)[i], ".csv")
        write.csv(listDataFrames[[i]], filename)
        allFileNames <- c(filename, allFileNames)
      }
      
      
      
      zip(file, allFileNames)
      
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)