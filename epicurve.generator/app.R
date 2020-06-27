# This code calls RMarkdown to dynmaically create a report
.libPaths(c("C:\\Users\\pwc1\\Documents\\rlib_personal",.libPaths()))

#library(Hmisc)
library(tidyverse)
library(lubridate)
library(tidytext)
library(viridis)
library(shiny)
library(rmarkdown)
library(zoo)



county_dat <- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")

county_dat_vert<-pivot_longer(county_dat,cols = 5:length(county_dat), 
                              names_to = "date", values_to = "cum_cases")

county_dat_vert<-county_dat_vert[county_dat_vert$countyFIPS !=0 & 
                                     county_dat_vert$countyFIPS!= 1,]

county_dat_vert$date<-as.Date(county_dat_vert$date, format = "%m/%d/%y")

county_dat_vert<-county_dat_vert %>% 
    filter(date>as.Date("2020-03-15")) %>%
    group_by(countyFIPS) %>%
    arrange(date) %>%
    mutate(daily_cases=cum_cases-lag(cum_cases))

county_dat_vert[which(county_dat_vert$daily_cases< 0),"daily_cases"]<-0

county_dat_vert<- county_dat_vert %>% 
    group_by(countyFIPS) %>% 
    arrange(date) %>% 
    mutate(day7ave=rollapply(daily_cases,7,mean,align='right',fill=NA))

county_dat<-county_dat_vert

pop<-read_csv("pop_data.csv") %>% 
    filter(SUMLEV %in% 50) %>%
    mutate(COUNTY=str_pad(COUNTY,3,side="left", pad="0"), 
           FIPS5=paste(STATE, COUNTY, sep="")) %>%
    mutate(FIPS5=as.numeric(FIPS5))

county_dat<-left_join(county_dat,pop[,c("CENSUS2010POP", "FIPS5")], 
                      by=c("countyFIPS"="FIPS5"))


shinyApp(
    ui = fluidPage(
        titlePanel("Generate Epidemic Curves for Specified Counties"),
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Upload CSV file with county FIPS (FIPS5) in first column",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Does table include column header?", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Table Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # Horizontal line ----
            tags$hr(),
            
            downloadButton("report", "Generate report")
            
        ),
        
       
        
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    ),
    server = function(input, output) {
        
        #output for rendered table shown on screen
        output$contents <- renderTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
            req(input$file1)
            
            validate(
                need(tools::file_ext(input$file1$datapath) == "csv", "ERROR: The uploaded file is not a .csv." )
                # ,need(tools::file_ext(input$file1$datapath) == "test",paste(Sys.time(),Sys.Date(),
                #                                                            file.info("data/Case_county_slopes.csv")$mtime,
                #                                                            file.exists("data/Case_county_slopes.csv")))
            )
            

            
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep)

            county.fips <- as.numeric(as.character(df[,1]))
            

            validate(
                need(length(which(is.na(county.fips)))==0,
                     paste("WARNING: NA or non-numeric entries detected in FIPS column."," See row: ",
                           which(is.na(county.fips)), sep="" )),
                need(min(nchar(county.fips), na.rm = T) %in% c(4,5),
                     paste("WARNING: Inappropriate FIPS codes with <4 or >5 digits detected."," See row: ",
                           which(nchar(county.fips)<4 | nchar(county.fips)>5), sep="" )),
                need(sum((county.fips[!is.na(county.fips)] %in% unique(county_dat$countyFIPS))==F, na.rm=T)==0,
                     paste("WARNING: FIPS code detected that was not in our dataset. See row: ",
                        which(!(county.fips %in% unique(county_dat$countyFIPS)) & !is.na(county.fips)), sep="" ))
            )
            
            
            if(input$disp == "head") {
                return(head(df))
            }
            else {
                return(df)
            }
            
        })
        
        #output for rmarkdown
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "report.html",
            content = function(file) {
                county.data <- read.csv(input$file1$datapath,
                                        header = input$header,
                                        sep = input$sep)
                
                county.fips <- as.numeric(as.character(county.data[,1]))
                
                withProgress(message = paste("Rendering, please wait! ","Time: ", 
                                             seconds_to_period((length(county.fips)*3)), sep=""), {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                


                county.fips[which(nchar(county.fips)<4 | nchar(county.fips)>5)]<-NA

                county.fips[which((county.fips %in% unique(county_dat$countyFIPS))==F)]<-NA


                county.fips<-county.fips[!is.na(county.fips)]

                
                data.date <- max(as.Date(county_dat$date))
                

                ##Knit the document.
                
                rmarkdown::render(tempReport, output_file = file
                                  # ,params = params,
                                  # envir = new.env(parent = globalenv())
                                  )
                }
                )
            }
        )

    }
)