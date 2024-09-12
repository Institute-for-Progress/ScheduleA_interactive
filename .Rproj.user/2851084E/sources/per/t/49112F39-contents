#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)

# Define UI for application 
ui <- fluidPage(
  
  tags$head(
    tags$script("(function () {function s() {const h = document.querySelector('.container-fluid').scrollHeight; console.log(`Sending height: ${h}`); window.parent.postMessage(JSON.stringify({height: h, }), '*'); } window.document.addEventListener('DOMContentLoaded', function () {s(); const o = new MutationObserver(s); o.observe(document.querySelector(`[role='main']`), { childList: true, subtree: true }); }); })()"),
    tags$style(HTML("@import url('https://ifp.org/wp-content/themes/institute-for-progress/assets/dist/css/shiny.css');")),
    tags$style(HTML("@import url('https://ifp.org/shiny/category-color.php?category=high-skilled-immigration');"))
    ),
  
  # Application title
 # titlePanel("Create your own data-driven update to Schedule A"),
  
  # Sidebar with slider inputs
  sidebarLayout(
    sidebarPanel(
      h4("1. Decide where to set the cutoff for your Schedule A update"),
      helpText("This app will rank occupations from most to least likely to have labor shortages. You will choose an unemployment rate to be your cutoff. The app will then add the ranked occupations to your Schedule A update until the number of total U.S. workers in those included occupations equals the number of U.S. workers in occupations with unemployment rates lower than the rate you set as your cutoff."),

      sliderInput("threshhold", "Where do you want to set the unemployment rate threshhold?", 
                  value = 1.8, 
                  min = 0, 
                  max = 5,step=0.1,post="%"),
      
      h4("2. Decide how much to weigh each indicator"),
      helpText("To generate a master shortage ranking, the app will take a weighted average of the shortage ranking of each indicator, using the weights you provide."),
      div(style="display: inline-block; width: 145px;",
          sliderInput("pay1", "One-year increase in pay",
                      min = 0, max = 100,
                      value = 50)),
      div(style="display: inline-block; width: 20px;"),
      
      div(style="display: inline-block; width: 145px;",
          sliderInput("pay3", "Three-year increase in pay",
                      min = 0, max = 100,
                      value = 50)),
      br(),
      div(style="display: inline-block; width: 145px;",
          sliderInput("jjrate", "Job-to-job transitions", 
                      min = 0, max = 100,
                      value = 50)),
      
      div(style="display: inline-block; width: 20px;"),
      div(style="display: inline-block; width: 145px;",
          sliderInput("emp", "Change in employment",
                      min = 0, max = 100,
                      value = 50)),
      br(),
      div(style="display: inline-block; width: 145px;",
          sliderInput("vacancy", "Vacancies", 
                      min = 0, max = 100,
                      value = 50)),
      div(style="display: inline-block; width: 20px;"),
      
      div(style="display: inline-block; width: 145px;",
          sliderInput("hours", "Change in hours", 
                      min = 0, max = 100,
                      value = 50)),
      br(),
      div(style="display: inline-block; width: 145px;",
          sliderInput("prem", "Income premium", 
                      min = 0, max = 100,
                      value = 50)),
      div(style="display: inline-block; width: 20px;"),
      
      div(style="display: inline-block; width: 145px;",
          sliderInput("unemp", "Unemployment", 
                      min = 0, max = 100,
                      value = 50)),
      br(),
      div(style="display: inline-block; width: 145px;",
          sliderInput("unemp_lag", "Three-year lagged unemployment", 
                      min = 0, max = 100,
                      value = 50)),
      div(style="display: inline-block; width: 20px;"),
      div(style="display: inline-block; width: 145px;",
          sliderInput("ooLaborForce", "Labor force nonparticipation", 
                      min = 0, max = 100,
                      value = 50))
    ),
    
    # establish panels for different outputs
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Your Schedule A update", 
                 htmlOutput("list")),
        tabPanel("How many U.S. workers are in these occupations?", h3("Share of U.S. workers in Schedule A occupations, by occupational category"),
                 plotOutput("employment",height=500)),
        
        tabPanel("How would your update impact labor certifications?", h3("Share of PERMs on Schedule A List"),
                 plotOutput("PERM",height=500)),

        tabPanel("Explore the data",
                 includeHTML(
                   "./heatmap.html"
                 )),
        tabPanel("Methods", "This app is based on the methods described in our full report, ",
                 tags$a(href="https://ifp.org/schedule-a/", "⁠\"⁠Help Wanted: Modernizing the Schedule A Shortage Occupation List.\"⁠"))
        )
      )))
       
    


# Define server logic
server <- function(input, output) {
  output$list<-renderTable({
    
  #replace with online-based data source (github??)
    sumACS<-read_csv((paste0("https://docs.google.com/uc?id=",id="1WHsoSz8KQMcO0XLanLTwoX9QQni2x5fw","&export=download"
      )))%>%
      #count workforce if under threshhold unemployment
      mutate(threshhold21=if_else(unemp<input$threshhold/100,employment_2021,0))%>%
      rowwise()%>%
      #define weighted mean of ranks, weighted by user inputs
      mutate(weightedRanks=mean(c(rank_pay1yr*input$pay1,
                                  rank_pay3yr*input$pay3,
                                  rank_jjrate*input$jjrate,
                                  rank_rp3*input$prem,
                                  rank_e2*input$vacancy,
                                  rank_hours*input$hours,
                                  rank_emp*input$emp,
                                  rank_unemp*input$unemp,
                                  rank_stock*input$ooLaborForce,
                                  rank_unemp18*input$unemp_lag),na.rm = TRUE)
               )%>%
      ungroup()
      
    #calculate target population
    targetNum21<-sum(sumACS$threshhold21,na.rm=TRUE)
    #arrange by weighted rank
    sumACS<-arrange(sumACS,weightedRanks)
    #count employment for everything in occupation and above
    sumACS$cumulative_PopAve21<-cumsum(sumACS$employment_2021)
    #define output as a table with the rank and occupation title
    Value<-sumACS%>%
      filter(cumulative_PopAve21<targetNum21)%>%
      mutate(Rank=as.integer(rank(weightedRanks)))%>%
     select(Rank,Title)
    
    
  })
  output$list<-renderTable({
    
    #replace with online-based data source (github??)
    sumACS<-read_csv((paste0("https://docs.google.com/uc?id=",id="1WHsoSz8KQMcO0XLanLTwoX9QQni2x5fw","&export=download"
    )))%>%
      #count workforce if under threshhold unemployment
      mutate(threshhold21=if_else(unemp<input$threshhold/100,employment_2021,0))%>%
      rowwise()%>%
      #define weighted mean of ranks, weighted by user inputs
      mutate(weightedRanks=mean(c(rank_pay1yr*input$pay1,
                                  rank_pay3yr*input$pay3,
                                  rank_jjrate*input$jjrate,
                                  rank_rp3*input$prem,
                                  rank_e2*input$vacancy,
                                  rank_hours*input$hours,
                                  rank_emp*input$emp,
                                  rank_unemp*input$unemp,
                                  rank_stock*input$ooLaborForce,
                                  rank_unemp18*input$unemp_lag),na.rm = TRUE)
      )%>%
      ungroup()
    
    #calculate target population
    targetNum21<-sum(sumACS$threshhold21,na.rm=TRUE)
    #arrange by weighted rank
    sumACS<-arrange(sumACS,weightedRanks)
    #count employment for everything in occupation and above
    sumACS$cumulative_PopAve21<-cumsum(sumACS$employment_2021)
    #define output as a table with the rank and occupation title
    Value<-sumACS%>%
      filter(cumulative_PopAve21<targetNum21)%>%
      mutate(Rank=as.integer(rank(weightedRanks)))%>%
      select(Rank,Title)
    
    
  })
  
  
 output$PERM<-renderPlot({
   #replace with online-based data source (github??)
   sumACS<-read_csv((paste0("https://docs.google.com/uc?id=",id="1WHsoSz8KQMcO0XLanLTwoX9QQni2x5fw","&export=download"
   )))%>%
     #count workforce if under threshhold unemployment
     mutate(threshhold21=if_else(unemp<input$threshhold/100,employment_2021,0))%>%
     rowwise()%>%
     #define weighted mean of ranks, weighted by user inputs
     mutate(weightedRanks=mean(c(rank_pay1yr*input$pay1,
                                 rank_pay3yr*input$pay3,
                                 rank_jjrate*input$jjrate,
                                 rank_rp3*input$prem,
                                 rank_e2*input$vacancy,
                                 rank_hours*input$hours,
                                 rank_emp*input$emp,
                                 rank_unemp*input$unemp,
                                 rank_stock*input$ooLaborForce,
                                 rank_unemp18*input$unemp_lag),na.rm = TRUE)
     )%>%
     ungroup()
   
   #calculate target population
   targetNum21<-sum(sumACS$threshhold21,na.rm=TRUE)
   #arrange by weighted rank
   sumACS<-arrange(sumACS,weightedRanks)
   #count employment for everything in occupation and above
   sumACS$cumulative_PopAve21<-cumsum(sumACS$employment_2021)
   
   sumACS<-mutate(sumACS,PERM21s_onList=if_else(cumulative_PopAve21<targetNum21,PERM21s,0))
   
   
   total<-sum(sumACS$PERM21s,na.rm=T)
   total2<-sum(sumACS$PERM21s_onList,na.rm=T)
   
   PERM_table<-as_tibble(c(.0621003,
                           1-.0621003,
                           sum(sumACS$PERM21s_onList,na.rm=T)/sum(sumACS$PERM21s,na.rm=T),
                           1-sum(sumACS$PERM21s_onList,na.rm=T)/sum(sumACS$PERM21s,na.rm=T)))
   PERM_table$List<- c("Help Wanted Index","Help Wanted Index","Your list","Your list")
   PERM_table$PERM<-c("Schedule A","Not Schedule A","Schedule A","Not Schedule A")
   
   
   #define output as a ggplot
   
    ggplot(PERM_table,
          aes(x=List,y=value,fill=PERM, label=paste(round(value*100,1),"%")))+
     geom_bar(position="fill", stat="identity") + 
     geom_text(size = 5, position = position_stack(vjust = 0.5),col="#fcfbeb")+
     xlab("") + ylab("Share") +
     theme(text=element_text(size=16))+ 
     scale_y_continuous(
       labels = scales::percent_format(scale = 100), # Converts to percentage format
       breaks = seq(0, 1, by = 0.25) # Sets breaks at 0, 0.25, 0.5, 0.75, 1
     )+ 
     theme(panel.background = element_rect(fill = "#fcfbeb"), # Changes background to white
           panel.grid.major = element_blank(),  # Removes major grid lines
           panel.grid.minor = element_blank(),
           plot.background = element_rect(fill = "#fcfbeb"),    # Background of the entire plot
           
           legend.background = element_rect(fill="#fcfbeb"),
           legend.title=element_blank()
           )  +
     scale_fill_manual(values = c("Schedule A" = "#3368ce", "Not Schedule A" = "#ff6565"))
   
  })
 
 
 
 
 output$employment<-renderPlot({
   #replace with online-based data source (github??)
   sumACS<-read_csv((paste0("https://docs.google.com/uc?id=",id="1WHsoSz8KQMcO0XLanLTwoX9QQni2x5fw","&export=download"
   )))%>%
     #count workforce if under threshhold unemployment
     mutate(threshhold21=if_else(unemp<input$threshhold/100,employment_2021,0))%>%
     rowwise()%>%
     #define weighted mean of ranks, weighted by user inputs
     mutate(weightedRanks=mean(c(rank_pay1yr*input$pay1,
                                 rank_pay3yr*input$pay3,
                                 rank_jjrate*input$jjrate,
                                 rank_rp3*input$prem,
                                 rank_e2*input$vacancy,
                                 rank_hours*input$hours,
                                 rank_emp*input$emp,
                                 rank_unemp*input$unemp,
                                 rank_stock*input$ooLaborForce,
                                 rank_unemp18*input$unemp_lag),na.rm = TRUE)
     )%>%
     ungroup()
   
   #calculate target population
   targetNum21<-sum(sumACS$threshhold21,na.rm=TRUE)
   #arrange by weighted rank
   sumACS<-arrange(sumACS,weightedRanks)
   #count employment for everything in occupation and above
   sumACS$cumulative_PopAve21<-cumsum(sumACS$employment_2021)
   sumACS<-filter(sumACS,cumulative_PopAve21<targetNum21)%>%
   mutate(category=as.numeric(substring(occ_code,1,2)))
   healthcare<-sum(filter(sumACS,category==29|category==31)$employment_2021)/157150348
   business<-sum(filter(sumACS,category==11|category==13)$employment_2021)/157150348
   STEM<-sum(filter(sumACS,category==15|category==17|category==19)$employment_2021)/157150348
   other<-sum(filter(sumACS,category!=15&category!=17&category!=19&
                       category!=11&category!=13&
                       category!=29&category!=31
                     )$employment_2021)/157150348
   
   employ_table<-as_tibble(c("Help Wanted Index","Help Wanted Index","Help Wanted Index","Help Wanted Index",
                           "Your list","Your list","Your list","Your list"))
   employ_table<-rename(employ_table,list=value)
   employ_table$category<-c("Healthcare","Management and business operations", 
                            "Engineering, science, math, computer, and architecture","other",
                            "Healthcare","Management and business operations", 
                            "Engineering, science, math, computer, and architecture","other")
   employ_table$value=c(.02766107,.02547816,.00477033,.01536501,
                        healthcare,business,STEM,other)
   
   ggplot(employ_table, aes(x = category, y = value, fill = list)) +
    
     geom_col(position = position_dodge(width=1)) + # Adjust width to match geom_text
  geom_text(aes(label = paste(round(value*100,2),"%")), 
            position = position_dodge(width=1), # Ensure labels dodge similar to bars
            vjust = -0.3, # Adjust vertical position to be above the bars
            size = 5)+
#     theme(axis.text.x = element_text(angle = 45, 
#                                      hjust = 1, 
#                                      vjust = 1, 
#                                      size = 12))+
     scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
     xlab("") + ylab("Share") +
     theme(text=element_text(size=16))+ 
     scale_y_continuous(
       labels = scales::percent_format(1))+ 
     theme(panel.background = element_rect(fill = "#fcfbeb"),
           plot.background = element_rect(fill = "#fcfbeb"),
           legend.background = element_rect(fill="#fcfbeb"),
           panel.grid.major = element_blank(),  # Removes major grid lines
           panel.grid.minor = element_blank(),
           legend.title=element_blank())+
     scale_fill_manual(values = c("Help Wanted Index" = "#3368ce", "Your list" = "#39d794"))
   
   

     
   
 
 })
 
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
