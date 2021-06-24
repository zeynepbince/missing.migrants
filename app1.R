library(shiny)
library(tidyverse)
library(readxl)
library(xlsx)
library(ggplot2)

MissingMigrants_2019 <- read_excel("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2019.xlsx")
MissingMigrants_2020 <- read_csv("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2020.csv")
MissingMigrants_2021 <- read_excel("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2021.xlsx")

MissingMigrants_2019 <- MissingMigrants_2019[ -c(1,3,7,14,15,18:20) ]
MissingMigrants_2020 <- MissingMigrants_2020[ -c(1,3,7,14,15,18:20) ]
MissingMigrants_2021 <- MissingMigrants_2021[ -c(1,3,7,14,15,18:20) ]

MissingMigrants_all <- bind_rows(MissingMigrants_2019, MissingMigrants_2020, MissingMigrants_2021)

MissingMigrants_all$`Number Dead`[is.na(MissingMigrants_all$`Number Dead`)]<-mean(MissingMigrants_all$`Number Dead`,na.rm=TRUE)
MissingMigrants_all$`Number of Survivors`[is.na(MissingMigrants_all$`Number of Survivors`)]<-mean(MissingMigrants_all$`Number of Survivors`,na.rm=TRUE)
MissingMigrants_all$`Number of Females`[is.na(MissingMigrants_all$`Number of Females`)]<-mean(MissingMigrants_all$`Number of Females`,na.rm=TRUE)
MissingMigrants_all$`Number of Males`[is.na(MissingMigrants_all$`Number of Males`)]<-mean(MissingMigrants_all$`Number of Males`,na.rm=TRUE)  
MissingMigrants_all$`Number of Children`[is.na(MissingMigrants_all$`Number of Children`)]<-mean(MissingMigrants_all$`Number of Children`,na.rm=TRUE)

MissingMigrants_all$`Number Dead`<-round(MissingMigrants_all$`Number Dead`,digits=0)
MissingMigrants_all$`Number of Females`<-round(MissingMigrants_all$`Number of Females`,digits=0)
MissingMigrants_all$`Number of Survivors`<-round(MissingMigrants_all$`Number of Survivors`,digits=0)
MissingMigrants_all$`Number of Males`<-round(MissingMigrants_all$`Number of Males`,digits=0)
MissingMigrants_all$`Number of Children`<-round(MissingMigrants_all$`Number of Children`,digits=0)
MissingMigrants_all$`Total Dead and Missing`<-round(MissingMigrants_all$`Total Dead and Missing`,digits=0)

colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Females")] <- "Number_of_Females"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Males")] <- "Number_of_Males"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Children")] <- "Number_of_Children"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Survivors")] <- "Number_of_Survivors"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number Dead")] <- "Number_Dead"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Total Dead and Missing")] <- "Total_Dead_and_Missing"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Reported Year")] <- "Reported_Year"

MissingMigrants_all_2019 <- MissingMigrants_all %>%
    filter('Reported_Year' == 2019)



ui <- fluidPage(
    
    sidebarLayout(#position = "right",
        
        
        sidebarPanel(
            
            
            selectInput(inputId = "x_axis", label = "X axis",
                        choices = c('Number_of_Females', 'Number_of_Males',
                                    'Number_of_Children', 'Number_of_Survivors'),
                        selected = 'Number_of_Females'),
            
            
            selectInput(inputId = "y_axis", label = "Y axis",
                        choices = c('Total_Dead_and_Missing','Number_of_Survivors','Number_Dead','Number_of_Females','Number_of_Males',
                                    'Number_of_Children'),
                        selected = 'Total_Dead_and_Missing')
        ),
        
        
        mainPanel(
            plotOutput(outputId = "scatterplot")
        )
    )
)


server <- function(input, output) {
    
    
    output$scatterplot <- renderPlot({
        
        ggplot(data = MissingMigrants_all, 
               aes_string(x = input$x_axis, y = input$y_axis)) + 
            geom_point(aes_string(color="Region")) +
            theme_minimal()
        
    })
}


shinyApp(ui = ui, server = server)

install.packages('DT')
library(DT)
library(readxl)
library(xlsx)
library(shiny)
library(tidyverse)
library(plotly)

MissingMigrants_2019 <- read_excel("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2019.xlsx")
MissingMigrants_2020 <- read_csv("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2020.csv")
MissingMigrants_2021 <- read_excel("/Users/zeynep//Desktop/Team Migraine/datasets/MissingMigrants_2021.xlsx")

MissingMigrants_2019 <- MissingMigrants_2019[ -c(1,3,7,14,15,18:20) ]
MissingMigrants_2020 <- MissingMigrants_2020[ -c(1,3,7,14,15,18:20) ]
MissingMigrants_2021 <- MissingMigrants_2021[ -c(1,3,7,14,15,18:20) ]

MissingMigrants_all <- bind_rows(MissingMigrants_2019, MissingMigrants_2020, MissingMigrants_2021)

MissingMigrants_all$`Number Dead`[is.na(MissingMigrants_all$`Number Dead`)]<-mean(MissingMigrants_all$`Number Dead`,na.rm=TRUE)
MissingMigrants_all$`Number of Survivors`[is.na(MissingMigrants_all$`Number of Survivors`)]<-mean(MissingMigrants_all$`Number of Survivors`,na.rm=TRUE)
MissingMigrants_all$`Number of Females`[is.na(MissingMigrants_all$`Number of Females`)]<-mean(MissingMigrants_all$`Number of Females`,na.rm=TRUE)
MissingMigrants_all$`Number of Males`[is.na(MissingMigrants_all$`Number of Males`)]<-mean(MissingMigrants_all$`Number of Males`,na.rm=TRUE)  
MissingMigrants_all$`Number of Children`[is.na(MissingMigrants_all$`Number of Children`)]<-mean(MissingMigrants_all$`Number of Children`,na.rm=TRUE)

MissingMigrants_all$`Number Dead`<-round(MissingMigrants_all$`Number Dead`,digits=0)
MissingMigrants_all$`Number of Females`<-round(MissingMigrants_all$`Number of Females`,digits=0)
MissingMigrants_all$`Number of Survivors`<-round(MissingMigrants_all$`Number of Survivors`,digits=0)
MissingMigrants_all$`Number of Males`<-round(MissingMigrants_all$`Number of Males`,digits=0)
MissingMigrants_all$`Number of Children`<-round(MissingMigrants_all$`Number of Children`,digits=0)
MissingMigrants_all$`Total Dead and Missing`<-round(MissingMigrants_all$`Total Dead and Missing`,digits=0)

colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Females")] <- "Number_of_Females"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Males")] <- "Number_of_Males"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Children")] <- "Number_of_Children"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number of Survivors")] <- "Number_of_Survivors"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Number Dead")] <- "Number_Dead"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Total Dead and Missing")] <- "Total_Dead_and_Missing"
colnames(MissingMigrants_all)[which(names(MissingMigrants_all) == "Reported Year")] <- "Reported_Year"




ui <- fluidPage(
    
   
    titlePanel("Regions Explorer"),
    
    sidebarLayout(
        
        
        sidebarPanel(
            
            
            
            selectInput(inputId = "Reported_Year", label = "Reported Year",
                        choices = unique(MissingMigrants_all$Reported_Year),
                        selected = 2019),
            
            selectInput(inputId = "x_axis", label = "X axis",
                        choices = c('Number_of_Females', 'Number_of_Males',
                                    'Number_of_Children', 'Number_of_Survivors'),
                        selected = 'Number_of_Females'),
            
            
            selectInput(inputId = "y_axis", label = "Y axis",
                        choices = c('Total_Dead_and_Missing','Number_of_Survivors','Number_Dead','Number_of_Females','Number_of_Males',
                                    'Number_of_Children'),
                        selected = 'Total_Dead_and_Missing'),
            
            
            selectInput(inputId = "size", label = "Point Size",
                        choices = c('Total_Dead_and_Missing','Number_of_Survivors','Number_Dead'),
                        selected = "Number_Dead"),
            
            
            sliderInput(inputId ="transvalue", label = "Transparency", 
                        min = 0, max = 1, value = 0.8),
            
            
            checkboxInput(inputId = "show_table", label = "Show table",
                          value = TRUE)
        ),
        
        
        
        
        mainPanel(
            
            plotlyOutput(outputId = "scatterplot"),
            
            dataTableOutput(outputId = "countries_table")
        )
    )
)


server <- function(input, output) {
    
    
    
    countries_subset <- reactive({
        MissingMigrants_all %>% 
            filter(Reported_Year == input$Reported_Year)
    })
    
    
    
    
    output$scatterplot <- renderPlotly({
        
        p_scatter <- ggplot(data = countries_subset(),  
                            aes_string(x = input$x_axis, y = input$y_axis,
                                       color = "Region",
                                       size = input$size,
                                       label = "Region"))+
            geom_point(alpha = input$transvalue)+
            theme_minimal()+
            labs(title = input$Reported_Year)
        
        ggplotly(p_scatter)
    })
    
    
    output$countries_table <- renderDataTable({
        if(input$show_table){
            datatable(countries_subset(), rownames = FALSE)
        }
    })
}


shinyApp(ui = ui, server = server)

