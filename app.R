library(shiny)
library(tidyverse)

US_birth <- read_csv(file = "/Users/hexiaotao/Desktop/shiny--lpan0037/us_births_2000-2014.csv")
US_birth$Date <-as.Date(with(US_birth,paste(year,month,date_of_month,sep="-")),"%Y-%m-%d")
US_birth$year <- as.character(US_birth$year)

ui <- fluidPage(

    titlePanel("US Birth Data"),

       h3("Birth Number by Different Dimension"),
       fluidRow(
         sidebarLayout(
           sidebarPanel(
            checkboxGroupInput("classify", "Select the year:",
                               choices = c("2001", "2002", "2003","2004","2005","2006","2007",
                                           "2008","2009","2010","2011","2012","2013","2014"),
                               selected = c("2001", "2002", "2003","2004","2005","2006","2007",
                                            "2008","2009","2010","2011","2012","2013","2014"))
        ),
          mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Overall", plotOutput("plot")),
                        tabPanel("By_Month", plotOutput("plot1")),
                        tabPanel("By_Day", plotOutput("plot2")),
                        tabPanel("By_Week", plotOutput("plot3"))
            )
          )
         )
       ),

    h3("Birth Number in Specific Period"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("dates", label = "Select the date range:",
                         start = "2000-01-01", end = "2014-12-31")
        ),
        mainPanel(
          plotOutput("trend_plot")
        )
      )
    ),

       fluidRow(
         column(10,
                div(class = "about",
                    uiOutput('about'))
         )
       ),
       includeCSS("styles.css")
       )



server <- function(input, output) {

    output$plot <- renderPlot({
      US_birth %>%
        filter(year == input$classify) %>%
        group_by(year) %>%
        summarise(births_year = sum(births)) %>%
        ggplot(aes(year,births_year))+
        geom_col()+
        labs(x = "Year",
             y = "US_Birth_Number",
             title = "US Birth Number in Defferent Years") +
        theme_bw()
    })

    output$plot1 <- renderPlot({
      US_birth %>%
        filter(year == input$classify) %>%
        group_by(year,month) %>%
        summarise(births_month = sum(births)) %>%
        ggplot(aes(x = month, y = births_month, fill = year)) +
        geom_col(position="stack") +
        labs(x = "Month",
             y = "US_Birth_Number",
             title = "US Birth Number from January to December") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
        theme_bw()
    })

    output$plot2 <- renderPlot({
      US_birth %>%
        filter(year == input$classify) %>%
        group_by(year,date_of_month) %>%
        summarise(births_date = sum(births)) %>%
        ggplot(aes(x = date_of_month, y = births_date, fill = year)) +
        geom_col(position="stack") +
        labs(x = "Day",
             y = "US_Birth_Number",
             title = "US Birth Number from 1st to 31st per Month") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,
                                      11,12,13,14,15,16,17,18,19,20,
                                      21,22,23,24,25,26,27,28,29,30,31)) +
        theme_bw()
    })

    output$plot3 <- renderPlot({
      US_birth %>%
        filter(year == input$classify) %>%
        group_by(year,day_of_week) %>%
        summarise(births_week = sum(births)) %>%
        ggplot(aes(x = day_of_week, y = births_week, fill = year)) +
        geom_col(position="stack") +
        labs(x = "Day",
             y = "US_Birth_Number",
             title = "US Birth Number from Monday to Sunday per Week") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
        theme_bw()
    })


    output$trend_plot <- renderPlot({
      US_birth %>%
        filter(between(Date, as.Date(input$dates[1]), as.Date(input$dates[2]))) %>%
        ggplot(aes(Date,births)) +
        geom_line() +
        labs(x = "Date",
             y = "US_Birth_Number") +
        theme_bw()
    })

    output$about <- renderUI({
      knitr::knit("about.Rmd", quiet = TRUE) %>%
        markdown::markdownToHTML(fragment.only = TRUE) %>%
        HTML()
    })
}


shinyApp(ui = ui, server = server)
