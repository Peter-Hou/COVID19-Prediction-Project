#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(latex2exp)

#Import Data

confirmed_case_df <- 
    read.csv("time_series_covid19_confirmed_global.csv")
death_case_df <- 
    read.csv("time_series_covid19_deaths_global.csv")
recovered_case_df <- 
    read.csv("time_series_covid19_recovered_global.csv")


# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Report"),

    sidebarLayout(
        sidebarPanel(
            strong("COVID-19 by Country Input Panel", style = "color:red"),
            selectInput("country_name", "Country Name",
                        unique(confirmed_case_df$Country.Region),
                        "Canada"),
            selectInput("chart_type", "Type",
                        c("Confirmed", "Death", "Recovered"),
                        "Confirmed"),
            strong("Simulation of Virus Spreading Panel", style = "color:red"),
            p("Change the population size and R_0 and submit these parameters, 
              drag the slider of days to see the infection process."),
            sliderInput("pop_size", "Population Size",
                        min = 10,
                        max = 5000,
                        value = 200,
                        step = 50),
            sliderInput("Rnot", withMathJax(helpText('$R_0$')),
                        min = 0.5,
                        max = 20,
                        value = 1,
                        step = 0.5),
            actionButton("goButton", "Submit Parameters"),
            sliderInput("time", "Days",
                        min = 0,
                        max = 10000,
                        value = 1,
                        step = 1,
                        animate = TRUE)
        ),
        mainPanel(
            plotOutput("confirmedPlot"),
            plotOutput("simulationPlot")
        )
    )
))
