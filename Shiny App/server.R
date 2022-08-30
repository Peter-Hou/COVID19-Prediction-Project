#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#Import Data




shinyServer(function(input, output) {
    confirmed_case_df <- 
        read.csv("time_series_covid19_confirmed_global.csv")
    death_case_df <- 
        read.csv("time_series_covid19_deaths_global.csv")
    recovered_case_df <- 
        read.csv("time_series_covid19_recovered_global.csv")
    
    #Information Plot
    output$confirmedPlot <- renderPlot({
        country_name <- input$country_name
        chart_type <- input$chart_type
        
        confirmed_country_case <-
            confirmed_case_df[which(confirmed_case_df$Country.Region==country_name),]
        death_country_case <-
            death_case_df[which(death_case_df$Country.Region==country_name),]
        recovered_country_case <-
            recovered_case_df[which(recovered_case_df$Country.Region==country_name),]
        
        plot_by_country <- function(country_df, col_code){
            col_names <- colnames(country_df)
            
            date_info <- as.Date(col_names, "X%m.%d.%y")

            data_info <- as.numeric(country_df[1,])
            
            plot(1:length(col_names),
                 data_info,
                 ylim = c(0,max(data_info)),
                 type = "l",
                 pch = 16,
                 main = paste(country_name, " COVID-19 ",
                              chart_type, " Statistics"),
                 xlab = "Date",
                 ylab = "Cases",
                 xaxt = "n",
                 col = col_code)
            
            axis(1, at = seq(from = 5, 
                             to = length(col_names),
                             by = 10)
                 , labels = date_info[seq(from = 5, 
                                          to = length(col_names),
                                          by = 10)])
        }
        
        case_lst <- list(confirmed_country_case,
                         death_country_case,
                         recovered_country_case)
        col_code_lst <- c("blue", "red", "green")
        
        if (chart_type == "Confirmed"){
            index <- 1
        }
        else if(chart_type == "Death"){
            index <- 2
        }
        else if(chart_type == "Recovered"){
            index <- 3
        }
        local_case_df <- case_lst[[index]]
        local_case_df <- local_case_df[,5:dim(local_case_df)[2]]
        local_case_df <- as.data.frame(
            t(apply(local_case_df, 2, sum)))
        col_code <- col_code_lst[index]
        plot_by_country(local_case_df, col_code)
    })
    
    
    
    #Simulation 
    pop_size <- eventReactive(input$goButton, {input$pop_size})
    r0 <- eventReactive(input$goButton, {input$Rnot})
    output$simulationPlot <- renderPlot({
        input_time <- input$time
        input_time <- as.integer(input_time)
        pop_size <- as.numeric(pop_size())
        r0 <- as.numeric(r0())
        set.seed(pop_size*r0)
        map_size <- pop_size*0.25
        
        int_x <- runif(pop_size, -map_size, map_size) + rnorm(pop_size)
        int_y <- runif(pop_size, -map_size, map_size) + rnorm(pop_size)
        
        inf_x <- sample(int_x, 1)
        inf_y <- sample(int_y, 1)
        
        sus_x <- int_x[int_x!=inf_x]
        sus_y <- int_y[int_y!=inf_y]
        
        rem_x <- c()
        rem_y <- c()
        
        radius_check <- function(other_point, centre){
            distance <- (centre[1]-other_point[1])^2+
                (centre[2]-other_point[2])^2
            if (distance <= (map_size/10)^2){
                return(T)
            }
            else{
                return(F)
            }
        }
        
        boundary_filter <- function(x){
            if(x < -map_size){
                -map_size
            }
            else if(x > map_size){
                map_size
            }
            else{
                x
            }
        }
        
        for (index in 1:input_time){
            #Random Walk of Each Unit
            if (length(inf_x)!=0){
                ## infected ones always move around!
                inf_pop_size <- length(inf_x)
                inf_x <- inf_x + runif(inf_pop_size, -20, 20) + rnorm(inf_pop_size)
                
                inf_x <- sapply(inf_x, boundary_filter)
                
                inf_y <- inf_y + runif(inf_pop_size, -20, 20) + rnorm(inf_pop_size)
                
                inf_y <- sapply(inf_y, boundary_filter)
            }
            if (length(sus_x)!=0){
                sus_pop_size <- length(sus_x)
                sus_x <- sus_x + runif(sus_pop_size, -2, 2) + rnorm(sus_pop_size)
                
                sus_x <- sapply(sus_x, boundary_filter)
                
                sus_y <- sus_y + runif(sus_pop_size, -2, 2) + rnorm(sus_pop_size)
                
                sus_y <- sapply(sus_y, boundary_filter)
            }
            #Infection Stage
            
            for (inf_index in length(inf_x)){
                sus_mat <- matrix(c(sus_x, sus_y), 
                                  nrow = 2, 
                                  byrow = TRUE)
                centre <- c(inf_x[inf_index], inf_y[inf_index])
                sus_index <- which(
                    apply(sus_mat, 2, radius_check, centre) %in% c(TRUE))
                
                
                if (length(sus_index)!=0){
                    inf_rate <- (r0 - 1)/r0
                    new_inf_index <- 
                        sample(sus_index, size = 
                                   ceiling(
                                       inf_rate*(
                                           length(sus_index))))
                    if(length(new_inf_index)!=0){
                        inf_x <- c(inf_x, sus_x[new_inf_index])
                        inf_y <- c(inf_y, sus_y[new_inf_index])
                        sus_x <- sus_x[- new_inf_index]
                        sus_y <- sus_y[- new_inf_index]
                    }
                }
            }
            #Recovering Stage
            print(index)
            if (index %% 15 == 0){
                new_rem_index <- sample(1:length(inf_x),
                                        size = floor(0.1*length(inf_x)))
                if(length(new_rem_index) != 0){
                    rem_x <- c(rem_x, inf_x[new_rem_index])
                    rem_y <- c(rem_y, inf_y[new_rem_index])
                    inf_x <- inf_x[- new_rem_index]
                    inf_y <- inf_y[- new_rem_index]
                }
            }
        }
        
        
        plot(sus_x, sus_y,
             xlim=(c(-map_size,map_size)),
             ylim=(c(-map_size,map_size)),
             pch =19,
             col = rgb(0, 0, 0, alpha=0.2),
             xlab = "X Coordinate",
             ylab = "Y Coordinate",
             main = "Infection Map")
        points(inf_x, inf_y, col = "red",
               pch = 3)
        points(rem_x, rem_y, col = "green",
               pch = 24)
        legend("bottomright", col = c("black", "red", "green"),
               legend = c("Susceptible",
                          "Infected",
                          "Removed"),
               pch = c(19, 3, 24))
    })
})
