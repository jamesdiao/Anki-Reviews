#title: "Modeling Anki Review Load"
#author: "James Diao"
#date: "February 23, 2019"

# rsconnect::deployApp('/Users/jamesdiao/Documents/R/Anki-Reviews')
# https://jamesdiao.shinyapps.io/ankireviews/

#Set working directory to file folder
#outdir <- getSrcDirectory(function(dummy) {dummy})
#setwd(outdir)

### To-do
# write-up methods
# complete simulation
# experimental validation
# handle "small cases" (unsmoothed estimates?)
# handle small span
# write-up blog post

require(shiny)
library(shinysky)
require(dplyr)
require(tidyr)
require(ggplot2)
require(plotly)

h_read <- function(timing) {
  timing <- as.numeric(timing*60)
  days <- floor(timing/86400)
  hours <- floor((timing - 86400*days)/3600)
  minutes <- floor((timing - 86400*days - 3600*hours)/60)
  seconds <- floor(timing - 86400*days - 3600*hours - 60*minutes)
  day.in <- ifelse(hours == 1, "1 day, ",ifelse(days == 0, "", paste(days,"days, ")))
  hour.in <- ifelse(hours == 1, "1 hour",ifelse(hours == 0, "", paste(hours,"hours")))
  paste(day.in, hour.in, sep = "")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Modeling Anki Review Load"),
   h5("Last Updated: 23 February 2019"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("cardsperday",
                     "Cards Per Day:",
                     min = 10,
                     max = 200,
                     value = 50, 
                     step = 5),
        sliderInput("cutoff",
                    "Total Cards in Deck:",
                    min = 2000,
                    max = 30000,
                    value = 28000, 
                    step = 1000),
        sliderInput("forget",
                    "Error Rate:",
                    min = 0,
                    max = 0.2,
                    value = 0.06, 
                    step = 0.01),
        radioButtons("cumul", 
                     label = "Graph", 
                     choices = c("Normal", "Cumulative"), 
                     selected = "Normal"
        ),
        radioButtons("format", 
                     label = "Display", 
                     choices = c("Cards", "Time"), 
                     selected = "Cards"
                     ),
        radioButtons("loadbalancer", 
                     label = "Load Balancer", 
                     choices = c("On", "Off"), 
                     selected = "On"
        ),
        conditionalPanel(
            condition = "input.format == 'Time'",
            sliderInput("revperminute",
                        "Reviews per Minute:",
                        min = 1,
                        max = 20,
                        value = 5, 
                        step = 1),
            sliderInput("newperminute",
                        "New Cards per Minute:",
                        min = 0.1,
                        max = 20,
                        value = 2, 
                        step = 0.1)
        )
      ),
      # Show a plot of the generated distribution
      mainPanel(
         #h3("Reviews Over Time"),
         plotlyOutput("plot", height = "500px")
         #h2(),
         #ableOutput("table")
      )
   ),
   p("For methodology and validation info: jamesdiao.com/archives/2019/02/23/anki-reviews")
   #uiOutput("website")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$website <- renderUI({
  #  tagList("For methodology and validation info:", a("jamesdiao.com/archives/2019/02/23/anki-reviews/", 
  #    href="jamesdiao.com/archives/2019/02/23/anki-reviews/"))
  #})
  
  output$plot <- renderPlotly({
     loadbalancer <- input$loadbalancer == "On"
     cardformat <- input$format == "Cards"
     cumul <- input$cumul == "Cumulative"
     newpermin <- input$newperminute
     revpermin <- input$revperminute
     if (cardformat) {
       revpermin <- 1
       newpermin <- 1
     }
     cutoff <- input$cutoff
     cardsperday <- input$cardsperday
     forget <- input$forget
     days <- cutoff / cardsperday
     rlen <- ceiling(1.5*days)
     multiplier <- 2.5
     terminal_interval <- 180
     added_terms = 0
     max_pwr <- floor(log(terminal_interval, base = multiplier))
     
     intervals <- ceiling(multiplier^(0:max_pwr)) #c(1, 3, 7, 16, 40, 98)
     if (rlen > terminal_interval) 
       added_terms <- floor((rlen-sum(intervals))/terminal_interval)
     # Cumulative sum of interval gaps (1, 4, 11, ...)
     csintervals <- c(intervals, rep(terminal_interval, added_terms)) %>% cumsum
     # Add even-odd jitter to the cumulative interval after some number (keep)
     # to stabilize stacking effects
     toggle <- csintervals[4:length(csintervals)]
     csintervals[4:length(csintervals)] <- toggle + (toggle %% 2 + rep(c(0,1), length.out = length(toggle)))
     # Scaling factor for adds
     scaling <- 1 + forget*(sum(days > csintervals)-2)
     # Power series of the forget decay factor (e.g., 1.0, 0.90, 0.81, ...)
     decay_0 <- (1-forget)^(0:length(csintervals))
     # Empty vector for number of reviews
     reviews <- rep(0, rlen)
     
     for (i in 1:days) {
       # `range` defines the index of consistently correct cards (2, 5, 12, ...)
       range <- i + csintervals
       # Make sure `range` fits inside the `reviews` vector
       range <- range[range <= rlen]
       # Make sure `decay` is the same size as `range`
       decay <- (1-forget)^(0:(length(range)-1))
       # Add reviews from "perfectly correct" cards initially added on day `i`
       # graded_scaling <- (1-(days-cut_csint[i]) / days) * (scaling-1) + 1
       reviews[range] <- reviews[range] + cardsperday * decay * scaling * 0.9
       # For each added day (j), add back the series of incorrect cards. 
       # Assume that none of them are wrong more than twice.
       
       
       for (j in 1:(length(range))) {
         # `j_range` is the indexes of the series of incorrect cards to add back for each `j`
         j_range <- range - 1 + range[j]
         # Make sure `j_range` fits into the `reviews` vector
         j_range <- j_range[j_range <= rlen]
         if (length(j_range) == 0) break
         # Make sure `decay` is the same size as `range`
         j_decay <- (1-forget)^((j-1):(length(j_range)+j-2))
         # Add back the series of incorrect cards (2nd term = # of incorrect cards). 
         reviews[j_range] <- reviews[j_range] + forget*cardsperday*j_decay*scaling #)[j]
         #if (max(j_range)+1 <= rlen) {
         #  reviews[j_range+1] <- reviews[j_range+1] + (forget^2*cardsperday) * (1-forget)^(j-1)
         #}
       }
     }
     
     # Detrending
     if (rlen/days > 2.5) {
       detrend_days <- (days*2.1):length(reviews)
       detrend <- lm(reviews[detrend_days] ~ detrend_days) %>% coef
       reviews[detrend_days] <- reviews[detrend_days] - detrend[2]*detrend_days
     }

     #exp_max <- cardsperday*sum(1/(1-forget)^(1:sum(days > csintervals)))
     #exp_converge <- cutoff/terminal_interval * (1 + forget * length(intervals))
     
     #reviews[csintervals[3]:days] <- reviews[csintervals[3]:days] * exp_max/max(reviews)
     #exp_max <- exp_max / revpermin
     #exp_converge <- exp_converge / revpermin
     #reviews[(days+1):length(reviews)] <- reviews[(days+1):length(reviews)] * 
     #                                     exp_converge/mean(reviews[days:(2*days)])
     
     if (!cardformat) {
       reviews <- reviews / revpermin + c(rep(cardsperday / newpermin, days), 
                                          rep(0, length(reviews)-days))
     }
     
     # Round review counts to integers
     reviews <- reviews %>% round(0)
     if (cumul)
       reviews <- cumsum(reviews)
     # Two-part smoothing process to simulate load balancing
     review_data <- data.frame(Day = 1:length(reviews), estimated = reviews)
     smoothed_1 <- reviews[1:4]
     smoothed_2 <- predict(loess(estimated ~ Day, 
                                 data = review_data[5:nrow(review_data),], 
                                 span = 0.1)) #%>% round(0)
     if (loadbalancer) {
       smoothed <- c(smoothed_1, smoothed_2)
     } else {
       smoothed <- reviews
     }
     # Final data frame     
     if (cardformat) {
        all_data <- data.frame(review_data, Reviews = smoothed)
        plotdata <- all_data %>% gather(key = "Type", value = "Reviews", -Day) %>% 
          filter(Type == "Reviews")
        plot <- plot_ly(data = plotdata, x= ~Day, y = ~Reviews, fill = 'tozeroy', 
                        type = 'scatter', mode = 'lines', hoverinfo = 'text',
                        text = ~paste("Day: ", Day, '<br>Reviews: ', Reviews %>% round(0))
        ) 
     } else {
        all_data <- data.frame(review_data, Time = smoothed)
        plotdata <- all_data %>% gather(key = "Type", value = "Time", -Day) %>% 
          filter(Type == "Time")
        plot <- plot_ly(data = plotdata, x= ~Day, y = ~Time, fill = 'tozeroy', 
                        type = 'scatter', mode = 'lines', hoverinfo = 'text', 
                        text = ~paste("Day: ", Day, '<br>Time (Min): ', Time %>% round(0))
        )  %>% layout(yaxis = list(title = "Time (Min)", range = c(0,max(reviews)*1.1)))
     }
     plot$elementId <- NULL
     plot #<- plot %>% layout(showlegend = FALSE)
     
    # plotdata <- all_data %>% gather(key = "Type", value = "Reviews", -Day) %>% 
    #   filter(Type == "Reviews")
    # plot <- ggplot(plotdata, aes(x=Day, y=Reviews)) + geom_line(lwd = 0.8) + 
    #   theme_bw() + geom_hline(yintercept = c(exp_converge, exp_max), linetype="dashed") + 
    #   #geom_vline(xintercept = days, linetype="dotted") + 
    #   theme(legend.position="bottom") +
    #   theme(legend.title=element_blank())
    # if (!cardformat) 
    #   plot <- plot + ylab("Minutes")
    # plot <- ggplotly(plot) 
    # plot$elementId <- NULL
    # plot
     
   })
   output$table <- renderTable({
     
     cardformat <- input$format == "Cards"
     revpermin <- input$revperminute
     newpermin <- input$newperminute
     if (cardformat) {
       revpermin <- 1
       newpermin <- 1
     }
     cutoff <- input$cutoff
     cardsperday <- input$cardsperday
     forget <- input$forget
     days <- cutoff / cardsperday
     rlen <- ceiling(1.5*days)
     multiplier <- 2.5
     terminal_interval <- 180
     added_terms = 0
     max_pwr <- floor(log(terminal_interval, base = multiplier))
     intervals <- ceiling(multiplier^(0:max_pwr)) #c(1, 3, 7, 16, 40, 98)
     if (rlen > terminal_interval) 
       added_terms <- floor((rlen-sum(intervals))/terminal_interval)
     # Cumulative sum of interval gaps (1, 4, 11, ...)
     csintervals <- c(intervals, rep(terminal_interval, added_terms)) %>% cumsum
     exp_max <- cardsperday*sum(1/(1-forget)^(1:sum(days > csintervals))) / revpermin
     exp_converge <- cutoff/terminal_interval * (1 + forget * length(intervals)) / revpermin

     if (cardformat) {
       output <- data.frame(Completion_Date = sprintf("Day %s", days %>% round(0)), 
                            Theoretical_Max_Review_Count = sprintf("%s Cards", exp_max%>% round(0)), 
                            Converged_Review_Count = sprintf("%s Cards", exp_converge %>% round(0))#,
                            #Total_Reviews = sprintf("%s Cards", sum(reviews[1:days]) %>% round(0))
                            )
       colnames(output) <- c("Completion Date","Peak Reviews Per Day","Maintenance Reviews Per Day")#,"Total Reviews before Completion")
     } else {
       output <- data.frame(Completion_Date = sprintf("Day %s", days %>% round(0)), 
                            Theoretical_Max_Time = sprintf("%s Min", (exp_max + cardsperday / newpermin) %>% round(0)), 
                            Converged_Time = sprintf("%s Min", exp_converge %>% round(0))#,
                            #Total_Time = sprintf("%s", sum(reviews[1:days]) %>% round(0) %>% h_read)
                            )
       colnames(output) <- c("Completion Date","Peak Time Per Day","Maintenance Time Per Day")#,"Total Time before Competion")
     }
     output
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

