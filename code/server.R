library(hash)
library(rlang)

server <- function(input, output, session) {

  timeDelayPlot <- reactive({

    timeCategory = input$timeCategory
    label <- hash::hash()
    label[["DayOfWeek_name"]] <- "Day"
    label[["Month_name"]] <- "Month"
    label[["CRSHour"]] <- "Hour"
  
    timeDelayPlot <- df %>% 
      group_by_at(timeCategory) %>% 
      summarize(avg_delay = mean(AvgDelay, na.rm=TRUE)) %>% 
      ggplot(aes(x=!! sym(timeCategory), y=avg_delay)) +
      geom_bar(stat='identity', aes(fill = avg_delay == min(avg_delay))) +
        scale_x_discrete(label[[timeCategory]]) +
        scale_y_continuous("Average delay (in minutes)", expand = expansion(mult = c(0,0.1))) +
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_text(size = 20, color="#000000",margin=margin(0,15,0,0)),
              axis.title.x = element_text(size = 25, color="#000000",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 15, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.position = "none",
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
      geom_text(aes(label =  paste(
        as.character(round(as.numeric(substr(lubridate::dminutes(avg_delay),1,5)))%/%60),
        "m",
        as.character(round(as.numeric(substr(lubridate::dminutes(avg_delay),1,5))) - round(as.numeric(substr(lubridate::dminutes(avg_delay),1,5)))%/%60*60),
        "s",
        sep="")), vjust=-0.5)+
      guides(fill = guide_legend(byrow = TRUE))
    return(timeDelayPlot)
    })
  output$timeDelayPlot = renderPlot(timeDelayPlot(), bg="transparent")
  

  output$weekday_delay_comb = renderPlot({weekday_delay_comb}, bg="transparent")
  output$month_delay_comb = renderPlot({monthDelayPlot}, bg="transparent")
  
  
  carrierDelayPlot <- reactive({


    carrierDelayPlot <-  df_carrierDelay %>%
      filter(UniqueCarrier %in% LongFlightCarriers$UniqueCarrier) %>% 
      group_by(Description) %>%
      summarize(avg_delay = mean(AvgDelay, na.rm=TRUE)) %>%
      arrange(avg_delay) %>%
      head(10) %>% 
      ggplot(aes(x=Description, y=avg_delay)) +
      geom_bar(stat='identity', aes(fill = avg_delay == min(avg_delay))) +
      scale_y_continuous("Delay", expand = expansion(mult = c(0,0.1))) +
      theme_classic() +
      theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size = 25, color="#000000",margin=margin(0,15,0,0)),
            axis.title.x = element_text(size = 25, color="#000000",margin=margin(-10,0,0,0)),
            axis.text.x = element_text(size = 10, face='bold', angle=15, margin=margin(15,0,0,0)),
            axis.ticks.x = element_line(linewidth = 1.4),
            legend.position = "none",
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_rect(fill='transparent')) +
      geom_text(aes(label=round(avg_delay,2)), vjust=-0.5) +
      labs(x = "Airline") +
      guides(fill = guide_legend(byrow = TRUE))
    
    return(carrierDelayPlot)
  })
  output$carrierDelayPlot = renderPlot(carrierDelayPlot(), bg="transparent")
  
  
  
  addResourcePath("PDU_Projekt2", "~/Studia/PDU_Projekt2")
  output$map <- renderUI({
    tags$iframe(seamless="seamless",
                src= "PDU_Projekt2/heatmap_with_time.html",
                width=700,
                height=700
    )

    })
  
  

}