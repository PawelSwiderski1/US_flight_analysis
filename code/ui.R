library("shiny")
library("bslib")
library("shinyWidgets")
library("shinyjs")

# my_theme <- bs_theme(
#   version = 5,
#   bg = "#FFFFFF",
#   fg = "#000000",
#   primary = "#000000",
#   base_font = 'Roboto_condensed',
#   heading_font = font_google("Proza Libre"),
#   code_font = 'Roboto_condensed'
# )

css <- HTML("
        .row.nudge-right {
            padding-right:100;
            }
        .row.nudge-left {
            padding-left:10;
            }")

ui <- navbarPage(div(align = "left","Airline on-time performance",style="font-size: 30px;width:400px; margin:20 auto;",class="moving right"),
                 # theme = my_theme,
                 tabPanel(div("When to fly?", style = "font-size: 30px;"),
                          fluidPage(
                            fluidRow(
                              column(2, align = "center", 
                                     sidebarPanel(
                                       awesomeRadio(
                                         inputId = "timeCategory",
                                         label = "Time Category",
                                         choices = c(
                                           "Hour" = "CRSHour",
                                           "Weekday" = "DayOfWeek_name",
                                           "Month" = "Month_name"
                                         ),
                                         status = "danger"
                                       ),
                                       width = 200,
                                       align = "left"
                                     )
                              ),
                              column(
                                10,
                                h1(
                                  "Best time to fly without delays",
                                  align = "center",
                                  style = "color: #ff9933" 
                                ),
                                align = "center", 
                                style = 'border-left:1px solid #65D36E; padding-right: 30px',
                               
                                
                                fluidRow(
                                  column(
                                    12,
                                    align = "center",
                                   plotOutput("timeDelayPlot")
                                  )

                                )
                              )
                            
                            ),
                            fluidRow(
                              column(
                                12,
                                align = "center",
                                tags$br(), tags$br(),
                                div(
                                style = "width: 90%; margin: auto;",
                                h3("So the best time to avoid delays is 6AM, Saturday, September.
                                   Less delay in early hours can be potentially explained by the fact that one delay causes another delay
                                   and that amounts to more delays as the day continues. As for weekday and month let us look
                                   at possible explanation by looking at these bar plots.", style = "text-align:center"),
                                ),
                                tags$br(), tags$br(),
                                
                                
                                
                              )
                            ),
                            fluidRow(
                              

                              column(
                                6,
                                h2("Weekday"),
                                align = "center",
                                plotOutput("weekday_delay_comb")
                              ),
                              column(
                                6,
                                h2("Month"),
                                align = "center",
                                style = 'border-left:1px solid #65D36E; padding-right: 30px',
                                plotOutput("month_delay_comb")
                            )
                          ),
                          fluidRow(
                            align = 'center',
                            div(
                            style = "width: 90%; margin: auto;",
                            h3("As we see on saturday there is also the least number of fligths, which could definitely lead to less delays,
                               as for other days there is no clear correlation between the number of flights and delays. 
                               However, when it comes to delays in different months we can see a strong correlation between 
                               the general delay and delay caused by weather, meaning the weather delay plays a big role in generating delays.", style="text-align:center"),
                            ),
                            tags$br(), tags$br()
                          )
                          )  
                 ),
                 tabPanel(div("Flights' location", style = "font-size:30px;"),
                          fluidPage(
                            #tags$head(tags$style(css)),
                            #useShinyjs(),
                            fluidRow(
                            column(
                              12,
                              align = "center",
                              h1("Where are people flying from?"),

                              tags$br(), tags$br(), tags$br(),
                              h3("Heatmap visualizing the number of flights from airports in different months")
                              
                            ),
                            fluidRow(
                            column(
                              12,
                              align = "center",
                            
                              htmlOutput('map'),
                              tags$br(),
                              tags$br()

                              )
                            ),
                            fluidRow(
                              column(12,
                              align = "center",
                              div (
                                style = "width: 80%; margin: auto;",
                                h3(HTML("From this heatmap we can gather that there is a dense distrubution of airports on the east coast and in California,
                                 and the amount of flights from these airports is relative large. The middle and mid-western regions have a sparse distribution of
                                 airports and quite few number of fligths. This heatmap has a lot of similiarites to the map depicting population denisty in the USA,
                                        which is not surprising, beacuse generally speaking: more people = more flights."),style="text-align:center"),
                                
                              ),
                              tags$br(),tags$br(),
                              img(src='USA_states_population_density_map.png', align = "center ")
                              )
                            )
                          )
                 )
                 ),
                 tabPanel(div("old planes", style = "font-size:30px;"),
                          fluidPage(
                            align = "center",
                            img(src='old_plane_correlation.png', align = "center "),
                            tags$br()
                          )
                 )
)
