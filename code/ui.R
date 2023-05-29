library("shiny")
library("bslib")
library("shinyWidgets")
library("shinyjs")

theme <- bs_theme(bootswatch = "cosmo")

ui <-
  navbarPage(
    div(
      align = "left",
      "Airline on-time",
      style = "font-size: 30px;width:400px; margin-left: 20px;",
      class = "moving right"
    ),
    theme = theme,
    tabPanel(
      div("Home", style = "font-size: 30px;"),
      fluidPage(
        tags$head(tags$style(
          HTML(
            "
      .center-text {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
    "
          )
        )),
        tags$br(), 
        div( style = "width: 90%; margin: auto;",
        h3("This dashboard contains various visualizations
            (plots, maps and more) that aim to answer multiple research questions that
            we decided are the most interesting and worth exploring. For data we are using
            the 'Data Expo 2009: Airline on time data' from Harvard Dataverse, and we are
            looking at the years 2007 and 2006. Our team consists of: Paweł Świderski,
            Michał Zajączkowski, and Michał Szewczak.")),
        tags$br()
      ),
      fluidRow(column(
        12,
        align = "center",
        img(src = 'home_photo.png', height = "80%", width = "70%", align = "center ")
      ))
      
    ),
    tabPanel(
      div("When to fly?", style = "font-size: 30px;"),
      fluidPage(
        fluidRow(
          column(
            2,
            align = "center",
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
            h1("Best time to fly without delays",
               align = "center"),
            align = "center",
            style = 'border-left:1px solid #000000; padding-right: 30px',
            
            
            fluidRow(column(12,
                            align = "center",
                            plotOutput("timeDelayPlot")))
          )
          
        ),
        fluidRow(column(
          12,
          align = "center",
          tags$br(),
          tags$br(),
          div(
            style = "width: 90%; margin: auto;",
            h3(
              "So the best time to avoid delays is 5AM, Saturday, September.
                                   Less delay in early hours can be potentially explained by the fact that one delay causes another delay,
                                   and that amounts to more delays as the day continues. As for weekday and month, let us look
                                   at possible explanation by looking at these bar plots.",
              style = "text-align:center"
            )
          )
        )),
        fluidRow(
          column(
            6,
            tags$br(),
            h2("Weekday"),
            align = "center",
            plotOutput("weekday_delay_comb")
          ),
          column(
            6,
            tags$br(),
            h2("Month"),
            align = "center",
            style = 'border-left:3px solid #000000; padding-right: 10px',
            plotOutput("month_delay_comb")
          )
        ),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3(
              "As we see on Saturday there is also the least number of flights, which could definitely lead to smaller and fewer delays,
                               as for other days there is no clear correlation between the number of flights and delays.
                               However, when it comes to delays in different months we can see a strong correlation between
                               the general delay and delay caused by weather, meaning the weather delay plays a big role in generating delays.",
              style = "text-align:center"
            )
          ),
          tags$br(),
          tags$br()
        ))
      )
    ),
    tabPanel(
      div("Where to fly?", style = "font-size:30px;"),
      fluidPage(
        fluidRow(column(12,
                        align = 'center',
                        h1("Where the highest delays occur?"))),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3("Mean delays by states.",
               style = "text-align:center")
            
          )
        )),
        fluidRow(column(
          12,
          align = "center",
          plotOutput("stateDelayPlot", width = "100%")
        )),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3("We see that higher delays occur on the Eastern Coast, where there are many flights and severe weather conditions occur regularly. Strong winds may be important reasons for delays in the middle states, where the delays are bigger than in similar states on the west.",
               style = "text-align:center"),
            h3(
              "Lets also look at the top 10 flight routes with the biggest delays."
            )
          ),
          tags$br()
        )),
        fluidRow(column(
          12,
          align = "center",
          plotOutput("routesDelayPlot")
        )),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3("We can see that all of them depart or arrive at airports located in the states with the biggest delays. Most of the routes begin/end at a big airport, 
               which might also be an important factor.",
               style = "text-align:center"),
            h3(
              "If we now look at this visualization of distribuition of risks of different categories
             of strong winds we see a lot of similiarities with the maps of delays."
            )
          ),
          tags$br()
      )),
      fluidRow(column(
        12,
        align = "center",
        img(src = 'wind_per_state.png', width = "50%", align = "center "),
        tags$br(),
        tags$br()
      ))
      )
    ),
    tabPanel(
      div("Flights' location", style = "font-size:30px;"),
      fluidPage(#tags$head(tags$style(css)),
        #useShinyjs(),
        fluidRow(
          column(
            12,
            align = "center",
            h1("Where are people flying from?"),
            
            tags$br(),
            tags$br(),
            tags$br(),
            h3(
              "Heatmap visualizing the number of flights from airports in different months"
            )
            
          ),
          fluidRow(column(
            12,
            align = "center",
            
            htmlOutput('map'),
            tags$br(),
            tags$br()
            
          )),
          fluidRow(column(
            12,
            align = "center",
            div (
              style = "width: 80%; margin: auto;",
              h3(
                "From this heatmap, we can gather that there is a dense distribution of airports on the East Coast and in California,
                                 and the amount of flights from these airports is relative large. The middle and mid-western regions have a sparse distribution of
                                 airports and quite few number of flights. This heatmap has a lot of similarities to the map depicting population density in the USA,
                                        which is not surprising, because generally speaking: more people = more flights.",
                style = "text-align:center"
              )
              
            ),
            tags$br(),
            tags$br(),
            img(src = 'USA_states_population_density_map.png', align = "center ")
          ))
          
        ))
    ),
    tabPanel(
      div("Old planes = delays?", style = "font-size:30px;"),
      fluidPage(
        fluidRow(column(
          12,
          align = 'center',
          h1("Do older planes cause more delays?")
        )),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3(
              "There is a common, as it turns out misconception, that older planes cause more delays. Maybe because it has more failures
                                    and malfunctions that need fixing, or maybe because it lacks necessary technology. Whatever the preconceived justification may be,
                                    it could not be further from the truth.",
              style = "text-align:center"
            ),
            h3(
              "The coefficient of correlation and p-value are near 0 and from the graph below we see there is no visible relation between older planes and more delays (necessary calculation in file old_plane_delays.ipynb)"
            )
          )
        )),
        fluidRow(column(
          12,
          align = "center",
          img(src = 'old_plane_correlation.png', align = "center "),
          tags$br()
        )),
        fluidRow(column(
          12,
          align = 'center',
          div(
            style = "width: 90%; margin: auto;",
            h3(
              "However, maybe older planes do have a downside, just not the delays. Let's look at
            cancelled flights instead.",
              style = "text-align:center"
            ),
            h3(
              "We can see a clear increase in the percentage of cancelled flights for older planes.
            For most of them, the percentage is far higher than the average for all planes (dotted red line).
            This can be a result of several factors, including the higher likelihood of technical issues and potential vulnerability to weather conditions."
            )
          )
        )),
        fluidRow(column(12,
                        align = "center",
                        plotOutput("cancelledPlot"))),
      )
    )
  )
