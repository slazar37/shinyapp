library(dplyr)


# Read the Sites.csv file, and pass it for the UI for user to select the station:
sites <- read.csv("./Data/Sites.csv")
sites_info <- setNames(sites$Site_ID, sites$Site_Name)



weather_selection <- c("Wind Speed"="wind_speed", "Air Temperature"="air_temperature", 
                       "Relative Hunidity"="rltv_hum", "Visibility"="visibility")


aggregation_plot <- c("Raw hourly data"="raw_h_data", "Daily averages"="daily_averages",
                      "Monthly averages"="monthly_averages", "Daily maxima"="daily_maxima",
                      "Daily minima"="daily_minima")


time_options <- c("Calendar time"="calendar_time", "Daywithin week"="day_in_week",
                  "Hour in the day"="hour_in_day")



fluidPage(
    titlePanel("Shiny App"),
    fluidRow(
        column(3,
            wellPanel(                      
                
                # Select the weather stations you want examined (at most 5):
                selectizeInput("stationGroup", "Please select the stations to examine (at most 5):",
                               choices=sites_info,
                               multiple=TRUE,
                               options = list(maxItems=5L)
                ),
                
                
                # Select which weather variable you'd like to plot:
                selectizeInput("weatherGroup", "Please select the weather variable:",
                                   choices=weather_selection,
                                   multiple=FALSE
                ),
                
                # Select which aggregation you'd like to plot:
                selectizeInput("aggregationGroup", "Please select which aggregation you'd like to plot: ", 
                               choices=aggregation_plot,
                               multiple=FALSE
                ),
                

                        
                radioButtons("timeOptions", "Please select how to process time:", 
                             choices=time_options
                 ),
                        

                actionButton("drawGraph", "Draw the graph"),
                downloadButton("downloadCSV", "Download CSV table")
            )   
        ),
        column(3,
            mainPanel(
               tableOutput('table'),          # Show the table
               plotOutput('plot'),            # Show the graph
            )
        )
    )
)
