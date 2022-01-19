library(dplyr)     # For working with data frames
library(ggplot2)   # For drawing plots
library(magrittr)  # For pipelines
library(tibble)    # For working with dataframes
library(tidyverse)



# Define a function that reads the specified wearther files from the folder:
getFileNames <- function(stations) {
  # Get all the filenames from the directory:
  fileNames = list.files(path="./Data/", pattern = "*.csv")
  # Remove the Sites.csv file as we are not using it for this function:
  fileNames <- fileNames[fileNames != "Sites.csv"]
  # Get the file numbers from their names:
  fileNumbers <- as.numeric(gsub(".*?([0-9]+).*", "\\1", fileNames))
  # Use the two vectors to convert fileNames into a key-value set:
  names(fileNames) <- fileNumbers
  
  # Form a list of files to read:
  files <- c()
  for (i in stations) {
    files <- c(files, fileNames[i])
  }
  
  # Return the list of selected file names:
  return(files)
}



# Define the function to read the sites defined by the user:
readFiles <- function(stations, weather_variable) {
  
  
  path="./Data"
  weather_df <- do.call(rbind, lapply(paste(path, stations, sep="/"), read.csv))
  
  
  # Extract only the needed columns:
  
  # Air Temperature (air_temperature) and Relative Humidity (rltv_hum) are needed for calculating the Hutton Criteria.
  # Make sure these two columns are always included in the data set:
  if (weather_variable == "air_temperature") {
    weather_df <- weather_df %>% select('ob_time', 'hour',	'day',	'month', 'Site', 'rltv_hum', weather_variable)
  }
  else if (weather_variable == "rltv_hum") {
    weather_df <- weather_df %>% select('ob_time', 'hour',	'day',	'month', 'Site', 'air_temperature', weather_variable)
  }
  else {
    weather_df <- weather_df %>% select('ob_time', 'hour',	'day',	'month', 'Site', 'air_temperature', 'rltv_hum', weather_variable)
  }
  
  
  # Replace the Site ids with their names:
  weather_df <- renameDf(weather_df)
  
  # Drop all the missing values from the data frame:
  weather_df <- weather_df %>% drop_na()
  
  return(weather_df)
  
}


# Define a function that replaces the ids of the Sites in the df with their names:
renameDf <- function(df) {
  
  sites <- read.csv("./Data/Sites.csv")
  sites_info <- setNames(sites$Site_ID, sites$Site_Name)
  
  
  # Get all the filenames from the directory:
  fileNames = sites$Site_Name

  # Get the file numbers from their names:
  fileNumbers <- sites$Site_ID
  
  # Use the two vectors to convert fileNames into a key-value set:
  names(fileNames) <- fileNumbers
  
  # Replace station ids with names:
  df$Site <- fileNames[ as.character(df$Site) ]
  
  # Return the dataframe with station names:
  return(df)
}


# Define a function that plots the graph based on the user selection:
drawPlot <- function(df) {
  
  # Get the column names:
  col_names <- colnames(df)
  # Get the weather variable column name:
  weather_variable <- col_names[length(col_names)]
  weather_variable_aggregation <- col_names[length(col_names) - 1]
  # Get the xAxis from the df (will always be in the first column of the df):
  xAxis <- col_names[1]
  
  graph_title <- "" # variable will store the graph title
  x_text <- ""      # variable will store the x-axis title
  y_text <- ""      # variable will store the y-axis title
  
  
  # Set the graph title/y axis name based on variable:
  graph_title_weather <- ""
  y_title_weather <- ""
  if(weather_variable == "wind_speed" || weather_variable_aggregation == "wind_speed") {
    graph_title_weather <- "Wind speed"
    y_title_weather <- "wind speed"
  }
  else if(weather_variable == "air_temperature" || weather_variable_aggregation == "air_temperature") {
    graph_title_weather <- "Air temperature"
    y_title_weather <- "air temperature"
  }
  else if (weather_variable == "rltv_hum" || weather_variable_aggregation == "rltv_hum") {
    graph_title_weather <- "Relative humidity"
    y_title_weather <- "relative humidity"
  }
  else if (weather_variable == "visibility" || weather_variable_aggregation == "visibility") {
    graph_title_weather <- "Visibility"
    y_title_weather <- "visibility"
  }
  
  
  # Find out what type of aggregation was performed, and adjust title and y-axis text accordingly:
  col_names <- colnames(df)
  if(col_names[length(col_names)] == "daily_average") {
    graph_title <- paste(graph_title_weather, "(daily averages)", sep = " ")
    y_text <- paste("Daily average", y_title_weather, sep = " ")
  }
  else if(col_names[length(col_names)] == "daily_minima") {
    graph_title <- paste(graph_title_weather, "(daily minima)", sep = " ")
    y_text <- paste("Daily minima", y_title_weather, sep = " ")
  }
  else if(col_names[length(col_names)] == "daily_maxima") {
    graph_title <- paste(graph_title_weather, "(daily maxima)", sep = " ")
    y_text <- paste("Daily maxima", y_title_weather, sep = " ")
  }
  else if(col_names[length(col_names)] == "monthly_average") {
    graph_title <- paste(graph_title_weather, "(monthly averages)", sep = " ")
    y_text <- paste("Monthly average", y_title_weather, sep = " ")
  }
  else {
    graph_title <- paste(graph_title_weather, "Raw hourly data (no aggregation)", sep = " ")
    y_text <- paste("raw hourly data (no aggregation)", y_title_weather, sep = " ")
  }
  
  
  # Get the name of the xAxis and graph type:
  graph_type <- ""
  if(xAxis == "ob_time") {
    x_text <- "Date"
    graph_type <- geom_line()
  } 
  else if (xAxis == "day") {
    x_text <- "Day"
    graph_type <- geom_point()
  }
  else if (xAxis== "hour") {
    x_text <- "Hour"
    graph_type <- geom_point()
  }
  
   
  # Draw the actual graph:
  p <- ggplot(data = df, aes_string(y = weather_variable, x = xAxis, group="Site", label="Site", color="Site" )) + 
  graph_type + 
  xlab(x_text) + 
  ylab(y_text) + 
  ggtitle(graph_title)
  
  
  show(p)
  
}


# Define a function to calculate the Hutton Criteria:
calculateHutton <- function(df, weather_variable) {
  
  # Hutton Variable is true if:
  # 1) two previous days have a temperature of 10 degrees Celsius.
  # 2) Two previous days have at least six hours of relative humidity of 90% or higher.
  
  
  # Find out if the minimal temperature of the previous two days is at least 10 degrees Celsius:
  df %<>% group_by(day) %>% transform(minimal_daily_temperature = min(air_temperature)) 
  
  # Make sure that the df still has the expected column order:
  col_order <- ""
  if (weather_variable == "air_temperature") {
    col_order <- c("ob_time", "minimal_daily_temperature", "hour", "day", "month", "rltv_hum", "Site", "air_temperature")
  }
  else if (weather_variable == "rltv_hum") {
    col_order <- c("ob_time", "minimal_daily_temperature", "hour", "day", "month", "air_temperature", "Site", "rltv_hum")
  }
  else {
    col_order <- c("ob_time", "minimal_daily_temperature", "hour", "day", "month", "air_temperature", "rltv_hum", "Site", weather_variable)
  }
  df <- df[, col_order]
  
  
  # Calculate the Hutton Criteria:
  df <- df %>% group_by(day) %>% group_by(hour) %>% mutate(hutton_criteria = (lag(minimal_daily_temperature, n=2) >= 10) & (lag(rltv_hum, n=6)) >= 90)
  
  
  # Get rid of unneeded columns, and reorder them:
  col_order2 <- ""
  if (weather_variable == "air_temperature") {
    df <- within(df, rm(minimal_daily_temperature))
    df <- within(df, rm(rltv_hum))
    col_order2 <- c("ob_time", "hour", "day", "month", "hutton_criteria", "Site", "air_temperature")
  }
  else if (weather_variable == "rltv_hum") {
    df <- within(df, rm(minimal_daily_temperature))
    df <- within(df, rm(air_temperature))
    col_order2 <- c("ob_time", "hour", "day", "month", "hutton_criteria", "Site", "rltv_hum")
  }
  else {
    df <- within(df, rm(minimal_daily_temperature))
    df <- within(df, rm(air_temperature))
    df <- within(df, rm(rltv_hum))
    col_order2 <- c("ob_time",  "hour", "day", "month", "hutton_criteria", "Site", weather_variable)
  }
  df <- df[, col_order2]
  
  
  return(df)
  
}



shinyServer(function(session, input, output) {
  

  # Allow user to select 5 weather stations to read data from at most: 
  observe({
    updateSelectizeInput(session, "station", selected = isolate(input$station), 
                           options = list(maxItems = 5L - (length(input$station))))
  })
  
  
  # Extract table based on user input when button clicked:
  getData <- eventReactive(input$drawGraph, {
    
    # Store the user-selected options into variables:
    stations <- input$stationGroup           # user-selected stations to examine
    weather_variable <- input$weatherGroup   # user-selected weather variable to examine
    aggregation <- input$aggregationGroup    # user-selected aggregation group
    xAxis <- input$timeOptions               # user-selected option for the xAxis
    
    
    # Store the user-selected stations into a variable:
    files <- getFileNames(stations)
    # Read the weather data from the user-selected sites into a data frame:
    sites_df <- readFiles(files, weather_variable)
    
    
    
    # Calculate the Hutton Criteria:
    sites_df <- calculateHutton(sites_df, weather_variable)
    
    
    # Perform the requested aggregation:
    if(aggregation == "daily_averages") {
      # sites_df <- within(sites_df, rm(ob_time))
      mean_fun <- paste0("mean(", weather_variable, ")")
      sites_df %<>% group_by(Site) %>% group_by(day) %>% mutate_(daily_average = mean_fun)
    }
    else if(aggregation == "daily_minima") {
      # Find the daily minima:
      # sites_df <- within(sites_df, rm(ob_time))
      min_fun <- paste0("min(", weather_variable, ")")
      sites_df %<>% group_by(Site) %>% group_by(day) %>% mutate_(daily_minima = min_fun)
    }
    else if(aggregation == "daily_maxima") {
      # Find the daily maxima:
      # sites_df <- within(sites_df, rm(ob_time))
      max_fun <- paste0("max(", weather_variable, ")")
      sites_df %<>% group_by(Site) %>% group_by(day) %>% mutate_(daily_maxima = max_fun)
    }
    else if(aggregation == "monthly_averages") {
      # Find the monthly averages:
      # sites_df <- within(sites_df, rm(ob_time))
      mean_fun <- paste0("mean(", weather_variable, ")")
      sites_df %<>% group_by(Site) %>% group_by(month) %>% mutate_(monthly_average = mean_fun)
    }
    
    
    # Get the selected value for the xAxis (remove the others):
    if (xAxis == "calendar_time") {
      sites_df <- within(sites_df, rm(hour))
      sites_df <- within(sites_df, rm(day))
      sites_df <- within(sites_df, rm(month))
    }
    else if (xAxis == "day_in_week") {
      sites_df <- within(sites_df, rm(ob_time))
      sites_df <- within(sites_df, rm(hour))
      sites_df <- within(sites_df, rm(month))
    }
    else if(xAxis == "hour_in_day") {
      sites_df <- within(sites_df, rm(ob_time))
      sites_df <- within(sites_df, rm(day))
      sites_df <- within(sites_df, rm(month))
    }
    
    return(sites_df)
    
  })
  
  
  # Show the first 6 rows of the selected table:
  output$table <- renderTable( { 
    

    df <- head(getData())
    sapply(df, class)
    df
    
    
  }, height = 200, width = 800)

  
  
  # Plot the graph based on the user input:
  output$plot <- renderPlot( { 
    
    drawPlot(getData())  
    
  }, height = 500, width = 1000)
  

  
  # Allow the user to download data:
  output$downloadCSV <- downloadHandler(
    
      filename = function(){
      
        paste("data-", Sys.Date(), ".csv", sep="")
      
      },
    
      content = function(file) {
      
        write.csv(getData(), file)
      
      }
    
    )
  
  
}) 
