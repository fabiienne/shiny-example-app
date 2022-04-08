options(shiny.maxRequestSize = 30*1024^2) #Increase filze size limit to 30MB
library(shiny) # Package for shiny applications
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs
library(shinythemes)
options(dplyr.summarise.inform = FALSE)


#course_data <- readRDS("europe.rds") %>% 
#  mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

# Define UI for application
ui <- fluidPage(
  
theme = shinytheme("lumen"),

titlePanel("My first Shiny!"),
"This is the first Shiny app I'm building It is pretty nice what shiny is able to do." ,
sidebarLayout(
  sidebarPanel( 
    "This is the sidebar panel",
      
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1, 
                  value = 2000,
                  sep = ''),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "country", label = "Country:",
                  choices = NULL),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "city", label = "City:",
                  choices = NULL),
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "temp_scale", label = "Temperature scale:",
                   choices = list("Fahrenheit" = "fahrenheit",
                                  "Celsius" = "celsius"),
                   selected = "fahrenheit"),
    
    #Input: Action button that subsets data ----
      actionButton(inputId= "button", label = "GO!"),
    #Input: Upload a Single RDS file ----
    fileInput(inputId =  "file",
              label = "Upload a file (RDS",
    multiple = FALSE, #Allow to upload multiple files
    accept = c(".rds")),
    
    downloadButton(outputId= "download", label = "download")
    ),
    
    # Main panel
    mainPanel(
      "This is the main panel",
      
      textOutput(outputId = "text_output"),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           h3("App description"),
                           hr(),
                          p("This is the course shiny app. It is created during the course 
                           exercises using the europe.rds data:", br(),
                          strong("Average daily temperatures (in Fahrenheit)"), "from cities around
                           Europe", strong("from 2000 to 2019")),
                          hr(),
                           
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",
                           
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(column(width= 12, plotOutput("lineplot"))
                                    ),
                           fluidRow(
                             column(width= 6, plotOutput("boxplot")),
                             column(width= 6, plotOutput("lineplotTemp"))
                  )
      )
    )
  )))


# Define server side logic ----
server <- function(input, output, session) {
  
  # Downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = "city_data.csv",
    content = function(file) {
      write.csv(city_df(), file, row.names = FALSE)
    }
  )
  
  
  #Upload file & read data ----
  course_data <- eventReactive(input$file, {
    readRDS(input$file$datapath) %>% 
    mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) 
  })

  #Reactive Expression 1 ----  
  country_df <- eventReactive(input$button,{
    course_data() %>%
      filter(Year >= input$year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$country) # Subset the rows to keep a specific country
  })
  
  #Reactive Expression 2 ----
  city_df <- reactive({
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) # Subset the rows for specific Year
  })
  
  year_df <- reactive({
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
    
  })
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$city, input$text_input, input$temp_scale)
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data())
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  
  # Output: Render a plot output  ----
  output$lineplotTemp <- renderPlot({
    
    if(input$temp_scale == "fahrenheit"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Fahrenheit)")
    }
    
    if(input$temp_scale == "celsius"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Celsius)")
    }
    
    return(res)
    
  })
  #Observer 1 ----
  observe({
    new_choices <- unique(course_data()$Country)
    
    updateSelectInput(session, inputId = "country", choices = new_choices)
    
    
    
  })## two Observers where needed because with the second new choices it has gone up and rework both codes
  #Observer 2----
  observe({
  new_choices <- unique(course_data()$City[course_data()$Country == input$country])
  
  updateSelectInput(session, inputId = "city", choices = new_choices)
  })

}
# Run the application 
shinyApp(ui = ui, server = server)


  
