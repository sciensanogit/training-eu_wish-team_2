# load packages ----
pkgs <- c("shiny", "plotly", "ggplot2", "dplyr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# load nation data (ensure it has siteName = "Belgium" for dropdown)
df_nation <- read.csv2("./data/Belgium_export-nation.csv", sep = ";") %>%
  select(date, value_pmmv, value_pmmv_avg14d_past) %>%
  mutate(siteName = "Belgium")

# load the data by site and select appropriate variables
df_site <- read.csv2("./data/Belgium_export-site.csv", sep = ";") %>%
  select(date, siteName, value_pmmv, value_pmmv_avg14d_past)

# join (stack) nation + site data
df <- bind_rows(df_nation, df_site)



# clean data
df$date <- as.Date(df$date)
df$value_pmmv <- as.numeric(df$value_pmmv)*1000
df$value_pmmv_avg14d_past <- as.numeric(df$value_pmmv_avg14d_past)*1000

# ui ----
ui <- navbarPage(
  title = "Wastewater surveillance",
  
  tabPanel(
    "About",
    # Custom CSS for green header bar
    tags$style(HTML("
    .top-bar {
      background-color: #1F77B4;
      padding: 15px;
      margin-bottom: 20px;
      border-radius: 5px;
      color: white;
    }
    .top-bar label {
      color: white;
      font-weight: bold;
    }
    .info-box {
      background-color: #1F77B4;
      padding: 15px;
      border-radius: 5px;
      text-align: center;
      font-weight: bold;
      border: 1px solid #c8e6c9;
      margin-bottom: 20px;
    }
  ")),
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "Welcome to this nice page describing wastewater surveillance data on SARS-CoV-2" )
  ),
  
  tabPanel(
    "Influenza",
    # Custom CSS for green header bar
    tags$style(HTML("
     .top-bar {
      background-color: #1F77B4;
      padding: 15px;
      margin-bottom: 20px;
      border-radius: 5px;
      color: white;
    }
    .top-bar label {
      color: white;
      font-weight: bold;
    }
    .info-box {
      background-color: #FFD700;
      padding: 15px;
      border-radius: 5px;
      text-align: center;
      font-weight: bold;
      border: 1px solid #c8e6c9;
      margin-bottom: 20px;
    }
  ")),
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "In 2027 data will be added here" )
  ),
  
  tabPanel(
    "SARS-CoV-2",
    # Bottom acknowledgment box
    div(class = "info-box",
        "The surveillance of data on SARS-CoV-2 is presented" )
    ,
    
    # Three horizontal info boxes
    fluidRow(
      column(4, div(class = "info-box", "Number of site = 30")),
      column(4, div(class = "info-box", "Next sampling date = Wednesay")),
      column(4, div(class = "info-box", "Next dashboard update = Monday")
      )
    ),
    
    # Top bar with dropdown
    div(class = "top-bar",
        selectInput(
          inputId = "site",
          label = "Select sampling site",
          choices = unique(df$siteName),
          selected = "Belgium"
        )
    ),
    
    # Main content
    plotlyOutput("viralPlot")
    ,
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "Acknowledgment: Croatia team (CIPH)" )
    
  )
  
  
)



# server ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>% filter(siteName == input$site)
  })
  
  output$viralPlot <- renderPlotly({
    
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = date)) +
      geom_point(aes(y = value_pmmv), colour = "#1F77B4", size = 1, na.rm = T) +
      geom_line(aes(y = value_pmmv_avg14d_past), colour = "#BCCF00FF", linewidth = 1, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = paste(input$site),
        x = "", y = "(c/c)"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
}



# shinyApp ----
shinyApp(ui, server)

