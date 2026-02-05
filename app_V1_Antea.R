
#done:
#- run 01_data_prep-solution.R code to create ./data/_.csv, ./data/_.rds, and ./data/_.xlsx
#- check that Belgium_export-site.csv contains the right data
#- Click on the "Run app" button to run app_V1.R and vizualise your dashboard
#- Adapt the app_V1.R script to explain what is presented in the "About" tab
#- In the "SARS-CoV-2" tab, change the colors of text, xaxis, yaxis, date format and acknowledgement text
#- Add a new "Influenza" tab with a text explaining that in 2027 data will be added here


#to be done: 
#- Add a line to the graph using the color "#BCCF00FF" and the variable value_pmmv_avg14d_past
#- At line 10, load the data by site, select appropriate variables and join it to the nation data
#- Run app to display changes
#- Share your version to your team through github and test apps of developed by others
#- Discover how to deploy your shinyapp online on https://www.shinyapps.io/
  
# prvo run code: 01_data_prep-solution.R

# load packages ----
pkgs <- c("shiny", "plotly", "ggplot2", "dplyr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# load nation data
df_nation <- read.csv2("./data/Belgium_export-nation.csv", sep = ";")

# load the data by site and select appropriate variables
# df_site <- read.csv2("??.csv", sep = ";") %>%
#   select(??)

# bind nation and site data
df <- rbind(df_nation)

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
      background-color: #e8f5e9;
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
      background-color: #4CAF50;
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
        "Welcome to this nice page describing..." )
  ),
  
  
  tabPanel(
    "SARS-CoV-2",
    # Bottom acknowledgment box
    div(class = "info-box",
        "For task 1: write description here - description..." )
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
    plotlyOutput("sars_plot")
    ,
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "Acknowledgment: task is to write here something: something" )
    
  ),
  
  tabPanel(
    "Influenza",
    fluidPage(
      h3("Influenza"),
      p(
        "put influenza data here",
        br(),
        "No data for now. Data will be provided in 2027 "
      )
    )
  )
  
  
)

# server ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>% filter(siteName == input$site)
  })
  
  output$sars_plot <- renderPlot({
    
    ggplot(df_nation, aes(x = date, y = value)) +
      geom_line(color = "black", linewidth = 1) +
      
      # FORMAT DATUMA NA X OSI
      scale_x_date(
        date_labels = "%d.%m.%Y",
        date_breaks = "1 month"
      ) +
      
      labs(
        x = "date",
        y = "SARS-CoV-2",
        caption = "Acknowledgement: Data provided by EU WISH project"
      ) +
      
      theme_minimal() +
      theme(
        # text – pink
        text = element_text(color = "deeppink"),
        
        # X axis - blue
        axis.text.x  = element_text(color = "blue"),
        axis.title.x = element_text(color = "blue"),
        
        # Y axis – green
        axis.text.y  = element_text(color = "darkgreen"),
        axis.title.y = element_text(color = "darkgreen"),
        
        # acknowledgement text (caption) - pink
        plot.caption = element_text(
          color = "deeppink",
          size = 10,
          hjust = 0
        )
      )
  })
}

# shinyApp ----
shinyApp(ui, server)