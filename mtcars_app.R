# 1.  Business Logic 

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(thematic)

# Activate {thematic} for consistent plot styiling 

thematic:: thematic_shiny()

# Convert some variables in mtcars to factors for categorical analysis
# These are used for discrete analysis and plots

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am)
mtcars$vs <- factor(mtcars$vs)

# 2. UI Definition for the App to get inputs and display outputs

ui <- fluidPage(
  
  # Custom theme with bslib
  theme = bslib::bs_theme(
    bg = "#FAFAFA",         # Dark solarized background
    fg = "#657B83",         # Light text color
    primary = "#268BD2",    # Cyan-like accent color
    base_font = bslib::font_google("Pacifico")  # Fun Google Font
  ),
  
  # App Title at the top
  titlePanel("MTCars Data Explorer"),
  
  # Layout with sidebar for inputs and main panel for content 
  sidebarLayout(
    
    #  Sidebar: Input controls
    sidebarPanel(
      
      # Show specific controls only when matching tab is active
      conditionalPanel(
        condition = "input.tabSelected == 'Data'",
        checkboxGroupInput("selected_vars", "Choose variables to display:",
                           choices = names(mtcars), selected = names(mtcars))
      ),
      conditionalPanel(
        condition = "input.tabSelected == 'BoxPlot'",
        selectInput("box_x", "Select categorical variable (X):", choices = names(mtcars)[sapply(mtcars, is.factor)]),
        selectInput("box_y", "Select numeric variable (Y):", choices = names(mtcars)[sapply(mtcars, is.numeric)])
      ),
      conditionalPanel(
        condition = "input.tabSelected == 'Bar'",
        selectInput("bar_var", "Select discrete variable:", choices = names(mtcars)[sapply(mtcars, is.factor)])
      ),
      conditionalPanel(
        condition = "input.tabSelected == 'Histogram'",
        selectInput("hist_var", "Select continuous variable:", choices = names(mtcars)[sapply(mtcars, is.numeric)]),
        sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 10)
      )
    ),
    
    # Main Panel: Outputs displayed here
    mainPanel(
      tabsetPanel(id = "tabSelected",
                  tabPanel("Data", value = "Data", tableOutput("data_table")),
                  tabPanel("Summary", value = "Summary",
                           h4("Continuous Variables Summary"),
                           verbatimTextOutput("cont_summary"),
                           h4("Discrete Variables Summary"),
                           verbatimTextOutput("disc_summary")
                  ),
                  tabPanel("BoxPlot", value = "BoxPlot", plotOutput("boxplot")),
                  tabPanel("Bar", value = "Bar", plotOutput("barplot")),
                  tabPanel("Histogram", value = "Histogram", plotOutput("histogram"))
      )
    )
  )
)

# 3. Server Logic
server <- function(input, output, session) {
  
  # Data tab: Display selected columns from mtcars
  output$data_table <- renderTable({
    req(input$selected_vars)
    mtcars[, input$selected_vars, drop = FALSE]
  })
  
  # Summary tab: Show numeric variable summaries 
  output$cont_summary <- renderPrint({
    nums <- sapply(mtcars, is.numeric)
    summary(mtcars[, nums])
  })
  
  # Summary tab: Show frequency tables for categorical variables
  output$disc_summary <- renderPrint({
    facts <- sapply(mtcars, is.factor)
    lapply(mtcars[, facts, drop = FALSE], table)
  })
  
  # BoxPlot tab: Show boxplot for selected categorical and numeric variables 
  output$boxplot <- renderPlot({
    req(input$box_x, input$box_y)
    ggplot(mtcars, aes_string(x = input$box_x, y = input$box_y)) +
      geom_boxplot() +
      labs(x = input$box_x, y = input$box_y, title = "Boxplot")
  })
  
  # Bar tab: Show bar plot of a categorical variable
  output$barplot <- renderPlot({
    req(input$bar_var)
    ggplot(mtcars, aes_string(x = input$bar_var)) +
      geom_bar() +
      labs(x = input$bar_var, title = "Bar Plot of Discrete Variable")
  })
  
  # Histogram tab: Show histogram of selected numeric variable
  output$histogram <- renderPlot({
    req(input$hist_var)
    ggplot(mtcars, aes_string(x = input$hist_var)) +
      geom_histogram(bins = input$bins) +
      labs(x = input$hist_var, title = "Histogram")
  })
}

# 4. Create and Run the application
shinyApp(ui, server)


