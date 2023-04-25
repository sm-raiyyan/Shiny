############################################
# BMI Web app by S.M.Raiyyan               #
# 4/25/2023                                #
############################################

library(shiny)
library(shinythemes)

# Custom CSS
custom_css <- "
body {
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background-color: #f0f3f4;
}

h1, h2, h3 {
  color: #2471a3;
  margin-top: 10px;
  margin-bottom: 10px;
}

.navbar {
  background-color: #2471a3;
  border-color: #1f618d;
}

.navbar-brand {
  color: #ffffff !important;
}

.slider-handle {
  background-color: #2471a3;
  border-color: #2471a3;
}

.btn-primary {
  background-color: #2471a3;
  border-color: #2471a3;
}

.sidebar {
  background-color: #ffffff;
  padding: 15px;
  border-radius: 5px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
}

.main {
  background-color: #ffffff;
  padding: 15px;
  border-radius: 5px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
}
"

####################################
# User Interface #
####################################
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$head(tags$style(HTML(custom_css))),
                navbarPage("BMI Calculator:",
                           tabPanel("Home",
                                    fluidRow(
                                      column(4,
                                             # Input values
                                             div(class = "sidebar",
                                                 HTML("<h3>Input parameters</h3>"),
                                                 sliderInput("height",
                                                             label = "Height",
                                                             value = 175,
                                                             min = 40,
                                                             max = 250),
                                                 sliderInput("weight",
                                                             label = "Weight",
                                                             value = 70,
                                                             min = 20,
                                                             max = 100),
                                                 actionButton("submitbutton",
                                                              "Submit",
                                                              class = "btn btn-primary")
                                             )
                                      ),
                                      column(8,
                                             # Output
                                             div(class = "main",
                                                 tags$label(h3('Status/Output')), # Status/Output Text Box
                                                 verbatimTextOutput('contents'), # Add this div with an ID
                                                 tableOutput('bmiResult')
                                             )
                                      )
                                    ) # fluidRow()
                           ), #tabPanel(), Home
                           
                           tabPanel("About",
                                    titlePanel("About"),
                                    div(includeMarkdown("about.md"),
                                        align="justify")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()

####################################
# Server #
####################################
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    bmi
  })
  
 weightStatus <- reactive({
    bmi_value <- datasetInput()[1, "BMI"]
    if (bmi_value < 18.5) {
      "Underweight"
    } else if (bmi_value >= 18.5 && bmi_value <= 24.9) {
      "Healthy Weight"
    } else if (bmi_value >= 25.0 && bmi_value <= 29.9) {
      "Overweight"
    } else {
      "Obesity"
    }
  })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
      if (input$submitbutton>0) {
        isolate("Calculation complete.")
      } else {
        return("Server is ready for calculation.")
      }
    })
    
    # Prediction results table
    output$bmiResult <- renderTable({
      if (input$submitbutton>0) {
        isolate({
          bmi_data <- datasetInput()
          Status <- weightStatus()
          bmi_data$Weight_Status <- Status
          bmi_data
        })
      }
    })
    }

####################################
# Create Shiny App #
####################################
shinyApp(ui = ui, server = server)
