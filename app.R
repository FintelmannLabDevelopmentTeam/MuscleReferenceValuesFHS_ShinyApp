library(shiny)
source("LMS_functions.R")
source("shiny_util.R")

MAX_AGE <- 80
MIN_AGE <- 38
STANDARD_AGE <- 50

MIN_MEASUREMENT <- 0
STANDARD_MEASUREMENT <- 150

#In order to allow for the input fields to temporarily be empty, these variables are created to 
#store the previous value of the measurement field globally.
age_value <- STANDARD_AGE
measurement_value <- STANDARD_MEASUREMENT



ui <- fluidPage(
  
  titlePanel("Z Score Calculation"),
  
  sidebarLayout(
    #Left sidebar
    sidebarPanel(
      h3("Insert Measurement:"),
      
      selectInput("SelectedMetric", "Metric:",
                  choices = c("Cross-Sectional Muscle Area (CSMA, [cm2])",
                              "Skeletal Muscle Index (SMI, [cm2/m2])",
                              "Skeletal Muscle Radio-Attenuation (SMRA, [HU])",
                              "Skeletal Muscle Gauge (SMG, [HU*cm2/m2])")
                  ),
      
      selectInput("lvl", "Vertebral Level:", 
                  choices=c("T5", "T8", "T10", "L3"), selected= NULL),
      
      radioButtons("sex", "Sex:", 
                   choices=c("Female", "Male"),
                   selected=NULL),
      
     
      numericInput("age", "Age (38-80 years):", min = MIN_AGE, 
                   max = MAX_AGE, value = STANDARD_AGE),
      
      #Display which measurement was selected:
      span(textOutput("SelectedMetric"), style="font-weight:bold;"),
      
      numericInput("measurement", label="", value=STANDARD_MEASUREMENT, min=MIN_MEASUREMENT),
      
      
      textOutput("Z_Score_String"),
      
      imageOutput('Fintelmann_Lab_Logo'),
      
      textOutput('publication')
      
    ),
    #Main panel:
    mainPanel(
      plotOutput("LMS_Curves"),
      textOutput("LMS_Curves_Title"),
      plotOutput("Z_Visualized"),
      textOutput('Z_Plot_Title')
      
    )
  ),
  
  #Title in Browser:
  title = "FHS Reference Values - Z Score Calculation"
)


server <- function(input, output, session){
  
  
  #contain age inside a reactive function which sanity checks the input
  #If the field is temporarily empty: return the previous value
  #If the value falls outside the allowed range, reset to the border
  age_r <- reactive({
    if(!is.null(input$age) & !is.na(input$age)){
      age_value <- input$age #update age_value after sanity check of the range 
      #This will be available later in case the field is left empty.
      
      #Make sure that age input does not fall out of range and that no crash happens when the field is emptied.
      if(input$age > MAX_AGE){
        updateNumericInput(session, "age", value=MAX_AGE)
        age_value <- MAX_AGE
      }
      if(input$age < MIN_AGE){
        updateNumericInput(session, "age", value=MIN_AGE)
        age_value <- MIN_AGE
      }
      
    }
    return(age_value)
  })
  
  #contain measurement inside a reactive function which sanity checks the input
  #If the field is temporarily empty: return the previous value
  #If the value falls outside the allowed range, reset to the border
  measurement_r <- reactive({
    if(!is.null(input$measurement) & !is.na(input$measurement)){
      measurement_value <- input$measurement
      #This will be available later in case the field is left empty.
      
      #Make sure that measurement value cannot fall below the minimum
      if(input$measurement < MIN_MEASUREMENT){
        updateNumericInput(session, "measurement", value=MIN_MEASUREMENT)
        measurement_value <- MIN_MEASUREMENT
      }
    }
    return(measurement_value)
  })
  
  metric <- reactive({get_metric_from_string(input$SelectedMetric)})
  
  z_score <- reactive({
    get_z(measurement_r(), age_r(), metric(), input$sex, input$lvl)
  })
  
  LMS_plot <- reactive({
    get_LMS_curve_plot(metric(), input$sex, input$lvl)
  })
  
  output$Z_Score_String <- renderText({
    paste("Z Score is: ", z_score(), ';   P(x <= ', measurement_r(), ') = ', get_percentile(measurement_r(), age_r(), metric(), input$sex, input$lvl), sep='')
  })
  
  output$SelectedMetric <- renderText(
    {paste(input$SelectedMetric, ":", sep="")}
  )
  
  
  
  output$LMS_Curves <- renderPlot({
    LMS_plot() +
      mark_point(age_r(), measurement_r())
  })
  
  output$LMS_Curves_Title <- renderText({
    paste('Reference centile curves at ', input$lvl, ' in ', input$sex, 's for ', metric(), '. The red x marks the ', metric(), ' of ', measurement_r(), ' ', get_unit_for_measurement(metric()), ' of an individual aged ', age_r(), ' years.', sep='')
  })
  
  output$Z_Plot_Title <- renderText({
    paste('Box-Cox-Power-Exponential distribution at age ', age_r(), ' in ', input$sex, 's at ', input$lvl, '. The red vertical bar marks the position of a ', metric() ,' of ', measurement_r(), ' ', get_unit_for_measurement(metric()), '.', sep='')
  })
  
  
  model_distribution_at_age <- reactive({
    plot_dist_at_age(age_r(), get_metric_from_string(input$SelectedMetric), input$sex, input$lvl)
  })
  output$Z_Visualized <- renderPlot({
    model_distribution_at_age() + 
      geom_vline(xintercept=measurement_r(), color='red')
  })
  
  output$Fintelmann_Lab_Logo <-
    renderImage({
      list(
        src='resources/fintelmann-logo.v2.gif',
        width='100%', 
        alt='Fintelmann Lab Logo'
      )
    }, deleteFile = F)
  
  output$publication <- renderText({'This web app was built around models described in our publication: "Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment: The Framingham Heart Study"'})
  }
  

shinyApp(ui = ui, server = server)