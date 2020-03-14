library(shiny)
library(shinyWidgets)
library(plotly)

fluidPage(
  titlePanel("Modeling COVID-19 Spread vs Healthcare Capacity"),
  hr(),
  p(div(HTML("Disclaimer: This simulation is for educational purposes only and is not intended to be a tool for decision-making. There are many uncertainties and debates about the details of COVID-19 infection and transmission and there are many limitations to this simple model."))),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
      h4(div(HTML("<em>Set clinical parameters...</em>"))),
      sliderInput("IncubPeriod", "Duration of incubation period", 0, 20, 5, step=0.5, post = " days"),
      sliderInput("DurMildInf", "Duration of mild infections", 0, 20, 6, step=1, post = " days"),
      sliderInput("FracSevere", "% of infections that are severe", 0, 100, 15, step=1, pre="%"),
      sliderInput("FracCritical", "% of infections that are critical",0, 20, 5, step=1, pre="%"),
      #uiOutput("FracCriticalSlider"),
      #sliderInput("CFR", "Case fatality rate", 0, 100, 2, step=5, pre="%"),
      sliderInput("ProbDeath", "Death rate for critical infections", 0, 100, 40, step=1, pre="%"),
      htmlOutput("CFR"),
      br(),
      sliderInput("DurHosp", "Duration of severe infection/hospitalization", 0, 10, 4, step=1, post = " days"),
      sliderInput("TimeICUDeath", "Duration critical infection/ICU stay", 0, 30, 10, step=1, post = " days"),
      hr(),
      h4(div(HTML("<em>Set transmission values...</em>"))),
      sliderInput("b1", div(HTML("Transmission rate (mild infections)")), 0, 1, 0.33, step=0.01),
      sliderInput("b21", div(HTML("Transmission rate (severe infections, relative to mild)")),0, 2, 0, step=0.1),
      sliderInput("b31", div(HTML("Transmission rate (critical infections, relative to mild)")), 0, 2, 0, step=0.1),
      hr(),
      h4(div(HTML("<em>Set simulation values...</em>"))),
      sliderInput("LogN", div(HTML("Total population size (log10)")), 1, 9, 3, step=0.1),
      htmlOutput("N"),
      br(),
      numericInput("InitInf","Initial # infected:",value = 1, min = 1, step = 1),
      sliderInput("Tmax", div(HTML("Maximum time")),0, 1000, 300, step=10, post=" days"),
      actionButton("reset", "Reset all")
      
    ),
    
    mainPanel(
      
      #p(div(HTML("Test")))
      navbarPage("Output:",
                 
                 tabPanel("Spread",
                          fluidPage(
                            fluidRow(
                              
                              h3("Predicted COVID-19 cases by clinical outcome"),
                              p(HTML("Simulate the natural course of a COVID-19 epidemic in a single population without any interventions.")),
                              
                              plotlyOutput("plot0"),
                              br(),
                              radioButtons("yscale", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log")),
                              p(HTML("")),
                              p(HTML("<b>User instructions:</b> The graph shows the expected infection levels per N individuals in a population, starting from 1 out of N individuals being infected, where N is the user-defined population size denominator. The population size and the parameter values used to simulate the spread of COVID-19 can be specified through the sliders located in the left-hand panel. Default slider values are equal to values taken from the literature. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. To zoom in a particular curve, click its name in the plot legend. A description of the model and the parameters is in the tab Model Description."))
                            )
                          )
                 ),
                 
                 tabPanel("Intervention",
                          fluidPage(
                            fluidRow(
                              h3("Reduction in predicted COVID-19 after intervention"),
                              p(HTML("Simulate the change in the time course of COVID-10 cases after applying an intervention to reduce transmission")),
                              plotlyOutput("plotInt"),
                              br(),
                              br(),
                              radioButtons("yscaleInt", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log")),
                              wellPanel(
                                h4(div(HTML("<em>Set intervention parameters...</em>"))),
                                selectInput("VarShowInt",
                                            label = "Select variable to show:",
                                            choices = c("Suceptible (S)"="S", "Exposed (E)"="E", "Mild Infections (I1)"="I1", "Severe Infections (I2)"="I2", "Critical Infections (I3)"="I3", "Recovered (R)"="R", "Dead (D)"="D", "All infected (E+I1+I2+I3)"="Inf","All symptomatic (I1+I2+I3)"="Cases","All hospitalized (I2+I3)"="Hosp"),
                                            selected = c("I3")
                                ),
                                numericInput("Tint","Intervention start time (days):",value = 0, min = 0, step = 10),
                                numericInput("Tend","Intervention end time (days):",value = 300, min = 0, step = 10),
                                p(HTML("<b>Intervention type: reducing transmission.</b> For example via social distancing or isolation of infected individuals. Transmission from each of the clinical stages of infection can be differntially reduced, if the user has chosen parameters such that these stages contribute to transmission.")),
                                sliderInput("s1", "Reduction in transmission from mild infections ", 0, 100, 30, pre="%",step=10, animate=TRUE),
                                sliderInput("s2", "Reduction in transmission from severe infections", 0, 100, 0, pre="%",step=10, animate=TRUE),
                                sliderInput("s3", "Reduction in transmission rate from critical infections", 0, 100, 0, pre="%",step=10, animate=TRUE)
                              ),
                              p(HTML("<b>User instructions:</b> The graph shows the expected infection levels per N individuals in a population, starting from 1 out of N individuals being infected, where N is the user-defined population size denominator. The population size and the parameter values used to simulate the spread of COVID-19 can be specified through the sliders located in the left-hand panel. Default slider values are equal to values taken from the literature. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. To zoom in a particular curve, click its name in the plot legend. A description of the model and the parameters is in the tab Model Description."))
                            )
                          )
                 ),
                 
                 tabPanel("Capacity",
                          fluidPage(
                            fluidRow(
                              h3("COVID-19 Cases vs Healthcare Capacity"),
                              p(HTML("Simulate predicted COVID-19 cases vs the capacity of the healthcare system to care for them. The care required depends on disease severity - individuals with `severe' infection require hospitalization and individuals with 'critical' infection often require ICU-level care and mechanical ventilation. ")),
                              plotlyOutput("plotCap"),
                              br(),
                              br(),
                              radioButtons("yscaleCap", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log")),
                              wellPanel(
                                h4(div(HTML("<em>Set intervention parameters...</em>"))),
                                selectInput("VarShowCap",
                                            label = "Select variable to show:",
                                            choices = c("Critical Infections (I3) vs ICU beds"="I3bed", "Critical Infections (I3) vs ventilator capacity"="I3mv", "Severe + Critical Infections (I2+I3) vs Hospital Beds"="Hosp", "All symptomatic cases (I1+I2+I3) vs Hospital Beds"="CasesCap"),
                                            selected = c("CasesCap")
                                ),
                                numericInput("TintC","Intervention start time (days):",value = 0, min = 0, step = 10),
                                numericInput("TendC","Intervention end time (days):",value = 300, min = 0, step = 10),
                                p(HTML("<b>Intervention type: reducing transmission.</b> For example via social distancing or isolation of infected individuals. Transmission from each of the clinical stages of infection can be differntially reduced, if the user has chosen parameters such that these stages contribute to transmission.")),
                                sliderInput("s1C", "Reduction in transmission rate (mild infections) ", 0, 100, 30, pre="%",step=10, animate=TRUE),
                                sliderInput("s2C", "Reduction in transmission rate (severe infections) ", 0, 100, 0, pre="%",step=10, animate=TRUE),
                                sliderInput("s3C", "Reduction in transmission rate (critical infections) ", 0, 100, 0, pre="%",step=10, animate=TRUE)
                              ),
                              p(HTML("<b>User instructions:</b> The graph shows the expected infection levels per N individuals in a population, starting from 1 out of N individuals being infected, where N is the user-defined population size denominator. The population size and the parameter values used to simulate the spread of COVID-19 can be specified through the sliders located in the left-hand panel. Default slider values are equal to values taken from the literature. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. To zoom in a particular curve, click its name in the plot legend. A description of the model and the parameters is in the tab Model Description."))
                            )
                          )
                 ),
                 
                 tabPanel("Code",
                          fluidPage(
                            br(),
                            uiOutput("tab")
                            
                          )),
                 tabPanel("Model Description", br(),
                          fluidRow(column(12,
                                          plotOutput("plot4", height=200),
                                          withMathJax(),
                                          includeMarkdown("SEIR.Rmd"),
                                          #h3("Equations"),br(),
                                          #helpText('An irrational number \\(\\sqrt{2}\\) and a fraction $$1-\\frac{1}{2}$$, $$a$$'),
                                          #includeMarkdown("SEIR.Rmd"),
                                          h3("Rate parameters of dynamic model"),
                                          p(HTML("These parameters can be changed using the sliders in the other tabs. The values in this table represent the current values chosen via the sliders. Note that the transmission rates chosen by the sliders are always scaled by \\(N\\), so that \\(\\beta*N\\) is constant as \\(N\\) changes.")),
                                          tableOutput("ParameterTable"),br(),br(),
                          ))),
                 
                 tabPanel("Sources",
                          fluidPage(
                            br(),
                            uiOutput("parameterDesc")
                          ))
                 
      )
      
    )
    
  )
  
)

