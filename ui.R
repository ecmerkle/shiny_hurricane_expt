library("shiny")

## NB assumes 3 practice trials and 3 experimental trials

## UI for hurricane study
shinyUI(basicPage(

  headerPanel(
    conditionalPanel(
      condition = "input.d1 > 0 && input.instruct < 5",
      "Hurricane Forecaster"),
    windowTitle = "Hurricane Forecaster"
    ),

  #sidebarPanel(
  #  tags$head(
  #    tags$style(type="text/css", ".span4{ max-width:10%; }",
  #               "textarea { max-width: 1px; }",
  #               "body {background-color: white; }",
  #               ".well { width:0px; }")
  #    )
  #  ),
  
  mainPanel(
  ## Slider and positioning mods
  tags$head(
    tags$style(type="text/css", ".jslider{ max-width: 100%; }",
               ".jslider .jslider-label{ font-size:14px; }",
               ".jslider .jslider-value{ font-size:14px; }",
               "body {padding-left: 15%; }")
    ),
    
  conditionalPanel(
    condition = "input.p1==0",
    htmlOutput("p1text"),
    actionButton("p1","I Agree")
    ),

  conditionalPanel(
    condition = "input.p1==1 && input.d1==0",
    htmlOutput("demtext"),
    selectInput(inputId = "education",
                label = "Highest education:",
                choices = c("Some high school","Graduated high school","Some college",
                            "Associates degree","Bachelors degree","Masters degree",
                            "Doctoral degree", "Other professional degree"),
                selected="Some high school"),
    sliderInput(inputId = "age",
                label = "Age (years):",
                min = 18, max = 100, step=1, value=18),
    actionButton("d1","Submit")
    ),                
      
  ## Intro text, page by page
  conditionalPanel(
    condition = "input.instruct==0 && input.d1==1",
    htmlOutput("text1")
    ),

  conditionalPanel(
    condition = "input.instruct==1",
    htmlOutput("text2")
    ),

  conditionalPanel(
    condition = "input.instruct==2",
    htmlOutput("text3")
    ),

   conditionalPanel(
    condition = "input.instruct==3",
    htmlOutput("text4")
    ),

   conditionalPanel(
    condition = "input.instruct==4",
    htmlOutput("text5")
    ),

  ## TODO add instructions/training about scoring rule here
      
  conditionalPanel(
    condition = "!$('html').hasClass('shiny-busy') && input.instruct <= 4 && input.d1 > 0",
    actionButton("instruct","Next")
    ),

  ## Practice trials
  conditionalPanel(
    condition = "input.instruct == 5 & input.submitp == input.okp2 & input.okp2 < 3",
    imageOutput("prac1", height="100%", width="100%"),
    sliderInput("slprac", "Forecast:", min=0, max=1, value=.5, step=.01),
    actionButton("submitp","Submit")
  ),

  conditionalPanel(
    condition = "input.instruct == 5 && input.submitp > input.okp",
    imageOutput("prac2", height="100%", width="100%"),
    actionButton("okp","Outcome")
  ),

  conditionalPanel(
    condition = "!$('html').hasClass('shiny-busy') && input.instruct == 5 && input.okp2 < 3 && input.okp > input.okp2 && input.submitp <= 3",
    htmlOutput("pracres"),
    actionButton("okp2","Next forecast")
  ),

      
  ## TODO: This now becomes instructions about how the practice trials
  ##       are ending, and the real trials will begin.
  conditionalPanel(
    condition = "input.instruct == 5 && input.okp2 == 3 && input.sc1 == 0",
    htmlOutput("scoretext"),
    tableOutput("scoring1"),
    actionButton("sc1","Next")
  ),

  ## Condition 1 stimulus
  conditionalPanel(
    condition = "input.sc1 == 1 && input.submit == input.ok2 && input.submit < 3 && input.ok < 3",
    imageOutput("c1", height="100%", width="auto"),
    sliderInput("c1sl", "Forecast:", min=0, max=1, value=.5, step=.01),
    tableOutput("c1slider"),
    actionButton("submit","Submit")
  ),
    
  ## Condition 1 results
  conditionalPanel(
    condition = "input.sc1 == 1 && input.submit > input.ok && input.ok2 < 3 && input.ok < 3",
    imageOutput("c1res", height="100%", width="100%"),
    actionButton("ok","Calculate points")
  ),

  ## Condition 1 score
  conditionalPanel(
    condition = "!$('html').hasClass('shiny-busy') && input.instruct == 5 && input.ok2 < 3 && input.ok > input.ok2 && input.submit <= 3",
    htmlOutput("c1score"),
    actionButton("ok2","Next forecast")
  ),

  ## End text
  conditionalPanel(
    condition = "!$('html').hasClass('shiny-busy') && input.ok2 == 3",
    htmlOutput("endtext")
   )#,
    
  #conditionalPanel(
  #  condition = "$('html').hasClass('shiny-busy')",
  #  imageOutput("loading")
  # ),

  #conditionalPanel(
  #  condition = "$('html').hasClass('shiny-busy')",
  #  br()
  #  )

  )



  ))
