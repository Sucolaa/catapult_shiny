# PACKAGES --------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(gganimate)
library(zoo)
library(shinydashboard)
library(howler)
library(video)
library(httr)

# DATA --------------------------------------------------------------------------------
mydata <- read.csv("./data/forehand.csv")

# DATA TIDY ----------------------------------------------------------------------------
mydata %>% 
  mutate(mygroup = seq(from = 0,
                       to = 19,
                       by = 0.01)) -> mydata

# UI ----------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  title = "Catapult",
  
  # HEADER -----------------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "catapult_log.PNG", height = 35), 
                 span("CATAPULT",
                      style = "font-weight: bold;"))
  ),
  
  # SIDERBAR ---------------------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("YEAR",
               selectInput(
                 inputId = "year_select",
                 label = "Year: ",
                 choices = 2019:2023
                )#end of year select
               ), # end of Item YEAR
      
      br(),
      menuItem("MATCH",
               conditionalPanel(
                "input.year_select == 2019",
                selectInput(
                  inputId = "match_select1",
                  label = "2019",
                  choices = c("match 2019 1",
                            "match 2019 2")
                  )
                ), # end of condition 2019 panel
               conditionalPanel(
                 "input.year_select == 2020",
                 selectInput(
                   inputId = "match_select2",
                   label = "2020",
                   choices = c("match 2020 1",
                             "match 2020 2")
                 )
               ), # end of condition 2020 panel
               conditionalPanel(
                 "input.year_select == 2021",
                 selectInput(
                   inputId = "match_select3",
                   label = "2021",
                   choices = c("match 2021 1",
                             "match 2021 2")
                 )
               ), # end of condition 2021 panel
               conditionalPanel(
                 "input.year_select == 2022",
                 selectInput(
                   inputId = "match_select4",
                   label = "2022",
                   choices = c("match 2022 1",
                             "match 2022 2")
                 )
               ), # end of condition 2022 panel
               conditionalPanel(
                 "input.year_select == 2023",
                 selectInput(
                   inputId = "match_select5",
                   label = "2023",
                   choices = c("match 2023 1",
                               "match 2023 2")
                 )
               ) # end of condition 2023 panel
               ), # end of Item MATCH
      
      br(),
      menuItem("PLAYER",
               selectInput(
                 inputId = "player",
                 label = "The Player:",
                 choices = c("a","b","c")
               )#end of player choose
               ), # end of Item PLAYER
      
      br(),
      menuItem("VARIABLE",
               selectInput(
                 inputId = "var1",
                 label = "The Variable",
                 choices = c("Aceeleration","Rotation")
               ), # the bigger level
               conditionalPanel(
                 "input.var1 == `Aceeleration`",
                 selectInput(
                   inputId = "acc",
                   label = "Specific (Acceleration) :",
                   choices = c("forward",
                               "backward",
                               "parelle")
                 ) # end of input selection of acceleration
               ), # end of condition panel for acceleration
               conditionalPanel(
                 "input.var1 == `Rotation`",
                 selectInput(
                   inputId = "rota",
                   label = "specific (Rotation) :",
                   choices = c("forward",
                               "backward",
                               "pare")
                 ) # end of input selection for rotation
               ) # end of condition panel for rotation
               ), # end of Item VARIABLE
      br(),
      
      # a play button that is designed for control both the video and the plot, since the 
      # plot has been designed to depend on the video, the play button is no longer needed.
      
      # tags$div(
      #   style = "display:inline-block",
      #   title = "Using the range slider",
      #   actionButton(
      #     "play", "", 
      #     icon = icon("play"), 
      #     width = "200px",
      #     style = 'padding:15px; font-size:120%',
      #     onclick = "if(document.getElementById('video1').paused){
      #                document.getElementById('video1').play();
      #                document.getElementById('video2').play();
      #              } else {
      #                document.getElementById('video1').pause();
      #                document.getElementById('video2').pause();
      #              }"
      #   )
      # )
      
      actionButton(inputId = "fil",
                   label = "That's it!",
                   width = "200px",
                   style = 'padding:15px; font-size:120%')
    ) # end of sidebarMenu
    
  ), # end of dashboardSidebar
  
  # BODY --------------------------------------------------------------------------------
  dashboardBody(
    #set of the overall font size, including the text in sidebar.
    useShinyjs(),
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }"))
    ), #change the font size to 18
    
    #1. the selected video
    
    fluidRow(
      #the first line
      box(title = "video",
          status = "primary",
          width = 7,
          height = 6,
          solidHeader = TRUE,
          background = "blue",
          collapsible = FALSE,
          #video
          video(
                files = "~/Desktop/catapult/catapult_shiny/www/shots1_forehand2.mp4",
                format = 'video/mp4',
                elementId = "video1",
                options = list(controls = TRUE,
                               loop = FALSE,
                               preload = "metadata"),
                seek_ping_rate = 500,
                width = '600px',
                height = '350px')
      ),
      column(width = 1,
             # sliderInput(inputId = "videoseek",
             #             label = "video(seek)",)
             ),
      column(
        width = 4,
        height = 8,
        infoBoxOutput("videoinfo", width = NULL),
        infoBoxOutput("movetrend", width = NULL),
        infoBoxOutput("shot", width = NULL),
        infoBoxOutput("onice", width = NULL)
      ),#end of column
      
      #the second line
      # box(title = "plot",
      #     status = "primary",
      #     width = 7,
      #     solidHeader = TRUE,
      #     collapsible = FALSE,
      #     plotOutput("myplot")
      #     ),
      # column(width = 1),
      # box(title = "Shoot Detector",
      #     width = 4,
      #     solidHeader = TRUE,
      #     collapsible = TRUE,
      #     status = "success")
    )# end of fluid row
  )# end of dashboard Body
)# end of UI

#SERVER ----------------------------------------------------------------------------------
server <- function(input, output) {
  
  # video test
  
  #dynamic inforboxes
  # 1. video informations
  output$videoinfo <- renderInfoBox({
    infoBox(
            HTML("<b>Video Info</b>"),
            width = NULL,
            icon = icon("circle-play"),
            color = "green",
            paste("Duration: ",round(input$video1_duration,2)),
            paste("Seek: ",round(input$video1_seek,2))
            )
  })
  # 2. moving trend
  output$movetrend <- renderInfoBox({
    infoBox(
      HTML("<b>Move Trend</b>"),
      width = NULL,
      icon = icon("arrow-trend-up"),
      color = "green"
    )
  })
  # 3. shoot
  output$shot <- renderInfoBox({
    infoBox(
      HTML("<b>Shoot</b>"),
      width = NULL,
      icon = icon("champagne-glasses"),
      color = "green"
    )
  })
  # 4. on ice
  output$onice <- renderInfoBox({
    infoBox(
      HTML("<b>On Ice?</b>"),
      width = NULL,
      icon = icon("person-skating"),
      color = "green"
    )
  })
  
  
  #plot
    # detect the video playing time and use it as the filter to continually filter data
  myobs <- reactive({
    myobs <- mydata[mydata$mygroup >= input$video1_seek - 0.5 & mydata$mygroup <= input$video1_seek + 0.5, ]
    return(myobs)
  })
  
    # create plot
  output$myplot <- renderPlot({
    ggplot(data = myobs(),
           aes(x = mygroup,
               y = Acceleration.forward)) +
      geom_line() +
      ylim(-3,3) +
      geom_vline(xintercept = mean(myobs()$mygroup),
                 color = "red") +
      xlab("time") +
      theme_bw()
  },
  width = 600,
  height = 250)
}

shinyApp(ui = ui, server = server)
