## initialize session
name.spc <- c("shiny", "shinyjs", "shinydashboard", "shinycssloaders", "moveVis", "move", "raster",
              "gridGraphics", "animation")
lapply(name.spc, function(x){library(x, character.only = TRUE)})
server.dir <- getwd()

#Get the sample movement data from the moveVis package
data("move_data")

#Convert the timestamps to the POSIXct format
move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#Create a moveStack class object
data_ani <- move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
                 time = move_data$dt, animal=move_data$individual, data=move_data)


## UI def
ui <- dashboardPage(title = "moveVis | Animate you movement data!",
  dashboardHeader(title = "moveVis", disable = TRUE),
  dashboardSidebar(disable = TRUE  #,
    #sidebarMenu(
    #  menuItem("General settings", tabName = "general", icon = icon("sliders")),
    #  menuItem("Advanced settings", tabName = "advanced", icon = icon("magic")),
    #  menuItem("About moveVis", tabName = "advanced", icon = icon("info"))
    #)
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(type="text/css", "body {padding-top: 50px;}")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    fluidRow(
             column(width = 6,
                    tags$head(tags$style(type="text/css", ' .incline {margin-left:-17px; margin-right:-17px;}')),
                    tags$div(class = "incline",
                      navbarPage("moveVis", position = "fixed-top", collapsible = TRUE, fluid = TRUE,
                                 tabPanel(
                                   title = "General settings", icon = icon("sliders"),
                                   box(title = "Data", width = NULL, solidHeader = FALSE, status = "primary",
                                       fluidRow(
                                         column(width = 6,
                                                selectInput("m", "Movement dataset", choices = list("My White Storks dataset" = "m"))
                                         ),
                                         column(width = 6,
                                                selectInput("map_type", "Map type", choices = list("satellite" = "satellite", "roadmap" = "roadmap", "hybrid" = "hybrid", "terrain" = "terrain"))
                                         )
                                       )
                                   ),
                                   tabBox(title = "Modes", width = NULL, side = "right", selected = "Options",
                                          tabPanel("Help",
                                                   strong("Time compution mode"),
                                                   helpText("Define, how moveVis should deal with time information. If set to 'true data times', paths are displayed based on true coverage times, skipping no-data time gaps. If set to 'true overall times', paths are displayed based on true coverage times, but gaps between coverage times will be filled with frames (uniform overall temporal resolution). If set to 'simple', all movement paths are displayed individually with no regard to the true coverage times (no frame time, each individual has its own time)."),
                                                   strong("Handling of no-data (NA) locations"),
                                                   helpText("Define, how moveVis should deal with time periods, where no location data are available. Choose, if last path location should be hold on frame for NA path locations, or if path should disappear until next non-NA location is reached.")
                                          ),
                                          tabPanel("Options",
                                                   fluidRow(
                                                     column(width = 6,
                                                            selectInput("paths_mode","Time compution mode", choices = list("true data times (recommended)" = "true_data", "true overall times (no time skips)" = "true_time", "simple (no uniform frame time)" = "simple"), width = "100%")
                                                     ),
                                                     column(width = 6,
                                                            selectInput("paths_na.hold","Handling of no-data (NA) locations", choices = list("hold last known position" = TRUE, "make path disappear" = FALSE),width = "100%")
                                                            
                                                     )
                                                   )
                                          )
                                   ),
                                   tabBox(title = "Appearance", width = NULL, side = "right", selected = "Options",
                                          tabPanel("Help",
                                                   strong("Tail elements"),
                                                   helpText("Define the number of path points to be displayed as path tail of the animation paths. The higher the number, the lomger the tails and the more past path points are displayed per frame."),
                                                   strong("Tail size"),
                                                   helpText("Define the size of the first tail element. The tail will get smaller per point."),
                                                   strong("Title, subtitle, caption and axis titles"),
                                                   helpText("Title and subtitle will appear at the top edge of the frame, the caption will appear at the bottom. The axis tiles will overwrite the automatic axis titles aside the bottom x and the left y axis."),
                                                   strong("North arrow, scale bar, time scale"),
                                                   helpText("Both are map elements that are recommended to be included, especially, if the spatial scale and orientation of the scenary should be unambiguous. You can change the colouring depending on the map colour. The time scale stamps will appear at the top of the map and give orientation about time and time speed.")
                                          ),
                                          tabPanel("Options",
                                                   fluidRow(
                                                     column(6, numericInput("tail_elements", strong("Tail length"), value = 10, min = 1, max = 20, width = "100%")),
                                                     column(6, numericInput("tail_size", strong("Tail size"), value = 4, min = 1, max = 10,width = "100%"))
                                                   ),
                                                   #tags$hr(),
                                                   checkboxInput("desc", "add map's title, subtitle and caption"),
                                                   conditionalPanel(
                                                     condition = "input.desc == true",
                                                     textInput("img_title", NULL, "title", width = "100%"),
                                                     textInput("img_sub", NULL, "subtitle", width = "100%"),
                                                     textInput("img_caption", NULL, "caption", width = "100%")
                                                   ),
                                                   checkboxInput("desc_labs", "specify axis titles"),
                                                   conditionalPanel(
                                                     condition = "input.desc_labs == true",
                                                     textInput("img_labs_x", NULL, "x", width = "100%"),
                                                     textInput("img_labs_y", NULL, "y", width = "100%")
                                                   ),
                                                   checkboxInput("map_elements", "add north arrow and scale bar", value = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.map_elements == true",
                                                     fluidRow(
                                                       column(width = 6,
                                                              selectInput("scalebar_col", "Scale bar colour",choices = list("white" = "white", "black" = "black"), width = "100%")
                                                       ),
                                                       column(width = 6,
                                                              selectInput("north_col", "North arrow colour",choices = list("white" = "white", "black" = "black"), width = "100%")
                                                       )
                                                     )
                                                   ),
                                                   checkboxInput("time_scale", "add time scale stamps", value = TRUE)
                                          )
                                   )
                                 ),
                                 tabPanel(
                                   title= "Advanced settings", icon = icon("magic"),
                                   tabBox(title = "Frame settings", width = NULL, side = "right", selected = "Options",
                                          tabPanel("Help",
                                                   strong("Frame rate paramaters"),
                                                   helpText("You can define different parameters to control the usage of frames:",
                                                            tags$ul(
                                                              tags$li("The number of frames, after which the animation should be finished (this will cause a counter counting from the initial frame until the defined number of frames and then quits the animation)," ), 
                                                              tags$li("the number of frames to be skipped (reducing the total amount of frames by skipping data points),"), 
                                                              tags$li("the output temporal resolution in seconds,"),
                                                              tags$li("the temporal displaying duration per frame (not changing the amount of frames, but the total animation time).")
                                                            )
                                                   ),
                                                   strong("Frame set-up parameters"),
                                                   helpText("You can define the resolution (ppi), width and height (pixels). This will define the spatial size of the output file (e.g. that of a GIF file)")
                                          ),
                                          tabPanel("Options",
                                                   checkboxInput("f1", "stop animation after n frames", width = "100%"),
                                                   conditionalPanel(
                                                     condition = "input.f1 == true",
                                                     numericInput("frames_nmax", strong("Number of frames, after which to stop animation:"), value = 50, min = 20, width = "100%")
                                                   ),
                                                   checkboxInput("f2", "set number of frames to be skipped periodically (every nth frame to be used)",width = "100%"),
                                                   conditionalPanel(
                                                     condition = "input.f2 == true",
                                                     numericInput("frames_nres", strong("Number of frames to skip periodically:"), value = 1, min = 1, width = "100%")
                                                   ),
                                                   checkboxInput("f3", "set temporal resolution (enforce interpolation)",width = "100%"),
                                                   conditionalPanel(
                                                     condition = "input.f3 == true",
                                                     numericInput("frames_tres", strong("Target temporal resoluton (seconds):"), value = 0, min = 0, width = "100%")
                                                   ),
                                                   checkboxInput("f4", "set frame displaying duration",width = "100%"),
                                                   conditionalPanel(
                                                     condition = "input.f4 == true",
                                                     numericInput("frames_interval", strong("Frame displaying duration (seconds):"), value = 0.04, width = "100%")
                                                   ),
                                                   checkboxInput("f5", "set resolution parameters defining output file size and extent.", width = "100%"),
                                                   conditionalPanel(
                                                     condition = "input.f5 == true",
                                                     fluidRow(
                                                       column(4, numericInput("frames_pixres", strong("Resolution (ppi)"), value = 80, width = "100%")),
                                                       column(4, numericInput("frames_width", strong("Fixed width (px)"), value = NA, width = "100%")),
                                                       column(4, numericInput("frames_height", strong("Fixed height (px)"), value = NA, width = "100%"))
                                                     )
                                                   )
                                          )
                                   ),
                                   tabBox(title = "Output", width = NULL, side = "right", selected = "Options",
                                          tabPanel("Help",
                                                   strong("File format"),
                                                   helpText("You can choose between several output file formats, which all have advantages and disadvantages, e. g. regarding file compression. Select the output file format depending on how you want to use the animation (presentation, display panel, PDF?).",
                                                            tags$ul(
                                                              tags$li("Universal image formats: GIF"), 
                                                              tags$li("Universal video fromats: mov, mp4, flv, avi etc.")
                                                            )
                                                   )
                                          ),
                                          tabPanel("Options",
                                                   selectInput("out_format","File format", choices = list(".gif" = "gif",
                                                                                                          ".mov" = "mov",
                                                                                                          ".mp4" = "mp4",
                                                                                                          ".flv" = "flv",
                                                                                                          ".avi" = "avi",
                                                                                                          ".m4a" = "m4a",
                                                                                                          ".mpeg" = "mpeg",
                                                                                                          ".3gp" = "3gp",
                                                                                                          ".ogg" = "ogg"),width = "100%")
                                          )
                                   )
                                 ),
                                 tabPanel(title= "About moveVis", icon = icon("info-circle"),
                                          box(title = "Technical basis", width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = FALSE,
                                              strong("moveVis R package"),
                                              helpText("This web application is based on moveVis for R, an R package providing tools to visualize movement data by creating path animations from geo-location point data. The package is under ongoing development. It is working hand in hand with the move R package by using the move and moveStack class, and the raster package. moveVis is based on a ggplot2 plotting architecture. The GUI web application was built using shiny, javaScript, CSS and HTML."),
                                              helpText("Please give me your ", strong(tags$a("feedback on the moveVis web application here!", href="mailto:movevis@schwalb-willmann.de?subject=moveVis GUI: Feedback"))),
                                              strong("Access"),
                                              helpText("The moveVis R package is available on CRAN (using: install_package('moveVis') and via GitHub (beta versions, using: devtools::install_github('16eagle/moveVis')."),
                                              helpText("Visit ",tags$a("https://github.com/16eagle/movevis", href="https://github.com/16eagle/movevis", target="_blank"), " to get to the moveVis GitHub repository.")
                                              
                                          ),
                                          box(title = "Framework", width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                              strong("Developer"),
                                              helpText("The moveVis web app and the moveVis R package are developed and maintained by ",
                                                       tags$a("Jakob Schwalb-Willmann.", href = "http://jakob.schwalb-willmann.de", target= "_blank")),
                                              strong("Aknowledgements"),
                                              helpText("The moveVis initiative is part of the ", tags$a(target="_blank", href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/research/projects/current_projects/opt4environment//", "Opt4Environment"), "project and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant ", tags$strong("50 EE 1403."))
                                              
                                              #tags$head(tags$style(type="text/css",
                                              #                     ".img-row{display:flex; flex-wrap:wrap}
                                              #                      .img-responsive{flex-grow:1; width:auto; max-width:180px; height:auto !important}"
                                              #                     )),
                                              #tags$div(class = "img-row",
                                              #  tags$div(class = "img-responsive",
                                              #    tags$a(href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/startseite//",
                                              #                     tags$img(src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg")),
                                              #    tags$a(href="http://www.dlr.de/eoc/en/",
                                              #                     tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png")),
                                              #    tags$a(href="http://www.bmub.bund.de/",
                                              #                     tags$img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q")),
                                              #    tags$a(href="http://www.orn.mpg.de/en/",
                                              #                     tags$img(src="https://www.molgen.mpg.de/188611/mpi_Seew_LogoText-1355515314.gif"))
                                              #  )
                                              #)
                                          ),
                                          box(title = "Bug reports", width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                              helpText("For bug reports, please use ", tags$a("https://github.com/16eagle/movevis/issues", href="https://github.com/16eagle/movevis/issues", target = "_blank"), ". Feature requests and other contributions are also welcome.")
                                          ),
                                          uiOutput("note2")
                                 )
                                          
                      )
                    )
             ),
             column(width = 6,
                    uiOutput("note"),
                    box(title = "Run moveVis", width = NULL, solidHeader = FALSE, status = "primary",
                        fluidRow(
                          column(6, actionButton("run_prev","| Create preview", width = "100%", icon = icon("picture-o"))),
                          column(6, actionButton("run_file","| Create animation file",  width = "100%", icon = icon("file-video-o")))
                        )
                    ),
                    uiOutput("viewer_prev"),
                    uiOutput("viewer_prog")
             )
      )
  )
)


## server def
server <- function(input, output, session) {
  #info("Please note that this is a demo application, intended to be used for demonstrational purposes only! This application is still under development. Some functionalities might be not working. For operational purposes, please use the moveVis R package available via CRAN or GitHub (moveVis.org). Thank you for testing the moveVis web application based on shiny.")
  
  #Define note box
  output$note <- renderUI(
    box(title = "Warning: This is a demo application only!", width = NULL, solidHeader = TRUE, status = "warning", 
        helpText("Please note that this is a demo application, intended to be used for demonstrational purposes only! This application is still under development. Some functionalities might be not working. For operational purposes, please use the moveVis R package available via CRAN or GitHub (moveVis.org). Thank you for testing the moveVis web application based on shiny."),
        helpText("This web application is running on a free shinyapps.io server with a ", strong("total RAM availability of 1GB"),". Large processing requests going beyond simple application tests will be killed without notification, when reaching the RAM limitations."),
        actionButton("note_dismiss","Close",  width = "25%")
    )
  )
  output$note2 <- renderUI(
    box(title = "Warning: This is a demo application only!", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE,
        helpText("Please note that this is a demo application, intended to be used for demonstrational purposes only! This application is still under development. Some functionalities might be not working. For operational purposes, please use the moveVis R package available via CRAN or GitHub (moveVis.org). Thank you for testing the moveVis web application based on shiny."),
        helpText("This web application is running on a free shinyapps.io server with a ", strong("total RAM availability of 1GB"),". Large processing requests going beyond simple application tests will be killed without notification, when reaching the RAM limitations.")
        )
  )
  hideElement(id = "note2")
  
  #Define progress box
  output$viewer_prog <- renderUI(
    box(title = "Progress", width = NULL, height = "62vh", solidHeader = TRUE, status = "primary", background = "black",
        textOutput("text"),
        br(),
        uiOutput("download")
    )
  )
  
  #Define viewer box
  output$viewer_prev <-  renderUI({box(title = "Viewer", width = NULL, height = "62vh", solidHeader = TRUE, status = "primary",
      tags$head(tags$style(type="text/css", "
                           #spinner {
                             position: absolute;
                             padding-top: 25%;
                             right: 44%;
                             width: 10%;
                           }

                        ")),
      tags$head(tags$style(type="text/css", "
                           #white {
                             position:absolute; top:0; left:0
                           }

                        ")),
      tags$head(tags$style(
        type="text/css",
        "#preview img {
                              position: relative;
                              top: 2%;
                              bottom: 2%;
                              max-width: 52vh;
                              width: auto;
                              max-height: 57vh;
                              height: auto}"
      )),
      
      fluidRow(
        column(width = 12, align = "center",
               tags$div(id = "wait",
                        imageOutput("white"),
                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                         tags$img(src = "spinner.gif", id = "spinner")),
                        imageOutput("preview")
               )
        )
      )
  )})
  hideElement(id = "viewer_prog")
  
  observeEvent(input$note_dismiss, {
    hideElement(id = "note")
    showElement(id = "note2")
  })
  
  observeEvent(input$run_file, {
    hideElement(id = "viewer_prev")
    showElement(id = "viewer_prog")
  })
  create_ani <- eventReactive(input$run_file, {
    hideElement(id="download")
    
    withCallingHandlers({
      shinyjs::html(id = "text", html = "")
      animate_move(data_ani, out_dir = server.dir, out_name = "moveVis", conv_dir = if(input$out_format == "gif"){"convert"}else{"avconv"},
                   map_type = input$map_type, paths_mode = input$paths_mode, tail_elements = input$tail_elements, tail_size = input$tail_size,
                   img_title = input$img_title, img_sub = input$img_sub, img_caption = input$img_caption,
                   img_labs = if(input$desc_labs){c(input$img_labs_x, input$img_labs_y)}else{"labs"},
                   map_elements = input$map_elements, time_scale = input$time_scale, scalebar_col = input$scalebar_col, north_col = input$north_col,
                   
                   frames_nmax = if(input$f1){input$frames_nmax}else{0},
                   frames_nres = if(input$f2){input$frames_nres}else{1},
                   frames_tres = if(input$f3){input$frames_tres}else{0},
                   frames_interval = if(input$f4){input$frames_interval}else{0.04},
                   frames_pixres = if(input$f5){input$frames_pixres}else{80},
                   frames_width = if(input$f5){input$frames_width}else{NA}, frames_height = if(input$f5){input$frames_height}else{NA},
                   out_format = input$out_format,
                   
                   shiny_mode = "ani", shiny_session = session, log_logical = TRUE, overwrite = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "text", html = paste0(m$message,"<br>"), add = TRUE)
    },
    warning = function(m) {
      if(length(grep("Inf", as.character(m$message))) == 0){shinyjs::html(id = "text", html = paste0(m$message,"<br>"), add = TRUE)}
    })
    showElement(id="download")
  })

  output$download <- renderUI({
    create_ani()
    output$file <- downloadHandler(
      filename = paste0("moveVis.",input$out_format),
      content <- function(file) {
        file.copy(paste0("moveVis.",input$out_format), file)
      },
      contentType = NULL #"image/gif"
    )
    downloadButton('file', 'Download Animation File')
  })
  
  output$white <- renderImage({
    list(src = "www/dot.png",
         contentType = 'image/png')
  }, deleteFile = FALSE)

  observeEvent(input$run_prev, {
    hideElement(id = "viewer_prog")
    showElement(id = "viewer_prev")
  })
  
  create_prev <- eventReactive(input$run_prev, {
    #shinyjs::html(id = "wait", html = as.character(tags$img(src = "spinner.gif", id = "spinner")))
    hideElement(id="preview")
    animate_move(data_ani, out_dir = server.dir, out_name = "moveVis", conv_dir = "convert",
                 map_type = input$map_type, paths_mode = input$paths_mode, tail_elements = input$tail_elements, tail_size = input$tail_size,
                 img_title = input$img_title, img_sub = input$img_sub, img_caption = input$img_caption,
                 img_labs =  if(input$desc_labs){c(input$img_labs_x, input$img_labs_y)}else{"labs"},
                 map_elements = input$map_elements, time_scale = input$time_scale, scalebar_col = input$scalebar_col, north_col = input$north_col,
                 
                 frames_nres = if(input$f2){input$frames_nres}else{1},
                 frames_tres = if(input$f3){input$frames_tres}else{0},
                 frames_interval = if(input$f4){input$frames_interval}else{0.04},
                 frames_pixres = if(input$f5){input$frames_pixres}else{80},
                 frames_width = if(input$f5){input$frames_width}else{NA}, frames_height = if(input$f5){input$frames_height}else{NA},
                 out_format = "gif",
                 
                 shiny_mode = TRUE, shiny_session = session,
                 frames_nmax = input$tail_elements+10,
                 log_logical = TRUE, overwrite = TRUE)
    showElement(id="preview")
  })
  
  output$preview <- renderImage({
    create_prev()
    list(src = "moveVis.gif",  contentType = 'image/gif')
  }, deleteFile = FALSE)
  
}


## Launch
shinyApp(ui, server)