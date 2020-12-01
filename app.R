### APP Hexagons

library(data.table)
library(readr)
library(h3)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(leaflet)
library(shiny)
library(shinycssloaders)
library(shinythemes)

accidents<- fread("~/Descargas/Motor_Vehicle_Collisions_-_Crashes.csv")

delitos<- fread("~/Descargas/NYPD_Complaint_Data_Historic.csv")

accidents= accidents %>% 
    mutate(`CRASH DATE` = as.Date(`CRASH DATE`, "%m/%d/%Y")) %>% 
    filter(LATITUDE!=0,
           LONGITUDE!=0,
           LONGITUDE< -70,
           LONGITUDE>-80)


delitos= delitos %>% 
    mutate(CMPLNT_FR_DT=as.Date(CMPLNT_FR_DT, "%m/%d/%Y")) %>% 
    filter(Latitude!=0,
           Longitude!=0,
           Longitude< -70,
           Longitude>-80,
           !is.na(Latitude),
           !is.na(Longitude))

button_color_css <- "
          .glyphicon-play,
          .glyphicon-pause{
          color: Crimson
          }
          .irs-bar,
          .irs-bar-edge,
          .irs-single,
          .irs-grid-pol {
            background: Crimson;
            border-color: Crimson;
          }
          #DivCompClear, #FinderClear, #EnterTimes{
          /* Change the background color of the update button
          to blue. */
          background: Crimson;
          /* Change the text size to 15 pixels. */
          font-size: 15px;
          }"

##### UI

# Define UI
ui <- fluidPage(
    navbarPage("NEW YORK DATA", theme = shinytheme("cosmo"),
               tabPanel("Accidents", fluid = TRUE, icon = icon("car-crash"),
                        tags$style(button_color_css),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                titlePanel("Choose desired characterics"),
                                #shinythemes::themeSelector(),
                                sliderInput("hl",
                                            label = h5("Hexagons Level:"),
                                            min = 0,
                                            max = 15,
                                            value = 7),
                                sliderInput("year",
                                            label = h5("Year:"),
                                            min = 2012,
                                            max = 2020,
                                            value = 2018), #animate = animationOptions(interval = 500, loop = TRUE)}
                                fluidRow( column(1, offset = 2,
                                                 checkboxGroupInput(inputId = "bor",
                                                                    label = "Borough",
                                                                    choices = levels(ordered(accidents$BOROUGH)),
                                                                    selected = "BRONX"),
                                                 hr(),
                                                 actionButton(inputId = "Clic", label = 'Filtrar datos')
                                ))
                            )
                            
                            
                            ,
                            mainPanel(
                                fluidRow(
                                    column(3, offset = 9,
                                           
                                           sliderInput("mes",
                                                       label = h5("Mes:"),
                                                       min = 1,
                                                       max = 12,
                                                       value = 1,
                                                       animate = animationOptions(interval = 500, loop = TRUE))
                                    )),
                                
                                leafletOutput("mymap",height = 700)
                                
                                
                                
                            )
                        )
               )
               ,
               
               tabPanel("Complaints", fluid = TRUE, icon = icon("skull-crossbones"),
                        tags$style(button_color_css),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                titlePanel("Choose desired characterics"),
                                #shinythemes::themeSelector(),
                                sliderInput("hl2",
                                            label = h5("Hexagons Level:"),
                                            min = 0,
                                            max = 15,
                                            value = 7),
                                sliderInput("year2",
                                            label = h5("Year:"),
                                            min = 2012,
                                            max = 2020,
                                            value = 2018), #animate = animationOptions(interval = 500, loop = TRUE)}
                                fluidRow( column(1, offset=2,
                                                 checkboxGroupInput(inputId = "bor2",
                                                                    label = "Borough",
                                                                    choices = levels(ordered(delitos$BORO_NM)),
                                                                    selected = "BRONX"),
                                        
                                
                                                checkboxGroupInput(inputId = "cat2",
                                                                      label = "Category",
                                                                      choices = levels(ordered(delitos$LAW_CAT_CD)),
                                                                      selected = "MISDEMEANOR"),
                                        
                                                 hr(),
                                                 actionButton(inputId = "Clic2", label = 'Filtrar datos')
                                )
                            )
                            )
                            
                            ,
                            mainPanel(
                                fluidRow(
                                    column(3, offset = 9,
                                           
                                           sliderInput("mes",
                                                       label = h5("Mes:"),
                                                       min = 1,
                                                       max = 12,
                                                       value = 1,
                                                       animate = animationOptions(interval = 500, loop = TRUE))
                                    )),
                                
                                leafletOutput("mymap2", height = 700)
                                
                                

                                                                
                            )
                        )
               )
               
               # navbarMenu("More", icon = icon("info-circle"),
               #            tabPanel("School Types & Rankings", fluid = TRUE,
               #                     fluidRow(
               #                       column(6,
               #                              h4(p("School Types")),
               #                              h5(p("US News and World Report uses four categories of schools for their rankings system:"),
               #                                 p("National universities are those that offer a “full range” of undergraduate majors, while also offering graduate programs, including at the doctoral level.  Intercollegiate sports, including swimming, are generally pursued by undergrads, or occasionally students in master’s degree programs, so a university having nor not having doctoral programs isn’t directly relevant.  That said, doctoral programs and faculty research go hand-in-hand, so faculty at national universities are nearly always active in research, in addition to their teaching duties.  National universities are usually, though not always, large.  Most state flagship universities would fall under this category."),
               #                                 p("Regional universities are similar to national universities in that they have a wide range of undergrad programs, and some master’s programs as well.  They generally do not have large doctoral programs, and correspondingly less faculty research."),
               #                                 p("National liberal arts colleges are undergraduate focused, with few graduate programs.  They award the majority of their degrees in arts and sciences, and may or may not have other undergraduate programs, like engineering or professional studies."),
               #                                 p("Regional colleges are also undergraduate focused institutions, but do not award the majority of their degrees in arts and/or sciences.  These colleges may have a particular focus, like technology or agriculture, or they may be primarily two year institutions that also grant some four year degrees.")
               #                              )
               #                       ),
               #                       column(6,
               #                              h4(p("US News Rankings")),
               #                              h5(p("Every year the US News and World Report issues a set of rankings for US colleges and universities.  They are a used in this setting as a guideline, and a general comparative device, but can often be misinterpreted or overvalued.  The major component of a given school’s rankings are graduation and retention rates, academic reputation (basically name recognition), and faculty resources (class size, faculty salary etc.).  Each school is given a score, and then placed in order.  That said the scored differences between schools of different rank can be quite small, so take the rankings with a grain of salt.
               #             The full methodology for the US News and World report college rankings can be found ",
               #                                   a("here.",
               #                                     href = "https://www.usnews.com/education/best-colleges/articles/ranking-criteria-and-weights"))
               #                              )
               #                       ))
               # 
               #            ),
               # 
               #            tabPanel("About", fluid = TRUE,
               #                     fluidRow(
               #                       column(6,
               #                              #br(),
               #                              h4(p("About the Project")),
               #                              h5(p("This project is intended to facilitate useful comparisons between colleges in the NCAA, based on swimming performance, location, and academic information.  Here a prospective student-athlete, or anyone else with an interest can find schools fitting a particular set of criterion relevant to them, for example, schools close to home, with times in a particular range, and of a specified academic profile.")),
               #                              br(),
               #                              h5(p("The project began as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below.")),
               #                              br(),
               #                              h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at gpilgrim2607@gmail.com"),
               #                                 p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/gpilgrim2670/SwimMap"), "."))
               # 
               #                              #hr(),
               # 
               #                       ),
               #                       column(6,
               #                              #br(),
               #                              #             HTML('<img src="GregPicCrop.png", height="110px"
               #                              # style="float:right"/>','<p style="color:black"></p>'),
               #                              h4(p("About the Author")),
               #                              h5(p("Greg is a former collegiate swimmer.  After completing his undergrad degree he joined USMS, earned a PhD in chemistry, and began officiating swimming at the high school level.  He now swims with his local USMS team and serves as an official in USA Swimming while also working as an engineer.  He is the author the", a("SwimmeR package", href = "https://github.com/gpilgrim2670/SwimmeR"), "for working with swimming results in the R environment."),
               #                                 p("For more work with swimming and R see Greg's articles at ", a("Swimming + Data Science", href = 'https://pilgrim.netlify.app/'), "."),
               # 
               #                              ),
               #                              HTML('<img src="GregPicCrop.png", height="200px"'),
               #                              br()
               #                       )
               #                     ),
               #                     br(),
               #                     hr(),
               #                     h5("Sources:"),
               #                     h6(
               #                       p("Swimming Information from ",
               #                         a("USA Swimming",
               #                           href = "https://www.usaswimming.org/Home/times/ncaa-information"))),
               #                     h6(
               #                       p("US News College Rankings from ",
               #                         a("US News",
               #                           href = "https://www.usnews.com/best-colleges/rankings"))),
               #                     h5("Built with",
               #                        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
               #                        "by",
               #                        img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
               #                        ".")
               
    )
)


server <- function(input, output) {
    
 
    
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            fitBounds(-73.71, 40.480, -74.27, 40.915)
    })
    
    
    reactivo <- eventReactive(input$Clic,
                              {
                                  hex_s= input$hl
                                  bor_s=input$bor
                                  year_s=input$year
                                  datos= accidents %>%
                                      mutate(`CRASH DATE` = as.Date(`CRASH DATE`),
                                             year= year(`CRASH DATE`)) %>%
                                      filter(BOROUGH %in% bor_s,
                                             year == year_s) %>%
                                      mutate(hex = geo_to_h3(c(LATITUDE,LONGITUDE),hex_s),
                                             mes=month(`CRASH DATE`)) %>%
                                      group_by(hex,mes) %>%
                                      summarise(
                                          n=length(COLLISION_ID)
                                      ) %>%
                                      filter(hex!="0")
                                  
                                  hexagons= h3_to_geo_boundary_sf(datos$hex) %>%
                                      mutate(
                                          n=datos$n,
                                          mes=datos$mes
                                      )
                                  
                                  return(list(hexagons= hexagons))
                                  
                              })
    
    filteredData <- reactive({
        req(reactivo())
        hexagons=reactivo()$hexagons
        #add rollified thing
        from<- input$mes
        hexagons %>% filter(mes == from)
    })                        
    observe({
        req(filteredData())
        req(reactivo())
        hexagons=reactivo()$hexagons
        pal <- colorBin("YlOrRd", domain = hexagons$n)
        leafletProxy("mymap", data = filteredData()) %>% 
            clearShapes()%>% 
            addPolygons(
                weight = 2,
                color = ~ pal(n),
                fillColor = ~ pal(n),
                fillOpacity = 0.8,
                label = ~ sprintf("Accidentes: %f \n ", n)
            )
    })
    
    output$mymap2 <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            fitBounds(-73.71, 40.480, -74.27, 40.915)
    })
    
    reactivo2 <- eventReactive(input$Clic2,
                              {
                                  hex_s= input$hl2
                                  bor_s=input$bor2
                                  year_s=input$year2
                                  datos=delitos %>%
                                      mutate(year= year(CMPLNT_FR_DT)) %>%
                                      filter(BORO_NM %in% bor_s,
                                             year == year_s) %>%
                                      mutate(hex = geo_to_h3(c(Latitude,Longitude),hex_s),
                                             mes=month(CMPLNT_FR_DT)) %>%
                                      group_by(hex,mes) %>%
                                      summarise(
                                          n=length(CMPLNT_NUM)
                                      ) %>%
                                      filter(hex!="0")
                                  
                                  hexagons= h3_to_geo_boundary_sf(datos$hex) %>%
                                      mutate(
                                          n=datos$n,
                                          mes=datos$mes
                                      )
                                  
                                  return(list(hexagons= hexagons))
                                  
                              })
    
    filteredData2 <- reactive({
        req(reactivo2())
        hexagons2=reactivo2()$hexagons
        #add rollified thing
        from<- input$mes
        hexagons2 %>% filter(mes == from)
    })                        
    observe({
        req(filteredData2())
        req(reactivo2())
        hexagons2=reactivo2()$hexagons
        pal <- colorBin("YlOrRd", domain = hexagons2$n)
        leafletProxy("mymap2", data = filteredData2()) %>% 
            clearShapes()%>% 
            addPolygons(
                weight = 2,
                color = ~ pal(n),
                fillColor = ~ pal(n),
                fillOpacity = 0.8,
                label = ~ sprintf("Delitos: %f \n ", n)
            )
    })
    # #stuff in server
    # filteredData <- reactive({
    #   #add rollified thing
    #   mes_s= input$mes
    #   hex_s= input$hl
    #   hexagons = accidentes %>% 
    #     mutate(hex = geo_to_h3(c(Latitude,Longitude), input$hl))
    #     filter
    #   
    # })
    # output$mapAct<-renderLeaflet({
    #   leaflet() %>%
    #     addProviderTiles("CartoDB.Voyager")
    #   
    # })
    # observe({
    #   leafletProxy("mapAct", data = filteredData()) %>% 
    #     clearShapes()%>% 
    #     addPolygons(
    #       weight = 2,
    #       color = ~ pal(n),
    #       fillColor = ~ pal(n),
    #       fillOpacity = 0.8,
    #       label = ~ sprintf("Accidentes: %f \n ", n)
    #     )
    # })
    # 
}


shinyApp(ui, server)


## themes: flatly, simplex, COSMO