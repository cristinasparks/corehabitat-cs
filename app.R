#Load Packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)

#Load and wrangle Excel data
Size_Ecoregion <- read_excel("Size_Ecoregion.xlsx")
Size_Ecoregion$Ecoregion <- factor(Size_Ecoregion$Ecoregion, levels = rev(sort(unique(Size_Ecoregion$Ecoregion)))) #Reverse alphabetical order so that coord_flip() works

Size_Both_Final <- read_excel("Size_Both_Final.xlsx")
Size_Both_Final$Ecoregion <- factor(Size_Both_Final$Ecoregion, levels = rev(sort(unique(Size_Both_Final$Ecoregion))))
Size_Both_Final$STATUS <- factor(Size_Both_Final$STATUS, levels = rev(sort(unique(Size_Both_Final$STATUS))))

#Load and wrangle core habitat shapefile data
core_data <- st_read(dsn = ".", "core_all")
core_final <- st_transform(core_data, "+init=epsg:4326")
core_final[core_final$STATUS == "1",]$STATUS = "1&2"

core_final$ECOREGION <- as.character(core_final$ECOREGION)
core_final[core_final$ECOREGION == "CASCADE RANGES",]$ECOREGION = "Cascade Ranges"
core_final[core_final$ECOREGION == "CENTRAL WESTERN CALIFORNIA",]$ECOREGION = "Central Western"
core_final[core_final$ECOREGION == "EAST OF SIERRA NEVADA",]$ECOREGION = "East of Sierra Nevada"
core_final[core_final$ECOREGION == "GREAT CENTRAL VALLEY",]$ECOREGION = "Great Central Valley"
core_final[core_final$ECOREGION == "MODOC PLATEAU",]$ECOREGION = "Modoc Plateau"
core_final[core_final$ECOREGION == "MOJAVE DESERT",]$ECOREGION = "Mojave Desert"
core_final[core_final$ECOREGION == "NORTHWESTERN CALIFORNIA",]$ECOREGION = "Northwestern"
core_final[core_final$ECOREGION == "SIERRA NEVADA",]$ECOREGION = "Sierra Nevada"
core_final[core_final$ECOREGION == "SONORAN DESERT",]$ECOREGION = "Sonoran Desert"
core_final[core_final$ECOREGION == "SOUTHWESTERN CALIFORNIA",]$ECOREGION = "Southwestern"

core_final$STATUS <- as.factor(core_final$STATUS)
core_final$ECOREGION <- as.factor(core_final$ECOREGION)

#Load and wrangle ecoregion shapefile data
jepson <- st_read(dsn = ".", "jepson_shape")
jepson_final <- st_transform(jepson, "+init=epsg:4326")

jepson_final$ECOREGION <- as.character(jepson_final$ECOREGION)
jepson_final[jepson_final$ECOREGION == "CASCADE RANGES",]$ECOREGION = "Cascade Ranges"
jepson_final[jepson_final$ECOREGION == "CENTRAL WESTERN CALIFORNIA",]$ECOREGION = "Central Western"
jepson_final[jepson_final$ECOREGION == "EAST OF SIERRA NEVADA",]$ECOREGION = "East of Sierra Nevada"
jepson_final[jepson_final$ECOREGION == "GREAT CENTRAL VALLEY",]$ECOREGION = "Great Central Valley"
jepson_final[jepson_final$ECOREGION == "MODOC PLATEAU",]$ECOREGION = "Modoc Plateau"
jepson_final[jepson_final$ECOREGION == "MOJAVE DESERT",]$ECOREGION = "Mojave Desert"
jepson_final[jepson_final$ECOREGION == "NORTHWESTERN CALIFORNIA",]$ECOREGION = "Northwestern"
jepson_final[jepson_final$ECOREGION == "SIERRA NEVADA",]$ECOREGION = "Sierra Nevada"
jepson_final[jepson_final$ECOREGION == "SONORAN DESERT",]$ECOREGION = "Sonoran Desert"
jepson_final[jepson_final$ECOREGION == "SOUTHWESTERN CALIFORNIA",]$ECOREGION = "Southwestern"

jepson_final$ECOREGION <- as.factor(jepson_final$ECOREGION)


ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Core Habitat in California",
                  titleWidth = 275),
  
  dashboardSidebar(
    width = 275,
    
    sidebarMenu(
      
      #Define two menu items, to provide metadata
      
      menuItem("About", tabName = "Metadata"),
      menuItem("Data", tabName = "Data"),
      
      
      #Let users decide how they want to look at core habitat
      
      radioButtons("radio1", h4("I want to split core habitat by:"), choices = list(
            "Ecoregion" = 1,
            "Management Category" = 2,
            "Ecoregion and Management Category" = 3)
              ),
      
      #Define what other user input there is depending on radio input
      
      conditionalPanel(
        condition = "input.radio1 == 1",
        checkboxGroupInput("select1", "Ecoregion",
                           choices = rev(sort(unique(Size_Ecoregion$Ecoregion))),
                           selected = Size_Ecoregion$Ecoregion)
      ),
      
      conditionalPanel(
        condition = "input.radio1 == 2",
        checkboxGroupInput("select2", "Management Category",
                           choices = unique(Size_Both_Final$STATUS),
                           selected = Size_Both_Final$STATUS)
      ),
      
      conditionalPanel(
        condition = "input.radio1 == 3",
        checkboxGroupInput("select3", "Ecoregion",
                           choices = rev(sort(unique(Size_Ecoregion$Ecoregion))),
                           selected = Size_Ecoregion$Ecoregion),
        checkboxGroupInput("select4", "Management Category",
                           choices = unique(Size_Both_Final$STATUS),
                           selected = unique(Size_Both_Final$STATUS))
      ),
      
      #Define an action button for any map reloads and style it
      
      actionButton("action", "Reload Map!"),
      
      tags$style(type='text/css', "#action { width:40%; margin-left: 83px;}")
      
      )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Data",
    
              fluidRow(
                
                #Conditional Panel for each map dependent on data split (radio input)
                
                conditionalPanel("input.radio1 == 1",
                                 leafletOutput("map1", width = 600, height = 300)),
                conditionalPanel("input.radio1 == 2",
                                 leafletOutput("map2", width = 600, height = 300)),
                conditionalPanel("input.radio1 == 3",
                                 leafletOutput("map3", width = 600, height = 300))
                ),
              
              fluidRow(
                
                tabsetPanel( 
                  
                  #Different tabs for different data
                  
                  tabPanel("Core Habitat Size", value = 1,
                           
                           #Conditional Panels dependent on data split (radio input)
                           
                           conditionalPanel("input.radio1 == 1", 
                                            plotOutput("graph_eco_size",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 2",
                                            plotOutput("graph_man_size",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 3",
                                            plotOutput("graph_both_size",
                                                       height = 300, width = 600))
                           ),
                  
                  tabPanel("% of Area", value = 2,
                           
                           conditionalPanel("input.radio1 == 1",
                                            plotOutput("graph_eco_areatot",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 2",
                                            plotOutput("graph_man_areatot",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 3",
                                            plotOutput("graph_both_areatot",
                                                       height = 300, width = 600))
                           ),
                  
                  tabPanel("% of Core Habitat", value = 3,
                           
                           conditionalPanel("input.radio1 == 1",
                                            plotOutput("graph_eco_coretot",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 2",
                                            plotOutput("graph_man_coretot",
                                                       height = 300, width = 600)),
                           
                           conditionalPanel("input.radio1 == 3",
                                            plotOutput("graph_both_coretot",
                                                       height = 300, width = 600))
                           
                           )
                  
                  ) #Closes tabset panel
                ) #Closes fluidRow
              ), #Closes tabItem (Data)
      
      tabItem(tabName = "Metadata",
              h2("About the app", align = "center"),
              
              fluidRow(
                box(title = "Using the app",
                    solidHeader = TRUE,
                    status = "success",
                    collapsible = TRUE,
                    width = 600,
                    collapsed = TRUE,
                    style = "font-size: 125%;",
                    
                    tags$p("This app provides an initial exploration of core habitat and its characteristics across California.  Core habitat can be explored in terms of ecoregion, management status, or both.  Characteristics of the core habitat include size, percent of the area that it covers, and percent of the core habitat that a particular category represents.  For instance, it can answer questions such as:",
                           tags$ul(
                             tags$li(
                               "What percentage of all core haibtat in California is unprotected?"),
                             tags$li(
                               "How much core habitat is there in the Sierra Nevada?"),  
                             tags$li(
                               "What percentage of the core habitat in the Great Central Valley is protected?")
                             )
                           
                           ),
                    tags$p("Further information on  data sources, definitions, and how to interpret the graphs are included in this About section.  Please allow some time for app loading. To reset the map according to user inputs, click the refresh map button (this can take a few seconds)."
                           )
                    )
                ),
              
              fluidRow(
              box(title = "Data Source",
                  solidHeader = TRUE,
                  status = "success",
                  collapsible = TRUE,
                  width = 600,
                  collapsed = TRUE,
                  style = "font-size: 125%;",
                  
                  tags$p("All data was sourced after: Sparks, C. (2012).",
                         tags$i(" Assessing the potential contribution of unmanaged core habitat to conservation efforts in California. "),
                         "Department of Geography, University of Leicester, unpublished.  For full citations and methods please see this report."
                         ),
                  tags$p("Data used in this report's analyses were accessed online in 2011 from",
                         tags$a("ESRI Census 2000 Tiger/Line Data website",
                                href = "http://www.esri.com/data/download/census2000-tigerline/index.html",
                                target = "_blank"),
                         "and the",
                         tags$a("California Gap Analysis Project website.",
                                href = "http://www.biogeog.ucsb.edu/projects/gap/gap_data_state.html",
                                target = "_blank")
                         )
                  )
              ),
              fluidRow(
                box(title = "Management Categories",
                    solidHeader = TRUE,
                    status = "success",
                    collapsible = TRUE,
                    width = 600,
                    collapsed = TRUE,
                    style = "font-size: 125%;",
                    
                    tags$p(
                      tags$b("Management Category 1&2"),
                      "lands are the most protected from alteration, and are combined due to their similarities.  They include National Parks."),
                    tags$p(
                      tags$b("Management Category 3"),
                      "lands are more open to intensive or extensive use, such as National Recreation Areas."),
                    tags$p(
                      tags$b("Management Category 4"),
                      "lands are completely unprotected and include private land.")
                )
              ),
              
              fluidRow(
              box(title = "Core habitat designation",
                  solidHeader = TRUE,
                  status = "success",
                  collapsible = TRUE,
                  width = 600,
                  collapsed = TRUE,
                  style = "font-size: 125%;",
                  
                  tags$p("Core areas in this report are defined as natural areas 5000 acres or larger, containing no agriculture or urban land cover, roads, or water bodies."),
                  tags$p("Core habitat designation and subsequent analyses were completed in ArcMap 9.3.")
                  )
              ),
              
              fluidRow(
                box(title = "Core habitat descriptors",
                    solidHeader = TRUE,
                    status = "success",
                    collapsible = TRUE,
                    width = 600,
                    collapsed = TRUE,
                    style = "font-size: 125%;",
                    
                    tags$p(
                      tags$b("Core habitat size"),
                      "simply represents the amount of core habitat for that ecoregion and/or management category in square kilometers."),
                    tags$p(
                      tags$b("Percent of area"),
                      "represents the percent of the ecoregion that is core habitat; for core habitat split only by management category, this represents the percent of California (i.e. all ecoregions combined) that is core habitat for each management status."),
                    tags$p(
                      tags$b("Percent of core habitat"),
                      "represents the percent of total core habitat that the ecoregion or the management status contains (for instance, 32% of all core habitat in California is located in the Mojave Desert).  If data are split by both, it artificially splits core areas so much that statistics are meaningless.",
                      tags$p("Therefore, splitting the data by both ecoregion and management data will show the percentage of",
                             tags$u("each ecoregion's"),
                             "total core habitat that is a certain management category."))
                )
              )
              
        ) #Closes tabItem
      
      ) #Closes tabItems
    ) #Closes dashboardBody
  ) #Closes dashboardPage


server <- function(input, output){
  
  #Set the desired palette and windows font
  
  pal <- colorFactor(palette = c("#9B0000", "#002673", "#267300"), levels =  core_final$STATUS)
  
  #Define map outputs
  
  output$map1 <- renderLeaflet({
    
    input$action #Map reload depends only on action button
    isolate({
      
      #Filter data depending on second UI input
      
      core_sub_eco <- core_final %>% 
        filter(ECOREGION %in% input$select1)
      
      #Map the filtered data
      
      leaflet(core_sub_eco) %>% 
      addTiles() %>%  
      addPolygons(data = jepson_final,
                  label = jepson_final$ECOREGION,
                  fillOpacity = 0.1,
                  weight = 1,
                  color = "black",
                  highlightOptions = highlightOptions(
                    color = "black", 
                    weight = 2, 
                    bringToFront = TRUE)) %>% 
      addPolygons(color = "#66c2a5",
                  fillOpacity = 0.5,
                  weight = 1)
    
  }) })
           
  output$map2 <- renderLeaflet({
    
    input$action
    isolate({
      
   core_sub_man <- core_final %>% 
      filter(STATUS %in% input$select2)
   
    leaflet(core_sub_man) %>% 
      addTiles() %>%
      addPolygons(color = ~pal(core_sub_man$STATUS),
                  fillOpacity = 1,
                  weight = 1) %>%
      addLegend(pal = pal,
                values = ~core_sub_man$STATUS,
                title = "Management\nCategory")
           
  }) })
           
  output$map3 <- renderLeaflet({
    
    input$action
    isolate({
      
      core_sub_both <- core_final %>% 
        filter(ECOREGION %in% input$select3 & STATUS %in% input$select4) 
      
      leaflet(core_sub_both) %>% 
        addTiles() %>%  
        addPolygons(data = jepson_final,
                    label = jepson_final$ECOREGION,
                    fillOpacity = 0.1,
                    weight = 1,
                    color = "black",
                    highlightOptions = highlightOptions(
                      color = "black", 
                      weight = 2, 
                      bringToFront = TRUE)) %>% 
        addPolygons(color = ~pal(core_sub_both$STATUS),
                    fillOpacity = 1,
                    weight = 1) %>% 
        addLegend(pal = pal,
                  values = ~core_sub_both$STATUS,
                  title = "Management\nCategory")
      
    }) })           

  
  #Define graph outputs
  
  output$graph_eco_size <- renderPlot({
    
    ggplot(subset(Size_Ecoregion, Ecoregion %in% input$select1),
           aes(x = Ecoregion, y = Core_Size)) +
      geom_col(fill = "#66c2a5") +
      labs(x = NULL,
           y = bquote('Size of Core Habitat'~(km^2))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
       
      
  })


  output$graph_man_size <- renderPlot({
    
    ggplot(subset(Size_Both_Final, STATUS %in% input$select2 & Ecoregion == "California"),
           aes(x = STATUS, y = Core_Size, fill = STATUS)) +
      geom_col() +
      scale_fill_manual(values = c("4" = "#9B0000", "3" = "#002673", "1&2" = "#267300"),
                        guide=FALSE) +
      labs(x = "Management Category",
           y = bquote('Size of Core Habitat'~(km^2))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
    
  })
    

  output$graph_both_size <- renderPlot({
    
    ggplot(subset(Size_Both_Final, Ecoregion %in% input$select3 & STATUS %in% input$select4),
           aes(x = Ecoregion, y = Core_Size, fill = STATUS)) +
      geom_col(aes(fill=STATUS)) +
      scale_fill_manual(values = c("1&2" = "#267300","3" = "#002673", "4" = "#9B0000"),
                        name = "Management\nCategory",
                        breaks = c("1&2", "3", "4")) +
      labs(x = NULL, y = bquote('Size of Core Habitat'~(km^2))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
    
  })


  output$graph_eco_areatot <- renderPlot({
    
    ggplot(subset(Size_Ecoregion, Ecoregion %in% input$select1),
           aes(x = Ecoregion, y = Per_Ecoregion)) +
      geom_col(fill = "#66c2a5") +
      labs(x = NULL, y = expression("% of"~italic("Ecoregion"))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})
  
  
  output$graph_man_areatot <- renderPlot({
    
    ggplot(subset(Size_Both_Final, STATUS %in% input$select2 & Ecoregion == "California"),
           aes(x = STATUS, y = Per_Total_Ecoregion, fill = STATUS)) +
      geom_col() +
      scale_fill_manual(values = c("4" = "#9B0000", "3" = "#002673", "1&2" = "#267300"),
                        guide=FALSE) +
      labs(x = "Management Category",
           y = expression("% of"~italic("California"))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})

  output$graph_both_areatot <- renderPlot({
    
    ggplot(subset(Size_Both_Final, Ecoregion %in% input$select3 & STATUS %in% input$select4),
           aes(x = Ecoregion, y = Per_Total_Ecoregion, fill = STATUS)) +
      geom_col(aes(fill=STATUS)) +
      scale_fill_manual(values = c("1&2" = "#267300","3" = "#002673", "4" = "#9B0000"),
                        name = "Management\nCategory",
                        breaks = c("1&2", "3", "4")) +
      labs(x = NULL,
           y = expression("% of"~italic("Ecoregion"))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})

  output$graph_eco_coretot <- renderPlot({
    
    ggplot(subset(Size_Ecoregion, Ecoregion %in% input$select1),
           aes(x = Ecoregion, y = Per_Core)) +
      geom_col(fill = "#66c2a5") +
      labs(x = NULL,
           y = expression("% of Total Core Habitat")) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})

  output$graph_man_coretot <- renderPlot({
    
    ggplot(subset(Size_Both_Final, STATUS %in% input$select2 & Ecoregion == "California"),
           aes(x = STATUS, y = Per_Ecoregion_Core, fill = STATUS)) +
      geom_col() +
      scale_fill_manual(values = c("4" = "#9B0000", "3" = "#002673", "1&2" = "#267300"),
                        guide=FALSE) +
      labs(x = "Management Category", y = expression("% of Total Core Habitat")) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})


  output$graph_both_coretot <- renderPlot({
    
    ggplot(subset(Size_Both_Final, Ecoregion %in% input$select3 & STATUS %in% input$select4),
           aes(x = Ecoregion, y = Per_Ecoregion_Core, fill = STATUS)) +
      geom_col(aes(fill=STATUS)) +
      scale_fill_manual(values = c("1&2" = "#267300","3" = "#002673", "4" = "#9B0000"),
                        name = "Management\nCategory",
                        breaks = c("1&2", "3", "4")) +
      labs(x = NULL,
           y = expression("% of"~italic("Each Ecoregion's")~"Total Core Habitat")) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=16)) +
      coord_flip()
  
})

}

shinyApp(ui = ui, server = server)


