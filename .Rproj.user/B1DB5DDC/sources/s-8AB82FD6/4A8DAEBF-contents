    ###############################################################################################
    ############################# Iraq MCNA Dashboard - RShiny #####################################
    ###############################################################################################
    ############################# import packages #####################################
    # spatial data
    library(sf)                     # vector data tools
    library(lwgeom)
    library(raster)                 # raster data tools
    library(leaflet)                # plot interactive maps
    library(geojsonio)              # deal with geojson layers
    library(spatialEco)             # calculate zonal statistics
    library(rmapshaper)             # tools for simplifying polygons
    library(HatchedPolygons)        # hatched polygon patterns with Leaflet
    
    # additional packages
    library(tidyverse)              # data wrangling packages
    library(shiny)                  # App building tools
    library(shinydashboard)         # calculate zonal statistics
    library(shinyjs)                # javascript plugin for Shiny
    library(widgetframe)            # app widgets
    library(rsconnect)              # connect to online shiny server
    library(highcharter)            # interactive graph packages
    library(readxl)                 # import excel files
    library(lubridate)              # smooth date manipulation
    library(htmltools)              # html scripting for R
    library(expss)                  # vlookup for R
    library(htmlwidgets)
    
      # set overall theme
    theme_set(theme_minimal())
  
    ###################################################################################
    ############################ GLOBAL VARIABLES #####################################
    ################################ COLOURS ##########################################
    # REACH colours
    reach_grey     <- "#58585A"
    
    reach_red      <- "#EE5859"
    reach_orpink   <- "#f8aa9b"
    
    reach_pink     <- "#f5a6a7"
    reach_hot_pink <- "#ef6d6f"
    
    reach_mddk_red <- "#bf4749"
    reach_dk_red   <- "#782c2e"
    
    reach_lt_grey  <- "#D1D3D4"
    
    reach_beige    <- "#D2CBB8"
    reach_orange   <- "#F69E61"
    
    white          <- "#FFFFFF"
    black          <- "#00000"
    
    #######################################################################
    # read in MCNA datasets for each pop group
    # returnee
    returnee_df <- read.csv("csv_data/datasets/mcna_returnee_results.csv", na.strings = c("NA", "#N/A", "N/A"), encoding = "UTF-8")
    returnee_df <- returnee_df %>% rename_with( ~ paste("Returnees", .x, sep = "_"))
    colnames(returnee_df)[1:2] <- c("ADM1_EN", "ADM2_EN")
    
     # in camp IDP
    idp_out_camp_df <- read.csv("csv_data/datasets/mcna_idp_out_camp_results.csv", na.strings = c("NA", "#N/A", "N/A"), encoding = "UTF-8")
    idp_out_camp_df <- idp_out_camp_df %>% rename_with( ~ paste("Out_Camp", .x, sep = "_"))
    colnames(idp_out_camp_df)[1:2] <- c("ADM1_EN", "ADM2_EN")
    
    # out of camp IDP
    idp_in_camp_df <- read.csv("csv_data/datasets/mcna_idp_in_camp_results.csv", na.strings = c("NA", "#N/A", "N/A"), encoding = "UTF-8")
    idp_in_camp_df <- idp_in_camp_df %>% rename_with( ~ paste("In_Camp", .x, sep = "_"))
    colnames(idp_in_camp_df)[1:2] <- c("ADM1_EN", "ADM2_EN")
    
    #########################################################################################
    # read in indicator code lookup tables for dropdowns in UI
    mcna_lookup <- read.csv("csv_data/lookups/mcna_lookup.csv", encoding = "UTF-8") 
    # if weird characters at beginning of first colname, remove them
    colnames(mcna_lookup)[1] <- "indicator_id"
    
    #########################################################################################
    ################ Read spatial data and convert to SF Objects
    # read in governorate layer
    irq_gov <- st_read("spatial_data/admin/irq_admbnda_adm1_cso_20190603.shp", options = "ENCODING=UTF-8") 
    
    # # convert gov boundaries to lines to display as boundaries on maps
    irq_gov_lines    <- st_cast(irq_gov,"MULTILINESTRING")
    irq_gov_lines    <- st_cast(irq_gov_lines,"LINESTRING")
    
    # ###################  DISTRICTS #############
    # read in district shapefile layer
    irq_dist <- st_read("spatial_data/admin/irq_admbnda_adm2_cso_20190603.shp", options = "ENCODING=UTF-8")
    
    ############################################################################################
    ########### join data to district shapefile
    irq_dist_data <- left_join(irq_dist, returnee_df, by = "ADM2_EN")
    
    irq_dist_data <- left_join(irq_dist_data, idp_out_camp_df, by = "ADM2_EN")
    
    irq_dist_data <- left_join(irq_dist_data, idp_in_camp_df, by = "ADM2_EN")
    
    # summarize coverage for full assessment
    irq_dist_data <- irq_dist_data %>% 
      mutate(All_indicative = case_when(Returnees_indicative == 0 | Out_Camp_indicative == 0 ~ 0,
                                      Returnees_indicative == 1 & is.na(In_Camp_indicative) ~ 1,
                                      Out_Camp_indicative == 1 & is.na(In_Camp_indicative) ~ 1,
                                      Returnees_indicative == 1 & !is.na(In_Camp_indicative) ~ 2,
                                      Out_Camp_indicative == 1 & !is.na(In_Camp_indicative) ~ 2,
                                      TRUE ~ NA_real_))

    
    #########################################################################################
    ################################## UI!!! ################################################
    #########################################################################################
    # # navbar page with tabs
    ui <- bootstrapPage(
      tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           color: white;
                           }
                           }
                           "))),
          navbarPage(title       = 
                       strong(HTML(sprintf("<span style='font-size: 20px; color: %s'>Multi-Cluster Needs Assessment - Iraq 2020</span>", white))), id="nav",
                     windowTitle = "Iraq MCNA 2020 Dashboard",
    
                     tabPanel(strong("Overview"),
                              value = "panel1",
                              icon= icon("map-marker"),
    div(class="outer",
    
        tags$head(
          # custom CSS
          includeCSS("styles.css")
        ),
        leafletOutput('mapOverview', width="100%", height="100%"),
        
    
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = F, top = 75, left = 20, right = "auto",
                                                width = 500,
    
                                                h3(HTML(sprintf("<span style='color: %s;'><strong>MCNA BACKGROUND</span></strong>", reach_red))),
                                                p(HTML("The Multi-Cluster Needs Assessment (MCNA) provides an overview of the type and severity of sectoral and cross-sectoral needs of conflict affected populations in Iraq. The MCNA is informed by a nationwide household-level survey, for which 9,634 returnee, out-of camp IDP and in-camp IDP households were interviewed between mid-July and mid-September 2020. This includes 2,547 interviews with IDP households living in 40 camps throughout Iraq. The MCNA has been conducted for the eight’s time, in close coordination with the Assessment Working Group (AWG), UN OCHA, and the Inter-Cluster Coordination Group (ICCG) to serve as a comprehensive evidence base for humanitarian actors on the type, severity, variance and development of multi-sectoral needs in Iraq.")),
                                                h3(HTML(sprintf("<span style='color: %s;'><strong>METHODOLOGY MCNA VIII</span></strong>", reach_red))),
                                                p(HTML("Due to the serious health risks that COVID-19 posed to both enumerators and respondents and due to the persisting movement and access restrictions related to government containment measures, data for the MCNA VIII had to be collected through a «hybrid» of face-to-face and phone-based interviews. In the districts that could be surveyed in-person (24/62), a two-staged stratified cluster sampling approach was employed to ensure that the findings for returnees and out-of camp IDPs in these districts are statistically representative with a level of confidence of 90% and a margin of error of 10%. ")),
                                                p(HTML("However, in all IDP camps and in districts where health risks and/or movement or access restrictions prevented face-to-face interviews (38/62), a non-probability purposive quota sampling approach with a minimum target of 60 surveys per population group was adopted. Due to the non-randomized sampling methodology, findings in these strata are not statistically representative and have to be considered as indicative only. All findings for in-camp IDP households are indicative, given the specific COVID-19 risks in camp settings and subsequent decision to survey this population group through phone-based interviews only. Note that the remote setting of phone-based interviews present additional limitations in terms of asking sensitive or technical questions."))),
                                                
                                  absolutePanel(id = "map_desc", class = "panel panel-default", fixed = FALSE,
                                                draggable = F, top = 75,  right = 20, left = "auto",
                                                h3(HTML("<img class='icon' src='noun_Map_2009543.svg' style='color: %s;'><span style='color: #EE5859'><strong>    Coverage Map</strong></span>")),
                                                p(HTML("<span style='color: #58585A'><strong>Data sources:</strong></span><br>Administrative boundaries: OCHA<br>MCNA coverage: REACH")),
                                                
                                                selectInput("coverage", "Population Group:",
                                                            width = 150,
                                                            choices = c("All groups" = "All", "Returnees", "Out of camp IDPs" = "Out_Camp", "In camp IDPs" = "In_Camp")),
                                                            selected = "All groups")
                                  )
                     ),
    
                     tabPanel(strong("Results by district"),
                              value = "panel2",
                              icon= icon("table"),
                              div(class="outer2",
  
                                  tags$head(
                                    includeCSS("styles.css")
                                  ),
  
                                  leafletOutput('map', width="100%", height="100%"),
  
                                  absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                                                draggable = F, top = 75, right = "auto", left = 20,
                                                width = 295,
  
                                                h3(HTML(sprintf("<span style='color: %s;'><strong>MCNA VIII results</span><br> <span style='color: %s;'>by district</span></strong>", reach_red, reach_grey))),
                                                selectInput("popgroup", "Population group:",
                                                            width = 250,
                                                            choices = c("Returnees", "Out of camp IDPs" = "Out_Camp", "In-camp IDPs" = "In_Camp"),
                                                            selected = "Returnees"),
                                                selectInput("sector", "Sector:",
                                                            width = 250,
                                                            choices = sort(unique(as.character(mcna_lookup$sector)), decreasing = FALSE),
                                                            selected = "Household Profile"),
                                                selectInput("indicator", "Indicator:",
                                                            width = 250,
                                                            choices = sort(mcna_lookup$indicator_desc[mcna_lookup$sector == "Household Profile"], decreasing = FALSE))
  
                                  ),
                                  
                                  absolutePanel(id = "info_box", class = "panel panel-default", fixed = TRUE,
                                                draggable = F, top = 75, left = "auto", right = 20,
                                                width = 320,

                                                p(htmlOutput("infobox"))),
                                  

                                  absolutePanel(id = "logo", class = "card", bottom = 35, left = 15, fixed=TRUE, draggable = FALSE, height = "auto",
                                                tags$a(href='https://www.reach-initiative.org', tags$img(src='reach_logo.png', height='50')))

  
                     ))
    ))
    
    ############################################################################################
    ################################## prepare data ############################################
    ############################################################################################
    ######### MAP LABELS #######################################################################
    # gov labels ###########
    # convert gov polygons to centroids for label positioning
    gov_labels <- st_read("spatial_data/gov_centroids.geojson")
    
    # gov label format --- main IRQ map
    gov_labels_halo <- sprintf(
      '<strong><span style="font-size: 9px; color: %s">%s</span><br><span style="font-size: 12px; color: %s">%s</span></strong>',
      reach_grey, gov_labels$ADM1_EN, reach_grey, gov_labels$ADM1_AR)%>% 
      lapply(htmltools::HTML)
    
    ######### LEGEND FORMATTING - LANDING PAGE MAP #############################################
    # construct legend for landing page map
    colors <- c(reach_mddk_red, reach_red, reach_pink, reach_lt_grey)
    labels <- c("In person surveys only","In person and phone surveys", "Phone surveys only", "District not assessed")
  
    addLegendCustom <- function(map, colors, labels, opacity = 0.8){
      
      make_shapes <- function(colors) {
        paste0(colors, "; width:20px; top: 0; height:20px; border:1px solid  white; text-align: center; border-radius:0%")
      }
      
      make_labels <- function(labels) {
        paste0("<div style='display: inline-block; text-align: center; height:20px; line-height: 20px;'>", labels, "</div>")
      }
      
      legend_colors <- make_shapes(colors)
      legend_labels <- make_labels(labels)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position = "bottomright"))
    }
    
    ##################################################################################################
    ################################## SERVER side ###################################################
    ##################################################################################################
    
    server <- function(input, output, session) {
      
      
      # build landing page map
      output$mapOverview <- renderLeaflet({
        
        # variable from selected population group
        select_group <- as.character(input$coverage)
        
        # put selected group into a value for colors and tooltip
        coverage_value <- irq_dist_data[[sprintf("%s_indicative", select_group)]]
        
        ####################### TOOLTIPS ##########################################################################################
        ################### districts ###################################
        coverage_tooltip <- sprintf(
          '<strong><span style="font-size: 20px; color: %s;">%s</span></strong><br><strong><span style="font-size: 15px; color: %s;">%s</span></strong><br><span style ="color: %s;">%s</span>',
          ifelse(coverage_value == 1, reach_pink, reach_hot_pink), 
          irq_dist$ADM2_EN, 
          reach_grey, 
          irq_dist$ADM1_EN, 
          reach_grey, 
          case_when(coverage_value == 1 ~ "phone surveys only", 
                    coverage_value == 0 ~ "in person surveys only",
                    coverage_value == 2 ~ "Both in person and phone surveys"))%>% 
          lapply(htmltools::HTML)
        
        dist_tooltip_not_assessed <- sprintf(
          '<strong><span style="font-size: 14px; color: %s;">%s District</span></strong><br><strong><span style="font-size: 12px; color: %s;">%s Governorate</span></strong><br>Not assessed',
          reach_grey, irq_dist$ADM2_EN, reach_grey, irq_dist$ADM1_EN)%>% 
          lapply(htmltools::HTML)
        
        # render map 
        irq_map <-leaflet(options=leafletOptions(
          zoomControl = FALSE, doubleClickZoom = FALSE, zoomSnap = 0.01, zoomDelta = 0.01, 
          attributionControl = FALSE, dragging = TRUE, scrollWheelZoom = FALSE, 
          easeLinearity = 0.35, minZoom = 6, maxZoom = 7)) %>%
          # add base map layer from carto DB
          addProviderTiles(providers$CartoDB.PositronNoLabels, 
                           options = providerTileOptions(opacity = 0.5)) %>%
          # add districts layer
          addPolygons(data         = irq_dist_data,
                      color        = white,
                      fillColor    = case_when(coverage_value == 0 ~ reach_mddk_red,
                                               coverage_value == 1 ~ reach_pink,
                                               coverage_value == 2 ~ reach_red,
                                               TRUE ~reach_lt_grey),
                      label        = ifelse(is.na(coverage_value),
                                            dist_tooltip_not_assessed,
                                            coverage_tooltip),
                      weight       = 0.2,
                      smoothFactor = 0.5,
                      opacity      = 1,
                      fillOpacity  = 0.9,
                      options      = list(zIndex = 400),
                      highlightOptions = highlightOptions(fillColor    = white,
                                                          color        = white,
                                                          weight       = 2,
                                                          opacity      = 0.9,
                                                          fillOpacity  = 0.4,
                                                          bringToFront = F),
                      labelOptions = labelOptions(noHide      = F,
                                                  opacity     = 0.9,
                                                  direction   = 'left',
                                                  offset      = c(-10,0),
                                                  textOnly    = F,
                                                  style       = list(
                                                    "padding" = "3px 8px",
                                                    "font-family" = "Arial Narrow"
                                                  ))) %>%
          # Add governorate lines for aesthetics
          addPolylines(data        = irq_gov_lines,
                       color       = reach_grey,
                       weight      = 1.1,
                       opacity     = 1.0,
                       options     = list(zIndex = 300))%>%
          # add governorate labelling
          addLabelOnlyMarkers(data         = gov_labels,
                              label        = gov_labels_halo,
                              labelOptions = labelOptions(noHide    = T,
                                                          direction = 'center',
                                                          textOnly  = T,
                                                          style     = list(
                                                            "padding"     = "3px 8px",
                                                            "font-family" = "Arial",
                                                            "text-shadow" = sprintf("-1px 0 %s, 0 1px %s, 1px 0 %s, 0 -1px %s, 0 0 7px %s", white, white, white, white, white)
                                                          ))) %>%
          # # add custom legend
          addLegendCustom(colors, labels) %>% 
  
          # add scale
          addScaleBar(position = "bottomright", scaleBarOptions(imperial = FALSE)) %>% 
          # set view coordinates
          setView(lng  = 41.5, 
                  lat  = 33.7, 
                  zoom = 6.6)
      })
      #############################
      # build district explorer map - 2nd tab!
      output$map <- renderLeaflet({
        
        # put population group input into a variable
        select_popgroup <- as.character(input$popgroup)
        
        # format popgroup for display in legend title
        popgroup        <- case_when(select_popgroup == "In_Camp" ~ "In-camp IDP",
                                     select_popgroup == "Out_Camp" ~ "Out of camp IDP",
                                     select_popgroup == "Returnees" ~ "Returnee")

        # put dropdown input into a variable
        select_indicator <- input$indicator
        select_ind_num   <- as.character(vlookup_df(select_indicator, mcna_lookup, result_column = 'indicator_id', lookup_column = 'indicator_desc'))

        # match to get full indicator name
        select_ind_full <- vlookup_df(select_indicator, mcna_lookup, result_column = 'variable', lookup_column = 'indicator_desc')
        select_ind_tooltip <- vlookup_df(select_indicator, mcna_lookup, result_column = 'tooltip', lookup_column = 'indicator_desc')
        
        # put dynamic variable into a short value name -- very important
        select_value <- irq_dist_data[[sprintf("%s_%s", select_popgroup, select_ind_num)]]
        
        # define palette to be used in the choropleth maps
        reach_red_pal <- colorNumeric(
          na.color = reach_lt_grey,
          palette = colorRampPalette(c("#FFE3E4", reach_red, reach_mddk_red))
          (length(as.numeric(select_value))),
          domain  = as.numeric(select_value))
        
        dist_tooltip <- sprintf(
          '<strong><span style="font-size: %s; color: %s;">%s District</span></strong><br><strong><span style="font-size: %s; color: %s;">%s Governorate</span></strong><br><strong><span style="font-size: %s; color: %s;">%s%s</span></strong><br>%s',
          ifelse(is.na(as.numeric(select_value)), "14px", "20px"),
          ifelse(is.na(as.numeric(select_value)), reach_grey, reach_red),
          irq_dist$ADM2_EN, 
          ifelse(is.na(as.numeric(select_value)), "10px", "15px"),
          reach_grey, 
          irq_dist$ADM1_EN,
          ifelse(is.na(as.numeric(select_value)), "10px", "20px"),
          ifelse(is.na(as.numeric(select_value)), reach_grey, reach_red_pal(as.numeric(select_value))),
          ifelse(!is.na(select_value), formatC(as.numeric(select_value),format="f", big.mark=",", digits=1), "not assessed"),
          case_when(select_ind_num == "a7" | select_ind_num == "a8" ~ "",
                    is.na(select_value) ~ "",
                    TRUE ~ "%"),
          ifelse(!is.na(select_value), select_ind_tooltip, ""))%>%
          lapply(htmltools::HTML)
        
        dist_map <-leaflet(options=leafletOptions(
          # map options
          zoomControl = FALSE, doubleClickZoom = FALSE, zoomSnap = 0.01, zoomDelta = 0.01, 
          attributionControl = FALSE, dragging = TRUE, scrollWheelZoom = FALSE, 
          easeLinearity = 0.35, minZoom = 6.6, maxZoom = 6.6)) %>%
          # add base map layer from carto DB
          addProviderTiles(providers$CartoDB.PositronNoLabels, 
                           options = providerTileOptions(opacity = 0.5)) %>%
          # add districts layer
          addPolygons(data         = irq_dist_data,
                      color        = white,
                      fillColor    = ifelse(is.na(as.numeric(select_value)), 
                                            reach_lt_grey, 
                                            reach_red_pal(as.numeric(select_value))),
                      label        = dist_tooltip,
                      weight       = 0.2,
                      smoothFactor = 0.5,
                      opacity      = 0.9,
                      fillOpacity  = 0.9,
                      options      = list(zIndex = 400),
                      highlightOptions = highlightOptions(fillColor    = white,
                                                          color        = white,
                                                          weight       = 2,
                                                          opacity      = 0.9,
                                                          fillOpacity  = 0.5,
                                                          bringToFront = F),
                      labelOptions = labelOptions(noHide      = F,
                                                  opacity     = 0.9,
                                                  direction   = 'left',
                                                  offset      = c(-10,0),
                                                  textOnly    = F,
                                                  style       = list(
                                                    "padding" = "3px 8px",
                                                    "font-family" = "Arial Narrow"
                                                  ))) %>%
        # Add governorate lines for aesthetics
        addPolylines(data        = irq_gov_lines,
                     color       = reach_grey,
                     weight      = 0.5,
                     opacity     = 1.0,
                     options     = list(zIndex = 300))%>%
          # add governorate labelling
          addLabelOnlyMarkers(data         = gov_labels,
                              label        = gov_labels_halo,
                              labelOptions = labelOptions(noHide    = T,
                                                          direction = 'center',
                                                          textOnly  = T,
                                                          style     = list(
                                                            "padding"     = "3px 8px",
                                                            "font-family" = "Arial",
                                                            "text-shadow" = sprintf("-1px 0 %s, 0 1px %s, 1px 0 %s, 0 -1px %s, 0 0 7px %s", white, white, white, white, white)
                                                          ))) %>%
          # add gradient legend
          leaflet::addLegend(position  = "bottomright",
                             pal       = reach_red_pal,
                             values    = select_value,
                             na.label  = "Not assessed",
                             labFormat = labelFormat(
                             prefix = "    "),
                             title   = case_when(select_ind_num == "a7" ~ sprintf("Average %s <br>household size", popgroup),
                                                 select_ind_num == "a8" ~ sprintf("Average %s <br>family size", popgroup),
                                                 select_ind_num == "a10_child" ~ "% child family <br>members",
                                                 select_ind_num == "a10_adult" ~ "% adult family <br>members",
                                                 select_ind_num == "a10_elder" ~ "% elderly family <br>members",
                                                 select_ind_num == "a11" | select_ind_num == "a14" ~ "% heads of <br> household",
                                                 select_ind_num == "a9_male" ~ "% male family <br>members",
                                                 select_ind_num == "a9_female" ~ "% female family <br>members",
                                                 TRUE ~ sprintf("%% of %s <br>households", popgroup)),
                             bins    = 5,
                             opacity = 1) %>%
          # add scale
          addScaleBar(position = "bottomleft", scaleBarOptions(imperial = FALSE)) %>% 
          
          # set view coordinates
          setView(lng  = 42, 
                  lat  = 33.7, 
                  zoom = 6.6)
      })
      
      # render info box for indicator select
      output$infobox <- renderUI({
        # get population group
        select_popgroup <- as.character(input$popgroup)
        # remove special characters from the IDP categories
        popgroup        <- case_when(select_popgroup == "In_Camp" ~ "In-camp IDPs",
                                     select_popgroup == "Out_Camp" ~ "Out of camp IDPs",
                                            TRUE ~ select_popgroup)
        
        # get sector
        select_sector_input <- input$sector
        select_indicator    <- input$indicator

        indicator_info <- mcna_lookup %>%
          filter(indicator_desc == select_indicator & sector == select_sector_input)

        HTML(sprintf("<img class='icon' src='%s.svg' '><span style='color: %s; font-size: 30px; line-height: 1.6;'><strong>    %s</strong></span><br><span style=' font-size: 20px; color: %s'><strong> %s</span><hr>%s</strong><br>%s",
              indicator_info$icon,
              reach_red, 
              reach_red, 
              select_sector_input, 
              reach_grey, 
              popgroup,  
              indicator_info$indicator_desc, 
              ifelse(!is.na(indicator_info$variable), indicator_info$variable,"")),
              '<style type="text/css"> .shiny-html-output { font-size: 15px; color:#58585A
               font-family: Arial} </style>')
      })
      
      # initiatize dropdowns to default on HH Profile
      observe({
        select_popgroup <- as.character(input$popgroup)
        select_sector <- as.character(input$sector)
        
        # filter indicator for selected pop group
        selected_ind_group <- mcna_lookup %>%
          filter(case_when(select_popgroup == "Returnees" ~ Returnees == 1,
                           select_popgroup == "Out_Camp" ~ Out_Camp == 1,
                           select_popgroup == "In_Camp" ~ In_Camp == 1))
        
        # filter indicator for selected sector
        selected_ind_group <- selected_ind_group %>%
          filter(sector == select_sector)

        updateSelectInput(session, "indicator", 
                          choices = unique(as.character(selected_ind_group$indicator_desc)))
      })
      
      } 
    
    # runApp(shinyApp(ui, server),launch.browser = TRUE)
    
    shinyApp(ui, server)
    
