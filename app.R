library(shiny)
library(tidyverse)
library(shinydashboard)
library(sf)
library(leaflet)
library(DT)
library(scales)
library(formattable)
library(geojsonio)
library(htmltools)
library(lwgeom)
library(readxl)
library(shinycssloaders)
library(tmap)
library(tidyr)
library(shinyjs)
library(foreign)
library(rmarkdown)
library(knitr)
library(formattable)
library(leafem)
library(tools)
library(leafgl)

#### load data ####


pm<-read.dbf("H:\\model\\project_level_analysis\\parcel_master2.dbf")

#pm %>% mutate(BEDROOMS=as.character(BEDROOMS)) %>% filter(TRPA_LANDU %in% c("Single Family Residential")) %>% 
 # filter(!BEDROOMS %in% c("<NA>","0","N/A","4   *","<NA>"))  %>%
 # filter(!is.na(BEDROOMS)) %>%
 # mutate(BEDROOMS=as.numeric(BEDROOMS)) %>%
 # summarise(avg_bed=mean(BEDROOMS, na.rm=T))

boundary<- geojson_read("https://opendata.arcgis.com/datasets/85a2e8e4bf994742a5855c1339517681_0.geojson", what="sp") %>% st_as_sf(crs=4326)

trip_rates<- read_excel("Commercial_Assessment_MC_2.0.xlsx", sheet="list_clean1") %>%
  filter(!use %in% c("Single-Family Detached","Senior Adult Housing – Attached","Congregate Care Facility (Residential Care)", "Multi-Family (low-rise, one or two levels)")) %>%
  filter(!is.na(Rate))

jur <- st_read(".", "jurisdictions")%>% st_as_sf()  %>%
  st_transform(crs=4326) %>%
  mutate(name=case_when(COUNTY == "CSLT" ~ "South Lake Tahoe", 
                        COUNTY == "CARSON" ~ "Carson", 
                        TRUE ~ as.character(paste(toTitleCase(tolower(COUNTY)), "County", sep=" "))))

pia_zones<-sf::st_read(".","PIA_Zones") %>%
  mutate(zone_no=extract_numeric(zone_id))

res_data<-read_csv("residential_data.csv") %>%
  select(-geometry) %>%
  mutate(sos=jur_total_vmt_capita*.85)

res_data_sf<-pia_zones %>% left_join(res_data, by="zone_id")%>%
  select(zone_id,zone_vmt_per_capita, jur_total_vmt_capita, percent_threshold,cat,sos, COUNTY, zone_vmt,zone_pop)

res_data_full<-read_csv("residential_data_full_length.csv") %>%
  #select(-geometry) %>%
  mutate(sos=jur_total_vmt_capita*.85)

res_data_sf_full<-pia_zones %>% left_join(res_data_full, by="zone_id")%>%
  select(zone_id,zone_vmt_per_capita, jur_total_vmt_capita, percent_threshold,cat,sos, COUNTY, zone_vmt,zone_pop)

#write.csv(res_data_sf %>% data.frame() %>% select(-c(geometry)), "residential_table.csv", row.names=F)

#tau_data<-read_csv("tau_data.csv") %>%
 # select(-geometry)

#tau_data_sf<-pia_zones %>% left_join(tau_data, by="zone_id")

length_data<-read_csv("trip_length_data.csv")%>%
  select(-geometry)

length_data_sf<-pia_zones %>% left_join(length_data, by="zone_id") 

length_data_full<-read_csv("trip_length_data.csv")%>%
  select(-geometry)

length_data_sf_full<-pia_zones %>% left_join(length_data_full, by="zone_id") 

length_jur_full<-st_join(st_buffer(length_data_sf_full,.00001), st_buffer(jur %>% dplyr::select("COUNTY"),.00001), largest=T) %>%
  mutate(COUNTY=case_when(zone_id=="Zone 21" ~ "CSLT",
                          TRUE ~as.character(COUNTY)),
         jur_average=case_when(COUNTY=="CARSON" ~ 25.4,
                               COUNTY=="CSLT" ~ 6.35,
                               COUNTY=="DOUGLAS" ~ 11.4,
                               COUNTY=="EL DORADO" ~ 10,
                               COUNTY=="PLACER" ~ 13.2,
                               COUNTY=="WASHOE" ~ 12),
         threshold_15_below = jur_average * .85,
         percent_threshold=avg_zone_trip_length/threshold_15_below,
         cat=case_when(percent_threshold < .5 ~ "Less than 50% of threshold",
                       between(percent_threshold,.5, .75) ~ "50-75% of threshold",
                       between(percent_threshold,.751, .99999) ~ "75-100% of threshold",
                       between(percent_threshold,1, 1.25) ~ "100-125% of threshold",
                       between(percent_threshold,1.251, 1.5) ~ "125-150% of threshold",
                       percent_threshold > 1.5 ~ "Over 150% of threshold"))

length_jur<-st_join(st_buffer(length_data_sf,.00001), st_buffer(jur %>% dplyr::select("COUNTY"),.00001), largest=T) %>%
  mutate(COUNTY=case_when(zone_id=="Zone 21" ~ "CSLT",
                          TRUE ~as.character(COUNTY)),
         jur_average=case_when(COUNTY=="CARSON" ~ 13.1,
                               COUNTY=="CSLT" ~ 4.26,
                               COUNTY=="DOUGLAS" ~ 6.37,
                               COUNTY=="EL DORADO" ~ 6.69,
                               COUNTY=="PLACER" ~ 6.51,
                               COUNTY=="WASHOE" ~ 5.56),
         threshold_15_below = jur_average * .85,
         percent_threshold=avg_zone_trip_length/threshold_15_below,
         cat=case_when(percent_threshold < .5 ~ "Less than 50% of threshold",
                       between(percent_threshold,.5, .75) ~ "50-75% of threshold",
                       between(percent_threshold,.751, .99999) ~ "75-100% of threshold",
                       between(percent_threshold,1, 1.25) ~ "100-125% of threshold",
                       between(percent_threshold,1.251, 1.5) ~ "125-150% of threshold",
                       percent_threshold > 1.5 ~ "Over 150% of threshold"))


#write.csv(length_jur %>% data.frame() %>% select(-c(geometry)), "trip_length_table.csv", row.names=F)

length_jur$cat1 <- factor(length_jur$cat, levels=c("Over 150% of threshold",
                                                   "125-150% of threshold",
                                                   "100-125% of threshold",
                                                   "75-100% of threshold",
                                                   "50-75% of threshold",
                                                   "Less than 50% of threshold"))

pal_length_jur <- colorFactor(ordered = T,
                              palette = c('#6B0402','#EE3835','#FAAAA9',   '#A1CDF4' ,'#0679DF','#033663'),
                              levels=levels(length_jur$cat1))

popup_length_jur = paste0('<strong>',"Zone: ", '</strong>',length_jur$zone_id,"<br>",
                          '<strong>',"Jurisdiction: ", '</strong>',length_jur$COUNTY,"<br>",
                          '<strong>',"Avg Zone Trip Length: ", '</strong>',round(length_jur$avg_zone_trip_length,2),"<br>",
                          '<strong>',"Jurisdiction Avg Trip Length: ", '</strong>',round(length_jur$jur_average,2),"<br>",
                          '<strong>',"Jurisdiction Threshold (15% Below Regional Avg): ", '</strong>',format(length_jur$threshold_15_below,digits=3, big.mark=","),"<br>",
                          '<strong>',"Zone Percent of Threshold (15% Below Regional Avg): ", '</strong>',percent(length_jur$percent_threshold,digits=0),"<br>",
                          '<strong>',"Zone Category: ", '</strong>',length_jur$cat
)

res_data_sf$cat1 <- factor(res_data_sf$cat, levels=c("Over 150% of threshold",
                                                     "125-150% of threshold",
                                                     "100-125% of threshold",
                                                     "75-100% of threshold",
                                                     "50-75% of threshold",
                                                     "Less than 50% of threshold"))

pal <- colorFactor(ordered = T,
                   palette = c('#6B0402','#EE3835','#FAAAA9',   '#A1CDF4' ,'#0679DF','#033663'),
                   levels=levels(res_data_sf$cat1))

popup = paste0('<strong>',"Zone: ", '</strong>',res_data_sf$zone_id,"<br>",
               '<strong>',"Jurisdiction: ", '</strong>',res_data_sf$COUNTY,"<br>",
               '<strong>',"Zone VMT: ", '</strong>',format(res_data_sf$zone_vmt,digits=0, big.mark=","),"<br>",
               '<strong>',"Zone Residential Population: ", '</strong>',format(res_data_sf$zone_pop,digits=0, big.mark=","),"<br>",
               '<strong>',"Avg Zone VMT Per Resident: ", '</strong>',format(res_data_sf$zone_vmt_per_capita,digits=3, big.mark=","),"<br>",
               '<strong>',"Jurisdiction Avg VMT Per Capita: ", '</strong>',format(res_data_sf$jur_total_vmt_capita,digits=4, big.mark=","),"<br>",
               '<strong>',"Jurisdiction Threshold (15% Below Regional Avg): ", '</strong>',format(res_data_sf$sos,digits=3, big.mark=","),"<br>",
               '<strong>',"Zone Percent of Threshold (15% Below Regional Avg): ", '</strong>',percent(res_data_sf$percent_threshold,digits=0),"<br>",
               '<strong>',"Zone Category: ", '</strong>',res_data_sf$cat
)

#tc_shape <- geojson_read("https://opendata.arcgis.com/datasets/85a2e8e4bf994742a5855c1339517681_4.geojson", what="sp") %>%
 # st_as_sf(crs=4326)

tc_half<-st_read("VMT_Analysis.gdb","Town_center_Buffer_half_mile") %>% st_transform(crs=4326) %>%
  filter(!Name %in% c("NEVADA SOUTH STATELINE RESORT AREA","STATELINE/SKI RUN"))
tc_half$area <- st_area(tc_half)
tc_half <-tc_half %>%
  summarise(area = sum(area))

regional_half <-st_read("VMT_Analysis.gdb","Town_center_Buffer_half_mile") %>% st_transform(crs=4326) %>% filter(Name %in% c("NEVADA SOUTH STATELINE RESORT AREA","STATELINE/SKI RUN"))

regional_half$area <- st_area(regional_half)

regional_half <-regional_half %>%
  summarise(area = sum(area))

town_reg_half <- rbind(regional_half %>% select(-area), tc_half %>% select(-area))
town_reg_half$area <- st_area(town_reg_half)
town_reg_half <-town_reg_half %>%
  summarise(area = sum(area))

bike_buff<-st_read(".","bike_path_half_buffer") %>% st_transform(crs=4326)

walk_buff<-st_read(".","walk_quarter_buffer") %>% st_transform(crs=4326)

buffer<-st_union(walk_buff, tc_half)
buffer  <- st_difference(buffer, st_combine(regional_half)) %>% select(area) %>%
  rename(Shape=geometry)

center_bike_buffer<-rbind(town_reg_half, buffer)
center_bike_buffer$area1 <- st_area(center_bike_buffer)
center_bike_buffer <-center_bike_buffer %>%
  summarise(area1 = sum(area1))

#st_write(buffer_parcel_final, dsn=".","buffer_parcel_final", driver="ESRI Shapefile")

buffer_parcel_final<-st_read(dsn=".","buffer_parcel_final")

#a<-leaflet(buffer_parcel_final) %>% addTiles() %>% addPolygons(data=town_reg_half,  fill=F, color="red")%>% 
 # addPolygons(stroke=F, fillOpacity = .7, )

#library(htmlwidgets)
#saveWidget(a, file="tc_buffer_&_parcels.html")

#tc_half  <- st_difference(tc_half, st_combine(regional_half))

tc_walk<-st_read("VMT_Analysis.gdb","Town_center_Walkshed") %>% st_transform(crs=4326)
tc_walk$area <- st_area(tc_walk)
tc_walk <-tc_walk %>%
  summarise(area = sum(area))

transit_buffer<-sf::st_read(".","transit_buffer_2019") %>% st_as_sf() %>%
  sf::st_transform(crs=4326)
transit_buffer <- st_intersection(transit_buffer, boundary)



length_data_sf$cat1 <- factor(length_data_sf$cat, 
                              levels=c("under 4 mi",
                                       "4-6 mi",
                                       "6-8 mi",
                                       "8-10 mi",
                                       "10-12 mi",
                                       "above 12 mi"))



pal1 <- colorFactor(ordered = T,
                    palette = c('#033663','#0679DF',   '#A1CDF4','#FAAAA9','#EE3835','#6B0402'),
                    levels=levels(length_data_sf$cat1))

popup1 = paste0('<strong>',"Zone: ", '</strong>',length_data_sf$zone_id,"<br>",
                '<strong>',"Zone Avg Trip Length: ", '</strong>',format(length_data_sf$avg_zone_trip_length,digits=2, big.mark=",")
)

commercial <- trip_rates %>% filter(!use %in% c("Hotel","Motel","Timeshare")) %>% distinct(use) %>% pull()

dataset <- length_jur %>% select(zone_id, zone_no, avg_zone_trip_length,threshold_15_below ) %>%
                     left_join(res_data_sf %>% data.frame() %>% select(zone_id,zone_vmt_per_capita,  sos), by="zone_id")

dataset_full <- length_jur_full %>% select(zone_id, zone_no, avg_zone_trip_length,threshold_15_below ) %>%
  left_join(res_data_sf %>% data.frame() %>% select(zone_id,zone_vmt_per_capita,  sos), by="zone_id")

#### --------------------------- ####

ui <- dashboardPage(skin="black", 
  dashboardHeader(title="DRAFT - Project Impact Analysis Tool - DRAFT", titleWidth = 550),
  dashboardSidebar(width=300,
                   sidebarMenu(
                     menuItem("TRPA Analysis",
                              tabName = "trpa",icon = icon("chart-area")),
                     menuItem("SB 743 Analysis (El Dorado/CSLT)",
                              tabName = "ed_cslt",icon = icon("buffer")),
                     menuItem("SB 743 Analysis (Placer)",
                              tabName = "placer",icon = icon("braille")),
                     menuItem("User Guidelines",
                              tabName = "guide",icon = icon("info"))),
                   box(width=12,background="black",
                                 img(src='zoom_background_50th.jpg',  height = 250, width = 250),
                                 p(tags$br(),HTML("The tool provides initial screening for all project types and more detailed analysis for residential, tourist accommodation unit, and public service projects.  All non-screened commercial, recreation, and other projects will need to complete a more detailed transportation analysis.<br> <br> Follow the steps below to analyze your project. For detailed information on the PIA  framework, tool usage, and calculations select the User Guidelines tab. <br> <br>
                        1) Select your project type from the dropdown.<br>
                        2) Select your project location on the map. <br>
                        3) Enter the number of proposed units<br>
                        4) Select mitigation strategies if applicable "))
  )),
  dashboardBody(tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden;}" ,
  HTML("
      .skin-black .main-sidebar { background-color: #000000;}")),
  tabItems(
    tabItem(tabName = "trpa",
   fluidRow(
    column(width=2,
           fluidRow(box(textInput("proj_name","Proposed Project Name"),
             selectInput("proj_type", "Proposed Project Type",choices=c(sort(c("Mixed-Use","Residential","Unique Project Type" ,
                                                                      unique(trip_rates$use))))), 
            uiOutput("custom_type"),
            uiOutput("ui_project_type"),
            uiOutput("ui_land_use1"),
            uiOutput("mixed_units_1"),
            uiOutput("ui_low_income1"),
            uiOutput("ui_land_use2"),
            uiOutput("mixed_units_2"),
            uiOutput("ui_low_income2"),
            uiOutput("ui_land_use3"),
            uiOutput("mixed_units_3"),
            uiOutput("ui_low_income3"),
            uiOutput("custom_rate"),
            uiOutput("ui_low_income"),
            uiOutput("ui_redevelopment"),
            uiOutput("ui_redev_land_use"),
            uiOutput("ui_redev_units"),
            uiOutput("ui_mitigations"),
            downloadButton("report_button", "Export Project Info"),
            radioButtons('format', '', c('PDF', 'HTML', 'Word'),inline = TRUE),
            width=12))),
    column(width=10,
           fluidRow(
             box(title="Select Project Location",
                 leafletOutput("map", height=700) %>% withSpinner(), width=12)),
           uiOutput("ui_land_use_1"),
           uiOutput("ui_land_use_2"),
           uiOutput("ui_land_use_3"),
           uiOutput("ui_tot_mixed_use")
    )
    )),
   tabItem(tabName = "ed_cslt",
           fluidRow()),
   tabItem(tabName = "placer",
           fluidRow()),
   tabItem(tabName = "guide",
           box(width = 9, status = 'info', solidHeader = TRUE, title = "",
               uiOutput("guidelines"))
           )
   ))
)
server <- function(input, output,session) {
  showModal(modalDialog(title = "TRPA Project Impact Analysis Tool",
                        HTML("The tool provides initial screening for all project types and more detailed analysis for residential, tourist accommodation unit, and public service projects.  All non-screened commercial, recreation, and other projects will need to complete a more detailed transportation analysis.<br> <br> Follow the steps below to analyze your project.<br><br>
                        1) Select your project type from the dropdown.<br>
                        2) Select your project location on the map. <br>
                        3) Enter the number of proposed units<br>
                        4) Select mitigation strategies if applicable"),
                        footer = modalButton("I Understand")))
  output$guidelines <- renderUI({
    PDFfile="PIA_User_Guidelines.pdf"
    tags$iframe(
      src=PDFfile,
      width="100%",
      height="800px")
  })
  datasetInput <- reactive({
         dataset
      })
  datasetInputFull <- reactive({
    dataset_full
  })
 output$export_button<- renderUI({
   req(input$map_shape_click,input$proj_type)
    actionButton("export_button", "Export Project Info")
  })
 observeEvent(input$export_button, {
   showModal(modalDialog(title = "See the attached PDF for your project information",
                         "Please contact TRPA at trpa@trpa.gov for additonal questions and comments about your project's approval process."))
   })
  output$ui_land_use1 <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use1", "1) Proposed Project Type",choices=c(sort(c("Residential","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$ui_land_use2 <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use2", "2) Proposed Project Type",choices=c(sort(c("Residential","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$ui_land_use3 <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use3", "3) Proposed Project Type",choices=c(sort(c("Residential","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$mixed_units_1 <-renderUI({
  # req(input$land_use2)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use1=="Residential"){
      numericInput("res_units1", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus1", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use1 =="Unique Project Type"){
      numericInput("input_custom_unit1", "# of Proposed Units",
                   value = 0)
    }else if (input$land_use1 =="Marina"){
      numericInput("berths1", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use1 %in% c('Developed Campground/RV Park')){
      numericInput("sites1", "# of Sites",
                   value = 0)
    }else if (input$land_use1 %in% c('Public Park')){
      numericInput("acres1", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use1 %in% c("Golf Course")){
      numericInput("holes1", "# of Holes",
                   value = 0)
    }else if (input$land_use1 %in% c("Bowling Alley")){
      numericInput("lanes1", "# of Lanes",
                   value = 0)
    }else if (input$land_use1 %in% c("Movie Theater (traditional)")){
      numericInput("screens1", "# of Screens",
                   value = 0)
    }else if (input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students1", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf1", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$mixed_units_2 <-renderUI({
    # req(input$map_shape_click)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use2=="Residential"){
      numericInput("res_units2", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus2", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use2 =="Unique Project Type"){
      numericInput("input_custom_unit2", "# of Prposed Units",
                   value = 0)
    }else if (input$land_use2 =="Marina"){
      numericInput("berths2", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use2 %in% c('Developed Campground/RV Park')){
      numericInput("sites2", "# of Sites",
                   value = 0)
    }else if (input$land_use2 %in% c('Public Park')){
      numericInput("acres2", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use2 %in% c("Golf Course")){
      numericInput("holes2", "# of Holes",
                   value = 0)
    }else if (input$land_use2 %in% c("Bowling Alley")){
      numericInput("lanes2", "# of Lanes",
                   value = 0)
    }else if (input$land_use2 %in% c("Movie Theater (traditional)")){
      numericInput("screens2", "# of Screens",
                   value = 0)
    }else if (input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students2", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf2", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$mixed_units_3 <-renderUI({
    # req(input$map_shape_click)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use3=="Residential"){
      numericInput("res_units3", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use3 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus3", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use3 =="Unique Project Type"){
      numericInput("input_custom_unit3", "# of Prposed Units",
                   value = 0)
    }else if (input$land_use3 =="Marina"){
      numericInput("berths3", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use3 %in% c('Developed Campground/RV Park')){
      numericInput("sites3", "# of Sites",
                   value = 0)
    }else if (input$land_use3 %in% c('Public Park')){
      numericInput("acres3", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use3 %in% c("Golf Course")){
      numericInput("holes3", "# of Holes",
                   value = 0)
    }else if (input$land_use3 %in% c("Bowling Alley")){
      numericInput("lanes3", "# of Lanes",
                   value = 0)
    }else if (input$land_use3 %in% c("Movie Theater (traditional)")){
      numericInput("screens3", "# of Screens",
                   value = 0)
    }else if (input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students3", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf3", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$ui_project_type <-renderUI({
   # req(input$map_shape_click)
    if (is.null(input$proj_type) | input$proj_type == "Mixed-Use"){
      return()
    }else if (input$proj_type=="Residential"){
      numericInput("res_units", "# of Proposed Units",
                   value = 0)
    } else if (input$proj_type %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus", "# of Proposed TAUS",
                   value = 0)
    } else if (input$proj_type =="Unique Project Type"){
      numericInput("input_custom_unit", "# of Prposed Units",
                   value = 0)
    }else if (input$proj_type =="Marina"){
      numericInput("berths", "# of Proposed Berths",
                   value = 0)
    }else if (input$proj_type %in% c('Public Park')){
      numericInput("acres", "# of Proposed Acres",
                   value = 0)
    }else if (input$proj_type %in% c("Golf Course")){
      numericInput("holes", "# of Holes",
                   value = 0)
    }else if (input$proj_type %in% c("Bowling Alley")){
      numericInput("lanes", "# of Lanes",
                   value = 0)
    }else if (input$proj_type %in% c("Movie Theater (traditional)")){
      numericInput("screens", "# of Screens",
                   value = 0)
    }else if (input$proj_type %in% c('Developed Campground/RV Park')){
      numericInput("sites", "# of Sites",
                   value = 0)
    }else if (input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$ui_redev_land_use <-renderUI({
    # req(input$map_shape_click)
    if (input$redevelopment=="No"){
      return()
    }else if (input$redevelopment=="Yes"){
      selectInput("current_use", "Existing Land Use",choices=c(sort(c("Residential","Unique Project Type" ,
                                                                      unique(trip_rates$use)))))
    } else {
      return()
    }
  })
  output$ui_redev_units <-renderUI({
    # req(input$map_shape_click)
    if (input$redevelopment=="No"){
      return()
    }else if (input$redevelopment=="Yes"){
      if (input$current_use=="Residential"){
        numericInput("res_units_redev", "Existing Residential Units",value = 0)
      } else if(input$current_use %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')) {
        numericInput("ksf_redev", "Existing Square Footage",value = 0)
      }else if (input$current_use %in% c("Hotel","Motel","Timeshare")){
        numericInput("taus_redev", "Existing TAUS",value = 0)
      } else if (input$current_use =="Unique Project Type"){
        numericInput("input_custom_unit_redev", "Existing Units",value = 0)
      }else if (input$current_use =="Marina"){
        numericInput("berths_redev", "Existing Berths",value = 0)
      }else if (input$current_use %in% c('Public Park',)){
        numericInput("acres_redev", "Existing Acres",value = 0)
      }else if (input$current_use %in% c("Golf Course")){
        numericInput("holes_redev", "# of Holes",value = 0)
      }else if (input$current_use %in% c("Bowling Alley")){
        numericInput("lanes_redev", "# of Lanes",value = 0)
      }else if (input$current_use %in% c("Movie Theater (traditional)")){
        numericInput("screens_redev", "# of Screens",value = 0)
      }else if (input$current_use %in% c('Developed Campground/RV Park')){
        numericInput("sites_redev", "# of Sites",value = 0)
      }else if (input$current_use %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
        numericInput("students_redev", "# of Students",value = 0)
        }
    }
  })
  output$custom_rate <-renderUI({
   # req(input$map_shape_click)
    if (input$proj_type == "Unique Project Type"){
      numericInput("input_custom_rate", "Trip Rate",
                   value = 0)
    } else {
      return()
    }
  })
  output$custom_type <-renderUI({
   # req(input$map_shape_click)
    if (input$proj_type == "Unique Project Type"){
      textInput("custom_proj_type","Enter Project Type")
    } else {
      return()
    }
  })
  output$ui_mitigations <-renderUI({
    req(input$map_shape_click, input$proj_type)
    checkboxGroupInput(inputId="mitigations", label="Select Mitigations", selected="None",
                       choices=c("Ride Sharing Program"=.01,
                                 "Telecommuting/Alternative Work Schedules"=.0007,
                                 "Trip Reduction Marketing"=.008,
                                 "Traffic Calming"=.0025, 
                                 "Workplace Parking Pricing"=.001, 
                                 "Employee Sponsored Vanpool/Shuttle"=.005 ,
                                 "Employee Shuttle"=.005 ,
                                 "Vanpool Incentives"=.003 ,
                                 "Targeted Behavior Interventions"=.008))
  })
  output$ui_redevelopment <-renderUI({
    req(input$map_shape_click, input$proj_type)
    radioButtons(inputId="redevelopment", label="Redevelopment Project", selected="No",
                       choices=c("Yes","No"))
  })
  output$ui_low_income <-renderUI({
   # req(input$proj_type)
    if(input$proj_type!="Residential"){return()
    } else{
      checkboxInput("low_income_screen", "Low Income Units")
    }
  })
  output$ui_low_income1 <-renderUI({
     #req(input$land_use1)
    if(input$land_use1!="Residential"){return()
    }else if(input$proj_type!="Mixed-Use"){return()
    } else{
      checkboxInput("low_income_screen1", "Low Income Units")
    }
  })
  output$ui_low_income2 <-renderUI({
    #req(input$land_use1)
    if(input$land_use2!="Residential"){return()
    }else if(input$proj_type!="Mixed-Use"){return()
    } else{
      checkboxInput("low_income_screen2", "Low Income Units")
    }
  })
  output$ui_low_income3 <-renderUI({
    #req(input$land_use1)
    if(input$land_use3!="Residential"){return()
    }else if(input$proj_type!="Mixed-Use"){return()
    } else{
      checkboxInput("low_income_screen3", "Low Income Units")
    }
  })
  map_event <-reactive({ 
   # req(input$map_shape_click)
    input$map_shape_click$id
  })
  output$map <- renderLeaflet({
    datasetInput() %>%
      leaflet() %>% addProviderTiles("OpenStreetMap.HOT") %>%
      addPolygons(data=boundary,color="black",options = pathOptions(interactive = FALSE), fill=F) %>%
      addPolygons(data=town_reg_half , options = pathOptions(interactive = FALSE), group="Town & Regional Center 1/2 Mile Buffer", fill=F, opacity=1) %>%
      addPolygons(data=jur,options = pathOptions(interactive = FALSE), group="Jurisdictions/Subregions", fillColor = "white",fillOpacity = .5, opacity=1, color="#C6C82C") %>%
      addStaticLabels(data=jur, label=jur$name,group="Jurisdictions/Subregions" ,
                      style = list("color" = "black", 
                                   "font-weight" = "bold",
                                   "font-size" = "12px")) %>%
      addPolygons(data=transit_buffer,options = pathOptions(interactive = FALSE), group="Bonus Unit Eligible Areas", fill=F, color="purple", opacity=1) %>%
      addPolygons(data=res_data_sf,options = pathOptions(interactive = FALSE),weight=2, fillColor=~pal(cat1), color="white", fillOpacity = 0.6,
                  label = lapply(popup, HTML), group="Zone Average Residential VMT",
                  highlightOptions = highlightOptions(stroke = 4, weight = 4, color="black", opacity=1)) %>% 
      addLegend(data=res_data_sf, pal = pal, values = ~cat1, opacity = 1, title="Residential VMT", position="bottomright",group="Zone Average Residential VMT") %>% 
      addPolygons(data=length_jur,options = pathOptions(interactive = FALSE),weight=2, fillColor=~pal_length_jur(cat1), color="white", fillOpacity = 0.6,
                  label = lapply(popup_length_jur, HTML), group="Zone Average Trip Lengths",
                  highlightOptions = highlightOptions(stroke = 4, weight = 4, color="black", opacity=1)) %>%
      addLegend(data=length_jur, pal = pal_length_jur, values = ~cat1, opacity = 1, title="Trip Lengths", position="bottomright", group="Zone Average Trip Lengths") %>%
      #addGlPolylines(data = pm, color="black",group="Parcels") %>%
      addPolygons(layerId=~zone_id,opacity = 0, fillOpacity = 0) %>%
      addLayersControl(
        overlayGroups = c("Zone Average Residential VMT","Zone Average Trip Lengths","Town & Regional Center 1/2 Mile Buffer","Bonus Unit Eligible Areas","Jurisdictions/Subregions","Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% hideGroup(c("Zone Average Residential VMT","Zone Average Trip Lengths","Town & Regional Center 1/2 Mile Buffer","Bonus Unit Eligible Areas","Jurisdictions/Subregions","Parcels"))
  })
  observeEvent(input$map_click, {
    click <- input$map_click
    text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    proxy <- leafletProxy("map")
    ## This displays the pin drop circle
    proxy %>% 
      clearGroup("new_point") %>%
      #clearMarkers(layerId=input$mymap_click$id) %>%
      #addPopups(click$lng, click$lat) %>%
      addMarkers(click$lng, click$lat, group = "new_point")
  })
  proj_loc<-reactive({
    click <- input$map_click
    points <- data.frame(lat=as.numeric(click$lat), lon=as.numeric(click$lng))
    points %>% st_as_sf(crs=4326, coords=c("lon","lat"))
  })
  in_out_region<-reactive({
    st_intersection(proj_loc(), st_buffer(boundary, 0))
  })
  town_reg_buffer<-reactive({
    st_intersection(proj_loc(), st_buffer(buffer_parcel_final, 0))
  })
  transit<-reactive({
    st_intersection(proj_loc(), st_buffer(transit_buffer, 0))
  })
tot_vmt_react<-reactive({
  if(input$proj_type != "Mixed-Use"){
    if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         ((input$ksf/1000) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    } else if(input$proj_type %in% c('Public Park')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c('Marina')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type %in% c("Hotel","Motel","Timeshare")){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    } else if(input$proj_type %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }  else if(input$proj_type=="Residential"  & input$low_income_screen==TRUE){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units) * 0.9) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$proj_type=="Residential"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    } else if (input$proj_type=="Unique Project Type"){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit * input$input_custom_rate)) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    } 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use1 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf1/1000) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use1 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations))) 
    }else if(input$land_use1 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use1 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use1 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use1 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use1 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }  else if(input$land_use1=="Residential"  & input$low_income_screen1==TRUE){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) * 0.9) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use1=="Residential"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) * (1- sum(as.numeric(input$mitigations)))
    } else if (input$land_use1=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit1 * input$input_custom_rate1)) * (1- sum(as.numeric(input$mitigations)))
    } 
  }
})
tot_vmt_react2<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf2/1000) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use2 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use2 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use2 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use2 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use2 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use2 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }  else if(input$land_use2=="Residential"  & input$low_income_screen2==TRUE){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) * 0.9) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use2=="Residential"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) * (1- sum(as.numeric(input$mitigations)))
    } else if (input$land_use2=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit2 * input$input_custom_rate2)) * (1- sum(as.numeric(input$mitigations)))
    } 
  }
})
tot_vmt_react3<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use3 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf3/1000) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use3 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use3 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use3 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use3 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use3 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$land_use3 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$land_use3 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }   else if(input$land_use3=="Residential"  & input$low_income_screen3==TRUE){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) * 0.9) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$land_use3=="Residential"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) * (1- sum(as.numeric(input$mitigations)))
    } else if (input$land_use3=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit3 * input$input_custom_rate3)) * (1- sum(as.numeric(input$mitigations)))
    } 
  }
})
display_vmt_redev <- reactive({
  if(input$redevelopment == "No"){
    0 } else{tot_vmt_react_redev() }
})
tot_vmt_react_redev<-reactive({
  if(input$proj_type == "Mixed-Use"){
    return() 
  } else if(input$proj_type != "Mixed-Use"){
    if(input$current_use %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf_redev/1000) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$current_use %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$current_use %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$current_use %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$current_use %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$current_use %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$current_use %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }else if(input$current_use %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus_redev) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))
    } else if(input$current_use %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites_redev) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull())) * (1- sum(as.numeric(input$mitigations)))) - display_vmt_redev()
    }   else if(input$current_use=="Residential"  & input$low_income_screen3==TRUE){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units_redev) * 0.9) * (1- sum(as.numeric(input$mitigations)))
    }else if(input$current_use=="Residential"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units_redev) * (1- sum(as.numeric(input$mitigations)))
    } else if (input$current_use=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit_redev * input$input_custom_rate_redev)) * (1- sum(as.numeric(input$mitigations)))
    } 
  }
})
display_vmt <- reactive({
  if(tot_vmt_react() < 0){
    0
  } else{tot_vmt_react()}
})
display_vmt2 <- reactive({
  tot_vmt_react2()
})
display_vmt3 <- reactive({
  tot_vmt_react3()
})
proj_sos_react<-reactive({
  if(input$proj_type != "Mixed-Use"){
 # req(input$map_shape_click)
    if (screened() =="Yes") {
      0
    } else if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales', 'Health and Fitness Club', 'Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
  } else  if(input$proj_type %in% c('Public Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$acres) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
  }else  if(input$proj_type %in% c('Marina')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
  }else if(input$proj_type %in% c('Golf Course')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$holes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c("Bowling Alley")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$lanes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c("Movie Theater (traditional)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$screens * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$students * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c("Hotel","Motel","Timeshare")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$taus * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c('Developed Campground/RV Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$sites * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type=="Residential"){
  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units,0)
    }  else if (input$proj_type=="Unique Project Type") {
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
        (input$input_custom_unit * input$input_custom_rate)
    } 
  } else if(input$proj_type == "Mixed-Use"){
    if (screened() =="Yes") {
      0
    }else if(input$land_use1 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales', 'Health and Fitness Club', 'Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              ((input$ksf1/1000) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))),0)
    } else  if(input$land_use1 %in% c('Public Park')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              ((input$acres1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))),0)
    }else  if(input$land_use1 %in% c('Marina')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              ((input$berths1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))),0)
    }else if(input$land_use1 %in% c('Golf Course')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$holes1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c("Bowling Alley")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$lanes1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c("Movie Theater (traditional)")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$screens1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$students1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$taus1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c('Developed Campground/RV Park')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$sites1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1=="Residential"){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units1,0)
    }  else if (input$land_use1=="Unique Project Type") {
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
        (input$input_custom_unit1 * input$input_custom_rate1)
    }
}
})
proj_sos_react2<-reactive({
  # req(input$map_shape_click)
  if (screened2() =="Yes") {
    0
  } else if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales', 'Health and Fitness Club', 'Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$ksf2/1000) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))),0)
  } else  if(input$land_use2 %in% c('Public Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$acres2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))),0)
  }else  if(input$land_use2 %in% c('Marina')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$berths2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))),0)
  }else if(input$land_use2 %in% c('Golf Course')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$holes2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use2 %in% c("Bowling Alley")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$lanes2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use2 %in% c("Movie Theater (traditional)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$screens2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$students2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$taus2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use2 %in% c('Developed Campground/RV Park')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$sites2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use2=="Residential"){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units2,0)
    }  else if (input$land_use2=="Unique Project Type") {
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
        (input$input_custom_unit2 * input$input_custom_rate2)
    }
})
proj_sos_react3<-reactive({
  # req(input$map_shape_click)
   if (screened3() =="Yes") {
    0
   } else if(input$land_use3 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales', 'Health and Fitness Club', 'Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             ((input$ksf3/1000) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))),0)
   } else  if(input$land_use3 %in% c('Public Park')){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             ((input$acres3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))),0)
   }else  if(input$land_use3 %in% c('Marina')){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             ((input$berths3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))),0)
   }else if(input$land_use3 %in% c('Golf Course')){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$holes3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3 %in% c("Bowling Alley")){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$lanes3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3 %in% c("Movie Theater (traditional)")){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$screens3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$students3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3 %in% c("Hotel","Motel","Timeshare")){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$taus3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3 %in% c('Developed Campground/RV Park')){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
             (input$sites3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
   }else if(input$land_use3=="Residential"){
     round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units3,0)
   }  else if (input$land_use3=="Unique Project Type") {
     (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
       (input$input_custom_unit3 * input$input_custom_rate3)
   }
})
output$tot_vmt <-renderValueBox({
 # req(input$map_shape_click)
  valueBox(subtitle="Project Total VMT", width=6, color="navy",
    value= tags$p(comma(display_vmt(), 0),
                  style = "font-size: 75%;"
    )
  )
})
output$tot_vmt2 <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Project Total VMT", width=6, color="navy",
           value= tags$p(comma(display_vmt2(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$tot_vmt3 <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Project Total VMT", width=6, color="navy",
           value= tags$p(comma(display_vmt3(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$tot_vmt_redev <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Project Total VMTff", width=6, color="navy",
           value= tags$p(comma(tot_vmt_react_redev(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$proj_sos <-renderValueBox({
#  req(input$map_shape_click)
  valueBox(subtitle="Project SOS VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$proj_sos2 <-renderValueBox({
  #  req(input$map_shape_click)
  valueBox(subtitle="Project SOS VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react2(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$proj_sos3 <-renderValueBox({
  #  req(input$map_shape_click)
  valueBox(subtitle="Project SOS VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react3(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$screened <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened(),
           style = "font-size: 75%;")
  )
})
output$screened2 <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened2(),
                        style = "font-size: 75%;")
  )
})
output$screened3 <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened3(),
                        style = "font-size: 75%;")
  )
})
mit_needed <- reactive({
  if(screened() == "Yes" | tot_vmt_react() - proj_sos_react() <1){
    0
  }else{
tot_vmt_react() - proj_sos_react()
  }
})
mit_needed2 <- reactive({
  if(screened2() == "Yes" | tot_vmt_react2() - proj_sos_react2() <1){
    0
  }else{
    tot_vmt_react2() - proj_sos_react2()
  }
})
mit_needed3 <- reactive({
  if(screened3() == "Yes" | tot_vmt_react3() - proj_sos_react3() <1){
    0
  }else{
    tot_vmt_react3() - proj_sos_react3()
  }
})
output$mitigate <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed(), 0),
           style = "font-size: 75%;")
  )
})
output$mitigate2 <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed2(), 0),
             style = "font-size: 75%;")
  )
})
output$mitigate3 <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed3(), 0),
             style = "font-size: 75%;")
  )
})
mob_fee_calc <- reactive({
  if(input$proj_type %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {round((display_vmt()*.9) * 100,0)
    }else if(!input$proj_type %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {round((display_vmt()*.1) * 100,0)
    }else if(land_use1 %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {round((display_vmt()*.9) * 100,0)
    } else if(!input$land_use1 %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {round((display_vmt()*.1) * 100,0)
    }
})
mob_fee_calc2 <- reactive({
  if(input$land_use2 %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {round((display_vmt2()*.9) * 100,0)
  }else{
    round((display_vmt2()*.1) * 100,0)
  }
})
mob_fee_calc3 <- reactive({
  if(input$land_use3 %in% c("Residential","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {round((display_vmt3()*.9) * 100,0)
  }else{
    round((display_vmt3()*.1) * 100,0)
  }
})
output$mobility_fee <-renderValueBox({
  #req(input$map_shape_click)
  valueBox(subtitle="Mobility Fee ($100/VMT)", width=6, color="red",
           value= tags$p(
             dollar(mob_fee_calc()),
             style = "font-size: 70%;"
           )
  )
})
  output$mobility_fee2 <-renderValueBox({
    #req(input$map_shape_click)
    valueBox(subtitle="Mobility Fee ($100/VMT)", width=6, color="red",
             value= tags$p(
               dollar(mob_fee_calc2()),
               style = "font-size: 70%;"
             )
    )
})
  output$mobility_fee3 <-renderValueBox({
    #req(input$map_shape_click)
    valueBox(subtitle="Mobility Fee ($100/VMT)", width=6, color="red",
             value= tags$p(
               dollar(mob_fee_calc3()),
               style = "font-size: 70%;"
             )
    )
  })
  output$ui_tot_mixed_use <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      fluidRow(  
        box(width=12,title="Mixed-Use Project Totals" ,
            valueBox(subtitle="Total Mixed-Use VMT", width=3, color="red",
               value= tags$p(
                 round(tot_mixed_use_vmt(),0),
                 style = "font-size: 70%;"
               )
      ), valueBox(subtitle="Total Mixed-Use Fee", width=3, color="red",
                  value= tags$p(
                    dollar(round(tot_mixed_use_fee(),0)),
                    style = "font-size: 70%;"
                  )
      )
      ))
    } else{return(NULL)}
  })
  tot_mixed_use_vmt<-reactive({
    if (
      (input$land_use1 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail")) |
      (input$land_use1 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use3 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail"))
    ){ (tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()) * .9
    } else{tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()}
  })
  tot_mixed_use_fee<-reactive({
    if (
      (input$land_use1 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail")) |
      (input$land_use1 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use3 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail"))
    ){ ((tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()) * .9) * 100
    } else{(tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()) * 100}
  })
  output$ui_land_use_1  <-renderUI({
    req(input$map_shape_click, input$proj_type)
    if(nrow(in_out_region()) == 0) {
    return()
  } else{
  fluidRow(  
    box(width=12,title="Land Use Type #1" ,
         column(width=2,valueBoxOutput("screened", width=12)),
        column(width=2,valueBoxOutput("tot_vmt", width=12)),
        column(width=2,valueBoxOutput("proj_sos", width=12)),
        column(width=2,valueBoxOutput("mitigate", width=12)),
        column(width=2,valueBoxOutput("mobility_fee", width=12))),
   # column(width=2,uiOutput("export_button", width=12))
    )
  }
   # }
  })
  output$ui_land_use_2  <-renderUI({
    if(display_vmt2() ==0 | nrow(in_out_region()) == 0) {
      return()
    } else{
    fluidRow(  
      box(width=12,title="Land Use Type #2" ,
          column(width=2,valueBoxOutput("screened2", width=12)),
          column(width=2,valueBoxOutput("tot_vmt2", width=12)),
          column(width=2,valueBoxOutput("proj_sos2", width=12)),
          column(width=2,valueBoxOutput("mitigate2", width=12)),
          column(width=2,valueBoxOutput("mobility_fee2", width=12))))
    }
  })
output$ui_land_use_3  <-renderUI({
  if(display_vmt3() ==0 | nrow(in_out_region()) == 0) {
    return()
  } else{
  fluidRow(  
  box(width=12,title="Land Use Type #3" ,
      column(width=2,valueBoxOutput("screened3", width=12)),
    column(width=2,valueBoxOutput("tot_vmt3", width=12)),
    column(width=2,valueBoxOutput("proj_sos3", width=12)),
    column(width=2,valueBoxOutput("mitigate3", width=12)),
    column(width=2,valueBoxOutput("mobility_fee3", width=12))))
  }
  })
observe({
  input$input$proj_type
  updateCheckboxInput(session, inputId="low_income_screen", value= FALSE)
})
observe({
  input$map_click
  updateCheckboxInput(session, inputId="low_income_screen", value= FALSE)
})
screened <- reactive({
  if(input$proj_type == "Mixed-Use"){
    if (tot_vmt_react() <= 715)
    {print("Yes")
    }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) > 0)
    {print("Yes")
    }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) == 0)
    {print("No")
    }else if (tot_vmt_react() > 1300 & input$land_use1 != "Residential")
    {print("No")
    }else if (tot_vmt_react() > 1300 & is.null(input$low_income_screen1))
    {print("No")
    }else if (tot_vmt_react() > 1300 & input$low_income_screen1 ==FALSE )
    {print("No")
    }else if (input$low_income_screen1 == TRUE & nrow(transit()) > 0)
    {print("Yes")
    }else if (input$low_income_screen1 == TRUE & nrow(transit()) == 0)
    {print("No")
    }
  } else{
  if (tot_vmt_react() <= 715)
  {print("Yes")
  }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) == 0)
  {print("No")
  }else if (tot_vmt_react() > 1300 & input$proj_type != "Residential")
  {print("No")
  }else if (tot_vmt_react() > 1300 & is.null(input$low_income_screen))
  {print("No")
  }else if (tot_vmt_react() > 1300 & input$low_income_screen ==FALSE )
  {print("No")
  }else if (input$low_income_screen == TRUE & nrow(transit()) > 0)
  {print("Yes")
  }else if (input$low_income_screen == TRUE & nrow(transit()) == 0)
  {print("No")
  }
  }
})
screened2 <- reactive({
  if (tot_vmt_react2() <= 715)
  {print("Yes")
  }else if (between(tot_vmt_react2(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (between(tot_vmt_react2(),715,1300) & nrow(town_reg_buffer()) == 0)
  {print("No")
  }else if (tot_vmt_react2() > 1300 & input$land_use2 != "Residential")
  {print("No")
  }else if (tot_vmt_react2() > 1300 & is.null(input$low_income_screen2))
  {print("No")
  }else if (tot_vmt_react2() > 1300 & input$low_income_screen2 ==FALSE )
  {print("No")
  }else if (input$low_income_screen2 == TRUE & nrow(transit()) > 0)
  {print("Yes")
  }else if (input$low_income_screen2 == TRUE & nrow(transit()) == 0)
  {print("No")
  }
})
screened3 <- reactive({
  if (tot_vmt_react3() <= 715)
  {print("Yes")
  }else if (between(tot_vmt_react3(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (between(tot_vmt_react3(),715,1300) & nrow(town_reg_buffer()) == 0)
  {print("No")
  }else if (tot_vmt_react3() > 1300 & input$land_use3 != "Residential")
  {print("No")
  }else if (tot_vmt_react3() > 1300 & is.null(input$low_income_screen3))
  {print("No")
  }else if (tot_vmt_react3() > 1300 & input$low_income_screen3 ==FALSE )
  {print("No")
  }else if (input$low_income_screen3 == TRUE & nrow(transit()) > 0)
  {print("Yes")
  }else if (input$low_income_screen3 == TRUE & nrow(transit()) == 0)
  {print("No")
  }
})
output$report_button <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  content = function(file) {
    src <- normalizePath('report.Rmd')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
   # )
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()),
      params=list(output1= screened(),
                  output2= comma(display_vmt(), 0),
                  output3= comma(proj_sos_react(),0),
                  output4= comma(mit_needed(),0),
                  output5= dollar(mob_fee_calc()),
                  output6= input$proj_name,
                  output7= input$proj_type,
      output8= if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical – Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',   'Developed Campground/RV Park',  'Bowling Alley', 'Movie Theater (traditional)', 'Health and Fitness Club', 'Recreational Community Center', 'University/College', 'High School', 'Middle School/Junior High School', 'Elementary School', 'Private School (K-12)', 'Church', 'Daycare Center', 'Library', 'Hospital')){
        paste(comma(input$ksf,0), "Square Feet" ,sep=" ")
        } else if(input$proj_type == "Residential"){
          paste(comma(input$res_units,0), "Residential Units", sep=" ")
        },
      output9= paste(input$mitigations, collapse=", "),
      output10= if(nrow(town_reg_buffer()) > 0){
        "Yes"
      } else {
        "No"
      },
      output11= if(nrow(transit()) > 0){
        "Yes"
      } else {"No"}#,
     # output12= if(input$low_income_screen == TRUE){
       # "Yes"
     # }else if(input$low_income_screen == FALSE ){
      #  "No"
    #} else if(is.null(input$low_income_screen)) {
      #  "Not Applicable"
     # }
  )
    )
    file.rename(out, file)
  }
)
}
shinyApp(ui, server)

pm<-st_read(dsn="H:\\model\\project_level_analysis", "parcel_master_5_25_21") %>%
  st_transform(crs=4326) %>%
  st_cast("MULTILINESTRING")%>%
  st_cast("LINESTRING")



