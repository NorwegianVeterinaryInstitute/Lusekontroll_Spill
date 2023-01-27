
# Packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(sf)
library(lubridate)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(DT)
library(shinyWidgets)


header <- dashboardHeader(title = "Lusestrategispill")


# Design sidepanel --------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Info om spillet", tabName = "info", icon = icon("info")),
    menuItem("Oppdrettsmiljø", tabName = "valg", icon = icon("fish")),
    menuItemOutput("nyfane1"),
    menuItemOutput("nyfane2")
  )
)


# Design main panel -------------------------------------------------------

## Design Info tab ---------------------------------------------------------

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    tabItem("info",
            h1("Lusestrategispill"),
            p("Lusespillet simulerer lakselussituasjonen i et oppdrettsanlegg gjennom en produksjonssyklus på 18 måneder."),
            br(),
            p("Du velger område og måned for produksjonsstart, så velges en tilfeldig lokalitet med tilhørende ytre miljøforhold. De ytre miljøforholdene som påvirker luseutviklingen er temperatur og smittepress. Antall laks i anlegget er 800 tusen, som fordeles ut over fire merder."),
            br(),
            p("Før du starter produksjonen, velger du om du vil bruke luseskjørt eller sette ut rensefisk. LUSESKJØRT stopper halvparten av luselarvene som kommer fra andre anlegg de første seks månedene. Antallet RENSEFISK som settes ut er 5 % av antallet laks, men antallet minker utover i produksjonssyklusen – og spesielt raskt hvis du bruker ikke-medikamentell lusebehandling. Rensefisk kan også tilsettes seinere, men ikke flere enn to ganger á 5 % totalt. Hver rensefisk kan spise rundt 0.1 lus per dag."),
            br(),
            p("For hver uke teller du 20 laks i hver merd. Du velger så om du skal behandle, og eventuelt hvilken behandling du skal bruke."),
            br(),
            p("En FÔRBEHANDLING virker på alle lusestadiene på fisken i 60 dager. Virkningen varierer mellom behandlinger. I gjennomsnitt fører behandlingene til en ekstra daglig dødelighet for lusa på 2 %, eller totalt 35 % for 60 dager. Fôrbehandlinger er kun tillatt én gang per merd per generasjon."),
            br(),
            p("Annen MEDIKAMENTELL behandling virker på voksne hunnlus og andre bevegelige lus i én dag. Virkningen varierer mellom behandlinger. I gjennomsnitt dør 74 % av lusa. "),
            br(),
            p("IKKE-MEDIKAMENTELL behandling virker på alle lusestadier i én dag. Virkningen er varierer mellom behandlinger. I gjennomsnitt dør 80 % av lusa. Ikke-medikamentelle behandlinger kan ikke gjøres før fisken er 1 kg."),
            br(),
            p("Hver lusebehandling fører også til en ekstra dødelighet for laksen på rundt 1 % (Walde mfl. 2021, https://doi.org/10.1111/jfd.13348)."),
            br(),
            p("Lusedynamikken er basert på en statistisk modell tilpasset observasjoner av lusetall og lusebehandlinger i virkelige anlegg (en videreutvikling av Aldrin mfl 2017). Videreutviklingen av modellen og utviklingen av dette spillet er finansiert av Fiskeri- og havbruksnæringens forskningsfinansiering:"),
            tags$a(href="https://www.fhf.no/prosjekter/prosjektbasen/901650/", "LuseKontroll: Statistisk modellering av kontrollstrategier for lakselus"),
            hr(),
            p(strong("Referanse:")),
            p("Aldrin M, Huseby RB, Stien A, Grøntvedt RN, Viljugrein H, Jansen PA (2017) A stage-structured Bayesian hierarchical model for salmon lice populations at individual salmon farms - Estimated from multiple farm data sets. Ecol Mod 359:333-348"),
            tags$a(href="https://doi.org/10.1016/j.ecolmodel.2017.05.019", "Link til artikkel"),
            h2("Poengberegning"),
            p(HTML(paste("1) Du får 400 poeng per uke",
                   "2)	Poengtrekkene avhenger av produksjonsområde, slik at det skal bli omtrent like vanskelig i alle områder. Poengtrekkene nedenfor gjelder for området det ifølge modellen er vanskeligst å holde god lusekontroll (PO3).",
                   "3) Behandling gir poengtrekk, 25 poeng trekk for fôrbehandling, og 50 poeng trekk for annen behandling",
                   "4) Du trekkes 350 poeng for å overskride lusegrensa, pluss ekstra trekk for størrelsen på overskridelsen",
                   "5) Du trekkes 0.1 poeng for hver laks som dør av lusebehandlinger og 0.1 poeng for hver rensefisk tilsatt",
                   "6) Poengberegning er under utvikling, og kan bli mer avansert etterhvert", 
                   sep = "<br/>"
                        )
                   )
            )),


## Design Oppdrettsmiljø tab -----------------------------------------------

    tabItem("valg",
            box(width = 5, height = 600,
                textInput("Navn", "Navn på deg som spiller", value = "Anonym"),
                selectInput("PO", "Velg produksjonsområde",
                            choices = c("Produksjonsområde 2" = "PO2",
                                        "Produksjonsområde 3" = "PO3",
                                        "Produksjonsområde 4" = "PO4",
                                        "Produksjonsområde 5" = "PO5",
                                        "Produksjonsområde 6" = "PO6",
                                        "Produksjonsområde 7" = "PO7",
                                        "Produksjonsområde 8" = "PO8",
                                        "Produksjonsområde 9" = "PO9",
                                        "Produksjonsområde 10" = "PO10",
                                        "Produksjonsområde 11" = "PO11",
                                        "Produksjonsområde 12" = "PO12")),
                selectInput("StartTime", "Velg oppstartsmåned",
                            choices = c("April" = 4,
                                        "Mai" = 5,
                                        "Juni" = 6,
                                        "Juli" = 7,
                                        "August" = 8,
                                        "September" = 9,
                                        "Oktober" = 10)),
                radioButtons("Cleaner", "Vil du ha rensefisk fra start?",
                             c("Nei" = 0,
                               "Ja" = 1)),
                radioButtons("Skirt", "Vil du bruke luseskjørt i anlegget?",
                             c("Nei" = 0,
                               "Ja" = 1)),
                selectInput("FromSkirt", "Hvis ja, fra når?",
                            choices = c("Fra oppstart" = 1,
                                        "Om 1 måned" = 31,
                                        "Om 2 måneder" = 61,
                                        "Om 3 måneder" = 91,
                                        "Om 4 måneder" = 121,
                                        "Om 5 måneder" = 151,
                                        "Om 6 måneder" = 181)),
                actionButton("Next", "Godkjenn valgene og vis kart"),
                hidden(
                  actionButton("switchSpill", "Fortsett til spillet")
                )
                ),
            box(width = 7, height = 500,
                leafletOutput("Map"))),


## Design Spill tab --------------------------------------------------------

    tabItem("spill",
            fluidRow(
              box(width = 4,
                  radioGroupButtons("TreatmentType", "Behandling:",
                                    c("Ikke-medikamentell" = "therm",
                                      "Fôrbehandling" = "EMcht",
                                      "Medikamentell" = "HPcht"),
                                    direction = "vertical",
                                    selected = character(0)),
                  disabled(
                    checkboxGroupInput("CageSel", "Hvilke merder vil du behandle?",
                                       c("Merd 1" = "1",
                                         "Merd 2" = "2",
                                         "Merd 3" = "3",
                                         "Merd 4" = "4"),
                                       selected = c()
                    )
                  ),

                  radioButtons("Cleaner2", "Tilsette mer rensefisk?",
                               c("Nei" = 0,
                                 "Ja" = 1)),
                  radioButtons("Continue", "Hvordan fortsette spillet?",
                               c("Uke for uke" = "week",
                                 "Til lusegrense er nådd" = "threshold")),
                  actionButton("Go","Kjør videre"),
                  actionButton("secCount", "Tell pånytt!"),
                  hidden(
                    actionButton("Summary", "Oppsummering")
                  ),
                  hidden(
                    actionButton("highscore", "Highscore")
                  )
                  ),
              infoBoxOutput("af1"),
              infoBoxOutput("om1"),
              infoBoxOutput("af2"), 
              infoBoxOutput("om2"),
              infoBoxOutput("af3"),
              infoBoxOutput("om3"),
              infoBoxOutput("af4"),
              infoBoxOutput("om4"),
              valueBoxOutput("meanaf"),
              valueBoxOutput("day"),
              valueBoxOutput("points")),
            bsModal(id = "endOfGame",
                    title = "Oppsummering av spillet",
                    trigger = "Summary",
                    size = "large",
                    withSpinner(
                      # dataTableOutput("oppsumm")
                      DT::DTOutput("oppsumm"))),
            bsModal(id = "top_list",
                    title = "Highscore",
                    trigger = "highscore",
                    size = "large",
                    withSpinner(
                      dataTableOutput("leaderboard_out"))),
            ),


## Design Tidsserier tab ---------------------------------------------------

    tabItem("grafer",
            fluidRow(
              actionButton("switchSpill2", "Tilbake til spillet"),
              plotOutput("sim_plot"),
              plotOutput("temp"),
              tableOutput("summarise")      # kan kommenteres ut/fjernes senere
              # DT::DTOutput("oppsummDF")   # er som bsModal rett over
              ))
            )
            
    )

# Server code -------------------------------------------------------------

shinyApp(ui = dashboardPage(header, sidebar, body), 
         server = function(input, output, session){
           

# Sourcing Modelfunctions and init ----------------------------------------

           ## Load functions
           source("ModelFunctions_v4.R", local = TRUE)
           
           ## Model initialisation
           # default model settings
           source("init.R", local = TRUE)
           ## OPPDRETTSMILJØ
           
           # observeEvent updateS model settings
           # update rec_env$default.model.settings
           

# Update sidepanel menu ---------------------------------------------------

           # Update sidebarmenu after clicking "Godkjenn valgene og vis kart")
           output$nyfane1 <- renderMenu({
             if( input$switchSpill == TRUE )
               menuItem("Lusespill", tabName = "spill", icon = icon("gamepad"))
           })
           
           output$nyfane2 <- renderMenu({
             if( input$switchSpill == TRUE )
               menuItem("Tidsserier", tabName = "grafer", icon = icon("chart-area"))
           })

           

# Creating reactive object ------------------------------------------------

           ## set_reactive_values in model settings
           rec_env <- reactiveValues(default.model.settings = default.model.settings,
                                     oppsDF = oppsDF,
                                     start.model.settings = NULL,
                                     SV_RE_start = NULL,
                                     SV = NULL,
                                     RE = NULL,
                                     CO = NULL,
                                     new.model.settings = NULL,
                                     summarised_data = NULL,
                                     lice_df = NULL,
                                     mort = NULL,
                                     t = NULL,
                                     dato = NULL)
           
           observeEvent(input$Next, {
             shinyjs::show("switchSpill")
             

# Storing user input in reactive object -----------------------------------

             ## Brukervalg
             rec_env$default.model.settings$Region <- input$PO
             rec_env$default.model.settings$start.mo <- as.numeric(input$StartTime)
             rec_env$default.model.settings$do_addclf <- input$Cleaner
             rec_env$default.model.settings$do_applyskirt <- input$Skirt
             rec_env$default.model.settings$skirtstartday <- input$FromSkirt %>% as.numeric
             
             rec_env$oppsDF$po <- input$PO
             rec_env$oppsDF$start <- as.numeric(input$StartTime)
             rec_env$oppsDF$skirt <- input$Skirt
             rec_env$oppsDF$skirt_start <- as.numeric(input$FromSkirt)
             rec_env$oppsDF$leppe <- input$Cleaner
             if (input$Cleaner == 1) rec_env$oppsDF$andel_leppe <- rec_env$default.model.settings$clfratio
             
             
             ## Create rec_env$SV and rec_env$RE start conditions
             # start with default model settings
             rec_env$start.model.settings <- rec_env$default.model.settings
             rec_env$SV_RE_start <- create.SV_RE_start(EnvList_local = EnvList,
                                               model.settings = rec_env$start.model.settings)
             
             # extract rec_env$SV and rec_env$RE from rec_env$SV_RE_start
             rec_env$SV <- rec_env$SV_RE_start$SV
             rec_env$RE <- rec_env$SV_RE_start$RE
             rec_env$CO <- round(rec_env$SV_RE_start$Coordinates)
             rec_env$t <- t
             rec_env$new.model.settings <- rec_env$start.model.settings
             

# Updating SV -------------------------------------------------------------

             ## First and second lice count at t = 1 
             SV_updated <- update.licecount1(SV_local = rec_env$SV, 
                                             RE_local = rec_env$RE, 
                                             t_local = rec_env$t, 
                                             model.settings = rec_env$new.model.settings
                                             )
             
             rec_env$SV <- SV_updated
             
             SV_updated <- update.licecount2(SV_local = rec_env$SV, 
                                             RE_local = rec_env$RE, 
                                             t_local = rec_env$t, 
                                             model.settings = rec_env$new.model.settings
                                             )
             
             rec_env$SV <- SV_updated
             
             ## Lice skirt
             SV_updated <- update.skirt(SV_local = rec_env$SV, t_local = rec_env$t, 
                                        model.settings = rec_env$new.model.settings)
             rec_env$SV <- SV_updated
             
             # Add cleaner fish from start of simulation?
             # If so, this is done by temporarily changing model settings
             # rec_env$new.model.settings$do_addclf <- 1
             SV_updated <- update.licecontrol(SV_local = rec_env$SV, t_local = rec_env$t, 
                                              model.settings = rec_env$new.model.settings)
             # rec_env$new.model.settings$do_addclf <- 0
             rec_env$SV <- SV_updated
             
             ## Run summarise_data()
             rec_env$summarised_data <- summarise_data(model.settings = rec_env$new.model.settings,
                                                SV_local = rec_env$SV)
             

# Creating map of selected location ---------------------------------------

             ## Leaflet map
             # Transform to latlong
             pkt <- data.frame(utmx = rec_env$CO[1],
                               utmy = rec_env$CO[2]) 
             pkt_ <- st_as_sf(x = pkt,
                              coords = c("utmx", "utmy"),
                              crs = 32633) %>% 
               st_transform(crs = 4326)
             
             # Plot map
             output$Map <- renderLeaflet({
               leaflet() %>% 
                 addTiles() %>% 
                 addCircleMarkers(lng = st_coordinates(pkt_)[1],
                                  lat = st_coordinates(pkt_)[2],
                                  radius = 10,
                                  popup = "Ditt anlegg")
             })
           })
           

## Spillfane ---------------------------------------------------------------
           
           observeEvent(input$switchSpill, {
             updateTabsetPanel(session, "tabs",selected = "spill")
             
               disable("PO")
               disable("StartTime")
               disable("Cleaner")
               disable("Navn")
               disable("Skirt")
               disable("FromSkirt")
               disable("Next")
               disable("swithSpill")
           })
           if(t < 2) disable(selector = "#TreatmentType button:eq(0)") 
      
           


# Insert valueboxes with initial values -----------------------------------
           #shinyjs::disable(selector="#CageSel")
           
           output$af1 <- renderInfoBox({
             infoBox("Hunnlus",
                     subtitle = "Merd 1",
                     0,
                     icon = icon_af(0),
                     color = color_af(0),
                     fill = TRUE)
           })
           
           output$om1 <- renderInfoBox({
             infoBox("Bevegelige",
                     subtitle = "Merd 1",
                     0,
                     icon = icon_om(0),
                     color = color_om(0))
           })
           
           output$af2 <- renderInfoBox({
             infoBox("Hunnlus",
                     subtitle = "Merd 2",
                     0,
                     icon = icon_af(0),
                     color = color_af(0),
                     fill = TRUE)
           })
           
           output$om2 <- renderInfoBox({
             infoBox("Bevegelige",
                     subtitle = "Merd 2",
                     0,
                     icon = icon_om(0),
                     color = color_om(0))
           })
           
           output$af3 <- renderInfoBox({
             infoBox("Hunnlus",
                     subtitle = "Merd 3",
                     0,
                     icon = icon_af(0),
                     color = color_af(0),
                     fill = TRUE)
           })
           
           output$om3 <- renderInfoBox({
             infoBox("Bevegelige",
                     subtitle = "Merd 3",
                     0,
                     icon = icon_om(0),
                     color = color_om(0))
           })
           
           output$af4 <- renderInfoBox({
             infoBox("Hunnlus",
                     subtitle = "Merd 4",
                     0,
                     icon = icon_af(0),
                     color = color_af(0),
                     fill = TRUE)
           })
           
           output$om4 <- renderInfoBox({
             infoBox("Bevegelige",
                     subtitle = "Merd 4",
                     0,
                     icon = icon_om(0),
                     color = color_om(0))
           })
           output$meanaf <- renderValueBox({
             valueBox("Snitt",
                      paste0("Hunnlus: ", 0, ", ", 
                             "Bevegelige: ", 0, ", ", 
                             "Laksevekt: ", 0),
                      icon = icon("exclamation")
             )
           })
           
           output$day <- renderValueBox({
             valueBox(paste0("Dag ", 0),
                      paste0("Dato: ", format(as.Date(paste0("15/", input$StartTime), "%d/%m")), ", ", 
                             "Lusegrense: ", rec_env$SV$Lusegrense[1], ", ",
                             "Temperatur: ", round(rec_env$SV$ST[1],1), " C"),
                      icon = icon("calendar-alt")
             )
           })
           
           output$points <- renderValueBox({
             valueBox("Poeng",
                      0,
                      icon = icon("crosshairs"))
           })           
           
           
           ### Disable treatment options -----------------------------------------------
           #initialize reactive values (will use to store selected boxes to identify newest selection)
           rv <- reactiveValues()
           #initialize selected boxes to NULL
           rv$disableBoxes <- NULL 
           find_disableTF <- function(x) {
             if(!is.null(rec_env$t)) {
               whichDisableTF <- rec_env$summarised_data %>%  ## Pipeline that looks for restrictions in feed treatments
                 group_by(cage) %>%
                 summarise(sumTreat = sum(EMcht, na.rm = T)) %>%
                 dplyr::select(sumTreat) %>%
                 mutate(sumTreat = sumTreat != 0) %>%
                 unlist %>%
                 unname
             } else {
               return(character(0))
             }
           }
           observeEvent(input$TreatmentType, {
             
             

             #create object that identifies newly selected checkbox (syntax found using selectorgadget)
             whichDisableTF <- find_disableTF()
             whichDisable <- c(1:4)[whichDisableTF]
             whichEnable  <- c(1:4)[!whichDisableTF]
             #disable single checkbox of group
             #print(subElement)
 
             if(input$TreatmentType == "EMcht") {
               subElement <- paste0("#CageSel .checkbox:nth-child(", c(1:4),") label")
               shinyjs::enable(selector=subElement)
               subElement <- paste0("#CageSel .checkbox:nth-child(", whichDisable,") label")
               shinyjs::disable(selector=subElement)
               # #store all selected checkboxes
               rv$disableBoxes <- input$CageSel
               updateCheckboxGroupInput( ## cage selector
                 session = session, 
                 inputId = "CageSel",
                 selected = whichEnable
               )
               rm(subElement)
               
             } else {
               subElement <- paste0("#CageSel .checkbox:nth-child(", c(1:4),") label")
               shinyjs::enable(selector=subElement)
               #store all selected checkboxes
               rv$disableBoxes <- input$CageSel
               updateCheckboxGroupInput( ## cage selector
                 session = session, 
                 inputId = "CageSel",
                 selected = c(1:4)
               )
               rm(subElement)
             }
             
             
           })
     
           
           observeEvent(input$Go,{
            

### Treatment or not --------------------------------------------------------

          # Velger parametere for behandling
             if(is.null(input$TreatmentType)) {             
               rec_env$new.model.settings$do_treat <- 0
             } else {
               rec_env$new.model.settings$do_treat <- 1
             }
             
### Limit amount of cleaner fish added --------------------------------------------------------
             
             if( rec_env$oppsDF$andel_leppe >= 0.10) {
               shinyjs::disable("Cleaner2")
             }


#### Updating reactive object (rec_env) --------------------------------------
                         
             rec_env$new.model.settings$trt.type <- input$TreatmentType
             rec_env$new.model.settings$do_addclf <- input$Cleaner2
             rec_env$new.model.settings$which_treat <- as.numeric(input$CageSel)
             
             if (input$Cleaner2 == 1) rec_env$oppsDF$andel_leppe <- rec_env$oppsDF$andel_leppe + rec_env$default.model.settings$clfratio
             

### Simulations (while loop) ------------------------------------------------

             while( rec_env$t < rec_env$new.model.settings$Ndays ) {
             
               SV_T <- update_SV(SV_local = rec_env$SV, 
                                 RE_local = rec_env$RE, 
                                 t_local = rec_env$t, 
                                 model.settings_local = rec_env$new.model.settings)
               
               rec_env$new.model.settings$do_treat <- 0
               rec_env$new.model.settings$do_addclf <- 0

               subElement <- paste0("#CageSel .checkbox:nth-child(", c(1:4),") label")
               shinyjs::disable(selector=subElement)
               rm(subElement)
               #store all selected checkboxes
               rv$disableBoxes <- input$CageSel
               updateCheckboxGroupInput( ## cage selector
                 session = session, 
                 inputId = "CageSel",
                 selected = 0
               )
               
               reset("Cleaner2")
               
               rec_env$t <- SV_T$t_stop
               rec_env$SV <- SV_T$SV
               
               shinyjs::toggle(id = "Summary",
                               condition = rec_env$t > 546)
               shinyjs::toggle(id = "highscore",
                               condition = rec_env$t > 546)
               if(rec_env$t > 546){
                 disable("Go")
                 disable("secCount")
               }
               
               # if(rec_env$t > 546){
               #   stand <- data.frame(Navn = input$Navn, Poeng = rec_env$oppsDF$poeng, Dato = as.character(Sys.Date ()), PO = input$PO)
               #   leaderboard <- read.csv('leaderboard.csv', sep = ',') %>% 
               #     bind_rows(stand) %>% 
               #     arrange(desc(Poeng))
               #   write.csv(leaderboard, file = 'leaderboard.csv', row.names = F)
               #   leaderboard <- as.data.frame(leaderboard) %>% filter(PO == input$PO)
               # }
               
               
               
               ## Beregner dødelighet
               rec_env$mort <- TreatMort(SV = rec_env$SV)
               
               

#### Updating summary table --------------------------------------------------

               rec_env$oppsDF$mort <- rec_env$mort
               rec_env$oppsDF$ikke_med_beh <- sum(rec_env$SV$use.therm)
               rec_env$oppsDF$for_beh <- sum(rec_env$SV$use.EMcht)
               rec_env$oppsDF$med_beh <- sum(rec_env$SV$use.HPcht)
               # rec_emv$oppsDF$andel_leppe
               

               
               
               ## Oppdaterer data
               rec_env$summarised_data <- summarise_data(SV_local = rec_env$SV, 
                                                  model.settings = rec_env$new.model.settings)

### Updating points ---------------------------------------------------------
               rec_env$oppsDF$poeng <- #standard_points(x = 
                 round(sum(rec_env$summarised_data$points_week_cage, na.rm = T) - 400 - 
                 threshold_penalty(rec_env$summarised_data, 
                                   PO = input$PO,
                                   mort = rec_env$mort,
                                   SV = rec_env$SV
                                   ), 
                 0)
               
               if(rec_env$t > 546){
                 stand <- data.frame(Navn = input$Navn, Poeng = rec_env$oppsDF$poeng, Dato = as.character(Sys.Date ()), PO = input$PO)
                 leaderboard <- read.csv('leaderboard.csv', sep = ',') %>% 
                   bind_rows(stand) %>% 
                   arrange(desc(Poeng))
                 write.csv(leaderboard, file = 'leaderboard.csv', row.names = F)
                 leaderboard <- as.data.frame(leaderboard) %>% filter(PO == input$PO)
               }
               
               ## Gammel poengberegning
               #rec_env$oppsDF$poeng <- 100 - (rec_env$mort*100) - sum(rec_env$SV$use.therm) - sum(rec_env$SV$use.EMcht) - sum(rec_env$SV$use.HPcht)

### Updating lice_df --------------------------------------------------------

              rec_env$lice_df$af1 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "1") %>% select(Y.AF)
              rec_env$lice_df$af2 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "2") %>% select(Y.AF)
              rec_env$lice_df$af3 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "3") %>% select(Y.AF)
              rec_env$lice_df$af4 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "4") %>% select(Y.AF)
              rec_env$lice_df$om1 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "1") %>% select(Y.OM)
              rec_env$lice_df$om2 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "2") %>% select(Y.OM)
              rec_env$lice_df$om3 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "3") %>% select(Y.OM)
              rec_env$lice_df$om4 <- rec_env$summarised_data %>% filter(day == rec_env$t & cage == "4") %>% select(Y.OM)
              
              rec_env$dato <- dmy(paste(15, paste0(0, rec_env$new.model.settings$start.mo), year(today()), sep = "-"))
               

### Info and Valueboxes -----------------------------------------------------

              a1 <- 10^(rec_env$lice_df$af1) - logoffset
               output$af1 <- renderInfoBox({
                 infoBox("Hunnlus",
                         subtitle = "Merd 1",
                         a1,
                         icon = icon_af(a1),
                         color = color_af(a1),
                         fill = TRUE)
               })
               
               o1 <- 10^(rec_env$lice_df$om1) - logoffset
               output$om1 <- renderInfoBox({
                 infoBox("Bevegelige",
                         subtitle = "Merd 1",
                         o1,
                         icon = icon_om(o1),
                         color = color_om(o1))
               })
               
               a2 <- 10^(rec_env$lice_df$af2) - logoffset
               output$af2 <- renderInfoBox({
                 infoBox("Hunnlus",
                         subtitle = "Merd 2",
                         a2,
                         icon = icon_af(a2),
                         color = color_af(a2),
                         fill = TRUE)
               })
               
               o2 <- 10^(rec_env$lice_df$om2) - logoffset
               output$om2 <- renderInfoBox({
                 infoBox("Bevegelige",
                         subtitle = "Merd 2",
                         o2,
                         icon = icon_om(o2),
                         color = color_om(o2))
               })
               
               a3 <- 10^(rec_env$lice_df$af3) - logoffset
               output$af3 <- renderInfoBox({
                 infoBox("Hunnlus",
                         subtitle = "Merd 3",
                         a3,
                         icon = icon_af(a3),
                         color = color_af(a3),
                         fill = TRUE)
               })
               
               o3 <- 10^(rec_env$lice_df$om3) - logoffset
               output$om3 <- renderInfoBox({
                 infoBox("Bevegelige",
                         subtitle = "Merd 3",
                         o3,
                         icon = icon_om(o3),
                         color = color_om(o3))
               })
               
               a4 <- 10^(rec_env$lice_df$af4) - logoffset
               output$af4 <- renderInfoBox({
                 infoBox("Hunnlus",
                         subtitle = "Merd 4",
                         a4,
                         icon = icon_af(a4),
                         color = color_af(a4),
                         fill = TRUE)
               })
               
               o4 <- 10^(rec_env$lice_df$om4) - logoffset
               output$om4 <- renderInfoBox({
                 infoBox("Bevegelige",
                         subtitle = "Merd 4",
                         o4,
                         icon = icon_om(o4),
                         color = color_om(o4))
               })
               
               output$meanaf <- renderValueBox({
                 valueBox("Snitt",
                          paste0("Hunnlus: ", round(((a1 + a2 + a3 + a4) / 4), 2), ", ", 
                                 "Bevegelige: ", round(((o1 + o2 + o3 + o4) / 4), 2), ", ", 
                                 "Laksevekt: ", round(rec_env$SV$W.SAL[rec_env$t, 1], 1)),
                          icon = icon("exclamation"))
               })
               
               output$day <- renderValueBox({
                 valueBox(paste0("Dag ", rec_env$t),
                          paste0("Dato: ", format(as.Date(rec_env$dato + rec_env$t), "%d/%m"), ", ", 
                                 "Lusegrense: ", rec_env$SV$Lusegrense[rec_env$t], ", ",
                                 "Temperatur: ", round(rec_env$SV$ST[rec_env$t], 1), " C"),
                          icon = icon("calendar-alt"))
               })
               
               output$points <- renderValueBox({
                 valueBox("Poeng",
                          round(rec_env$oppsDF$poeng, 2),
                          icon = icon("crosshairs"))
               })
               renderText(threshold_penalty)
               
               break_val <- (  10^(rec_env$lice_df$af1) - logoffset +
                               10^(rec_env$lice_df$af2) - logoffset +
                               10^(rec_env$lice_df$af3) - logoffset +
                               10^(rec_env$lice_df$af4) - logoffset)/4
               

### Flow: week or second count or to lice boundary --------------------------
               
               if( input$Continue == "week" | input$Continue == "secCount" ) {
                 break
               } else if ( break_val > rec_env$SV$Lusegrense[rec_env$t]) {
               
                 break
               }
               
               ## Summary model
               output$oppsumm <- DT::renderDT({
                 opps <- t(rec_env$oppsDF)
                 rownames(opps) <- opps_navn
                 colnames(opps) <- "Verdi"
                 DT::datatable(
                   opps,
                   options = list(iDisplayLength = 25),
                   escape = FALSE
                 )
               })
               
               # output$oppsumm <- renderDataTable(
               #   t(rec_env$oppsDF) 
               # )
               
               ## Leaderboard
               output$leaderboard_out <- renderDataTable(
                 leaderboard
               )
               
              
             }  ## End while loop
             
             

### Medikamentell treatment available or not --------------------------------
              
             whichDisableTF <- find_disableTF()
             whichDisable <- c(1:4)[whichDisableTF]
             disabled_choices <- character()
             if(length(whichDisable) == 4) disabled_choices <- c(disabled_choices, "EMcht")

             if((rec_env$SV$W.SAL[rec_env$t, 1] < 1)) {
               disabled_choices <- c(disabled_choices, "therm")# Disabling the choice for ikkemedikamentell behandling under ett kilo fiskevekt
             }
             updateRadioGroupButtons( ## Update treatment type
               session = session, 
               inputId = "TreatmentType",
               selected = character(0), 
               disabledChoices = disabled_choices
             )
           }) ## End observe event
           

### Second count ------------------------------------------------------------

           observeEvent(input$secCount, {
             t <- rec_env$t
             rec_env$lice_df$af1 <- rec_env$summarised_data %>% filter(day == t & cage == "1") %>% select(Y2.AF)
             rec_env$lice_df$af2 <- rec_env$summarised_data %>% filter(day == t & cage == "2") %>% select(Y2.AF)
             rec_env$lice_df$af3 <- rec_env$summarised_data %>% filter(day == t & cage == "3") %>% select(Y2.AF)
             rec_env$lice_df$af4 <- rec_env$summarised_data %>% filter(day == t & cage == "4") %>% select(Y2.AF)
             rec_env$lice_df$om1 <- rec_env$summarised_data %>% filter(day == t & cage == "1") %>% select(Y2.OM)
             rec_env$lice_df$om2 <- rec_env$summarised_data %>% filter(day == t & cage == "2") %>% select(Y2.OM)
             rec_env$lice_df$om3 <- rec_env$summarised_data %>% filter(day == t & cage == "3") %>% select(Y2.OM)
             rec_env$lice_df$om4 <- rec_env$summarised_data %>% filter(day == t & cage == "4") %>% select(Y2.OM)
             
             ## Info og valueboxes
             a1 <- 10^(rec_env$lice_df$af1) - logoffset
             output$af1 <- renderInfoBox({
               infoBox("Hunnlus",
                       subtitle = "Merd 1",
                       a1,
                       icon = icon_af(a1),
                       color = color_af(a1),
                       fill = TRUE)
             })
             
             o1 <- 10^(rec_env$lice_df$om1) - logoffset
             output$om1 <- renderInfoBox({
               infoBox("Bevegelige",
                       subtitle = "Merd 1",
                       o1,
                       icon = icon_om(o1),
                       color = color_om(o1))
             })
             
             a2 <- 10^(rec_env$lice_df$af2) - logoffset
             output$af2 <- renderInfoBox({
               infoBox("Hunnlus",
                       subtitle = "Merd 2",
                       a2,
                       icon = icon_af(a2),
                       color = color_af(a2),
                       fill = TRUE)
             })
             
             o2 <- 10^(rec_env$lice_df$om2) - logoffset
             output$om2 <- renderInfoBox({
               infoBox("Bevegelige",
                       subtitle = "Merd 2",
                       o2,
                       icon = icon_om(o2),
                       color = color_om(o2))
             })
             
             a3 <- 10^(rec_env$lice_df$af3) - logoffset
             output$af3 <- renderInfoBox({
               infoBox("Hunnlus",
                       subtitle = "Merd 3",
                       a3,
                       icon = icon_af(a3),
                       color = color_af(a3),
                       fill = TRUE)
             })
             
             o3 <- 10^(rec_env$lice_df$om3) - logoffset
             output$om3 <- renderInfoBox({
               infoBox("Bevegelige",
                       subtitle = "Merd 3",
                       o3,
                       icon = icon_om(o3),
                       color = color_om(o3))
             })
             
             a4 <- 10^(rec_env$lice_df$af4) - logoffset
             output$af4 <- renderInfoBox({
               infoBox("Hunnlus",
                       subtitle = "Merd 4",
                       a4,
                       icon = icon_af(a4),
                       color = color_af(a4),
                       fill = TRUE)
             })
             
             o4 <- 10^(rec_env$lice_df$om4) - logoffset
             output$om4 <- renderInfoBox({
               infoBox("Bevegelige",
                       subtitle = "Merd 4",
                       o4,
                       icon = icon_om(o4),
                       color = color_om(o4))
             })
             
             output$meanaf <- renderValueBox({
               valueBox("Snitt",
                        paste0("Hunnlus: ", round(((a1 + a2 + a3 + a4) / 4), 2), ", ", 
                               "Bevegelige: ", round(((o1 + o2 + o3 + o4) / 4), 2), ", ", 
                               "Laksevekt: ", round(rec_env$SV$W.SAL[rec_env$t, 1], 1)),
                        icon = icon("exclamation"))
             })
             
             output$day <- renderValueBox({
               valueBox(paste0("Dag ", rec_env$t),
                        paste0("Dato: ", format(as.Date(rec_env$dato + rec_env$t), "%d/%m"), ", ", 
                               "Lusegrense: ", rec_env$SV$Lusegrense[rec_env$t], ", ",
                               "Temperatur: ", round(rec_env$SV$ST[rec_env$t], 1), " C"),
                        icon = icon("calendar-alt"))
             })
             
             output$points <- renderValueBox({
               valueBox("Poeng",
                        round(rec_env$oppsDF$poeng, 2),
                        icon = icon("crosshairs"))
             })
             
           })
           

## Tidsserier tab ----------------------------------------------------------
           
           observeEvent(input$switchSpill2, {
             updateTabsetPanel(session, "tabs",selected = "spill")
           })
           
           output$sim_plot <- renderPlot({
             ggplot(rec_env$summarised_data, aes(day, Y.OM)) +
               theme_classic() +
               scale_y_continuous(name="Lus pr laks", breaks = yat, labels = labels, limits=c(min(yat), log10(ymax.plot+logoffset))) +
               geom_line(aes(day, log10(llimit), colour = "lusegrense", linetype = "lusegrense"), size = 1.25) +
               geom_point(aes(colour = "bevegelige", shape = "bevegelige"), size = 2.5) +
               geom_point(aes(day, Y.AF, colour = "hunnlus", shape = "hunnlus"), size = 2.5) +
               geom_vline(aes(xintercept = rec_env$summarised_data$treatment + 1, 
                              colour = "behandling",
                              linetype = "behandling"),
                          show.legend = F) +
               scale_color_manual(name = "Tegnforklaring",
                                  breaks = c("lusegrense", "bevegelige", "hunnlus", "behandling"),
                                  values = c("lusegrense" = "green",
                                             "bevegelige" = "blue",
                                             "hunnlus" = "red",
                                             "behandling" = "rosybrown"),
                                  guide = guide_legend(override.aes = list(
                                    linetype = c(2, 0, 0, 1),
                                    shape = c(NA, 19, 17, NA)))
                                  ) +
               guides(shape = "none", linetype = "none") +
               facet_wrap(~cage,  ncol=2) +
               theme(text = element_text(size=20),
                     legend.position = "bottom")
           })
           
           
           
           output$temp <- renderPlot({
             ggplot(rec_env$summarised_data, aes(day, seatemp)) +
               theme_classic() +
               geom_point(mapping = aes(x = day, y = seatemp), size = 0, alpha = 0) +
               geom_point(data = rec_env$summarised_data %>% filter(!is.nan(Y.AF)),
                          mapping = aes(day, seatemp))
           })
           
           #output$oppsummDF <- renderTable({rec_env$oppsDF})
           
           # output$oppsummDF <- DT::renderDT({
           #   opps <- rec_env$oppsDF()
           #   rownames(opps) <- c("Poeng",
           #                       "Laksedødelighet",
           #                       "Ant ikke-med behandlinger",
           #                       "Ant forbehandlinger",
           #                       "Ant med behandlinger",
           #                       "Produksjonsområde",
           #                       "Oppstartsmåned",
           #                       "Luseskjørt",
           #                       "Luseskjørt start",
           #                       "Rensefisk",
           #                       "Andel rensefisk")
           #   DT::datatable(
           #     opps,
           #     escape = FALSE
           #   )
           #   })
           
           output$summarise <- renderTable({rec_env$summarised_data})
           
           
         }
         # }
         ) 