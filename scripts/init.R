# Initial model setting and intialise simulation -------
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

## Load data
Coef <- readRDS("Coef20211209.Rds")
load("EnvListShort.Rdata")

trt.sample <- trt.sample.rangen

default.model.settings <- list(
  
  # Place and time to simulate:
  # Region = c("PO 1-4", "PO 5-7", "PO 8-13")[1], # Region or PO. Note jan. 2022: "PO1", "PO2" etc. also works
  Region = "PO1",
  start.mo = 5,       # Start month. Note jan. 2022: Replaces POstart = c("Vaar", "Hoest")[1]
  # Note: Should be between 4 and 10 to have enough environmental data 
  # from real production cycles.
  
  Ncages = 4,  # Number of cages
  Ndays = 553, # Number of days in production cycle. Maximum 600
  
  # Number and weight of salmon:
  w0 = 0.2, # Initial weight (kg) of salmon,
  nstock = 1, # Total number of salmon (millions) stocked in farm (equally divided between cages),
  dstock = 0, # Cage-to-cage delay (days) in sequential stocking of salmon,
  mnat = 0.005/30, # Baseline daily mortality of salmon,
  tslaught = 600, # Time (days) that slaughter starts,
  dslaught = 0, # Cage-to-cage delay (days) in sequential slaughter of salmon.
  
  # Weekly counting
  stepsize = 7, # days between lice counts
  ncount = 20, # number of salmon counted per cage in first count
  
  # Lice skirts
  # Note: Has to be decided at start of simulation (for technical reasons).
  do_applyskirt = 0, # apply lice skirt (0: no, 1: yes)? 
  skirtstartday = 1, # from which day of production are skirts applied?
  skirtduration = 180, # how many days will skirts stay?
  skirteffect = 0.5, # proportion of external lice larvae stopped by skirt
  
  # Second lice count
  do_count2 = 1, # should a second count be performed (0: no, 1: yes)?
  n2count = 20, # number of salmon counted in second count, if performed
  
  # Lice treatment
  do_treat = 0, # apply treatment (0: no, 1: yes)?
  which_treat = "all", # which cages should be treated? ("all" or a vector with cage numbers)
  trt.type = c("HPcht", "DMcht", "AZcht", "EMcht", "DBcht",
               "therm", "freshw", "mech", 
               "fx"),#[6],
  treat.delay = 4, # days from lice count to treatment, if performed
  M.trt = 0.8, # treatment mortality (if treat.type == "fx")
  
  # Cleaner fish
  do_addclf = 0, # add cleaner fish (0: no, 1: yes)?
  which_clf = "all", # into which cages should cleaner fish be added?
  clfratio = 0.05 # cleaner fish ratio when added
  
)

## Star time
t <- 1

## Plot settings
ymax.plot <- 20
logoffset <- .01
labels <- c(0,.1,.5,2,5,20,50,100,1000,1e4,1e5,1e6,1e7,1e8,1e9,1e19)
yat    <- log10(labels+logoffset)


## summarise_data function
summarise_data <- function(SV_local = SV,
                           model.settings = default.model.settings) {
  
  SV <- SV_local
  Ncages <- model.settings$Ncages
  Ndays <- model.settings$Ndays
  treat.delay <- model.settings$treat.delay
  
  logoffset <- .01
  log_transform <- function(lus, cage_no) {
    log10(logoffset + lus[, cage_no]/SV$n.SAL[, cage_no])
  }
  shift_treatment <- function(treatment, 
                              cage_no) 
    { # To fit the treatments to the filtered days  
    c(treatment[(treat.delay+1):nrow(treatment), cage_no], rep(0, treat.delay))
  }
  
  map(1:Ncages, function(x) {
    data.frame(Y.CH  = log_transform(lus = SV$Y.CH, cage_no = x)) %>% 
      mutate(Y.OM  = log_transform(lus = SV$Y.OM, cage_no = x)) %>% 
      mutate(Y.AF  = log_transform(lus = SV$Y.AF, cage_no = x)) %>% 
      mutate(Y2.CH = log_transform(lus = SV$Y2.CH, cage_no = x)) %>% 
      mutate(Y2.OM = log_transform(lus = SV$Y2.OM, cage_no = x)) %>% 
      mutate(Y2.AF = log_transform(lus = SV$Y2.AF, cage_no = x)) %>% 
      # mutate(N.CH  = SV$N.CH[,,x] + logoffset) %>%  # dele på N.Sal?
      # mutate(N.OM  = SV$N.OM[,,x] + logoffset) %>% 
      # mutate(N.AF  = SV$N.AF[,,x] + logoffset) #%>% 
      mutate(cage  = as.character(x)) %>% 
      mutate(day = (1:Ndays)) %>% 
      mutate(seatemp = SV$ST) %>% 
      mutate(lpress = SV$N.AF.Ext) %>% 
      mutate(llimit = SV$Lusegrense) %>%
      mutate(use.HPcht  = shift_treatment(SV$use.HPcht, x)) %>% 
      mutate(use.DMcht  = shift_treatment(SV$use.DMcht, x)) %>% 
      mutate(use.AZcht  = shift_treatment(SV$use.AZcht, x)) %>%
      mutate(use.EMcht  = shift_treatment(SV$use.EMcht, x)) %>%      # 11. mar: la til EMcht
      mutate(EMcht  = shift_treatment(SV$use.EMcht, x)) %>%          # lagt inn 4. nov - for mulighet for disabling
      mutate(use.DBcht  = shift_treatment(SV$use.DBcht, x)) %>% 
      mutate(use.therm  = shift_treatment(SV$use.therm, x)) %>% 
      mutate(use.freshw = shift_treatment(SV$use.freshw, x)) %>% 
      mutate(use.mech   = shift_treatment(SV$use.mech, x)) %>% 
      mutate(use.fx     = shift_treatment(SV$use.fx, x)) %>% 
      filter(((day)%%7 == 0) | (day == 1))                           # 28.02.22 fjernet -1 fra day
  }
  ) %>% 
    do.call(rbind, .) %>% 
    mutate(treatment = ((use.HPcht + use.DMcht + use.AZcht + use.EMcht + use.DBcht + use.therm + use.freshw + use.mech + use.fx != 0) * day)) %>%
    # mutate(treatment = (use.HPcht + use.DMcht + use.AZcht + use.DBcht + use.therm + use.freshw + use.mech + use.fx != 0)) %>%
    dplyr::select(!starts_with("use")) %>% 
    mutate(treatment = na_if(treatment, 0))
  
  
  
}

## Dataramme med hunnlustall
# hl_df <- data.frame(merd1 = NA,
#                     merd2 = NA,
#                     merd3 = NA,
#                     merd4 = NA)

lice_df <- data.frame(af1 = NA,
                      af2 = NA,
                      af3 = NA,
                      af4 = NA,
                      om1 = NA,
                      om2 = NA,
                      om3 = NA,
                      om4 = NA)

## Colouring of female lice infoboxes
# x: gjennomsnittlig antall hunnlus
color_af <- function(x) {
  if( x < 0.1 ) {
    "green"
  } else if( x > rec_env$SV$Lusegrense ) {
    "red"
  } else {
    "orange"
  }
}

## Colouring of other mobils infoboxes
color_om <- function(x) {
  if( x < 0.2 ) {
    "light-blue"
  } else if( x > rec_env$SV$Lusegrense*2 ) {
    "navy"
  } else {
    "blue"
  }
}

## Endring av ikoner basert på lusetall
# x: gjennomsnittlig antall hunnlus
icon_af <- function(x) {
  if( x < 0.1 ) {
    icon("grin-alt")
  } else if( x > rec_env$SV$Lusegrense ) {
    icon("dizzy")
  } else {
    icon("grimace")
  }
}

## Endring av ikoner basert på lusetall
# x: gjennomsnittlig antall other mobiles 
icon_om <- function(x) {
  if( x < 0.2 ) {
    icon("smile")
  } else if( x > rec_env$SV$Lusegrense*2 ) {
    icon("flushed")
  } else {
    icon("meh")
  }
}

## Første eller andre telling
firstOrSec <- function(x) {
  if (x == "secCount") {
    return(select(Y2.AF))
  } else {
    return(select(Y.AF))
  }
}

## Skalar for dato
dato <- NA

## Funksjon for å beregne behandlingsdødelighet
TreatMort <- function(SV){
  
  # Based on:
  # Walde, CS, Jensen, BB, Pettersen, JM, Stormoen, M. 2021;
  # Estimating cage level mortality distributions following different delousing treatments
  # of Atlantic salmon (salmo salar) in Norway. J Fish Dis. 44: 899– 912.
  # https://doi.org/10.1111/jfd.13348
  
  
  # These numbers are mean mortalities within 14 days after different treatments from Table S2:
  
  m_therm <- 0.0108 # round(1 -  exp(-.000776 * 14), 4)
  m_mech <- 0.0117 # round(1 -  exp(-.0008385 * 14), 4)
  m_freshw <- 0.0126 # round(1 -  exp(-.0009067 * 14), 4)
  m_HPcht <- 0.0135 # round(1 -  exp(-.0009682 * 14), 4)
  m_DMcht <- 0.0042     # round(1 -  exp(-.0002985 * 14), 4)
  m_AZcht <- 0.0042     # round(1 -  exp(-.0002985 * 14), 4)
  
  # "We did not expect
  # that delousing with medicinal feed would give increased mortality (Veterinærkatalogen, 2020);
  # therefore treatment with medicinal feed was not included in this study"
  
  m_EMcht <- 0
  m_DBcht <- 0
  m_fx <- m_therm # the most commonly used treatment
  
  
  
  # Number of dead fish (millions)
  
  N_dead_therm <- sum(apply(SV$N.SAL * SV$use.therm * m_therm, 2, sum))
  N_dead_mech <- sum(apply(SV$N.SAL * SV$use.mech * m_mech, 2, sum))
  N_dead_freshw <- sum(apply(SV$N.SAL * SV$use.freshw * m_freshw, 2, sum))
  N_dead_HPcht <- sum(apply(SV$N.SAL * SV$use.HPcht * m_HPcht, 2, sum))
  N_dead_DMcht <- sum(apply(SV$N.SAL * SV$use.DMcht * m_DMcht, 2, sum))
  N_dead_AZcht <- sum(apply(SV$N.SAL * SV$use.AZcht * m_AZcht, 2, sum))
  N_dead_EMcht <- sum(apply(SV$N.SAL * SV$use.EMcht * m_EMcht, 2, sum))
  N_dead_DBcht <- sum(apply(SV$N.SAL * SV$use.DBcht * m_DBcht, 2, sum))
  N_dead_fx <- sum(apply(SV$N.SAL * SV$use.fx * m_fx, 2, sum))
  
  
  N_dead_trt <-
    
    N_dead_therm +
    N_dead_mech +
    N_dead_freshw +
    N_dead_HPcht +
    N_dead_DMcht +
    N_dead_AZcht +
    N_dead_EMcht +
    N_dead_DBcht +
    N_dead_fx
  
  
  # Total number of fish added:
  
  N_tot <- sum(apply(SV$N.SAL,2,max))
  
  
  # Proportion of fish added that died because of treatments:
  
  P_dead <- N_dead_trt / N_tot
  
  return(P_dead)
  
}

## Oppsummeringstabell
vars <- c("Poeng",
          "Laksed?delighet",
          "Ant ikke-med behandlinger",
          "Ant forbehandlinger",
          "Ant med behandlinger",
          "Produksjonsomr?de",
          "Oppstartsm?ned",
          "Luseskj?rt",
          "Luseskj?rt start",
          "Andel leppefisk")

oppsDF <- data.frame(poeng = NA,
                     mort = NA,
                     ikke_med_beh = NA,
                     for_beh = NA,
                     med_beh = NA,
                     po = NA,
                     start = NA,
                     skirt = NA,
                     skirt_start = NA,
                     leppe = NA)


# ## Run summarise_data()
# summarised_data <- summarise_data(model.settings = new.model.settings,
#                                   SV_local = SV)