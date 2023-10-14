#----------------------------------
#Attach packages  
#----------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(here)
library(ggplot2)
library(ggridges)
library(forcats)
library(tidyverse)
library(here)
library(janitor)
library(LBSPR)
library(dplyr)
library(kableExtra)
library(ggalt)
library(ggrepel)
library(knitr)
library(directlabels)
library(gridExtra)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(workflowr)

# Read in Species List
species <- read_csv("Final_Species_List.csv")

# Read in Output.rds
output <- readRDS("output.rds")

# Read in MyParsList.rds
list <- readRDS("MyParsList.rds")

# Species Drop Down
species_list <- species %>% 
  select("Kaʻūpūlehu Species")

#----------------------------------
#Read in data:
#----------------------------------
species_chart <- read_csv("Size_Limit_Chart.csv") %>% 
  select(3:8)

# Make NAs blank in kable tables
options(knitr.kable.NA = "")

#Create table for first tab
species_kable <- kable(species_chart, align = "llllccc", caption = "Hawaiian Species Included in Analysis") %>% 
  kable_styling(c("condensed", "responsive", "bordered"),
                bootstrap_options = "striped", 
                full_width = F)

#----------------------------------
#Create the user interface:
#----------------------------------
ui <- navbarPage(title = "Kaʻūpūlehu - Minimum Size Limit Decision Tool",
                 theme = shinytheme("cosmo"),
                 
                 tabPanel(title = "Intro", icon = icon("home"),
                          mainPanel(width = 20,
                                    align = "center",
                                    h1("Objectives of this Tool"),
                                    h4("Provide explanatory information on minimum size limits"),
                                    h4("Provide an interactive tool to inform minimum size limit discussions and decision-making"),
                                    h4("Demonstrate likely outcomes of different size limits on population health and fishing yield"),
                                    br(),
                                    hr(),
                                    h1("Why Consider Size Limits as a Management Tool?"),
                                    h3("The Basics"),
                                    h4("Minimum size limits allow fish to spawn before they can be caught"),
                                    h5("If all fish are caught before they reach length at maturity, 
                     they won’t spawn and produce offspring, and the population will decline."),
                                    
                                    br(),
                                    hr(),
                                    h1("General Guidance"),
                                    h3("1. Set minimum size limit at or above length at maturity"),
                                    h4("It is recommended to set minimum size limits somewhere between length at maturity (Lm) and mean maximum length (Linf) for a given species."),
                                    h4("In this way you can assure the species is able to spawn and reproduce before capture (> Lm) and that fishers will be able to find fish large enough to catch (< Linf)."),
                                    h3("2. Ensure population can achieve a minimum spawning biomass"),
                                    h4("Spawning Potential Ratio (SPR)  measures if the population is able to reproduce enough to sustain itself."),
                                    h4("Spawning biomass (SPR) values range from 0-1. The higher the value, the less exploited and 'healthier' the population."),
                                    h4("It is recommended to achieve a minimum spawning biomass value of 0.3-0.5."),
                                    h3("3. Achieve 'pretty good yield' for fishers"),
                                    h4("Yield-per-recruit (YPR)  estimates the capture rate that optimizes fishing yield."),
                                    h4("Catch (YPR) values range from 0-1. The higher the value, the more productive the fishery can be."),
                                    h4("To optimize catch potential for fishers, aim for a higher catch value."),
                                    br(),
                                    tableOutput("species_kable")
                                    
                          )),
                 
                 tabPanel(title = "SPR & YPR", icon = icon("info-circle"),
                          mainPanel(width = 20,
                                    align = "center",
                                    
                                    h2("The Analysis"),
                                    h4("The analysis quantitatively identifies tradeoffs between maximizing fishing yield (YPR) and ensuring population replenishment (SPR) for a given species. It is conducted for a range of size limits and fishing pressures, and returns corresponding Spawning Potential Ratio (SPR) and Yield per Recruit (YPR) values for each combination."),
                                    br(),
                                    hr(),
                                    
                                    h1("Comparing SPR and YPR values"),
                                    h4("Ensure population replenishment (SPR)"),
                                    h4("Maximize fishing yield (YPR)"),
                                    br(),
                                    h2("What effects SPR & YPR Values?"),
                                    h3("Fishing Pressure"),
                                    h5("Fishing Pressure is based on an F/M value, fishing mortality (F)/natural mortality (M). The larger the value, the higher the fishing pressure. For the analysis, the F/M values, 1 (low), 2 (medium), and 4 (high) were used."),
                                    h5("F/M is effected by a range of factors including - number of fishers, hours or days on the water/fisher, fishing restrictions, gear type used (effort), etc."),
                                    h3("Size Limits"),
                                    h5("Size limit options are presented as factors of length at maturity. The higher the multiplier, the larger the size limit. For the analysis, the multipliers used were 0.9xLm - 2xLm."),
                                    br(),
                                    hr(),
                                    
                                    h5("*Kala (Naso unicornis - Bluespine Unicornfish) is used as the example species for the following explanations"),
                                    fluidRow(
                                      column(6, align = "center",
                                             h2("What is SPR?"),
                                             h5("Spawning Potential Ratio"),
                                             h5("aka Spawning Biomass"),
                                             h5("SPR is a measure of current egg production relative to egg production when a stock is unfished. In simple terms, it measures if the population is able to reproduce enough to sustain itself."),
                                             hr(),
                                             h5("This plot shows SPR values as compared to Fishing Pressure for 33 fish species at Kaʻūpūlehu. As fishing pressure increases, spawning biomass decreases."),
                                             img(src = "spr lines.png"),
                                             h4("SPR = 1.0 = untouched population, no fishing"),
                                             h4("SPR = 0.0 = fully exploited population, no spawning"),
                                             h5("Generally, an SPR value of 0.30 or higher is considered sustainabale for fisheries in Hawai'i."),
                                             hr(),
                                             h5("This table shows SPR values across a range of size limits and fishing pressures for Kala."),
                                             h4("Optimal SPR value (green circle) = Largest minimum size limit, paired with “Low” fishing pressure"),
                                             h4("Least favorable SPR value (red circle) = Smallest minimum size limit, paired with “High” fishing pressure"),
                                            h5("Less fishing pressure and a larger minimum size limit will lead to more spawning potential and a healthier population."),
                                             br(),
                                             img(src = "spr.png")
                                             
                                             
                                      ),
                                      column(6, align = "center",
                                             h2("What is YPR?"),
                                             h5("Yield per Recruit"),
                                             h5("aka Fishing Yield"),
                                             h5("Yield-per-recruit estimates the capture rate that optimizes fishing yield. The higher the YPR value, the more productive the fishery can be."),
                                             hr(),
                                             h5("This plot shows YPR values as compared to Fishing Pressure for 33 fish species at Kaʻūpūlehu. As fishing pressure increases, yield will increase as more fish are caught, up to a certain point. When more fish are being captured than young fish entering the fishery, yield per recruit will begin to level out or decrease."),
                                             img(src = "ypr lines.png"),
                                             h4("YPR = 1.0 = optimally productive fishery, maximizing yield"),
                                             h4("YPR = 0.0 = no fishing and no yield"),
                                             hr(),
                                             h5("This table shows YPR values across a range of size limits and fishing pressures for Kala."),
                                             h4("Optimal YPR value (green circle) = Minimum size limit just large enough to maintain the population (15.4 in), paired with “High” fishing pressure"),
                                             h4("Least favorable YPR value (red circle) = Largest (most restrictive) minimum size limit, paired with a “Low” fishing pressure"),
                                             h5("Over the long term, fishers capturing a large amount of fish, with just enough conservation (via a size limit) to maintain population equilibrium, will optimize yield."),
                                             br(),
                                             img(src = "ypr.png")
                                             
                                      )
                                    ),
                                    hr(),
                                    h1("Choosing a Size Limit that"),
                                    h1("Optimizes Spawning Biomass (SPR) and Fishing Yield (YPR)"),
                                    br(),
                                    sliderInput("Minimum Size Limit", "Minimum Size Limit:", 
                                                min=0, max=18, value = 9),
                                    sliderInput("Fishing Pressure", "Fishing Pressure:", 
                                                min=0, max=4, value = 2),
                                    img(src = "pareto.png"),
                                    br()
                                      
                          )),
                 
                 tabPanel(title = "Decision Tool", icon = icon("area-chart"),
                          sidebarPanel(width = 4,
                                       h4("Step 1"),
                                       selectInput("species", label = "Select a Species",
                                                   choices = species_list, selected =  "Kala - Naso unicornis - Bluespine Unicornfish"),
                                       br(),
                                       br(),
                                       h4("Step 2"),
                                       chooseSliderSkin("Flat"),
                                       sliderInput("Fishing Pressure", "Select a Fishing Pressure Scenario", min=0, max=4, value = 4, step = 1, ticks = FALSE, width = '300px'),
                                       p("*Strongly recommended to select 'High' Fishing Pressure"),
                                       br(),
                                       br(),
                                       h4("Step 3"),
                                       sliderInput("Minimum Size Limit", "Select a Range of Minimum Size Limit Options (in)",
                                                   min=0, max=18, value = c(0, 18)),
                                       br(),
                                       br(),
                                       p("**All lengths are in Fork Length (FL)")),
                          mainPanel(width = 16,
                                    h3("Species Information"),
                                    tableOutput(outputId = "species_table"),
                                    br()),
                          mainPanel(width = 16, align = "center",
                                    img(src = "simple_plot.png"),
                                    #plotOutput(outputId = "pareto")
                                    br(),
                                    hr(),
                                    h3("Species Decision"),
                                    numericInput("decision", "Minimum Size Limit (in)", min=0, max=18, value=0, step=1),
                                    actionButton(
                                      inputId="saveSL",
                                      label="Save",
                                      icon=icon('paper-plane'),
                                      width='15%'
                                    ),
                                    br(),
                                    br(),
                                    br()
                                    ),
                                      ),
                 tabPanel(title = "Advanced Analysis", icon = icon("bar-chart"))
                          )





#----------------------------------
#Create the server function:
#----------------------------------

server <- function(input, output, session) {
  
  # Kable Species Table (first tab)
  output$species_kable <- function() {
    species_chart %>% 
      knitr::kable("html", align = "lllccc", caption = "Kaʻūpūlehu Species") %>% 
      kable_styling(c("condensed", "responsive", "bordered"),
                    bootstrap_options = "striped", 
                    full_width = F)
  }
  
  # Species Information Table (third tab)
  species_info <- reactive({
    species %>% 
      filter(`Kaʻūpūlehu Species` == input$species) %>% 
      dplyr::select(`Hawaiian Name`, `Common Name`, `Scientific Name`, `Length at Maturity`, `Mean Maximum Length`, `Current Minimum Size Limit (DAR Regulation)`)
  })
  
  output$species_table <- renderTable({
    species_info()
  })
  
  # Sliders (third tab)
  # reactive({
  #   updateSliderInput(session, "Minimum Size Limit",
  #                     max = input$species$Linf_FL_in)
  # })
  
  # Pareto Plot (third tab)
  # pareto <- reactive({
  #   function(MyParsList, output, k,  Flevel=c(0, 4)) {
  #   EU_grid <- expand.grid(Lc = output$EU[[k]]$Lc / 25.4,
  #                          F_M = output$EU[[k]]$F_M)
  #   EU_grid$ypr <- as.vector(t(output$EU[[k]]$YPR_EU))
  #   EU_grid$spr <- as.vector(t(output$EU[[k]]$SPR_EU))
  #   
  #   # Pareto plots prep
  #   EU_tmp <- EU_grid %>%
  #     filter(F_M > Flevel[1],
  #            F_M < Flevel[2])
  #   values <- c(
  #     0,
  #     (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc) - 0.01,
  #     (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc),
  #     (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc),
  #     (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc) + 0.01,
  #     1
  #   )
  #   colors <- c("#A50026", "#FDAE61", "black", "#A6D96A", "#006837")
  #   pareto_title_one <- c(paste0(MyParsList[[k]]$MyPars@Species))
  #   pareto_subtitle_one <- c(paste0("SPR-YPR tradeoffs"))
  #   pareto_title_two <- c(paste0(MyParsList[[k]]$MyPars@Species))
  #   }
  #   })
  
}

#----------------------------------
#Combine them into an app: 
#----------------------------------


shinyApp(ui = ui, server = server)
