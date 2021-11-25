## R Shiny app for visualization of CMIP6 global climate model simulations for North America and subregions
## author: Colin Mahony colin.mahony@gov.bc.ca

# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(rgdal)
library(shiny)
library(RColorBrewer)
library(DT)
library(scales)
library(shinydashboard)
library(markdown)
library(plotly)
library(stinepack) # for interpolation splines
library("shinyLP")
library("shinyBS")
library("shinythemes")
library("shinyWidgets")
# ----------------------------------------------
# Load the input data
# ----------------------------------------------

modelMetadata <- read.csv("data/ModelList.csv")
kkzRank.includeUKESM <- read.csv("data/kkzRank.includeUKESM.csv")
kkzRank.excludeUKESM <- read.csv("data/kkzRank.excludeUKESM.csv")

# Define the subregions
ipccregions <- readOGR("data/ipccregions.shp")
regions <- c("NAM", ipccregions$Acronym[match(names(kkzRank.includeUKESM)[-1], ipccregions$Acronym)])
region.names <- c("North America", ipccregions$Name[match(names(kkzRank.includeUKESM)[-1], ipccregions$Acronym)])

# Define climate elements
elements <- c("Tmax", "Tmin", "PPT")
element.names <- c("Mean daily maximum temperature (Tmax)", "Mean daily minimum temperature (Tmin)", "Precipitation")
element.names.units <- c(bquote(Mean~daily~bold(maximum)~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(minimum)~temperature~"("*degree*C*")"), "Precipitation (mm)")
variable.names <- read.csv("data/Variables_ClimateBC.csv")

# extract the global climate models and scenarios from an arbitrary file. 
template <- read.csv("data/change.NAM.csv", stringsAsFactors = F)
gcms <- unique(template$gcm[-c(1:2)])
scenarios <- unique(template[-c(1:2),2])
scenario.names <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
proj.years <- unique(template[-c(1:2),3])
proj.year.names <- c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

mods <- substr(gcms, 1, 2)

colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
set.seed(2)
ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(gcms)-12))
ColScheme[11] <- "blue"

# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

seasons <- c("wt", "sp", "sm", "at")
season.names <- c("Winter", "Spring", "Summer", "Autumn")

yeartimes <- c(seasons, monthcodes)
yeartime.names <- c(season.names, month.name)

tab_block <- function(text, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                      <img src="img/',icon,'">
                    </div>
                  </div>
              </div></a>'))
}

# Define UI ----
ui <- fluidPage(
  navbarPage(id="CMIP6-NA", title = "CMIP6 ensemble for ClimateNA", theme = "bcgov.css", 
             
             
             ## -----------------------------------------------------
             ## LANDING PAGE
             
             tabPanel(
               title = "Intro",
               value = "Intro",
               column(width = 12,
                      wellPanel(
                        HTML("<h2><b>CMIP6-NA</b> - The new climate model ensemble for ClimateNA</h2>"),
                        HTML("<h4>This tool provides visualizations and documentation of the global climate model ensemble featured in Version 7 
                                    of ClimateNA. The ensemble is from the new generation of global climate model simulations, the sixth Coupled Model 
                                    Intercomparison Project (CMIP6). Use this tool to learn about the model simulations in ClimateNA and choose a small 
                                    ensemble suited for your research. A similar app with additional features for British Columbia is available at <a href='https://bcgov-env.shinyapps.io/cmip6-BC/' target='_blank'>https://bcgov-env.shinyapps.io/cmip6-BC/</a></h5></h4>")
                      )
               ),
               # column(width = 2, align = "left",
               #        wellPanel(
               #          actionButton("link_to_timeSeries", HTML("<h4><b>Time series</b></h4>")),
               #          HTML("<h5> Compare historical and future model projections against observations,
               #                                    for individual models and customizable ensembles,
               #                                    with and without bias correction.</h5 >")
               #        )
               # ),
               column(width = 4, align = "left",
                      wellPanel(
                        actionButton("link_to_Change", HTML("<h4><b>Choose models</b></h4>")),
                        HTML("<h5>Compare model projections in a two-variable climate space. 
                                    Create smaller ensembles based on predefined or custom criteria.</h5 >")
                      )
               ),
               # column(width = 2, align = "left",
               #        wellPanel(
               #          actionButton("link_to_Bias", HTML("<h4><b>Assess Bias</b></h4>")),
               #          HTML("<h5>Assess model biases relative to historical observations.</h5 >")
               #        )
               # ),
               column(width = 4, align = "left",
                      wellPanel(
                        actionButton("link_to_Maps", HTML("<h4><b>Maps</b></h4>")),
                        HTML("<h5>Compare spatial variation in climate change among models. </h5 >")
                      )
               ),
               # column(width = 2, align = "left",
               #        wellPanel(
               #          actionButton("link_to_Guidance", HTML("<h4><b>Guidance</b></h4>")),
               #          HTML("<h5>Guidance for selecting models, emissions scenarios, and time periods. </h5 >")
               #        )
               # ),
               column(width = 12,
                      HTML("<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u> <br>
                            Mahony, C.R., T. Wang, A. Hamann, and A.J. Cannon. 2021. <a href='https://eartharxiv.org/repository/view/2510/' target='_blank'>A CMIP6 ensemble for downscaled monthly climate normals over North America</a>. EarthArXiv. <a href='https://doi.org/10.31223/X5CK6Z' target='_blank'>https://doi.org/10.31223/X5CK6Z</a> </h5>
                            <h4><b>Contributors</b></h4>
                            <h5> <u>App created by:</u><br>
                                 Colin Mahony<br>
                                 Research Climatologist<br>
                                 BC Ministry of Forests, Lands, Natural Resource Operations and Rural Development<br>
                                 colin.mahony@gov.bc.ca<br>
                               <br>
                               CMIP6 data downloaded and subsetted by Tongli Wang, Associate Professor at the UBC Department of Forest and Conservation Sciences.<br></h5>
                            <h4><b>Code</b></h4>
                            <h5> The code and data for this tool are available at <a href='https://github.com/bcgov/cmip6-NA-eval' target='_blank'>https://github.com/bcgov/cmip6-NA-eval</a></h5>
                               <br>")
                      
               ),
               column(width = 12,
                      style = "background-color:#003366; border-top:2px solid #fcba19;",
                      
                      tags$footer(class="footer",
                                  tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                           tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                   tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                           )
                                  )
                      )
               )
             ),
             
             ## -----------------------------------------------------
             ## ABOUT
             
             tabPanel("About",
                      
                      includeMarkdown("about.Rmd"),
                      
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             

             
             ## -----------------------------------------------------
             ## CHANGE
             
             tabPanel("Choose models", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This tab shows the amount of change projected by each model, relative to the 1961-1990 period.
                                   You can use this tab to reduce the ensemble size base on predefined or custom model selection methods; 
                                   see the 'About' tab for more information on the predefined ensembles. 
                                   Click on a legend item to hide it; double-click to isolate it. 
                                   Drag a box on the plot to zoom in; double-click the plot to zoom back out."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                          
                          
                          radioButtons("modeChange", "Ensemble selection mode",
                                       choiceNames = c("Predefined", "Custom"),
                                       choiceValues = c("Predefined", "Custom"),
                                       selected = "Predefined",
                                       inline = T),
                          
                          conditionalPanel(
                            condition = "input.modeChange == 'Predefined'",
                            
                            checkboxInput("includeUKESM", label = "Include UKESM1 in small ensembles (<9 models)", value = T),
                            
                            sliderInput("kkzN", label = "Reduce ensemble size in predefined order", min = 1, max = 13, value = 13, step=1),
                          ),
                          
                          
                          conditionalPanel(
                            condition = "input.modeChange == 'Custom'",
                            
                            checkboxGroupInput("gcms.change", "Choose global climate models:",
                                               choiceNames = gcms,
                                               choiceValues = gcms,
                                               selected = gcms,
                                               inline = T
                            ),
                          ),
                          
                          radioButtons("proj.year.change", inline = TRUE,
                                       label = "Choose a time slice",
                                       choiceNames = proj.year.names,
                                       choiceValues = proj.years,
                                       selected = proj.years[3]),
                          
                          radioButtons("scenario.change", "Choose emissions scenario",
                                       choiceNames = scenario.names,
                                       choiceValues = scenarios,
                                       selected = scenarios[2],
                                       inline = T),
                          
                          checkboxInput("trajectories", label = "Include model trajectories", value = T),
                          
                          selectInput("element1.change",
                                      label = "x-axis: choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[1]),
                          
                          selectInput("yeartime1.change",
                                      label = "x-axis: Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                          selectInput("element2.change",
                                      label = "y-axis: choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[3]),
                          
                          selectInput("yeartime2.change",
                                      label = "y-axis: Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                          selectInput("region.name.change",
                                      label = "Choose a region",
                                      choices = as.list(region.names),
                                      selected = region.names[1]),
                          
                          img(src = "ipccregions.png", height = 1861*1/5, width = 1600*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotlyOutput(outputId = "ChangePlot", height="600px"),
                          downloadButton(outputId = "downloadData_change", label = "Download data")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             

             
             ## -----------------------------------------------------
             ## Maps TAB
             
             tabPanel("Maps", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("These maps show the spatial pattern of simulated climate change for each model relative to the 1961-1990 period. 
                                   All maps are derived from raw GCM files.
                                   Temperature units (K) are Kelvins, which are equivalent to degrees Celsius. 
                                   "),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          radioButtons("areaMap", inline = F,
                                       label = "Choose the zoom level",
                                       choices = c("North America", "Pacific Northwest"),
                                       selected = "North America"),
                          
                          radioButtons("mapType", inline = F,
                                       label = "Choose the map type",
                                       choices = c("Climate change", "Topography"),
                                       selected = "Climate change"),
                          
                          
                          conditionalPanel(
                            condition = "input.mapType == 'Climate change'",
                            
                            radioButtons("elementMap", inline = F,
                                         label = "Choose the climate element",
                                         choiceNames = as.list(element.names),
                                         choiceValues = as.list(elements),
                                         selected = elements[1]),
                            
                            radioButtons("seasonsOrMonths", "Months or Seasons",
                                         choiceNames = c("Months", "Seasons"),
                                         choiceValues = c("Months", "Seasons"),
                                         selected = "Seasons",
                                         inline = T),
                            
                            conditionalPanel(
                              condition = "input.seasonsOrMonths == 'Seasons'",
                              
                              radioGroupButtons(
                                inputId = "seasonbuttons",
                                label = "Choose a season",
                                choices = season.names, 
                                selected = season.names[3]
                              ),
                              
                              conditionalPanel(
                                condition = "input.areaMap == 'North America'",
                                
                                sliderTextInput("proj.year.map", 
                                                label = "Choose a time slice", 
                                                choices = proj.year.names, 
                                                selected = proj.year.names[3]),
                                
                                sliderTextInput("scenario.map", 
                                                label = "Choose emissions scenario", 
                                                choices = scenario.names, 
                                                selected = scenario.names[2]),
                                
                               ),
                              
                              conditionalPanel(
                                condition = "input.areaMap == 'Pacific Northwest'",
                                
                                radioButtons("proj.year.map.fixed1", inline = TRUE,
                                             label = "time slice",
                                             choiceNames = proj.year.names[3],
                                             choiceValues = proj.years[3],
                                             selected = proj.years[3]),
                                
                                radioButtons("scenario.map.fixed1", "emissions scenario",
                                             choiceNames = scenario.names[2],
                                             choiceValues = scenarios[2],
                                             selected = scenarios[2],
                                             inline = T)
                                
                              ),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.seasonsOrMonths == 'Months'",
                              
                              sliderTextInput("monthslider", 
                                              label = "Choose a month", 
                                              choices = month.abb, 
                                              selected = month.abb[7]),
                              
                              radioButtons("proj.year.map.fixed2", inline = TRUE,
                                           label = "time slice",
                                           choiceNames = proj.year.names[3],
                                           choiceValues = proj.years[3],
                                           selected = proj.years[3]),
                              
                              radioButtons("scenario.map.fixed2", "emissions scenario",
                                           choiceNames = scenario.names[2],
                                           choiceValues = scenarios[2],
                                           selected = scenarios[2],
                                           inline = T)
                              
                            )
                          )
                        ),    
                        
                        mainPanel(
                          
                          imageOutput("changeMap", width="100%", height="100%")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             ## -----------------------------------------------------
             ## MODEL INFO
             
             tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  observeEvent(input$link_to_Change, {
    updateNavbarPage(session, "CMIP6-NA", selected="Choose models")
  })
  
  observeEvent(input$link_to_Maps, {
    updateNavbarPage(session, "CMIP6-NA", selected="Maps")
  })
  
  # This is the gcm selection for the time series plot. done as a renderUI to allow the reset button
  output$reset_gcms <- renderUI({
    times <- input$reset_input
    div(id=letters[(times %% length(letters)) + 1],
        checkboxGroupInput("gcms.ts2", "Choose global climate models:",
                           choiceNames = gcms,
                           choiceValues = gcms,
                           selected = gcms[select],
                           inline = T
        )
    )
  })
  

  
  output$ChangePlot <- renderPlotly({

    # region <- regions[1]
    # yeartime1 <- yeartimes[1]
    # yeartime2 <- yeartimes[1]
    # element1 <- elements[1]
    # element2 <- elements[4]
    # proj.year <- proj.years[3]
    # scenario <- scenarios[2]
    # gcms.change <- gcms

    # observe(updateCheckboxGroupInput(session, "gcms.change", selected = gcms[which(gcms%in%kkzRank[1:input$kkzN,which(region.names==input$region.name.change)])]))

    region <- regions[which(region.names==input$region.name.change)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1.change)]
    yeartime2 <- yeartimes[which(yeartime.names==input$yeartime2.change)]
    element1 <- elements[which(element.names==input$element1.change)]
    element2 <- elements[which(element.names==input$element2.change)]
    proj.year <- input$proj.year.change
    scenario <- input$scenario.change
    if(input$modeChange=="Predefined"){
      if(input$includeUKESM==T){
        gcms.change <- gcms[which(gcms%in%kkzRank.includeUKESM[1:input$kkzN,which(region.names==input$region.name.change)])]
      } else {
        gcms.change <- gcms[which(gcms%in%kkzRank.excludeUKESM[1:input$kkzN,which(region.names==input$region.name.change)])]
      }
    } else {
      gcms.change <- input$gcms.change
    }

    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")

    data <- read.csv(paste("data/change", region, "csv", sep="."))

    x <- data[, which(names(data)==variable1)]
    y <- data[, which(names(data)==variable2)]
    x0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable1)]
    y0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable2)]
    x.mean <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms.change), which(names(data)==variable1)])
    y.mean <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms.change), which(names(data)==variable2)])
    x.mean.ClimateBC <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms), which(names(data)==variable1)])
    y.mean.ClimateBC <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms), which(names(data)==variable2)])

    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)

    #initiate the plot
    fig <- plot_ly(x=x,y=y, type = 'scatter', mode = 'markers', marker = list(color ="lightgrey", size=5), hoverinfo="none", color="All models/scenarios/times")

    fig <- fig %>% layout(xaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable1)]),
                                       range=xlim),
                          yaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable2)]),
                                       range=ylim)
    )

    fig <- fig %>% add_markers(x=x0,y=y0, color="Observed (2001-2020)", text="Observed\n(2001-2020)", hoverinfo="text",
                               marker = list(size = 25,
                                             color = "grey"))

    fig <- fig %>% add_markers(x=x.mean,y=y.mean, color="Custom ensemble mean", text="Custom ensemble mean", hoverinfo="text",
                               marker = list(size = 20,
                                             color = "grey", symbol = 3))

    fig <- fig %>% add_markers(x=x.mean.ClimateBC,y=y.mean.ClimateBC, color="ClimateNA 13-model mean", text="ClimateNA 13-model mean", hoverinfo="text",
                               marker = list(size = 20,
                                             color = "black", symbol = 103))

    gcm=gcms.change[3]
    for(gcm in gcms.change){
      i=which(gcms==gcm)
      x1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable1)]
      y1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable2)]
      x2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable1)]
      y2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable2)]

      if(input$trajectories==T){
        if(length(unique(sign(diff(x2))))==1){
          x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
          y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
          s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/1500)) # way better than interpSpline, not prone to oscillations
          fig <- fig %>% add_trace(x=s$x, y=s$y, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i], width = 2, dash = 'dash'), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
          limit <- if(unique(sign(diff(x2)))==-1) which(s$x>x1) else which(s$x<x1)
          fig <- fig %>% add_trace(x=s$x[limit], y=s$y[limit], type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        } else {
          fig <- fig %>% add_trace(x=x2, y=y2, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i], width = 2, dash = 'dash'), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
          limit <- c(1, (which(proj.years <= proj.year)+1))
          fig <- fig %>% add_trace(x=x2[limit], y=y2[limit], type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        }
        fig <- fig %>% add_markers(x=x2,y=y2, color=gcms[i], text=gcms[i], hoverinfo="text",
                                   marker = list(size = 8,
                                                 color = ColScheme[i]),
                                   legendgroup=paste("group", i, sep=""), showlegend = FALSE)
      }

      fig <- fig %>% add_markers(x=x1,y=y1, color=gcms[i],
                                 marker = list(size = 20,
                                               color = ColScheme[i],
                                               line = list(color = "black",
                                                           width = 1)),
                                 legendgroup=paste("group", i, sep=""))

      fig <- fig %>% add_annotations(x=x1,y=y1, text = sprintf("<b>%s</b>", mods[i]), xanchor = 'center', yanchor = 'center', showarrow = F,
                                     legendgroup=paste("group", i, sep="")    )

    }

    if(element1=="PPT") fig <- fig %>% layout(xaxis = list(tickformat = "%"))
    if(element2=="PPT") fig <- fig %>% layout(yaxis = list(tickformat = "%"))

    fig

  }
  )

  # Downloadable csv of selected dataset ----
  data_change <- reactive(read.csv(paste("data/change", regions[which(region.names==input$region.name.change)], "csv", sep=".")))

  output$downloadData_change <- downloadHandler(

    filename = function() {
      paste("CMIP6NA.change", regions[which(region.names==input$region.name.change)], "csv", sep=".")
    },
    content = function(file) {
      write.csv(data_change(), file, row.names = FALSE)
    }
  )

  output$changeMap <- renderImage({

    if(input$mapType=="Topography"){
      if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("Orography.PNW.png",sep=".")))
      } else {filename <- normalizePath(file.path('./www', paste("Orography.NorthAmerica.png",sep=".")))}
    }

    if(input$mapType=="Climate change"){
      yeartimeMap <- if(input$seasonsOrMonths=="Seasons") seasons[which(season.names==input$seasonbuttons)] else monthcodes[which(month.abb==input$monthslider)]

      if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("changeMap", input$elementMap, yeartimeMap, "png",sep=".")))
      } else { if(input$seasonsOrMonths == "Seasons"){
        filename <- normalizePath(file.path('./www', paste("changeMap.NorAm", input$elementMap, yeartimeMap, scenarios[which(scenario.names==input$scenario.map)], proj.years[which(proj.year.names==input$proj.year.map)] , "png",sep=".")))
      } else {
        filename <- normalizePath(file.path('./www', paste("changeMap.NorAm", input$elementMap, yeartimeMap, "png",sep=".")))
      }
      }
    }

    list(src = filename, width="100%", height="100%")

  }, deleteFile = FALSE)

  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata,
                  options = list(pageLength = dim(modelMetadata)[1]),
                  rownames= FALSE,
                  caption = HTML("<p><h4><b>Information about CMIP6 models featured in this app.</b>
                                 ECS is equilibrium climate sensitivity (long-term temperature change in response to an instant doubling of CO2), and values are quoted from <a href='https://advances.sciencemag.org/content/6/26/eaba1981.abstract' target='_blank'>Meehl et al. (2020)</a>.
                                 Grid resolutions are in degrees of latitude and longitude. The last five columns are the number of model runs for each scenario that are included in ClimateNA and this app</p></h4>")
    )
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)


