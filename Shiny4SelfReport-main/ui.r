## ==================================================================================== ##
#
# Copyright (C) 2021  Eurico Noleto-Filho
# 
# Shiny4SelfReport is a free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# You may contact the author of this code, Eurico Noleto-Filho, at <euriconoleto@hotmail.com>
## ==================================================================================== ##




library(shiny)
library(rdrop2)
library(shinymaterial)
library(DT)
library(shinyalert)
library(readtext)
library(data.table)
library(shinycssloaders)
library(rclipboard)
library(shinyjs)
library(imager)



# Specify which information is uploaded to the dropbox

fields <-
    c(
        "User_ID",
        "Vessel_ID",
        "Date",
        "Days_fishing",
        "Species1_1",
        "Species1_2",
        "Species1_3",
        "Species1_4",
        "Species2_1",
        "Species2_2",
        "Species2_3",
        "Species2_4",
        "Species3_1",
        "Species3_2",
        "Species3_3",
        "Species3_4"
    )


# Diretories created in dropbox

outputDir <- "responses/csv"
outputDir2 <- "responses/imgs"
drop_auth()


# Starts the user interface

shinyUI( fluidPage(
    title = "Triatlas",
    useShinyalert(),
    useShinyjs(),
    rclipboardSetup(),
    material_page(
        include_nav_bar = FALSE,
        
        
        
        ## CSS styling
        
        tags$head(
            tags$style(type = "text/css", "#img img {max-width: 100%; width: 100%; height: 100%}")
        ),
        tags$head(includeScript("sw.js")),
        tags$script(
            HTML(
                "if ('serviceWorker' in navigator) { navigator.serviceWorker.register('/sw.js', { scope: '/' }).then(function(registration) { console.log('Service Worker Registered'); }); navigator.serviceWorker.ready.then(function(registration) { console.log('Service Worker Ready'); }); }"
            )
        ),
        tags$head(
            tags$style(
                type = "text/css",
                "#code11 {font-size: 12px; font-family: calibri light; background-color: rgba(255,255,255,0.40); color: black; border-style: none;}"
            ),
            tags$style(
                type = "text/css",
                "#code22 {font-size: 12px; font-family: calibri light; background-color: rgba(255,255,255,0.40); color: black; border-style: none;}"
            ),
            
            ### CARD title properties
            
            tags$style(type = "text/css",
                       ".card .card-title {
          font-size: 22px;
          font-weight:300
        }"),
        
        ### Size of the slider ball numbers
        
        tags$style(
            type = "text/css",
            "input[type=range] + .thumb.active .value {
          color: #fff;
            margin-left: -2px;
          margin-top: 6px;
          font-size:16px
        }"
        ),
        
        ### Size of the slider ball
        
        tags$style(
            type = "text/css",
            "input[type=range]::-webkit-slider-thumb {
          border: none;
          height: 25px;
          width: 25px;
          border-radius: 50%;
          background: #26a69a;
            -webkit-transition: -webkit-box-shadow .3s;
          transition: -webkit-box-shadow .3s;
          transition: box-shadow .3s;
          transition: box-shadow .3s, -webkit-box-shadow .3s;
          -webkit-appearance: none;
          background-color: #26a69a;
            -webkit-transform-origin: 50% 50%;
          transform-origin: 50% 50%;
          margin: -12px 0 0 0
        }"
        )
        ),
        
        ### PWA Configuration
        
        tags$head(
            tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
            tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "https://i.ibb.co/WKXm5cf/apple-touch-icon.png"),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "32x32",
                href = "https://i.ibb.co/7rwYtgm/favicon-32x32.png"
            ),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "16x16",
                href = "https://i.ibb.co/5GwHXH3/favicon-16x16.png"
            ),
            
            
            
            tags$link(href = "https://i.ibb.co/0XPvWHr/iphone5-splash.png", media =
                          "(device-width: 320px) and (device-height: 568px) and (-webkit-device-pixel-ratio: 2)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/x8sv8nq/iphone6-splash.png", media =
                          "(device-width: 375px) and (device-height: 667px) and (-webkit-device-pixel-ratio: 2)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/vknxBFT/iphoneplus-splash.png", media =
                          "(device-width: 621px) and (device-height: 1104px) and (-webkit-device-pixel-ratio: 3)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/h1ZKyn9/iphonex-splash.png", media =
                          "(device-width: 375px) and (device-height: 812px) and (-webkit-device-pixel-ratio: 3)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/XXw3rr7/ipad-splash.png", media =
                          "(device-width: 768px) and (device-height: 1024px) and (-webkit-device-pixel-ratio: 2)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/zZf4wkq/ipadpro1-splash.png", media =
                          "(device-width: 834px) and (device-height: 1112px) and (-webkit-device-pixel-ratio: 2)", rel =
                          "apple-touch-startup-image"),
            tags$link(href = "https://i.ibb.co/vhLB8cY/ipadpro2-splash.png", media =
                          "(device-width: 1024px) and (device-height: 1366px) and (-webkit-device-pixel-ratio: 2)", rel =
                          "apple-touch-startup-image")
            
            
        ),
        
        uiOutput("img") %>% withSpinner(type = 6, color = "lightblue"),
        material_tabs(
            color = "blue",
            tabs = c("Questions" = "f_tab", "About" = "w_tab")
        ),
        material_tab_content(
            "w_tab",
            conditionalPanel(
                condition = "input.User_ID == 'developer'",
                material_card(
                    "Developer Session",
                    material_tabs(
                        color = "blue",
                        tabs = c("Inputs" = "ip_tab", "Code" = "cod_tab")
                    ),
                    material_tab_content(
                        "ip_tab",
                        material_card(
                            textInput("nametype0", label = "", value = "User Information"),
                            checkboxInput("input_type0", "Add User INFO BOX", value = TRUE),
                            material_row(
                                material_column(material_card(
                                    checkboxInput("input_type00", "Add Text Input", value = TRUE),
                                    textInput("input_type00_name", "Name the text input", value = "User ID")
                                )),
                                material_column(material_card(
                                    checkboxInput("input_type000", "Add Text Input 2", value = TRUE),
                                    textInput("input_type000_name", "Name the text input", value = "Vessel ID")
                                ))
                            )
                        ),
                        material_card(
                            textInput("nametype3", label = "", value = "Period & Effort"),
                            checkboxInput("input_type3", "Add Period & Effort", value = TRUE),
                            material_row(
                                material_column(material_card(
                                    checkboxInput("input_type33", "Add Date Input", value = TRUE),
                                    textInput("input_type33_name", "Name the Date input", value = "Date")
                                )),
                                material_column(material_card(
                                    checkboxInput("input_type333", "Add Effort Input", value = TRUE),
                                    textInput("input_type333_name", "Name the Effort input", value = "Days fishing")
                                )),
                                
                            )
                        ),
                        material_card(
                            textInput("namespecies", label = "", value = "Species Abundance and Composition"),
                            checkboxInput("namespecies0", "Add Species Abundance and Composition", value = TRUE),
                            material_card(
                                checkboxInput("input_type1", "Add species 1", value = TRUE),
                                material_row(
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type11", "Add input 1", value = TRUE),
                                            selectInput(
                                                "input_type111",
                                                "Choose input1",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type1111_name", "Input Name", value = "Species 1 in kg")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type11a", "Add input 2", value = TRUE),
                                            selectInput(
                                                "input_type111a",
                                                "Choose input2",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type1111a_name", "Input Name", value = "Catch Value in R$/Kg")
                                        ))
                                        
                                    )),
                                    
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type11b", "Add input 3", value = TRUE),
                                            selectInput(
                                                "input_type111b",
                                                "Choose input3",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Photo"
                                            ),
                                            textInput("input_type1111b_name", "Input Name", value = "Photo")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type11c", "Add input 4", value = FALSE),
                                            selectInput(
                                                "input_type111c",
                                                "Choose input4",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slide"
                                            ),
                                            textInput("input_type1111c_name", "Input Name", value = "Slide")
                                        ))
                                        
                                    ))
                                )
                            ),
                            
                            material_card(
                                checkboxInput("input_type2", "Add species 2", value = TRUE),
                                material_row(
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type22", "Add input 1", value = TRUE),
                                            selectInput(
                                                "input_type222",
                                                "Choose input1",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type2222_name", "Input Name", value = "Species 2 in kg")
                                        )),
                                        
                                        material_column(material_card(
                                            checkboxInput("input_type22a", "Add input 2", value = TRUE),
                                            selectInput(
                                                "input_type222a",
                                                "Choose input2",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type2222a_name", "Input Name", value = "Catch Value in R$/Kg")
                                        ))
                                    )),
                                    
                                    material_row(material_column(
                                        material_column(material_card(
                                            checkboxInput("input_type22b", "Add input 3", value = TRUE),
                                            selectInput(
                                                "input_type222b",
                                                "Choose input3",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Photo"
                                            ),
                                            textInput("input_type2222b_name", "Input Name", value = "Photo")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type22c", "Add input 4", value = FALSE),
                                            selectInput(
                                                "input_type222c",
                                                "Choose input4",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slide"
                                            ),
                                            textInput("input_type2222c_name", "Input Name", value = "Slide")
                                        ))
                                        
                                    ))
                                )
                            ),
                            
                            material_card(
                                checkboxInput("input_type5", "Add species 3", value = TRUE),
                                material_row(
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type55", "Add input 1", value = TRUE),
                                            selectInput(
                                                "input_type555",
                                                "Choose input1",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type5555_name", "Input Name", value = "Species 3 in kg")
                                        )),
                                        
                                        material_column(material_card(
                                            checkboxInput("input_type55a", "Add input 2", value = TRUE),
                                            selectInput(
                                                "input_type555a",
                                                "Choose input2",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type5555a_name", "Input Name", value = "Catch Value in R$/Kg")
                                        ))
                                    )),
                                    
                                    material_row(material_column(
                                        material_column(material_card(
                                            checkboxInput("input_type55b", "Add input 3", value = TRUE),
                                            selectInput(
                                                "input_type555b",
                                                "Choose input3",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Photo"
                                            ),
                                            textInput("input_type5555b_name", "Input Name", value = "Photo")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type55c", "Add input 4", value = FALSE),
                                            selectInput(
                                                "input_type555c",
                                                "Choose input4",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slide"
                                            ),
                                            textInput("input_type5555c_name", "Input Name", value = "Slide")
                                        ))
                                        
                                    ))
                                )
                            ),
                            
                            material_card(
                                checkboxInput("input_type6", "Add species 4", value = FALSE),
                                material_row(
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type66", "Add input 1", value = TRUE),
                                            selectInput(
                                                "input_type666",
                                                "Choose input1",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type6666_name", "Input Name", value = "Species 4 in kg")
                                        )),
                                        
                                        material_column(material_card(
                                            checkboxInput("input_type66a", "Add input 2", value = TRUE),
                                            selectInput(
                                                "input_type666a",
                                                "Choose input2",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type6666a_name", "Input Name", value = "Catch Value in R$/Kg")
                                        ))
                                    )),
                                    
                                    material_row(material_column(
                                        material_column(material_card(
                                            checkboxInput("input_type66b", "Add input 3", value = TRUE),
                                            selectInput(
                                                "input_type666b",
                                                "Choose input 3",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Photo"
                                            ),
                                            textInput("input_type6666b_name", "Input Name", value = "Photo")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type66c", "Add input 4", value = FALSE),
                                            selectInput(
                                                "input_type666c",
                                                "Choose input 4",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slide"
                                            ),
                                            textInput("input_type6666c_name", "Input Name", value = "Slide")
                                        ))
                                        
                                    ))
                                )
                            ),
                            
                            material_card(
                                checkboxInput("input_type7", "Add species 5", value = FALSE),
                                material_row(
                                    material_column(material_row(
                                        material_column(material_card(
                                            checkboxInput("input_type77", "Add input 1", value = TRUE),
                                            selectInput(
                                                "input_type777",
                                                "Choose input1",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type7777_name", "Input Name", value = "Species 5 in kg")
                                        )),
                                        
                                        material_column(material_card(
                                            checkboxInput("input_type77a", "Add input 2", value = TRUE),
                                            selectInput(
                                                "input_type777a",
                                                "Choose input2",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slider"
                                            ),
                                            textInput("input_type7777a_name", "Input Name", value = "Catch Value in R$/Kg")
                                        ))
                                    )),
                                    
                                    material_row(material_column(
                                        material_column(material_card(
                                            checkboxInput("input_type77b", "Add input 3", value = TRUE),
                                            selectInput(
                                                "input_type777b",
                                                "Choose input 3",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Photo"
                                            ),
                                            textInput("input_type7777b_name", "Input Name", value = "Photo")
                                        )),
                                        material_column(material_card(
                                            checkboxInput("input_type77c", "Add input 4", value = FALSE),
                                            selectInput(
                                                "input_type777c",
                                                "Choose input 4",
                                                choices = c("Slider", "Text", "Photo"),
                                                selected = "Slide"
                                            ),
                                            textInput("input_type7777c_name", "Input Name", value = "Slide")
                                        ))
                                        
                                    ))
                                )
                            )
                            
                            
                            
                        )
                    ),
                    
                    material_tab_content("cod_tab",
                                         tabsetPanel(
                                             tabPanel("Client Code",
                                                      material_card(
                                                          uiOutput("clip"),
                                                          verbatimTextOutput("code11")
                                                      )),
                                             tabPanel(
                                                 "Developer Code",
                                                 material_card(uiOutput("clip2"),
                                                               verbatimTextOutput("code22"))
                                             )
                                         ))
                )
            ),
            
            material_card(
                "The Project",
                p(
                    HTML(
                        "Shiny4SelfReport is an application encoded to provide a tool for self-reporting data in small-scale fisheries."
                    )
                ),
                br(),
                p(
                    HTML(
                        "The app is part of the <b>TRIATLAS project: South and Tropical Atlantic
    Climate-Based Marine Ecosystem Prediction for Sustainable Management (<a>https://triatlas.w.uib.no</a>).</b> This project has
    received funding from the European Union’s Horizon 2020
            research and innovation programme under grant agreement No 817578."
                    )
                ),
    br(),
    p(HTML("<b>Authors:</b>")),
    br(),
    p(HTML(
        "Dr. Eurico Noleto-Filho - euriconoleto@hotmail.com"
    )),
    
    p(HTML(
        "Dr. Ronaldo Angelini - ronangelini@gmail.com"
    )),
    
    p(HTML(
        "Dr. Adriana Carvalho - acarvalho.ufrn@gmail.com"
    )),
    br(),
    p(
        HTML(
            "<b>Disclaimer</b><br>The content of this website does not reflect the official opinion of the European Union.
            Responsibility for the information and views expressed in the web lies entirely with
            the authors..</b>"
        )
    )
    
    
            ),
    material_card("Terms of use",
                  p(
                      HTML(
                          "
Copyright (C) 2021 Eurico Noleto-Filho <br>
<br>
Shiny4SelfReport is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version. This program is distributed
in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program.
If not, see http://www.gnu.org/licenses/ <br>
<br>
You may contact the author of this code, Eurico Noleto-Filho, at <b>euriconoleto@hotmail.com</b> <br>
<br>

Shinyapps.io TERMS OF USE <br>
<br>
This application is hosted on a Shinyapps.io server https://www.shinyapps.io/.
By using this app you are agreeing to the terms of use as described by Shinnyaps.io:
https://www.rstudio.com/about/shinyapps-terms-use/<br>
<br>
We (the authors and maintainers of this app) will not save your data on our servers.
The Shinyapps server is not HIPAA (the Health Insurance Portability and Accountability Act)
compliant, you must not upload protected health information or confidential data with this app.
You may instead download the code and run the app locally on your private computer and network.
We are not responsible for the confidentiality, availability, security, loss, misuse
or misappropriation of any data you submit to this application. <br>
<br>
TRIATLAS Researchers will treat personal data according to the General Data Protection Regulations (GDPR),
European Regulation (EU) 2016/679. TRIATLAS does not accept any responsibility for modification made by third
parties and their treatment of personal data.


                           "
                      )
                  ))
        ),

material_tab_content(
    "f_tab",
    uiOutput("ui1") %>% withSpinner(type = 6, color = "lightblue"),
    uiOutput("ui2") %>% withSpinner(type = 6, color = "lightblue"),
    uiOutput("ui3") %>% withSpinner(type = 6, color = "lightblue"),
    
    
    actionButton("submit", "Submit"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
)
    )
)

)
