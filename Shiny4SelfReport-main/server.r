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




#Server

shinyServer(function(input, output, session) {
  
  #Export code session
  
   ##Pre-defined (fixed) code
  
    output$code22 <- renderText({
        d <- readtext("mydata2.txt")
        paste0(d)
    })
    output$code11 <- renderText({
        d <- readtext("code1.txt")
        d4 <- readtext("code4.txt")
        d5 <- readtext("code5.txt")
        d6 <- readtext("code6.txt")
        
   ##Non-defined code (Code users can customize)
        
        if (input$input_type0) {
            d7 <- c("material_tab_content('f_tab',",
                    "material_card('",
                    input$nametype0,
                    "'),")
            
            if (input$input_type00) {
                d7 <- c(
                    "material_card('",
                    input$nametype0,
                    "',",
                    "textInput('User_ID','",
                    input$input_type00_name,
                    "')),"
                )
                if (input$input_type000) {
                    d7 <- c(
                        "material_card('",
                        input$nametype0,
                        "',",
                        "textInput('User_ID','",
                        input$input_type00_name,
                        "'), textInput('Vessel_ID','",
                        input$input_type000_name,
                        "')),"
                    )
                }
            }
            
            if (input$input_type000) {
                d7 <- c(
                    "material_card('",
                    input$nametype0,
                    "',",
                    "textInput('Vessel_ID','",
                    input$input_type000_name,
                    "')),"
                )
                if (input$input_type00) {
                    d7 <- c(
                        "material_card('",
                        input$nametype0,
                        "',",
                        "textInput('User_ID','",
                        input$input_type00_name,
                        "'), textInput('Vessel_ID','",
                        input$input_type000_name,
                        "')),"
                    )
                }
            }
            
            
            
            
        } else {
            d7 <- ("")
        }
        
        if (input$input_type3) {
            d8 <- c("material_card('",
                    input$nametype3,
                    "'),")
            
            if (input$input_type33) {
                d8 <- c(
                    "material_card('",
                    input$nametype3,
                    "',",
                    "dateInput('Date','",
                    input$input_type33_name,
                    "')),"
                )
                if (input$input_type333) {
                    d8 <- c(
                        "material_card('",
                        input$nametype3,
                        "',",
                        "dateInput('Date','",
                        input$input_type33_name,
                        "'), material_slider('Days_fishing','",
                        input$input_type333_name,
                        "', 0, 100, initial_value = 0)),"
                    )
                }
            }
            
            if (input$input_type333) {
                d8 <- c(
                    "material_card('",
                    input$nametype3,
                    "',",
                    "material_slider('Days_fishing','",
                    input$input_type333_name,
                    "', 0, 100, initial_value = 0)),"
                )
                if (input$input_type33) {
                    d8 <- c(
                        "material_card('",
                        input$nametype3,
                        "',",
                        "dateInput('Date','",
                        input$input_type33_name,
                        "'), material_slider('Days_fishing','",
                        input$input_type333_name,
                        "', 0, 100, initial_value = 0)),"
                    )
                }
            }
            
            
            
        }
        else {
            d8 <- ("")
        }
        
        if (input$input_type1) {
            d91 <- c("material_card('",
                     input$namespecies,
                     "',material_card(''")
            d92 <- c("))")
            if (input$input_type2) {
                d92 <- c(")")
            }
            if (input$input_type3) {
                d92 <- c(")")
            }
            if (input$input_type11) {
                if (input$input_type111 == "Slider") {
                    d93 <- c(
                        ",material_slider('Species1_1','",
                        input$input_type1111_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d93_1 <- ("")
                }
                if (input$input_type111 == "Text") {
                    d93 <- c(",material_text_box('Species1_1','",
                             input$input_type1111_name,
                             "')")
                    d93_1 <- ("")
                }
                if (input$input_type111 == "Photo") {
                    d93 <- c(
                        ",fileInput(inputId = 'Species1_1', label ='",
                        input$input_type1111_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d93_1 <- c(
                        "
            observe({
          if (is.null(input$Species1_1)) {
            return(NULL)}
            else{
      files1_1=file.rename(input$Species1_1$datapath, paste0(Sys.time(),'1_1.png'))
      files1_1 = paste0(Sys.time(),'1_1.png')
      drop_upload(files1_1, path = outputDir2)}
                      })"
                    )
                }
            } else{
                d93 <- ("")
                d93_1 <- ("")
            }
            if (input$input_type11a) {
                if (input$input_type111a == "Slider") {
                    d94 <- c(
                        ",material_slider('Species1_2','",
                        input$input_type1111a_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d94_1 <- ("")
                }
                if (input$input_type111a == "Text") {
                    d94 <- c(",material_text_box('Species1_2','",
                             input$input_type1111a_name,
                             "')")
                    d94_1 <- ("")
                }
                if (input$input_type111a == "Photo") {
                    d94 <- c(
                        ",fileInput(inputId = 'Species1_2', label ='",
                        input$input_type1111a_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d94_1 <- c(
                        "
            observe({
          if (is.null(input$Species1_2)) {
            return(NULL)}
            else{
      files1_2=file.rename(input$Species1_2$datapath, paste0(Sys.time(),'1_2.png'))
      files1_2 = paste0(Sys.time(),'1_2.png')
      drop_upload(files1_2, path = outputDir2)}
                      })"
                    )
                }
            } else{
                d94 <- ("")
                d94_1 <- ("")
            }
            if (input$input_type11b) {
                if (input$input_type111b == "Slider") {
                    d95 <- c(
                        ",material_slider('Species1_3','",
                        input$input_type1111b_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d95_1 <- ("")
                }
                if (input$input_type111b == "Text") {
                    d95 <- c(",material_text_box('Species1_3','",
                             input$input_type1111b_name,
                             "')")
                    d95_1 <- ("")
                }
                if (input$input_type111b == "Photo") {
                    d95 <- c(
                        ",fileInput(inputId = 'Species1_3', label ='",
                        input$input_type1111b_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d95_1 <- c(
                        "
            observe({
          if (is.null(input$Species1_3)) {
            return(NULL)}
            else{
      files1_3=file.rename(input$Species1_3$datapath, paste0(Sys.time(),'1_3.png'))
      files1_3 = paste0(Sys.time(),'1_3.png')
      drop_upload(files1_3, path = outputDir2)}
                      })"
                    )
                }
            } else{
                d95 <- ("")
                d95_1 <- ("")
            }
            if (input$input_type11c) {
                if (input$input_type111c == "Slider") {
                    d96 <- c(
                        ",material_slider('Species1_4','",
                        input$input_type1111c_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d96_1 <- ("")
                }
                if (input$input_type111c == "Text") {
                    d96 <- c(",material_text_box('Species1_4','",
                             input$input_type1111c_name,
                             "')")
                    d96_1 <- ("")
                }
                if (input$input_type111c == "Photo") {
                    d96 <- c(
                        ",fileInput(inputId = 'Species1_4', label ='",
                        input$input_type1111c_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d96_1 <- c(
                        "
            observe({
          if (is.null(input$Species1_4)) {
            return(NULL)}
            else{
      files1_4=file.rename(input$Species1_4$datapath, paste0(Sys.time(),'1_4.png'))
      files1_4 = paste0(Sys.time(),'1_4.png')
      drop_upload(files1_4, path = outputDir2)}
                      })"
                    )
                }
            } else{
                d96 <- ("")
                d96_1 <- ("")
            }
            
            d9 <- c(d91, d93, d94, d95, d96, d92)
            d9_1 <- c(d93_1, d94_1, d95_1, d96_1)
        }
        else{
            d9 <- ("")
            d9_1 <- ("")
        }
        
        if (input$input_type2) {
            d101 <- c("material_card('",
                      input$namespecies,
                      "',material_card(''")
            if (input$input_type1) {
                d101 <- c(",material_card(''")
            }
            d102 <- c(")")
            if (input$input_type22) {
                if (input$input_type222 == "Slider") {
                    d103 <- c(
                        ",material_slider('Species2_1','",
                        input$input_type2222_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d103_1 <- ("")
                }
                if (input$input_type222 == "Text") {
                    d103 <- c(",material_text_box('Species2_1','",
                              input$input_type2222_name,
                              "')")
                    d103_1 <- ("")
                }
                if (input$input_type222 == "Photo") {
                    d103 <- c(
                        ",fileInput(inputId = 'Species2_1', label ='",
                        input$input_type2222_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                        
                    )
                    d103_1 <- c(
                        "
            observe({
          if (is.null(input$Species2_1)) {
            return(NULL)}
            else{
                      files2_1=file.rename(input$Species2_1$datapath, paste0(Sys.time(),'2_1.png'))
      files2_1 = paste0(Sys.time(),'2_1.png')
      drop_upload(files2_1, path = outputDir2)}
                      })"
                    )
                }
            } else{
                d103 <- ("")
                d103_1 <- ("")
            }
            if (input$input_type22a) {
                if (input$input_type222a == "Slider") {
                    d104 <- c(
                        ",material_slider('Species2_2','",
                        input$input_type2222a_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d104_1 <- ("")
                }
                if (input$input_type222a == "Text") {
                    d104 <- c(",material_text_box('Species2_2','",
                              input$input_type2222a_name,
                              "')")
                    d104_1 <- ("")
                }
                if (input$input_type222a == "Photo") {
                    d104 <- c(
                        ",fileInput(inputId = 'Species2_2', label ='",
                        input$input_type2222a_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d104_1 <- c(
                        "
                      observe({if (is.null(input$Species2_2)) {
            return(NULL)}
            else{files2_2=file.rename(input$Species2_2$datapath, paste0(Sys.time(),'2_2.png'))
      files2_2 = paste0(Sys.time(),'2_2.png')
      drop_upload(files2_2, path = outputDir2)}
                    })"
                    )
                }
            } else{
                d104 <- ("")
                d104_1 <- ("")
            }
            if (input$input_type22b) {
                if (input$input_type222b == "Slider") {
                    d105 <- c(
                        ",material_slider('Species2_3','",
                        input$input_type2222b_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d105_1 <- ("")
                }
                if (input$input_type222b == "Text") {
                    d105 <- c(",material_text_box('Species2_3','",
                              input$input_type2222b_name,
                              "')")
                    d105_1 <- ("")
                }
                if (input$input_type222b == "Photo") {
                    d105 <- c(
                        ",fileInput(inputId = 'Species2_3', label ='",
                        input$input_type2222b_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d105_1 <- c(
                        "
            observe({
            if (is.null(input$Species2_3)) {
            return(NULL)}
            else{files2_3=file.rename(input$Species2_3$datapath, paste0(Sys.time(),'2_3.png'))
            files2_3 = paste0(Sys.time(),'2_3.png')
            drop_upload(files2_3, path = outputDir2)}


            })"
                    )
                }
            } else{
                d105 <- ("")
                d105_1 <- ("")
            }
            if (input$input_type22c) {
                if (input$input_type222c == "Slider") {
                    d106 <- c(
                        ",material_slider('Species2_4','",
                        input$input_type2222c_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d106_1 <- c("")
                }
                if (input$input_type222c == "Text") {
                    d106 <- c(",material_text_box('Species2_4','",
                              input$input_type2222c_name,
                              "')")
                    d106_1 <- c("")
                }
                if (input$input_type222c == "Photo") {
                    d106 <- c(
                        ",fileInput(inputId = 'Species2_4', label ='",
                        input$input_type2222c_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d106_1 <- c(
                        "
            observe({
            if (is.null(input$Species2_4)) {
            return(NULL)}
            else{
            files2_4=file.rename(input$Species2_4$datapath, paste0(Sys.time(),'2_4.png'))
            files2_4 = paste0(Sys.time(),'2_4.png')
            drop_upload(files2_4, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d106 <- ("")
                d106_1 <- ("")
            }
            
            d10 <- c(d101, d103, d104, d105, d106, d102)
            d10_1 <- c(d103_1, d104_1, d105_1, d106_1)
        }
        else{
            d10 <- ("")
            d10_1 <- ("")
        }
        
        if (input$input_type5) {
            d111 <- c("material_card('",
                      input$namespecies,
                      "',material_card(''")
            if (input$input_type1) {
                d111 <- c(",material_card(''")
            }
            if (input$input_type2) {
                d111 <- c(",material_card(''")
            }
            d112 <- c(")")
            if (input$input_type55) {
                if (input$input_type555 == "Slider") {
                    d113 <- c(
                        ",material_slider('Species3_1','",
                        input$input_type5555_name,
                        "', 0, 110, initial_value = 0)"
                    )
                    d113_1 <- ("")
                }
                if (input$input_type555 == "Text") {
                    d113 <- c(",material_text_box('Species3_1','",
                              input$input_type5555_name,
                              "')")
                    d113_1 <- ("")
                }
                if (input$input_type555 == "Photo") {
                    d113 <- c(
                        ",fileInput(inputId = 'Species3_1', label ='",
                        input$input_type5555_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d113_1 <- c(
                        "
            observe({
            if (is.null(input$Species3_1)) {
            return(NULL)}
            else{
            files3_1=file.rename(input$Species3_1$datapath, paste0(Sys.time(),'3_1.png'))
            files3_1 = paste0(Sys.time(),'3_1.png')
            drop_upload(files3_1, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d113 <- ("")
                d113_1 <- ("")
            }
            if (input$input_type55a) {
                if (input$input_type555a == "Slider") {
                    d114 <- c(
                        ",material_slider('Species3_2','",
                        input$input_type5555a_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d114_1 <- ("")
                }
                if (input$input_type555a == "Text") {
                    d114 <- c(",material_text_box('Species3_2','",
                              input$input_type5555a_name,
                              "')")
                    d114_1 <- ("")
                }
                if (input$input_type555a == "Photo") {
                    d114 <- c(
                        ",fileInput(inputId = 'Species3_2', label ='",
                        input$input_type5555a_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d114_1 <- c(
                        "
            observe({
            if (is.null(input$Species3_2)) {
            return(NULL)}
            else{
            files3_2=file.rename(input$Species3_2$datapath, paste0(Sys.time(),'3_2.png'))
            files3_2 = paste0(Sys.time(),'3_2.png')
            drop_upload(files3_2, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d114 <- ("")
                d114_1 <- ("")
            }
            if (input$input_type55b) {
                if (input$input_type555b == "Slider") {
                    d115 <- c(
                        ",material_slider('Species3_3','",
                        input$input_type5555b_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d115_1 <- ("")
                }
                if (input$input_type555b == "Text") {
                    d115 <- c(",material_text_box('Species3_3','",
                              input$input_type5555b_name,
                              "')")
                    d115_1 <- ("")
                }
                if (input$input_type555b == "Photo") {
                    d115 <- c(
                        ",fileInput(inputId = 'Species3_3', label ='",
                        input$input_type5555b_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d115_1 <- c(
                        "
            observe({
            if (is.null(input$Species3_3)) {
            return(NULL)}
            else{
            files3_3=file.rename(input$Species3_3$datapath, paste0(Sys.time(),'3_3.png'))
            files3_3 = paste0(Sys.time(),'3_3.png')
            drop_upload(files3_3, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d115 <- ("")
                d115_1 <- ("")
            }
            if (input$input_type55c) {
                if (input$input_type555c == "Slider") {
                    d116 <- c(
                        ",material_slider('Species3_4','",
                        input$input_type5555c_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d116_1 <- ("")
                }
                if (input$input_type555c == "Text") {
                    d116 <- c(",material_text_box('Species3_4','",
                              input$input_type5555c_name,
                              "')")
                    d116_1 <- ("")
                }
                if (input$input_type555c == "Photo") {
                    d116 <- c(
                        ",fileInput(inputId = 'Species3_4', label ='",
                        input$input_type5555c_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d116_1 <- c(
                        "
            observe({
            if (is.null(input$Species3_4)) {
            return(NULL)}
            else{
            files3_4=file.rename(input$Species3_4$datapath, paste0(Sys.time(),'3_4.png'))
            files3_4 = paste0(Sys.time(),'3_4.png')
            drop_upload(files3_4, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d116 <- ("")
                d116_1 <- ("")
            }
            
            d11 <- c(d111, d113, d114, d115, d116, d112)
            d11_1 <- c(d113_1, d114_1, d115_1, d116_1)
        }
        else{
            d11 <- ("")
            d11_1 <- ("")
        }
        
        if (input$input_type6) {
            d121 <- c("material_card('",
                      input$namespecies,
                      "',material_card(''")
            if (input$input_type1) {
                d121 <- c(",material_card(''")
            }
            if (input$input_type2) {
                d121 <- c(",material_card(''")
            }
            d122 <- c(")")
            if (input$input_type66) {
                if (input$input_type666 == "Slider") {
                    d123 <- c(
                        ",material_slider('Species4_1','",
                        input$input_type6666_name,
                        "', 0, 110, initial_value = 0)"
                    )
                    d123_1 <- ("")
                }
                if (input$input_type666 == "Text") {
                    d123 <- c(",material_text_box('Species4_1','",
                              input$input_type6666_name,
                              "')")
                    d123_1 <- ("")
                }
                if (input$input_type666 == "Photo") {
                    d123 <- c(
                        ",fileInput(inputId = 'Species4_1', label ='",
                        input$input_type6666_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d123_1 <- c(
                        "
            observe({
            if (is.null(input$Species4_1)) {
            return(NULL)}
            else{
            files4_1=file.rename(input$Species4_1$datapath, paste0(Sys.time(),'4_1.png'))
            files4_1 = paste0(Sys.time(),'4_1.png')
            drop_upload(files4_1, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d123 <- ("")
                d123_1 <- ("")
            }
            if (input$input_type66a) {
                if (input$input_type666a == "Slider") {
                    d124 <- c(
                        ",material_slider('Species4_2','",
                        input$input_type6666a_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d124_1 <- ("")
                }
                if (input$input_type666a == "Text") {
                    d124 <- c(",material_text_box('Species4_2','",
                              input$input_type6666a_name,
                              "')")
                    d124_1 <- ("")
                }
                if (input$input_type666a == "Photo") {
                    d124 <- c(
                        ",fileInput(inputId = 'Species4_2', label ='",
                        input$input_type6666a_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d124_1 <- c(
                        "
            observe({
            if (is.null(input$Species4_2)) {
            return(NULL)}
            else{
            files4_2=file.rename(input$Species4_2$datapath, paste0(Sys.time(),'4_2.png'))
            files4_2 = paste0(Sys.time(),'4_2.png')
            drop_upload(files4_2, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d124 <- ("")
                d124_1 <- ("")
            }
            if (input$input_type66b) {
                if (input$input_type666b == "Slider") {
                    d125 <- c(
                        ",material_slider('Species4_3','",
                        input$input_type6666b_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d125_1 <- ("")
                }
                if (input$input_type666b == "Text") {
                    d125 <- c(",material_text_box('Species4_3','",
                              input$input_type6666b_name,
                              "')")
                    d125_1 <- ("")
                }
                if (input$input_type666b == "Photo") {
                    d125 <- c(
                        ",fileInput(inputId = 'Species4_3', label ='",
                        input$input_type6666b_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d125_1 <- c(
                        "
            observe({
            if (is.null(input$Species4_3)) {
            return(NULL)}
            else{
            files4_3=file.rename(input$Species4_3$datapath, paste0(Sys.time(),'4_3.png'))
            files4_3 = paste0(Sys.time(),'4_3.png')
            drop_upload(files4_3, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d125 <- ("")
                d125_1 <- ("")
            }
            if (input$input_type66c) {
                if (input$input_type666c == "Slider") {
                    d126 <- c(
                        ",material_slider('Species4_4','",
                        input$input_type6666c_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d126_1 <- ("")
                }
                if (input$input_type666c == "Text") {
                    d126 <- c(",material_text_box('Species4_4','",
                              input$input_type6666c_name,
                              "')")
                    d126_1 <- ("")
                }
                if (input$input_type666c == "Photo") {
                    d126 <- c(
                        ",fileInput(inputId = 'Species4_4', label ='",
                        input$input_type6666c_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d126_1 <- c(
                        "
            observe({
            if (is.null(input$Species4_4)) {
            return(NULL)}
            else{
            files4_4=file.rename(input$Species4_4$datapath, paste0(Sys.time(),'4_4.png'))
            files4_4 = paste0(Sys.time(),'4_4.png')
            drop_upload(files4_4, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d126 <- ("")
                d126_1 <- ("")
            }
            
            d12 <- c(d121, d123, d124, d125, d126, d122)
            d12_1 <- c(d123_1, d124_1, d125_1, d126_1)
        }
        else{
            d12 <- ("")
            d12_1 <- ("")
        }
        
        if (input$input_type7) {
            d131 <- c("material_card('",
                      input$namespecies,
                      "',material_card(''")
            if (input$input_type1) {
                d131 <- c(",material_card(''")
            }
            if (input$input_type2) {
                d131 <- c(",material_card(''")
            }
            d132 <- c(")")
            if (input$input_type77) {
                if (input$input_type777 == "Slider") {
                    d133 <- c(
                        ",material_slider('Species5_1','",
                        input$input_type7777_name,
                        "', 0, 110, initial_value = 0)"
                    )
                    d133_1 <- ("")
                }
                if (input$input_type777 == "Text") {
                    d133 <- c(",material_text_box('Species5_1','",
                              input$input_type7777_name,
                              "')")
                    d133_1 <- ("")
                }
                if (input$input_type777 == "Photo") {
                    d133 <- c(
                        ",fileInput(inputId = 'Species5_1', label ='",
                        input$input_type7777_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d133_1 <- c(
                        "
            observe({
            if (is.null(input$Species5_1)) {
            return(NULL)}
            else{
            files5_1=file.rename(input$Species5_1$datapath, paste0(Sys.time(),'5_1.png'))
            files5_1 = paste0(Sys.time(),'5_1.png')
            drop_upload(files5_1, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d133 <- ("")
                d133_1 <- ("")
            }
            if (input$input_type77a) {
                if (input$input_type777a == "Slider") {
                    d134 <- c(
                        ",material_slider('Species5_2','",
                        input$input_type7777a_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d134_1 <- ("")
                }
                if (input$input_type777a == "Text") {
                    d134 <- c(",material_text_box('Species5_2','",
                              input$input_type7777a_name,
                              "')")
                    d134_1 <- ("")
                }
                if (input$input_type777a == "Photo") {
                    d134 <- c(
                        ",fileInput(inputId = 'Species5_2', label ='",
                        input$input_type7777a_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d134_1 <- c(
                        "
            observe({
            if (is.null(input$Species5_2)) {
            return(NULL)}
            else{
            files5_2=file.rename(input$Species5_2$datapath, paste0(Sys.time(),'5_2.png'))
            files5_2 = paste0(Sys.time(),'5_2.png')
            drop_upload(files5_2, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d134 <- ("")
                d134_1 <- ("")
            }
            if (input$input_type77b) {
                if (input$input_type777b == "Slider") {
                    d135 <- c(
                        ",material_slider('Species5_3','",
                        input$input_type7777b_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d135_1 <- ("")
                }
                if (input$input_type777b == "Text") {
                    d135 <- c(",material_text_box('Species5_3','",
                              input$input_type7777b_name,
                              "')")
                    d135_1 <- ("")
                }
                if (input$input_type777b == "Photo") {
                    d135 <- c(
                        ",fileInput(inputId = 'Species5_3', label ='",
                        input$input_type7777b_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d135_1 <- c(
                        "
            observe({
            if (is.null(input$Species5_3)) {
            return(NULL)}
            else{
            files5_3=file.rename(input$Species5_3$datapath, paste0(Sys.time(),'5_3.png'))
            files5_3 = paste0(Sys.time(),'5_3.png')
            drop_upload(files5_3, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d135 <- ("")
                d135_1 <- ("")
            }
            if (input$input_type77c) {
                if (input$input_type777c == "Slider") {
                    d136 <- c(
                        ",material_slider('Species5_4','",
                        input$input_type7777c_name,
                        "', 0, 100, initial_value = 0)"
                    )
                    d136_1 <- ("")
                }
                if (input$input_type777c == "Text") {
                    d136 <- c(",material_text_box('Species5_4','",
                              input$input_type7777c_name,
                              "')")
                    d136_1 <- ("")
                }
                if (input$input_type777c == "Photo") {
                    d136 <- c(
                        ",fileInput(inputId = 'Species5_4', label ='",
                        input$input_type7777c_name,
                        "', accept = c('image/png', 'image/jpeg', 'image/jpg'))"
                    )
                    d136_1 <- c(
                        "
            observe({
            if (is.null(input$Species5_4)) {
            return(NULL)}
            else{
            files5_4=file.rename(input$Species5_4$datapath, paste0(Sys.time(),'5_4.png'))
            files5_4 = paste0(Sys.time(),'5_4.png')
            drop_upload(files5_4, path = outputDir2)
            }
            })
            "
                    )
                }
            } else{
                d136 <- ("")
                d136_1 <- ("")
            }
            
            d13 <- c(d131, d133, d134, d135, d136, d132)
            d13_1 <- c(d133_1, d134_1, d135_1, d136_1)
        }
        else{
            d13 <- ("")
            d13_1 <- ("")
        }
        
        
        ## Assembling all the codes customized
        
        codecrazy <- paste0(
            c(
                paste0(d),
                d7,
                paste0(d8),
                paste0(d9),
                paste0(d10),
                paste0(d11),
                paste0(d12),
                paste0(d13),
                paste0(d4),
                paste0(d5),
                paste0(d9_1),
                paste0(d10_1),
                paste0(d11_1),
                paste0(d12_1),
                paste0(d13_1),
                paste0(d6)
            )
        )
        
        sink('mydata.txt')
        cat(codecrazy)
        sink()
        
        codecrazy
    })
    
    output$clip <- renderUI({
        clipboard <- readtext("mydata.txt")
        rclipButton("clipbtn",
                    "Copy to Clipboard",
                    clipText = clipboard,
                    icon("clipboard"))
    })
    
    output$clip2 <- renderUI({
        clipboard2 <- readtext("mydata2.txt")
        rclipButton("clipbtn2",
                    "Copy to Clipboard",
                    clipText = clipboard2,
                    icon("clipboard"))
    })
    
    
    ## Bringing the customized buttons to the UI
    
    
    output$ui1 <- renderUI({
        switch(
            input$input_type0 == TRUE,
            material_card(
                title = input$nametype0,
                conditionalPanel(
                    condition = "input.input_type00 ==  true",
                    textInput("User_ID", label = input$input_type00_name)
                ),
                conditionalPanel(
                    condition = "input.input_type000 ==  true",
                    textInput("Vessel_ID", label = input$input_type000_name)
                )
            )
        )
        
    })
    output$ui2 <- renderUI({
        switch(
            input$input_type3 == TRUE,
            material_card(
                title = input$nametype3,
                conditionalPanel(
                    condition = "input.input_type33 ==  true",
                    dateInput('Date', label = input$input_type33_name),
                ),
                conditionalPanel(
                    condition = "input.input_type333 ==  true",
                    material_slider(
                        input_id = 'Days_fishing',
                        input$input_type333_name,
                        0,
                        100,
                        initial_value = 0
                    )
                )
            )
        )
        
    })
    output$ui3 <- renderUI({
        switch(
            input$namespecies0 == TRUE,
            material_card(
                title = input$namespecies,
                uiOutput("ui4"),
                uiOutput("ui5"),
                uiOutput("ui6"),
                uiOutput("ui7"),
                uiOutput("ui8")
                
                
            )
        )
    })
    output$ui4 <- renderUI({
        switch(
            input$input_type1 == TRUE,
            material_card(
                conditionalPanel(condition = "input.input_type11 ==  true",
                                 switch(
                                     input$input_type111,
                                     "Slider" =     material_slider(
                                         "Species1_1",
                                         input$input_type1111_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species1_1", input$input_type1111_name),
                                     "Photo" = fileInput(
                                         inputId = "Species1_1",
                                         label = input$input_type1111_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type11a ==  true",
                                 switch(
                                     input$input_type111a,
                                     "Slider" =     material_slider(
                                         "Species1_2",
                                         input$input_type1111a_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species1_2", input$input_type1111a_name),
                                     "Photo" = fileInput(
                                         inputId = "Species1_2",
                                         label = input$input_type1111a_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                
                conditionalPanel(condition = "input.input_type11b ==  true",
                                 switch(
                                     input$input_type111b,
                                     "Slider" =     material_slider(
                                         "Species1_3",
                                         input$input_type1111b_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species1_3", input$input_type1111b_name),
                                     "Photo" = fileInput(
                                         inputId = "Species1_3",
                                         label = input$input_type1111b_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type11c ==  true",
                                 switch(
                                     input$input_type111c,
                                     "Slider" = material_slider(
                                         "Species1_4",
                                         input$input_type1111c_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" = material_text_box("Species1_4", input$input_type1111c_name),
                                     "Photo" = fileInput(
                                         inputId = "Species1_4",
                                         label = input$input_type1111c_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 ))
            )
        )
        
    })
    output$ui5 <- renderUI({
        switch(
            input$input_type2 == TRUE,
            material_card(
                conditionalPanel(condition = "input.input_type22 ==  true",
                                 switch(
                                     input$input_type222,
                                     "Slider" =     material_slider(
                                         "Species2_1",
                                         input$input_type2222_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species2_1", input$input_type2222_name),
                                     "Photo" = fileInput(
                                         inputId = "Species2_1",
                                         label = input$input_type2222_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type22a ==  true",
                                 switch(
                                     input$input_type222a,
                                     "Slider" =     material_slider(
                                         "Species2_2",
                                         input$input_type2222a_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species2_2", input$input_type2222a_name),
                                     fileInput(
                                         inputId = "Species2_2",
                                         label = input$input_type2222a_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 )),
                
                conditionalPanel(condition = "input.input_type22b ==  true",
                                 switch(
                                     input$input_type222b,
                                     "Slider" =     material_slider(
                                         "Species2_3",
                                         input$input_type2222b_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species2_3", input$input_type2222b_name),
                                     fileInput(
                                         inputId = "Species2_3",
                                         label = input$input_type2222b_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                     
                                 )),
                conditionalPanel(condition = "input.input_type22c ==  true",
                                 switch(
                                     input$input_type222c,
                                     "Slider" =     material_slider(
                                         "Species2_4",
                                         input$input_type2222c_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species2_4", input$input_type2222c_name),
                                     fileInput(
                                         inputId = "Species2_4",
                                         label = input$input_type2222c_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 ))
            )
            
        )
    })
    output$ui6 <- renderUI({
        switch(
            input$input_type5 == TRUE,
            material_card(
                conditionalPanel(condition = "input.input_type55 ==  true",
                                 switch(
                                     input$input_type555,
                                     "Slider" =     material_slider(
                                         "Species3_1",
                                         input$input_type5555_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species3_1", input$input_type5555_name),
                                     "Photo" = fileInput(
                                         inputId = "Species3_1",
                                         label = input$input_type5555_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type55a ==  true",
                                 switch(
                                     input$input_type555a,
                                     "Slider" =     material_slider(
                                         "Species3_2",
                                         input$input_type5555a_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species3_2", input$input_type5555a_name),
                                     fileInput(
                                         inputId = "Species3_2",
                                         label = input$input_type5555a_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 )),
                
                conditionalPanel(condition = "input.input_type55b ==  true",
                                 switch(
                                     input$input_type555b,
                                     "Slider" =     material_slider(
                                         "Species3_3",
                                         input$input_type5555b_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species3_3", input$input_type5555b_name),
                                     fileInput(
                                         inputId = "Species3_3",
                                         label = input$input_type5555b_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                     
                                 )),
                conditionalPanel(condition = "input.input_type55c ==  true",
                                 switch(
                                     input$input_type555c,
                                     "Slider" =     material_slider(
                                         "Species3_4",
                                         input$input_type5555c_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species3_4", input$input_type5555c_name),
                                     fileInput(
                                         inputId = "Species3_4",
                                         label = input$input_type5555c_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 ))
            )
            
        )
    })
    output$ui7 <- renderUI({
        switch(
            input$input_type6 == TRUE,
            material_card(
                conditionalPanel(condition = "input.input_type66 ==  true",
                                 switch(
                                     input$input_type666,
                                     "Slider" =     material_slider(
                                         "Species4_1",
                                         input$input_type6666_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species4_1", input$input_type6666_name),
                                     "Photo" = fileInput(
                                         inputId = "Species4_1",
                                         label = input$input_type6666_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type66a ==  true",
                                 switch(
                                     input$input_type666a,
                                     "Slider" =     material_slider(
                                         "Species4_2",
                                         input$input_type6666a_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species4_2", input$input_type6666a_name),
                                     fileInput(
                                         inputId = "Species4_2",
                                         label = input$input_type6666a_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 )),
                
                conditionalPanel(condition = "input.input_type66b ==  true",
                                 switch(
                                     input$input_type666b,
                                     "Slider" =     material_slider(
                                         "Species4_3",
                                         input$input_type6666b_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species4_3", input$input_type6666b_name),
                                     fileInput(
                                         inputId = "Species4_3",
                                         label = input$input_type6666b_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                     
                                 )),
                conditionalPanel(condition = "input.input_type66c ==  true",
                                 switch(
                                     input$input_type666c,
                                     "Slider" =     material_slider(
                                         "Species4_4",
                                         input$input_type6666c_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species4_4", input$input_type6666c_name),
                                     fileInput(
                                         inputId = "Species4_4",
                                         label = input$input_type6666c_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 ))
            )
            
        )
    })
    output$ui8 <- renderUI({
        switch(
            input$input_type7 == TRUE,
            material_card(
                conditionalPanel(condition = "input.input_type77 ==  true",
                                 switch(
                                     input$input_type777,
                                     "Slider" =     material_slider(
                                         "Species5_1",
                                         input$input_type7777_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species5_1", input$input_type7777_name),
                                     "Photo" = fileInput(
                                         inputId = "Species5_1",
                                         label = input$input_type7777_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                 )),
                conditionalPanel(condition = "input.input_type77a ==  true",
                                 switch(
                                     input$input_type777a,
                                     "Slider" =     material_slider(
                                         "Species5_2",
                                         input$input_type7777a_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species5_2", input$input_type7777a_name),
                                     fileInput(
                                         inputId = "Species5_2",
                                         label = input$input_type7777a_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 )),
                
                conditionalPanel(condition = "input.input_type77b ==  true",
                                 switch(
                                     input$input_type777b,
                                     "Slider" =     material_slider(
                                         "Species5_3",
                                         input$input_type7777b_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     
                                     
                                     "Text" =    material_text_box("Species5_3", input$input_type7777b_name),
                                     fileInput(
                                         inputId = "Species5_3",
                                         label = input$input_type7777b_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                     )
                                     
                                 )),
                conditionalPanel(condition = "input.input_type77c ==  true",
                                 switch(
                                     input$input_type777c,
                                     "Slider" =     material_slider(
                                         "Species5_4",
                                         input$input_type7777c_name,
                                         0,
                                         1000,
                                         initial_value = 0
                                     ),
                                     "Text" =    material_text_box("Species5_4", input$input_type7777c_name),
                                     fileInput(
                                         inputId = "Species5_4",
                                         label = input$input_type7777c_name,
                                         accept = c('image/png', 'image/jpeg', 'image/jpg')
                                         
                                     )
                                 ))
            )
            
        )
        
        
        
    })
    
    
    
    output$img <- renderUI({
        tags$img(src = "https://i.ibb.co/pJSYRnQ/tri.png", width = "100%")
    })
    
    
    
    formData <- reactive({
        data <- sapply(fields, function(x)
            as.character(input[[x]]))
        data
    })
    
    observeEvent(input$submit, {
        saveData(formData())
        shinyalert(
            "Success! The data has been sent.",
            type = "success",
            closeOnClickOutside = FALSE
        )
        
        
        delay(2000, session$reload())
    })
    
    
    #Save the data as CSV
    
    saveData <- function(data)
        ({
            data <- t(data)
            # Create a unique file name
            fileName <-
                sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
            # Write the data to a temporary file locally
            filePath <- file.path(tempdir(), fileName)
            write.csv(data, filePath, row.names = FALSE, quote = TRUE)
            # Upload the file to Dropbox
            drop_upload(filePath, path = outputDir)
            
            #save images
            
            observe({
                if (is.null(input$Species1_1)) {
                    return(NULL)
                } else {
                    if (input$input_type111 == "Photo") {
                        files1_1 = file.rename(input$Species1_1$datapath,
                                               paste0(Sys.time(), '1_1.png'))
                        files1_1 = paste0(Sys.time(), '1_1.png')
                        drop_upload(files1_1, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species1_2)) {
                    return(NULL)
                } else {
                    if (input$input_type111a == "Photo") {
                        files1_2 = file.rename(input$Species1_2$datapath,
                                               paste0(Sys.time(), '1_2.png'))
                        files1_2 = paste0(Sys.time(), '1_2.png')
                        drop_upload(files1_2, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species1_3)) {
                    return(NULL)
                } else {
                    if (input$input_type111b == "Photo") {
                        files1_3 = file.rename(input$Species1_3$datapath,
                                               paste0(Sys.time(), '1_3.png'))
                        files1_3 = paste0(Sys.time(), '1_3.png')
                        drop_upload(files1_3, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species1_4)) {
                    return(NULL)
                } else {
                    if (input$input_type111c == "Photo") {
                        files1_4 = file.rename(input$Species1_4$datapath,
                                               paste0(Sys.time(), '1_4.png'))
                        files1_4 = paste0(Sys.time(), '1_4.png')
                        drop_upload(files1_4, path = outputDir2)
                    }
                }
            })
            
            #Photos species 2
            observe({
                if (is.null(input$Species2_1)) {
                    return(NULL)
                } else {
                    if (input$input_type222 == "Photo") {
                        files2_1 = file.rename(input$Species2_1$datapath,
                                               paste0(Sys.time(), '2_1.png'))
                        files2_1 = paste0(Sys.time(), '2_1.png')
                        drop_upload(files2_1, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species2_2)) {
                    return(NULL)
                } else {
                    if (input$input_type222a == "Photo") {
                        files2_2 = file.rename(input$Species2_2$datapath,
                                               paste0(Sys.time(), '2_2.png'))
                        files2_2 = paste0(Sys.time(), '2_2.png')
                        drop_upload(files2_2, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species2_3)) {
                    return(NULL)
                } else {
                    if (input$input_type222b == "Photo") {
                        files2_3 = file.rename(input$Species2_3$datapath,
                                               paste0(Sys.time(), '2_3.png'))
                        files2_3 = paste0(Sys.time(), '2_3.png')
                        drop_upload(files2_3, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species2_4)) {
                    return(NULL)
                } else {
                    if (input$input_type222c == "Photo") {
                        files2_4 = file.rename(input$Species2_4$datapath,
                                               paste0(Sys.time(), '2_4.png'))
                        files2_4 = paste0(Sys.time(), '2_4.png')
                        drop_upload(files2_4, path = outputDir2)
                    }
                }
            })
            #Photos Species 3
            observe({
                if (is.null(input$Species3_1)) {
                    return(NULL)
                } else {
                    if (input$input_type555 == "Photo") {
                        files3_1 = file.rename(input$Species3_1$datapath,
                                               paste0(Sys.time(), '3_1.png'))
                        files3_1 = paste0(Sys.time(), '3_1.png')
                        drop_upload(files3_1, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species3_2)) {
                    return(NULL)
                } else {
                    if (input$input_type555a == "Photo") {
                        files3_2 = file.rename(input$Species3_2$datapath,
                                               paste0(Sys.time(), '3_2.png'))
                        files3_2 = paste0(Sys.time(), '3_2.png')
                        drop_upload(files3_2, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species3_3)) {
                    return(NULL)
                } else {
                    if (input$input_type555b == "Photo") {
                        files3_3 = file.rename(input$Species3_3$datapath,
                                               paste0(Sys.time(), '3_3.png'))
                        files3_3 = paste0(Sys.time(), '3_3.png')
                        drop_upload(files3_3, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species3_4)) {
                    return(NULL)
                } else {
                    if (input$input_type555c == "Photo") {
                        files3_4 = file.rename(input$Species3_4$datapath,
                                               paste0(Sys.time(), '3_4.png'))
                        files3_4 = paste0(Sys.time(), '3_4.png')
                        drop_upload(files3_4, path = outputDir2)
                    }
                }
            })
            #Photos Species 4
            observe({
                if (is.null(input$Species4_1)) {
                    return(NULL)
                } else {
                    if (input$input_type666 == "Photo") {
                        files4_1 = file.rename(input$Species4_1$datapath,
                                               paste0(Sys.time(), '4_1.png'))
                        files4_1 = paste0(Sys.time(), '4_1.png')
                        drop_upload(files4_1, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species4_2)) {
                    return(NULL)
                } else {
                    if (input$input_type666a == "Photo") {
                        files4_2 = file.rename(input$Species4_2$datapath,
                                               paste0(Sys.time(), '4_2.png'))
                        files4_2 = paste0(Sys.time(), '4_2.png')
                        drop_upload(files4_2, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species4_3)) {
                    return(NULL)
                } else {
                    if (input$input_type666b == "Photo") {
                        files4_3 = file.rename(input$Species4_3$datapath,
                                               paste0(Sys.time(), '4_3.png'))
                        files4_3 = paste0(Sys.time(), '4_3.png')
                        drop_upload(files4_3, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species4_4)) {
                    return(NULL)
                } else {
                    if (input$input_type666c == "Photo") {
                        files4_4 = file.rename(input$Species4_4$datapath,
                                               paste0(Sys.time(), '4_4.png'))
                        files4_4 = paste0(Sys.time(), '4_4.png')
                        drop_upload(files4_4, path = outputDir2)
                    }
                }
            })
            #Photos Species 5
            observe({
                if (is.null(input$Species5_1)) {
                    return(NULL)
                } else {
                    if (input$input_type777 == "Photo") {
                        files5_1 = file.rename(input$Species5_1$datapath,
                                               paste0(Sys.time(), '5_1.png'))
                        files5_1 = paste0(Sys.time(), '5_1.png')
                        drop_upload(files5_1, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species5_2)) {
                    return(NULL)
                } else {
                    if (input$input_type777a == "Photo") {
                        files5_2 = file.rename(input$Species5_2$datapath,
                                               paste0(Sys.time(), '5_2.png'))
                        files5_2 = paste0(Sys.time(), '5_2.png')
                        drop_upload(files5_2, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species5_3)) {
                    return(NULL)
                } else {
                    if (input$input_type777b == "Photo") {
                        files5_3 = file.rename(input$Species5_3$datapath,
                                               paste0(Sys.time(), '5_3.png'))
                        files5_3 = paste0(Sys.time(), '5_3.png')
                        drop_upload(files5_3, path = outputDir2)
                    }
                }
            })
            observe({
                if (is.null(input$Species5_4)) {
                    return(NULL)
                } else {
                    if (input$input_type777c == "Photo") {
                        files5_4 = file.rename(input$Species5_4$datapath,
                                               paste0(Sys.time(), '5_4.png'))
                        files5_4 = paste0(Sys.time(), '5_4.png')
                        drop_upload(files5_4, path = outputDir2)
                    }
                }
            })
        })
    
    loadData <- function() {
        # Read all the files into a list
        filesInfo <- drop_dir(outputDir)
        filePaths <- filesInfo$path_display
        data <-
            lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data <-
            datatable(
                data,
                options = list(dom = 't', pageLength = 15),
                filter = list(position = "top")
            )
        data
    }
    
    
    
    
  

})

options(shiny.reactlog = TRUE)
