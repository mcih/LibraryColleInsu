library(shiny)
library(DT)
library(stringr)
library(readr)
library(dplyr)
library(shinyBS)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(data.table)
library(openxlsx)
library(plotly)
library(scales)
library(readxl)
library(writexl)
library(clipr)

source(paste0(base.dir,"global.R"))
shinyServer(function(input, output, session) {
  
  output$sidebar_ui <- renderUI({

        sidebarMenu(id = "sidebar_tabs",
                    menuItem('Biblioteca diffusa', icon = icon("server"), tabName = 'create_tl')
        )
   
  })


  source(paste0(base.dir,"render_library.R"), local=TRUE)$value
})
