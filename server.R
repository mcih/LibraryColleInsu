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
library(mailR)

source(paste0(base.dir,"global.R"))
shinyServer(function(input, output, session) {
  
  output$sidebar_ui <- renderUI({

        sidebarMenu(id = "sidebar_tabs",
                    menuItem('Biblioteca diffusa', icon = icon("server"), tabName = 'create_tl')
        )
   
  })
############
## Fixed UI
############
  
output$library_ui <- renderUI({
  fluidPage(
     mainPanel(width = 12, 
               fluidRow(
               column(2, actionButton("newEntry", width = "75px", icon("plus"), style="color: #fff; background-color: #2E8B57; border-color: #00A572")),
               column(2, actionButton("modifyEntry", width = "75px", icon("save"), style="color: #fff; background-color: #FF0800; border-color: #B80F0A")),
               column(2, actionButton("sendRequest", width = "75px", icon("at"), style="color: #fff;  background-color: #337ab7; border-color: #2e6da4")),
               hr(''),
               box(width = 12,title = "Ricerca", collapsible = T, collapsed = F, solidHeader = T, status = "primary",
                    column(12, textInput(inputId = "titol", label = "Titolo", value = "", width = NULL)),
                    column(6, textInput(inputId = "sottotitol", label = "Sottotitolo", value = "", width = NULL)),
                    column(6, textInput(inputId = "titolOrig", label = "Titolo originale", value = "", width = NULL)),
                    column(6, selectizeInput("series", "Serie", choices = c("All", unique(data$source$SERIE_LIBRI)[which(unique(data$source$SERIE_LIBRI)!=""&!is.na(unique(data$source$SERIE_LIBRI)))]), selected ="All", multiple = T)),
                    column(6, selectizeInput("author", "Autore", choices = c("All", unique(data$source$AUTORE)[which(!is.na(unique(data$source$AUTORE)))]), selected = "All", multiple = T)),
                    column(6, selectizeInput("editor", "Casa editrice", choices = c("All", unique(data$source$CASA_EDITRICE)[which(!is.na(unique(data$source$CASA_EDITRICE)))]), selected = "All", multiple = T)),
                    column(6, selectizeInput("transl", "Traduttore", choices = c("All", unique(data$source$TRADUTTORE))[which(!is.na(unique(data$source$TRADUTTORE)))], selected = "All", multiple = T)),
                    column(6, selectizeInput("year", "Anno pubblicazione", choices = c("All", unique(data$source$ANNO_PUBBLICAZIONE)[which(!is.na(unique(data$source$ANNO_PUBBLICAZIONE)))]), selected = "All", multiple = T)), 
                    column(6, selectizeInput("yearOrig", "Anno prima edizione", choices = c("All", unique(data$source$PRIMA_EDIZIONE)[which(!is.na(unique(data$source$PRIMA_EDIZIONE)))]), selected = "All", multiple = T)), 
                    column(6, selectizeInput("language", "Lingua", choices = c("All", unique(data$source$LINGUA)[which(!is.na(unique(data$source$LINGUA)))]), selected = "All", multiple = T)),
                    column(6, selectizeInput("location", "Collocazione principale", choices = c("All", unique(data$source$COLLOCAZIONE_PRINCIPALE)[which(!is.na(unique(data$source$COLLOCAZIONE_PRINCIPALE)))]), selected = "All", multiple = T)), 
                    column(6, selectizeInput("genere", "Genere", choices = c("All", unique(data$source$GENERE)[which(!is.na(unique(data$source$GENERE)))]), selected = "All", multiple = T)),
                    column(6, textInput(inputId = "tag", label = "TAGs", value = "", width = NULL))
               ),
                fluidRow( column(12, checkboxGroupInput("show_vars", "Colonne da mostrare:",
                                                  c("SOTTOTITOLO", "TITOLO ORIGINALE", "SERIE", "NUMERO SERIE", "COLLANA", 
                                                      "ANNO PRIMA EDIZIONE", "PAGINE", "DESCRIZIONE", "GENERE", "LINGUA", "PROPRIETARIO", 
                                                      "PRESTITO", "PRESTATO A", "ISBN"), selected = "", inline = T)),
                    shinydashboard::box(
                        title = "Select deviation to explore", width = 12, status = "info", 
                        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, 
                        div(style = 'overflow-x: scroll', DT::dataTableOutput('Table'))
                      )           
                  )                )
      ),
      fixedPanel(
        conditionalPanel(
          condition="($('html').hasClass('shiny-busy'))", 
          img(src='ajax_loader_green_128.gif')
        )
        ,top = 300, left = 400, right = 0)
    )
  })
  
  
  source(paste0(base.dir,"render_library.R"), local=TRUE)$value
})
