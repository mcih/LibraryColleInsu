############
## Fixed UI
############

output$library_ui <- renderUI({
  fluidPage(
    mainPanel(width = 12, 
              fluidRow(
                column(2, actionButton("newEntry", width = "75px", icon("plus"), style="color: #fff; background-color: #2E8B57; border-color: #00A572")),
                hr(''),
                box(width = 12,title = "Ricerca", collapsible = T, collapsed = F, solidHeader = T, status = "primary",
                    column(12, textInput(inputId = "titol", label = "Titolo", value = "", width = NULL)),
                    column(6, selectizeInput("author", "Autore", choices = c("All", unique(data$source$AUTORE)), selected = "All", multiple = T)),
                    column(6, selectizeInput("editor", "Casa editrice", choices = c("All", unique(data$source$CASA_EDITRICE)), selected = "All", multiple = T)),
                    column(6, selectizeInput("year", "Anno pubblicazione", choices = c("All", unique(data$source$ANNO_PUBBLICAZIONE)), selected = "All", multiple = T)), 
                    column(6, selectizeInput("language", "Lingua", choices = c("All", unique(data$source$LINGUA)), selected = "All", multiple = T)),
                    column(6, selectizeInput("location", "Collocazione principale", choices = c("All", unique(data$source$COLLOCAZIONE_PRINCIPALE)), selected = "All", multiple = T)), 
                    column(6, selectizeInput("genere", "Genere", choices = c("All", unique(data$source$GENERE)), selected = "All", multiple = T)),
                    
                    column(12, checkboxGroupInput("show_vars", "Colonne da mostrare:",
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


#################
## Reactive data
#################

data <- reactiveValues(
  source = NULL, 
  theData = NULL
)



#############
## Observers
#############

stageLevel1Obs <- observe({
  
  df <- data.table(readxl::read_excel("data/books.xlsx"))
  data$source = df

  
  #filter on author
  selectedAuthor <- input$author
  if(is.null(selectedAuthor))
    return(NULL)
  if(!"All" %in% selectedAuthor & !"empty" %in% selectedAuthor) 
    df <- df[as.character(AUTORE) %in% selectedAuthor,]
 
  #filter on editor
  selectedEditor<- input$editor
  if(is.null(selectedEditor))
    return(NULL)
  if(!"All" %in% selectedEditor & !"empty" %in% selectedEditor) 
    df <- df[as.character(CASA_EDITRICE) %in% selectedEditor,]
  
  #filter on language
  selectedLang <- input$language
  if(is.null(selectedLang))
    return(NULL)
  if(!"All" %in% selectedLang & !"empty" %in% selectedLang) 
    df <- df[as.character(LINGUA) %in% selectedLang,]
  
  #filter on location
  selectedLoc <- input$location
  if(is.null(selectedLoc))
    return(NULL)
  if(!"All" %in% selectedLoc & !"empty" %in% selectedLoc)
    df <- df[as.character(COLLOCAZIONE_PRINCIPALE) %in% selectedLoc,]
  
  #filter on year
  selectedYear <- input$year
  if(is.null(selectedYear))
    return(NULL)
  if(!"All" %in% selectedYear & !"empty" %in% selectedYear) 
    df <- df[as.character(ANNO_PUBBLICAZIONE) %in% selectedYear,]
  
  #filter on genere
  selectedGen <- input$genere
  if(is.null(selectedGen))
    return(NULL)
  if(!"All" %in% selectedGen & !"empty" %in% selectedGen) 
    df <- df[as.character(GENERE) %in% selectedGen,]
  
  #filter title
  selectedTitle <- as.character(input$titol)
  if(is.null(selectedTitle))
    return(NULL)
  if(selectedTitle == "")
    df
  else{
    if(length(grep("\\|", selectedTitle)) > 0 |
       (length(grep(",", selectedTitle)) == 0 &
        length(grep("&", selectedTitle)) == 0)){
      selectedTitle = paste(unlist(lapply(strsplit(selectedTitle,"\\|")[[1]], 
          function(x) gsub(" \\*|\\* ", "", paste0(" ", x, " ")))), collapse = "|")
      df = df[which(Reduce(`|`, lapply(strsplit(selectedTitle,"\\|")[[1]], grepl, paste0(" ", df$TITOLO),ignore.case=T))), ]
    }else if(length(grep(",", selectedTitle)) > 0){
      selectedTitle = paste(unlist(lapply(strsplit(selectedTitle,",")[[1]], 
          function(x) gsub(" \\*|\\* ", "", paste0(" ", x, " ")))), collapse = ",")
      df = df[which(Reduce(`&`, lapply(strsplit(selectedTitle,",")[[1]], grepl, paste0(" ", df$TITOLO),ignore.case=T))), ]
    }else if(length(grep("&", selectedTitle)) > 0){
      selectedTitle = paste(unlist(lapply(strsplit(selectedTitle,"&")[[1]], 
          function(x) gsub(" \\*|\\* ", "", paste0(" ", x, " ")))), collapse = "&")
      df = df[which(Reduce(`&`, lapply(strsplit(selectedTitle,"&")[[1]], grepl, paste0(" ", df$TITOLO),ignore.case=T))), ]
    }
  }
  
  setnames(df, old = c("CASA_EDITRICE","COLLOCAZIONE_PRINCIPALE", "A_CHI", "SERIE_LIBRI", 
                       "NUMERO_SERIE", "PRIMA_EDIZIONE", "ANNO_PUBBLICAZIONE", "NUMERO_INVENTARIO", "TITOLO_ORIGINALE"),
           new = c("CASA EDITRICE","COLLOCAZIONE PRINCIPALE", "PRESTATO A", "SERIE", "NUMERO SERIE", 
                   "ANNO PRIMA EDIZIONE", "ANNO PUBBLICAZIONE", "NUMERO INVENTARIO", "TITOLO ORIGINALE"))
  
  data$theData <- df
  
})

observeEvent(input$newEntry, {
  showModal(dataModalNewEntry())
})
dataModalNewEntry <- function(failed = FALSE) {
  modalDialog(
    title = "Inserire i dati del nuovo volume",
    textInput(inputId = "TITOLO", "TITOLO", ""), textInput(inputId = "AUTORE", "AUTORE", ""),
    textInput(inputId = "SOTTOTITOLO", "SOTTOTITOLO", ""), textInput(inputId = "TITOLO_ORIGINALE", "TITOLO ORIGINALE", ""),
    textInput(inputId = "SERIE_LIBRI", "SERIE", ""), textInput(inputId = "NUMERO_SERIE", "NUMERO SERIE", ""),
    textInput(inputId = "TRADUTTORE", "TRADUTTORE", ""), textInput(inputId = "CASA_EDITRICE", "CASA EDITRICE", ""),
    textInput(inputId = "COLLANA", "COLLANA", ""), textInput(inputId = "ANNO_PUBBLICAZIONE", "ANNO PUBBLICAZIONE", ""), 
    textInput(inputId = "PRIMA_EDIZIONE", "ANNO PRIMA EDIZIONE", ""), textInput(inputId = "PAGINE", "PAGINE", ""),
    textInput(inputId = "DESCRIZIONE", "DESCRIZIONE", ""), textInput(inputId = "GENERE", "GENERE", ""),
    textInput(inputId = "LINGUA", "LINGUA", ""), textInput(inputId = "PROPRIETARIO", "PROPRIETARIO", ""),
    textInput(inputId = "COLLOCAZIONE_PRINCIPALE", "COLLOCAZIONE PINCIPALE", ""), textInput(inputId = "PRESTITO", "PRESTITO", ""),
    textInput(inputId = "A_CHI", "PRESTATO A", ""), textInput(inputId = "TAG", "TAG", ""),
    textInput(inputId = "ISBN", "ISBN", ""),

    column(5, actionButton(inputId = "confirmActionButtonLabel", label = "Confirm"), align = "left"), 
    column(5, actionButton(inputId = "closeModalDialogLabel", label = "Close"), align = "right"),
    footer = "", easyClose = TRUE, size = "m"
    
  )
}  
observeEvent(input$confirmActionButtonLabel, {
  if(!is.null(input$confirmActionButtonLabel)){

    switch(input$COLLOCAZIONE_PRINCIPALE, 
           'Campoli Corridoio' = {loc = "CC"}, 
           'Campoli Salone' = {loc = "CSL"}, 
           'Campoli Soggiorno' = {loc = "CSG"},
           'Campoli Biblioteca' = {loc = "CB"}, 
           'Campoli Cameretta' = {loc = "CT"}, 
           'Roma' = {loc = "RM"}, 
           'Milano' = {loc = "MI"}
    )
    if(is.null(loc)) loc = "NA"
   
    older = data$theData$`NUMERO INVENTARIO`[grep(paste0(loc, input$ANNO_PUBBLICAZIONE),
                data$theData$`NUMERO INVENTARIO`)]
    older = as.numeric(gsub(paste0(loc, input$ANNO_PUBBLICAZIONE, "/"), "", older))
    if(length(older) > 0){
      id = paste0(loc, input$ANNO_PUBBLICAZIONE, "/", max(older) + 1)
    }else{
      id = paste0(loc, input$ANNO_PUBBLICAZIONE, "/", 1)
    }

    feedback <- data.table(TITOLO = input$TITOLO,
                           AUTORE = input$AUTORE,
                           SOTTOTITOLO = input$SOTTOTITOLO, 
                           TITOLO_ORIGINALE = input$TITOLO_ORIGINALE,
                           SERIE_LIBRI = input$SERIE_LIBRI,
                           NUMERO_SERIE = input$NUMERO_SERIE,
                           TRADUTTORE = input$TRADUTTORE,
                           CASA_EDITRICE = input$CASA_EDITRICE,
                           COLLANA = input$COLLANA,
                           ANNO_PUBBLICAZIONE = input$ANNO_PUBBLICAZIONE,
                           ANNO_PRIMA_EDIZIONE = input$ANNO_PRIMA_EDIZIONE,
                           PAGINE = input$PAGINE,
                           DESCRIZIONE = input$DESCRIZIONE,
                           GENERE = input$GENERE,
                           LINGUA = input$LINGUA,
                           PROPRIETARIO = input$PROPRIETARIO,
                           COLLOCAZIONE_PRINCIPALE = input$COLLOCAZIONE_PRINCIPALE,
                           PRESTITO = input$PRESTITO,
                           A_CHI = input$A_CHI,
                           TAG = input$TAG, 
                           NUMERO_INVENTARIO = id,
                           ISBN = input$ISBN)
 
  
  
  writexl::write_xlsx(rbind(data$source, feedback), "data/books.xlsx")
  }
})
observeEvent(input$closeModalDialogLabel, {
  removeModal()
})

###########
## Outputs
###########
output$Table <- DT::renderDataTable({
  df <- data$theData
  if(is.null(df))
    return(NULL)

  cols = c("AUTORE", "TITOLO", "TRADUTTORE", "CASA EDITRICE", "ANNO PUBBLICAZIONE", 
           "COLLOCAZIONE PRINCIPALE", "TAG", "NUMERO INVENTARIO", input$show_vars)

   DT::datatable(
    df[, cols, with = F],
    selection = list(mode="single", selected = c(1)), 
    filter = list(position = 'none'), 
    options = list(pageLength = 20, 
                   searchHighlight = TRUE, 
                   scrollX = TRUE,
                   autoWidth = TRUE),                   
    rownames = FALSE)
})

