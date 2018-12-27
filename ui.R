## ui.R ##
library(shinydashboard)
options(shiny.maxRequestSize = 9*1024^2)

ui <- dashboardPage(skin = "blue",
                    # header = dashboardHeader(title = environment_title, titleWidth  = 400),
                    header = dashboardHeader( titleWidth = NULL,
                                              title = "Gnome production"),
                    
                    sidebar = dashboardSidebar(
                      width = 231,
                      uiOutput("sidebar_ui"),
                      tags$br(),
                      tags$a(tags$img(src = 'reading_tiger_by_mmerz.jpg', height = 250, width = 200, align = "left"))
                      
                    ),
                    body = dashboardBody(
                      fluidRow(

                        tags$head(tags$style(HTML('/* length of content-wrapper */
                                                  #Reporting { height:90vh !important; }
                                                  
                                                  /* logo */
                                                  .skin-blue .main-header .logo {
                                                  background-color: #10384F;
                                                  }
                                                  
                                                  /* solid primary box colors */
                                                  .box.box-solid.box-primary>.box-header {
                                                                  color:#fff;
                                                  background:#10384F
                                                  }
                                                  
                                                  /* solid primary box colors */
                                                  .box.box-solid.box-primary{
                                                  border-bottom-color:#10384F;
                                                  border-left-color:#10384F;
                                                  border-right-color:#10384F;
                                                  border-top-color:#10384F;
                                                  }
                                                  
                                                  /* logo when hovered */
                                                  .skin-blue .main-header .logo:hover {
                                                  background-color: #10384F;
                                                  }
                                                  
                                                  /* navbar (rest of the header) */
                                                  .skin-blue .main-header .navbar {
                                                  background-color: #10384F;
                                                  }
                                                  
                                                  /* main sidebar */
                                                  .skin-blue .main-sidebar {
                                                  background-color: #10384F;
                                                  }
                                                  
                                                  /* active selected tab in the sidebarmenu */
                                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                                  background-color: #D30F4B;
                                                  }
                                                  
                                                  /* other links in the sidebarmenu */
                                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .color: #FFFFFF a{
                                                  background-color: #10384F;
                                                  color: #000000;
                                                  }
                                                  
                                                  /* other links in the sidebarmenu when hovered */
                                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                                  background-color: #D30F4B;
                                                  }
                                                  /* toggle button when hovered  */
                                                  .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                                  background-color: #D30F4B;
                                                  }
                                                  .skin-blue .content-wrapper {
                                                  background-color: #ffffff;
                                                  }
                                                  '))),
                        tabItems(


                          tabItem(tabName = "create_tl",
                                   box(title = "", id = "tabset_tl", width = 12, height = NULL,
                                         uiOutput("library_ui")
                                                            )
                          )
                        )
                        )
                        )  
                    )