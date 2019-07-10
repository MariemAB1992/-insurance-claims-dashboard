library(shiny)
library(shinydashboard)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(ECharts2Shiny)
library(RODBC)
library(DBI)
library(Rcpp)
ui <- dashboardPage(skin ="yellow",
                    dashboardHeader(title = " Production", tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                                                                     tags$title("Production"))  ),
                    dashboardSidebar(sidebarMenu(id="sbmenu",
                                                 
                                                 menuItem("Visit-us", icon = icon("send",lib='glyphicon')),
                                                 menuItem("Tables", tabName = "widgets", icon = icon("th")),
                                                 menuItem("Graphiques", tabName = "plots", icon = icon("line-chart")),
                                                 downloadButton("downloadData", "Download")
                    )),
                    
                    dashboardBody(tabItems(
                      tabItem("widgets",box(title = "Date and Agent choices", status = "primary", solidHeader = T, width = 12,
                                            fluidPage(
                                              
                                              fluidRow(
                                                column(1,offset = 0, style='padding:1px;',
                                                       dateInput("select1","date debut",datesdisabled = c("NONE",as.Date(DATA$date, format = "%m/%d/%Y")))),
                                                column(1,offset = 0, style='padding:1px;',
                                                       dateInput("select11","date fin",datesdisabled = c("NONE",as.Date(DATA$date, format = "%m/%d/%Y")))),
                                                
                                                column(3,offset = 1, style='padding:1px;',
                                                       selectInput("select2","Agent",c("ALL",sort(unique(as.character(DATA$agent))))))
                                              )),
                                            column(
                                              DT::dataTableOutput("Table_SP"), width = 12
                                            ),
                                            valueBoxOutput("vboc3",width = 2),
                                            valueBoxOutput("ibox",width = 3),
                                            valueBoxOutput("vboc",width = 2),
                                            valueBoxOutput("vboc2",width =2),
                                            valueBoxOutput("vboc4",width = 3)
                                            
                      ),
                      
                      box(title = "Results",  status = "info", solidHeader  = T, width = 12,
                          
                          column(
                            DT::dataTableOutput("Table_us"), width =3
                          ),column(
                            DT::dataTableOutput("Table_gar"), width =3
                          ),column(
                            DT::dataTableOutput("Table_le"), width = 3
                          ),column(
                            DT::dataTableOutput("Table_bn"), width = 3
                          ))),
                      
                      
                      tabItem("plots",box(title = "Graphiques explicatifs", status = "primary", solidHeader = T, width = 12,
                                          fluidPage(loadEChartsLibrary(),fluidRow(column(3,offset = 0, style='padding:1px;',
                                                                                         selectInput("select3","Agent",c("ALL",sort(unique(as.character(DATA$agent)))))),
                                                                                  column(12,
                                                                                         h4("Courbes", align = "center"),
                                                                                         tags$div(id="test_1", style="width:100%;height:400px;"),  # Specify the div for the chart. Can also be considered as a space holder
                                                                                         deliverChart(div_id = "test_1")
                                                                                         # Deliver the plotting
                                                                                  )
                                          )
                                          )
                      )))
                    ))





