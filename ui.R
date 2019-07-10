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
odbc.key = odbcConnect("MAGBI", uid="bi", pwd="pwdbi")
ex <- sqlQuery( channel= odbc.key, query = "SELECT POL,agent ,TARIF as usage ,CLBM as bonmal,DTEMIS as date,PTOT,CAST( 
                CASE 
                WHEN (INC != '0' or VOL != '0') and DCL ='0' and PRDOM='0' 
                THEN 'VOLINC'
                
                WHEN RCRTI!='0' and VOL='0' and INC='0' and DCL='0' and PRDOM='0'
                THEN 'RC'
                
                WHEN DCL!='0'
                THEN 'DC'
                WHEN PRDOM!='0'
                THEN 'TR'  
                
                END AS char) as garantie
                FROM [MAGBI].[dbo].[biemis] where TYPQ='2' and RISQ='30' and PTOT>5.7 ")

ex1<- sqlQuery( channel= odbc.key, query ="SELECT nant, pol
                FROM [MAGBI].[dbo].[Binant] ")
ex2<-sqlQuery( channel= odbc.key, query ="SELECT pol,mec
               FROM [MAGBI].[dbo].[BIvehicule] ")

ex3<-merge(ex,ex1,by.x='POL',by.y='pol',all.x=TRUE)
ex3[is.na(ex3)]<-'x'
ex4<-merge(ex3,ex2,by.x='POL',by.y='pol',all.x=TRUE)
vie<-difftime(ex4$date, ex4$mec, units = "days")
ex4<-cbind(ex4,vie)
ex4<-ex4[,c(2,3,4,5,6,7,8,9,10,1)]
ex4<-ex4[,-8]

DATA<-as.data.frame(ex4)



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





