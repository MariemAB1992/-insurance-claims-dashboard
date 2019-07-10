

server <- function(input, output) {
 
   #macro des fitres: 
  macro_dat <- function(ag,dat,dat1){
    DATA$date<-as.Date(DATA$date,format = "%d/%m/%Y")
    if((ag =="ALL")&((length(dat)==0))&(length(dat1)==0)){
      return (DATA)}
    
    if((length(dat)>0)&(length(dat1)>0)){
      DATA <- DATA[DATA$date>=dat,]
      DATA <- DATA[DATA$date<=dat1,]
      }
    
    if((ag!="ALL")&((length(dat)>0))&(length(dat1)>0)){
      DATA <- DATA[DATA$agent==ag & DATA$date>=dat,]
      DATA <- DATA[DATA$date<=dat1,]
      }
    if((ag!="ALL")&((length(dat)==0))&(length(dat1)==0)){
      DATA <- DATA[DATA$agent==ag,]}
    
    return(DATA)} 
  
  
  
  macro_dat2<- function(dat,dat1){
    DATA$date<-as.Date(DATA$date,format = "%d/%m/%Y")
    if((length(dat)==0)&(length(dat1)==0)){
      return (DATA)}
    
    else{
      DATA <- DATA[DATA$date>=dat,]
      DATA <- DATA[DATA$date<=dat1,]}
    
    return(DATA)} 
  
  
  
  #macro des  petits tables au dessous 
  
  macro_usage <- function(base){
    
    value<-sqldf("select usage, count(*) as totale  from base group by usage")
    
    return(value)}
  
  
  macro_gar <- function(base){
    
    value1<-sqldf("select garantie, count(*) as totale from base group by garantie")
    
    return(value1)} 
  
  macro_leas <- function(base){
    
    value2<-sqldf("select Nant , count(*) as totale  from base group by Nant")
    
    return(value2)} 
  
  macro_bn <- function(base){
    
    value2<-sqldf("select bonmal, count(*) as totale  from base group by bonmal")
    
    return(value2)} 

  
  #macro des boxes
  #total contrats
  macro_count <- function(base,ag,dat,dat1){
    if((ag == "") &(length(dat)==0)&(length(dat1)==0)){
      return (0)
    }
    else{
      value3<-sqldf("select  count(*)  from base ")}
    
    return(h4(value3))}
  
  
  #totale prime(modification ici):  
  
  macro_count2<- function(base,ag,dat,dat1){
    if((ag == "") &(length(dat)==0)&(length(dat1)==0)){
      
      return (h4(0))}
    
    else{
      valeur<-sum(as.numeric(base$PTOT))
      return(h4(valeur))
    }}
  
  #Maximum(modification ici): 
  macro_count3<- function(base){
    
    
    base<-as.data.frame(base)
    m<- max(as.numeric(base[,5]),na.rm = TRUE)
    
    return (m)} 
  
  #moyenne age vehicule
  macro_moy<-function(base,ag,dat,dat1){
    if((ag == "") &(length(dat)==0)&(length(dat)==0)){
      
      return (h4(0))}
    else
      
      return(c(round(mean(as.numeric(base$vie)/365),2)))
  }
  
  
  
  #tables reactives
  
  table_date2<- reactive({
    macro_dat2(input$select1,input$select11)})
  
  
  table_date <- reactive({
    macro_dat(input$select2,input$select1,input$select11)})
  
  
  tablegraph<-reactive({
    macro_dat3(input$select3)
  })
  
  
  
  #output des petites tables 
  
  output$Table_SP <- DT::renderDataTable({
    DT::datatable(table_date(), options = list(pageLength = 2))
  })
  
  output$Table_le<-DT::renderDataTable({
    DT::datatable(macro_leas(table_date()),options = list(
      pageLength = 2, autoWidth = TRUE))
  }) 
  
  
  output$Table_us<-DT::renderDataTable({
    DT::datatable(macro_usage(table_date()),options = list(
      pageLength = 2, autoWidth = TRUE))
  })
  
  output$Table_gar<-DT::renderDataTable({
    DT::datatable(macro_gar(table_date()),options = list(
      pageLength = 2, autoWidth = TRUE))
  }) 
  output$Table_bn<-DT::renderDataTable({
    DT::datatable(macro_bn(table_date()),options = list(
      pageLength = 2, autoWidth = TRUE))
  }) 
  
  
  # output des box
  
  output$vboc<- renderValueBox({
    valueBox(
      h4("Totale Contrats"),
      color="yellow",
      macro_count(table_date(),input$select2,input$select1,input$select11),
      icon = icon("calendar"))})
  
  
  output$vboc2<- renderValueBox({
    valueBox(
      h4("Totale Primes(dt)"),
      color="yellow",
      macro_count2(table_date(),input$select2,input$select1,input$select11),
      icon = icon("calendar")) })
  
  output$vboc3<- renderValueBox({
    valueBox(
      h4("Prime max(dt)"),
      color="blue",
      h4(macro_count3(table_date2()))
      
    )
  })
  output$ibox <- renderInfoBox({
    infoBox(
      h5("Prime max pour l'agent"),
      
      DATA[DATA$PTOT==macro_count3(table_date2()),]$agent,
      icon = icon("thumbs-up", lib = "glyphicon"))})
  
  
  output$vboc4<- renderValueBox({
    valueBox(
      h4("Age moyen des voitures(ans) "),
      color="red",
      h4(macro_moy(table_date(),input$select2,input$select1,input$select11)),
      icon =  icon("refresh")
    )
  })
  
  
  
  
  #output des downloads  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select2," ", input$select1,"to",input$select11, ".csv", sep = "")
    },
    content = function(file) {
      
      
      write.table(macro_gar(table_date()),file,sep=",", row.names = FALSE)
      write.table(macro_usage(table_date()),file,sep=",", row.names = FALSE,append=TRUE)
      
      write.table(macro_leas(table_date()),file,sep=",", row.names = FALSE,append=TRUE)
      write.table(macro_bn(table_date()),file,sep=",", row.names = FALSE,append=TRUE)
      
      write.table(cbind(macro_count3(table_date2()),DATA[DATA$PTOT==macro_count3(table_date2()),]$agent),file,sep=",",col.names=c("prime max","agentMax"), row.names = FALSE,append=TRUE)
      
    }
  )
  
  
  DATA2<-DATA[,c(1,4,5)]
  macro_dat4<- function(ag){
    if (ag=="ALL"){
      return (DATA2)
    }else
      
    {DATA2 <- DATA2[DATA2$agent==ag,]}
    
    
    return(DATA2)}
  
  macro_count4 <- function(base){
    base$date<-as.Date(base$date,format = "%d/%m/%Y")
    DATA3<-sqldf("select date, sum(PTOT) as t , count(*) as n from base group by date   ")
    date<-as.Date(DATA3$date,format = "%d/%m/%Y")
    DATA3<-DATA3[,2:3]
    DATA3<-data.frame(DATA3)
    row.names(DATA3)<-date
    names(DATA3)<-c("Prime Totale","Totale Contrats")
    DATA3 <- DATA3[ order(row.names(DATA3)), ]
    return(DATA3)}
  
  observeEvent(macro_count4(macro_dat4(input$select3)),
               {
                 renderLineChart(div_id = "test_1",show.slider.axis.x = TRUE,font.size.axis.x = 20,
                                 data = macro_count4(macro_dat4(input$select3)),
                                 line.width =3,
                                    
                                 line.type = "solid"
                 )
               })

  
}

shinyApp(ui, server)
