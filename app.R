library(shiny)
library(ggplot2)
library(plotly)
library(stringr)

library(xml2)
library(markdown)
library(shinythemes)
data = list()


Plot <- function(data,input){
  tableau = data[["tableau"]]
  Title = paste("<b>Gallicagram a épluché", as.character(sum(tableau$base_temp)))
  Title = Title %>% paste(' numéros,\n et trouvé "', data[["mot"]],sep="") 
  Title = Title %>% paste(as.character(sum(tableau$nb_temp)),sep = '" dans ')
  Title = Title %>% paste("d'entre eux</b>")
  width = length(unique(tableau$date))
  span = 2/width + input$span*(width-2)/(10*width)
  tableau$loess = tableau$nb_temp
  for(mot in str_split(data$mot,"&")[[1]]){
    z = which(tableau$mot==mot)
    x = 1:length(z)
    tableau$loess[z] = loess(tableau$ratio_temp[z]~x,span=span)$fitted
  }
  tableau$hovers = str_c(tableau$date,": x = ",tableau$nb_temp,", N = ",tableau$base_temp)
  plot = plot_ly(tableau, x=~date,y=~loess,text=~hovers,color =~mot,type='scatter',mode='spline',hoverinfo="text")
  y <- list(title = "Fréquence d'occurence dans Gallica-presse",titlefont = 41,tickformat = "%")
  x <- list(title = data[["resolution"]],titlefont = 41)
  plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  if(length(grep(",",data$mot))==0){plot = layout(plot,showlegend=TRUE)}
  if(input$barplot){
    width = nrow(tableau)
    span = 2/width + input$span*(width-2)/(10*width)
    tableau$hovers = str_c(tableau$date,": N = ",tableau$base_temp)
    plot1 = plot_ly(tableau, x=~date,y=~base_temp,text=~hovers,type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'))
    y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
    x <- list(title = data[["resolution"]],titlefont = 41)
    plot1 = layout(plot1, yaxis = y, xaxis = x,title = Title,showlegend = FALSE)
    plot = subplot(plot,plot1,nrows = 2,legend=NULL,shareX = T)
    return(plot)
  } else{
    return(plot)
  }
}

get_data <- function(mot,from,to,resolution,doc_type){
  mots = str_split(mot,"&")[[1]]
  tableau<-as.data.frame(matrix(nrow=0,ncol=4),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  for (i in from:to){
    for(mot in mots){
      mot2 = str_replace_all(mot," ","%20")
      ###
      or<-""
      or_end<-""
      if(str_detect(mot2,"[+]")){
      mots_or = str_split(mot2,"[+]")[[1]]
      or1<-NA
      or1_end<-NA
      for (j in 2:length(mots_or)) {
        
        or1[j]<-str_c("or%20text%20adj%20%22",mots_or[j],"%22%20")
        or1_end[j]<-str_c("%20",mots_or[j])
        or<-str_c(or,or1[j])
        or_end<-str_c(or_end,or1_end[j])
      }
      mot1<-mots_or[1]}
      else{mot1=mot2}
      ###
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      if( i%%4==0){end_of_month[2]=29} #Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
      y<-as.character(i)
      if(resolution=="Année"){beginning = str_c(y,"/01/01")
      end = str_c(y,"/12/31")}
      I = 1
      if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
      
      
      if(doc_type==1){
      for(j in I){
        if(resolution=="Mois"){
          z = as.character(j)
          if(nchar(z)<2){z<-str_c("0",z)}
          beginning = str_c(y,"/",z,"/01")
          end = str_c(y,"/",z,"/",end_of_month[j])}
        url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
        ngram<-as.character(read_xml(url))
        a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
        url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
        ngram_base<-as.character(read_xml(url_base))
        b<-str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
        tableau[nrow(tableau)+1,] = NA
        date=y
        if(resolution=="Mois"){date = paste(y,z,sep="/")}
        tableau[nrow(tableau),]<-c(date,a,b,mot)
      }}
      
      if(doc_type==2){
        url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=true&exactSearch=true&version=1.2&query=(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)&suggest=10&keywords=",mot1,or_end)
        ngram<-as.character(read_xml(url))
        a<-str_extract(str_extract(ngram,"numberOfRecords>[:digit:]+"),"[:digit:]+")
        url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=true&exactSearch=true&version=1.2&query=(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords=")
        ngram_base<-as.character(read_xml(url_base))
        b<-str_extract(str_extract(ngram_base,"numberOfRecords>[:digit:]+"),"[:digit:]+")
        tableau[nrow(tableau)+1,] = NA
        date=y
        tableau[nrow(tableau),]<-c(date,a,b,mot)
      }
    }
    progress$inc(1/(to-from), detail = paste("Gallicagram ratisse l'an", i))
  }
  colnames(tableau)<-c("date","nb_temp","base_temp","mot")
  format = "%Y"
  if(resolution=="Mois"){format=paste(format,"%m",sep="/")}
  tableau.date = as.Date(as.character(tableau$date),format=format)
  tableau$nb_temp<-as.integer(tableau$nb_temp)
  tableau$base_temp<-as.integer(tableau$base_temp)
  tableau$ratio_temp<-tableau$nb_temp/tableau$base_temp
  data = list(tableau,paste(mots,collapse="&"),resolution)
  names(data) = c("tableau","mot","resolution")
  return(data)}


ui <- navbarPage("Gallicagram",
                 tabPanel("Graphique",fluidPage(),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            textInput("mot","Terme(s) à chercher","Clemenceau"),
                                            p('Séparer les termes par un "&" pour une recherche multiple'),
                                            p('Utiliser "a+b" pour rechercher a OU b'),
                                            radioButtons("doc_type", "Corpus :",choices = list("Presse" = 1, "Livres" = 2),selected = 1),
                                            numericInput("beginning","Début",1914,min=1631,max=2019),
                                            numericInput("end","Fin",1920,min=1631,max=2019),
                                            sliderInput("span",
                                                        "Lissage de la courbe :",
                                                        min = 0,
                                                        max = 10,
                                                        value = 0),
                                            selectInput("resolution", label = "Résolution :", choices = c("Année","Mois")),
                                            actionButton("do","Générer le graphique"),
                                            checkboxInput("barplot", "Afficher la distribution des numéros de presse\ndans la base Gallica", value = FALSE),
                                            downloadButton('downloadData', 'Télécharger les données')
                                          ),
                                          
                                          mainPanel(plotlyOutput("plot"),
                                                    headerPanel(""),
                                                    plotlyOutput("plot1")))),
                 tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                 tabPanel("Corpus",plotlyOutput("corpus_presse"),plotlyOutput("corpus_livres"))
)



# Define server logic required to draw a histogram
server <- function(input, output){
  output$corpus_presse = renderPlotly(Barplot1())
  output$corpus_livres = renderPlotly(Barplot2())
  observeEvent(input$do,{
    datasetInput <- reactive({
      data$tableau})
    df = get_data(input$mot,input$beginning,input$end,input$resolution,input$doc_type)
    output$plot <- renderPlotly({Plot(df,input)})
    if(input$barplot){
      output$plot1 <- renderPlotly({Plot1(df,input)})}
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(df$tableau, con)
      })
  })
  
  
}
Barplot1 <- function(){table<-read.csv("distribution_gallica_presse_ocerise.csv")
table$hovers = str_c(table$date,": N = ",table$base_temp)
plot2<-plot_ly(table, x=~date,y=~base_temp,text=~hovers,type='bar',hoverinfo="text")
Title = paste("<b>Répartition des numéros de presse océrisés dans Gallica<b>")
y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
x <- list(title = "Date",titlefont = 41)
plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
plot2}

Barplot2 <- function(){table<-read.csv("distribution_gallica_livres_ocerise_collapsing_true.csv")
table$hovers = str_c(table$date,": N = ",table$base_temp)
plot2<-plot_ly(table, x=~date,y=~base_temp,text=~hovers,type='bar',hoverinfo="text")
Title = paste("<b>Répartition des livres océrisés dans Gallica<b>")
y <- list(title = "Nombre de livres dans Gallica",titlefont = 41)
x <- list(title = "Date",titlefont = 41)
plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
plot2}

shinyApp(ui = ui, server = server)
