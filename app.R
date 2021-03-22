library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(Hmisc)
library(xml2)
library(markdown)
library(shinythemes)
library(htmlwidgets)
library(httr)
data = list()

js <- "
function(el, x) {
el.on('plotly_click', function(d) {
var point = d.points[0];
var url = point.data.customdata[point.pointIndex];
window.open(url);
});
}"

Plot <- function(data,input){
  tableau = data[["tableau"]]
  Title = paste("")
  width = length(unique(tableau$date))
  span = 2/width + input$span*(width-2)/(10*width)
  tableau$loess = tableau$nb_temp
  for(mot in str_split(data$mot,"&")[[1]]){
    z = which(tableau$mot==mot)
    x = 1:length(z)
    tableau$loess[z] = loess(tableau$ratio_temp[z]~x,span=span)$fitted
  }
  tableau$hovers = str_c(tableau$date,": x = ",tableau$nb_temp,", N = ",tableau$base_temp)
  plot = plot_ly(tableau, x=~date,y=~loess,text=~hovers,color =~mot,type='scatter',mode='spline',hoverinfo="text",customdata=tableau$url)
  #plot = onRender(plot,js)
  y <- list(title = "Fréquence d'occurence dans\nle corpus",titlefont = 41,tickformat = ".1%")
  x <- list(title = data[["resolution"]],titlefont = 41)
  plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  if(length(grep(",",data$mot))==0){plot = layout(plot,showlegend=TRUE)}
  
  if(input$delta==TRUE){
    mots<-str_split(input$mot,"&")
    x = 1:sum(tableau$mot==unlist(mots)[1])
    tableau$delta[tableau$mot==unlist(mots)[1]]<-loess((tableau$ratio_temp[tableau$mot==unlist(mots)[1]]-tableau$ratio_temp[tableau$mot==unlist(mots)[2]]~x),span=span)$fitted
    tableau$hovers2 = str_c(tableau$date,": delta = ",round(tableau$delta*100,digits=2),"%, N = ",tableau$base_temp)
    plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~delta,text=~hovers2,type='scatter',mode='spline',hoverinfo="text")
    y <- list(title = "Différence de fréquence\nd'occurence dans le corpus",titlefont = 41,tickformat = ".1%")
    x <- list(title = data[["resolution"]],titlefont = 41)
    Title = paste("Freq(",unlist(mots)[1],") – Freq(",unlist(mots)[2],")")
    Title=str_remove_all(Title," ")
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  }
  if(input$barplot){
    width = nrow(tableau)
    span = 2/width + input$span*(width-2)/(10*width)
    tableau$hovers = str_c(tableau$date,": N = ",tableau$base_temp)
    plot1 = plot_ly(tableau, x=~date[tableau$mot==mot[1]],y=~base_temp[tableau$mot==mot[1]],text=~hovers[tableau$mot==mot[1]],type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'))
    y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
    x <- list(title = data[["resolution"]],titlefont = 41)
    plot1 = layout(plot1, yaxis = y, xaxis = x,title = Title,showlegend = FALSE)
    plot= plot%>%add_lines()
    plot = plotly::subplot(plot,plot1,nrows = 2,legend=NULL,shareX = T)
    return(onRender(plot,js))
  } else{
    plot=layout(plot)
    return(onRender(plot,js))
  }
}

get_data <- function(mot,from,to,resolution,doc_type,titres){
  mots = str_split(mot,"&")[[1]]
  tableau<-as.data.frame(matrix(nrow=0,ncol=5),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  if(doc_type==1 & resolution=="Année"){
    base=read.csv("base_presse_annees.csv")
  } else  if(doc_type==1 & resolution=="Mois"){
    base=read.csv("base_presse_mois.csv")
  } else if(doc_type==2){
    base=read.csv("base_livres_annees.csv")
  }
  
  
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
      mot1<-mots_or[1]}else{mot1=mot2}
      
      if(doc_type==3 & length(titres)>1){
        ark1<-titres[1]
        ark3<-""
        for (v in 2:length(titres)) 
        {
          ark<-titres[v]
          ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
          ark3<-str_c(ark3,ark2)
        }
      }else
      {
        ark1<-titres
        ark3<-""
        }
      ###
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(i%%4==0){end_of_month[2]=29} #Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
      y<-as.character(i)
      if(resolution=="Année" | doc_type==2){beginning = str_c(y,"/01/01")
      end = str_c(y,"/12/31")}
      I = 1
      if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
      
      
      if(doc_type !=2){
      for(j in I){
        if(resolution=="Mois"){
          z = as.character(j)
          if(nchar(z)<2){z<-str_c("0",z)}
          beginning = str_c(y,"/",z,"/01")
          end = str_c(y,"/",z,"/",end_of_month[j])}
        url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
        if(doc_type == 3){
          url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)sortby%20dc.date%20")
          }
        ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
        a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
        
        if(doc_type == 3){
          url_base <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)%20sortby%20dc.date")
          ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
          b<-str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
        }
        if(resolution=="Mois"& doc_type==1){
          date=str_c(y,"/",z)
          b<-as.integer(base$base_temp[base$date==date])}
        else if (resolution=="Année" & doc_type==1){b<-as.integer(base$base_temp[base$date==y])}
        if(length(b)==0L){b=0}
        tableau[nrow(tableau)+1,] = NA
        date=y
        if(resolution=="Mois"){date = paste(y,z,sep="/")}
        tableau[nrow(tableau),]<-c(date,a,b,mot,url)
        progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))
      }}
      
      if(doc_type==2){
        url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=true&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)&suggest=10&keywords=",mot1,or_end)
        ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
        a<-str_extract(str_extract(ngram,"numberOfRecords>[:digit:]+"),"[:digit:]+")
        b<-as.integer(base$base_temp[base$date==y])
        if(length(b)==0L){b=0}
        tableau[nrow(tableau)+1,] = NA
        date=y
        tableau[nrow(tableau),]<-c(date,a,b,mot,url)
        progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))
        
      }
      
    }
  }
  colnames(tableau)<-c("date","nb_temp","base_temp","mot","url")
  tableau$url = str_replace(tableau$url,"SRU","services/engine/search/sru")
  tableau$url = str_replace(tableau$url,"maximumRecords=1","maximumRecords=25")
  format = "%Y"
  if(resolution=="Mois"){format=paste(format,"%m",sep="/")}
  tableau.date = as.Date(as.character(tableau$date),format=format)
  tableau$nb_temp<-as.integer(tableau$nb_temp)
  tableau$base_temp<-as.integer(tableau$base_temp)
  tableau$ratio_temp<-tableau$nb_temp/tableau$base_temp
  tableau$ratio_temp[is.na(tableau$ratio_temp)]<-0
  data = list(tableau,paste(mots,collapse="&"),resolution)
  names(data) = c("tableau","mot","resolution")
  return(data)}

data=list(read.csv("exemple.csv",encoding = "UTF-8"),"Joffre&Pétain&Foch","Années")
names(data)=c("tableau","mot","resolution")


correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = TRUE, 
                               replacement = ""){
  
 df=df[["tableau"]]
 df=select(df,mot,ratio_temp)
 mots<-unlist(unique(df$mot))
 a<-df$ratio_temp[df$mot==mots[1]]
 for (i in 2:length(mots)) {
   a<-cbind(a,df$ratio_temp[df$mot==mots[i]])
 }
 df=as.data.frame(a)
 colnames(df)=mots
   # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

ui <- navbarPage("Gallicagram",
                 tabPanel("Graphique",fluidPage(),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            textInput("mot","Terme(s) à chercher","Joffre&Pétain&Foch"),
                                            p('Séparer les termes par un "&" pour une recherche multiple'),
                                            p('Utiliser "a+b" pour rechercher a OU b'),
                                            radioButtons("doc_type", "Corpus :",choices = list("Presse" = 1, "Livres" = 2,"Recherche par titre de presse" = 3),selected = 1),
                                            conditionalPanel(condition="input.doc_type == 3",selectizeInput("titres","Titre des journaux",choices = "",selected=NULL,multiple = TRUE)),
                                            numericInput("beginning","Début",1914),
                                            numericInput("end","Fin",1920),
                                            sliderInput("span",
                                                        "Lissage de la courbe :",
                                                        min = 0,
                                                        max = 10,
                                                        value = 0),
                                            conditionalPanel(condition="input.doc_type != 2",
                                                              selectInput("resolution", label = "Résolution :", choices = c("Année","Mois"))),
                                            conditionalPanel(condition="input.doc_type == 2",
                                                             selectInput("resolution", label = "Résolution :", choices = c("Année"))),
                                            actionButton("do","Générer le graphique"),
                                            checkboxInput("barplot", "Afficher la distribution des documents\nde la base Gallica sur la période", value = FALSE),
                                            checkboxInput("correlation_test", "Afficher la matrice de corrélation", value = FALSE),
                                            checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE)
                                          ),
                                          
                                          mainPanel(plotlyOutput("plot"),
                                                    headerPanel(""),
                                                    div(style="display: inline-block;vertical-align:top",downloadButton('downloadData', 'Télécharger les données')),
                                                    div(style="display: inline-block;vertical-align:top",downloadButton('downloadPlot', 'Télécharger le graphique interactif')),
                                                    conditionalPanel(condition="input.correlation_test",tableOutput("corr")),
                                                    conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="left")),
                                                    fluidRow(textOutput("legende"),align="right"),
                                                    fluidRow(textOutput("legende0"),align="right"),
                                                    fluidRow(textOutput("legende1"),align="right"),
                                                    fluidRow(textOutput("legende2"),align="right"),
                                                    fluidRow(textOutput("legende3"),align="right"),
                                                    p("")
                                                    ))),
                 tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                 tabPanel("Corpus",plotlyOutput("corpus_presse"),plotlyOutput("corpus_livres")),
                 tabPanel(title=HTML("<li><a href='http://gallicagram.hopto.org:3838/gallicapresse/' target='_blank'>Gallicapresse"))
)



# Define server logic required to draw a histogram
server <- function(input, output,session){
  observeEvent(
    input$doc_type,
    {if(input$doc_type==3)
      {
        liste_journaux<-read.csv("liste_journaux.csv",encoding="UTF-8")
        updateSelectizeInput(session,"titres",choices = setNames(liste_journaux$ark,liste_journaux$title),selected="cb39294634r")
        
        titres<-reactive({liste_journaux$title[liste_journaux$ark==input$titres]})
        output$legende1<-renderText(paste(titres()))
      }
      
      if(input$doc_type!=3){output$legende1<-renderText(str_c(if(input$doc_type==1){"Corpus : presse\n"} else if (input$doc_type==2){"Corpus : livres\n"}))}
  })
  output$corpus_presse = renderPlotly(Barplot1())
  output$corpus_livres = renderPlotly(Barplot2())
  output$plot <- renderPlotly({Plot(data,input)})
  output$corr<-renderTable(correlation_matrix(data),rownames = TRUE)
  output$pvalue=renderText("***p<.001 ; **p<.01 ; *p<.05")
  output$legende=renderText("Source : gallica.bnf.fr")
  output$legende0=renderText("Affichage : Gallicagram par Benjamin Azoulay et Benoît de Courson")
  output$legende2<-renderText(str_c(as.character(sum(data[["tableau"]]$base_temp))," numéros épluchés\n"))
  output$legende3<-renderText(str_c(as.character(sum(data[["tableau"]]$nb_temp))," résultats trouvés"))
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data$tableau, con,row.names = F,fileEncoding = "UTF-8")
    })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('plot-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      htmlwidgets::saveWidget(as_widget(Plot(data,input)), con)
    })
  
  # observeEvent(input$doc_type,
  #              {
  #                titres<-reactive({input$titres})
  #                output$legende1<-renderText(str_c(if(input$doc_type==1){"Corpus : presse\n"} else if (input$doc_type==2){"Corpus : livres\n"} else{paste(titres(),"\n")}))
  #              })

  
  observeEvent(input$do,{
    datasetInput <- reactive({
      data$tableau})
    df = get_data(input$mot,input$beginning,input$end,input$resolution,input$doc_type,input$titres)
    
    output$plot <- renderPlotly({Plot(df,input)})
    
    output$legende2<-renderText(str_c(as.character(sum(df[["tableau"]]$base_temp))," numéros épluchés\n"))
    output$legende3<-renderText(str_c(as.character(sum(df[["tableau"]]$nb_temp))," résultats trouvés"))
    output$corr<-renderTable(correlation_matrix(df),rownames = TRUE)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(df$tableau, con,row.names = F,fileEncoding = "UTF-8")
      })
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste('plot-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        htmlwidgets::saveWidget(as_widget(Plot(df,input)), con)
      })
    })

  
}
Barplot1 <- function(){table<-read.csv("base_presse_annees.csv")
somme<-sum(table$base_temp)
table$hovers = str_c(table$date,": N = ",table$base_temp)
plot2<-plot_ly(table, x=~date,y=~base_temp,text=~hovers,type='bar',hoverinfo="text")
Title = paste("<b>Répartition des ",somme," numéros de presse océrisés dans Gallica<b>")
y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
x <- list(title = "Date",titlefont = 41)
plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
plot2}

Barplot2 <- function(){table<-read.csv("base_livres_annees.csv")
somme<-sum(table$base_temp)
table<-table[table$date>=1450,]
table$hovers = str_c(table$date,": N = ",table$base_temp)
plot2<-plot_ly(table, x=~date,y=~base_temp,text=~hovers,type='bar',hoverinfo="text")
Title = paste("<b>Répartition des ",somme," livres océrisés dans Gallica<b>")
y <- list(title = "Nombre de livres dans Gallica",titlefont = 41)
x <- list(title = "Date",titlefont = 41)
plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
plot2}

# compteur<-read.csv("/home/benjamin/Bureau/compteur_gallicagram.csv",encoding = "UTF-8")
# a<-as.data.frame(cbind(as.character(Sys.Date()),1))
# colnames(a)=c("date","count")
# compteur<-rbind(compteur,a)
# write.csv(compteur,"/home/benjamin/Bureau/compteur_gallicagram.csv",fileEncoding = "UTF-8",row.names = FALSE)

shinyApp(ui = ui, server = server)
