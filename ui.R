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
library(ngramr)
library(dplyr)
library(htmltools)
library(shinyWidgets)

shinyUI(navbarPage("Gallicagram",
                   tabPanel("Graphique",fluidPage(),
                            tags$head(
                                tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                textInput("mot","Recherche","Joffre&Pétain&Foch"),
                                                uiOutput("instructions"),
                                                conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                                conditionalPanel(condition="(input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2",p('Recherche limitée à un seul syntagme dans 5 000 documents au maximum')),
                                                conditionalPanel(condition="((input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2) || input.doc_type == 4",textOutput("avertissement")),
                                                conditionalPanel(condition="input.language == 1",radioButtons("bibli", "",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3),inline = T)),
                                                selectInput("doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,"Livres / Ngram Viewer - Google Books" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27, "Presse québécoise / BAnQ"=28),selected = 1),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",selectInput("language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",selectInput("search_mode", "Mode de recherche",choices = list("Par document" = 1),selected = 1)),

                                                conditionalPanel(condition="input.doc_type == 3",radioButtons("filtre", "",choices = list("Filtre thématique"=1,"Filtre géographique"=2),inline = T)),
                                                conditionalPanel(condition="input.doc_type == 3 && input.filtre == 1",selectInput("theme_presse", "Thématique",choices = list("Liste de titres personnalisée"=1,"Principaux quotidiens"=2,
                                                                                                                                                                              "Hebdomadaires"=3,"Feuilles de tranchée"=4,
                                                                                                                                                                              "Principaux titres de la Résistance"=5,"Journaux clandestins - zone sud"=6,
                                                                                                                                                                              "Journaux clandestins - zone réservée"=7,"Journaux clandestins - Alsace-Moselle"=8,
                                                                                                                                                                              "Journaux clandestins - Empire français"=9,"Journaux clandestins - Région parisienne"=10,
                                                                                                                                                                              "Journaux clandestins - zone nord"=11,"Anciens combattants"=12,
                                                                                                                                                                              "Faits-divers"=13,"Presse artistique"=14,
                                                                                                                                                                              "Presse coloniale"=15,"Presse culinaire"=16,
                                                                                                                                                                              "Presse d'annonces"=17,"Presse de cinéma"=18,
                                                                                                                                                                              "Presse de loisir"=19,"Presse de mode"=20,
                                                                                                                                                                              "Presse de musique"=21,"Presse de spectacles"=22,
                                                                                                                                                                              "Presse des immigrations"=23,"Presse économique"=24,
                                                                                                                                                                              "Presse enfantine"=25,"Journaux d'entreprises"=26,
                                                                                                                                                                              "Presse financière"=27,"Presse par secteurs d'activité"=28,
                                                                                                                                                                              "Annuaires professionnels"=29,"Presse féminine"=30,
                                                                                                                                                                              "Presse féministe"=31, "Presse littéraire"=32,
                                                                                                                                                                              "Presse médicale"=33,
                                                                                                                                                                              "Presse ouvrière"=34,"Presse politique : Révolution-Empire"=35,
                                                                                                                                                                              "Presse politique : Restauration-Second Empire"=36,"Presse politique : 3e République"=37,
                                                                                                                                                                              "Presse professionnelle"=38,"Presse religieuse"=39,
                                                                                                                                                                              "Presse scientifique"=40,"Presse sportive"=41,
                                                                                                                                                                              "Presse syndicale"=42
                                                                                                                                                                              ))),
                                                conditionalPanel(condition="input.doc_type == 3 && input.filtre == 2",selectInput("theme_presse", "Thématique",choices = list( "Liste de titres personnalisée"=1,
                                                                                                                                                                               "Ain "=51,                    "Aisne"=52,                   "Allier "=53,                 "Alpes-de-Haute-Provence"=54, "Alpes Maritimes"=55,         "Ardèche"=56,                
                                                                                                                                                                               "Ardennes"=57,                "Ariège"=58,                  "Aube"=59,                    "Aude"=60,                    "Aveyron"=61,                 "Bas-Rhin"=62,               
                                                                                                                                                                               "Bouches-du-Rhône"=63,        "Calvados"=64,                "Cantal"=65,                  "Charente"=66,                "Charente-maritime"=67,       "Cher"=68,                   
                                                                                                                                                                               "Corrèze"=69,                 "Corse"=70,                   "Côte d'or"=71,               "Côtes d'Armor"=72,           "Creuse"=73,                  "Deux-Sèvres"=74,            
                                                                                                                                                                               "Dordogne"=75,                "Doubs"=76,                   "Drôme"=77,                   "Essonne"=78,                 "Eure"=79,                    "Eure-et-Loir"=80,           
                                                                                                                                                                               "Finistère"=81,               "Gard"=82,                    "Gers"=83,                    "Gironde"=84,                 "Haut-Rhin"=85,               "Haute-Garonne"=86,          
                                                                                                                                                                               "Haute-Loire"=87,             "Haute-Marne"=88,             "Haute-Saône"=89,             "Haute-Vienne"=90,            "Hautes-Alpes"=91,            "Hautes Pyrénées"=92,        
                                                                                                                                                                               "Hauts-de-Seine"=93,          "Hérault"=94,                 "Ille-et-Vilaine"=95,         "Indre"=96,                   "Indre-et-Loire"=97,          "Isère"=98,                  
                                                                                                                                                                               "Jura"=99,                    "Landes"=100,                  "Loir-et-Cher"=101,            "Loire"=102,                   "Loire-Atlantique"=103,        "Loiret"=104,                 
                                                                                                                                                                               "Lot"=105,                     "Lot-et-Garonne"=106,          "Lozère"=107,                  "Maine-et-Loire"=108,          "Manche"=109,                  "Marne"=110,                  
                                                                                                                                                                               "Mayenne"=111,                 "Meurthe-et-Moselle"=112,      "Meuse"=113,                   "Morbihan"=114,                "Moselle"=115,                 "Nièvre"=116,                 
                                                                                                                                                                               "Nord"=117,                    "Oise"=118,                    "Orne"=119,                    "Outre-mer"=120,               "Paris"=121,                   "Pas-de-Calais"=122,          
                                                                                                                                                                               "Puy-de-Dôme"=123,             "Pyrenées-Atlantiques"=124,    "Pyrénées-Orientales"=125,     "Rhône"=126,                   "Saône-et-Loire"=127,          "Sarthe"=128,                 
                                                                                                                                                                               "Savoie"=129,                  "Seine-et-Marne"=130,          "Seine-maritime"=131,          "Seine-Saint-Denis"=132,       "Somme"=133,                   "Tarn"=134,                   
                                                                                                                                                                               "Tarn-et-Garonne"=135,         "Territoire de Belfort"=136,   "Val-d'Oise"=137,              "Val-de-Marne"=138,            "Var"=139,                     "Vaucluse"=140,               
                                                                                                                                                                               "Vendée"=141,                  "Vienne"=142,                  "Vosges"=143,                  "Yonne"=144,                   "Yvelines"=145
                                                                                                                                                                              ))),
                                                conditionalPanel(condition="input.doc_type == 3",uiOutput("titres")),
                                                conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                                                           accept = c(
                                                                                                               'text/csv',
                                                                                                               'text/comma-separated-values',
                                                                                                               '.csv'
                                                                                                           ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("beginning","Début",1914)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("end","Fin",1920)),
                                                radioButtons("resolution", label = "Résolution", choices = c("Année","Mois"),inline=T),
                                                conditionalPanel(condition="input.doc_type == 1 || (input.doc_type == 3 && input.search_mode == 1) || input.doc_type == 5 || input.doc_type == 6 || input.doc_type == 7 || input.doc_type == 8 || input.doc_type == 9 || input.doc_type == 10 || input.doc_type == 11 || input.doc_type == 12 || input.doc_type == 13 || input.doc_type == 14 || input.doc_type == 15 || input.doc_type == 16 || input.doc_type == 17 || input.doc_type == 18 || input.doc_type == 19 || input.doc_type == 20 || input.doc_type == 21 || input.doc_type == 22 || input.doc_type == 23 || input.doc_type == 24 || input.doc_type == 25 || input.doc_type == 26 || input.doc_type == 27 || input.doc_type == 28 || (input.doc_type == 2 && input.search_mode == 1) || (input.doc_type == 4 && output.fileUploaded == 1 && output.avertissement.includes('Modifiez')==false) || ((input.doc_type == 2 || input.doc_type == 3) && input.search_mode == 2 && output.avertissement.includes('Modifiez')==false)",actionButton("do","Générer le graphique")),
                                                p(""),
                                                sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0)
                                            ),
                                            
                                            mainPanel(dropdownButton(tags$h3("Options avancées"),
                                                                     checkboxInput("barplot", "Afficher la distribution des documents de la base de données ", value = FALSE),
                                                                     checkboxInput("correlation_test", "Afficher les matrices de corrélation", value = FALSE),
                                                                     checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                     checkboxInput("scale", "Rééchelonner les résultats", value = FALSE),
                                                                     # checkboxInput("ponderation", "Pondérer les résultats par le volume de la base", value = FALSE),
                                                                     checkboxInput("multicourbes", "Afficher toutes les données de la session dans le graphique", value = FALSE),
                                                                     downloadButton("data_session","Télécharger les données de la session"),
                                                                     circle = TRUE, status = "default",
                                                                     icon = icon("sliders"), width = "300px",
                                                                     tooltip = tooltipOptions(title = "Afficher les options avancées")
                                                                     ),
                                                      plotlyOutput("plot"),
                                                      fluidRow(uiOutput("legende"),align="right"),
                                                      fluidRow(textOutput("legende0"),align="right"),
                                                      fluidRow(textOutput("legende1"),align="right"),
                                                      fluidRow(textOutput("legende4"),align="right"),
                                                      fluidRow(textOutput("legende3"),align="right"),
                                                      fluidRow(textOutput("legende2"),align="right"),
                                                      conditionalPanel(condition="input.correlation_test",p("")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr2"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="right")),
                                                      div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Télécharger les données')),
                                                      div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Télécharger le graphique interactif')),
                                                      p(""),
                                                      h2(textOutput("currentTime"), style="color:white")
                                            ))),
                   tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                   tabPanel("Corpus de presse",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                radioButtons("corpus_structure_p", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Mode d'accès"=3,"Bibliothèque d'origine"=4, "Classement thématique de Dewey" = 5,"Périodicité" = 6),selected = 1),
                                                conditionalPanel(condition="input.corpus_structure_p!=0",checkboxInput("corpus_relative_p", "Afficher les résultats en valeurs relatives", value = FALSE))
                                            ),
                                            mainPanel(
                                                fluidRow(plotlyOutput("corpus1")),
                                                p("")
                                            )
                            )
                   ),
                   tabPanel("Corpus de livres",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                radioButtons("corpus_structure_l", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Droits d'auteur" = 3, "Bibliothèque d'origine" = 4,"Volume en nombre de pages" = 5,"Etat de la numérisation"=7,"Qualité d'océrisation"=8,"Date de numérisation"=9, "Classement thématique de Dewey" = 10),selected = 1),
                                                conditionalPanel(condition="(input.corpus_structure_l==1 || input.corpus_structure_l==2 || input.corpus_structure_l==3 || input.corpus_structure_l==4 || input.corpus_structure_l==7 || input.corpus_structure_l==10)",
                                                                 checkboxInput("corpus_relative_l", "Afficher les résultats en valeurs relatives", value = FALSE)
                                                ),
                                                conditionalPanel(condition="input.corpus_structure_l==1",checkboxInput("corpus_ngram_l", "Distribution des livres dans Google Ngram Viewer", value = FALSE))
                                            ),
                                            mainPanel(
                                                conditionalPanel(condition="input.corpus_structure_l!=8",fluidRow(plotlyOutput("corpus2")),
                                                                 p("")),
                                                conditionalPanel(condition="input.corpus_structure_l==8",img(src = "nqamoyen.png", height = 589, width = 681)),
                                                conditionalPanel(condition="input.corpus_structure_l==9",fluidRow(plotlyOutput("corpus3"))),
                                                conditionalPanel(condition="input.corpus_structure_l==9",img(src = "numerisation.png", height = 589, width = 681)),
                                                conditionalPanel(condition="input.corpus_structure_l==5",fluidRow(plotlyOutput("corpus4")),
                                                                 p(""),
                                                                 fluidRow(plotlyOutput("corpus5"))),
                                                p("")
                                            )
                            )
                   ),
                   tabPanel("Tutoriel",headerPanel("Tutoriel"),
                            fluidPage(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicapresse' target='_blank'>Gallicapresse"))
))