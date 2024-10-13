library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(DT)
library(bslib)

# Données et autres préparatifs
data_path <- "C:/Users/OLIVIER PORTABLE/Downloads/Projet R SHINY/app/cleaned_donnees_dpe_existant_renamed.csv"
if(file.exists(data_path)) {
  data <- read.csv(data_path, sep = ",", header = TRUE)
} else {
  stop("Data file not found.")
}

credentials <- data.frame(
  user = c("a", "A"),
  password = c("a", "A"),
  stringsAsFactors = FALSE
)

my_theme <- bs_theme(
  version = 4,
  bg = "#f7f7f7",
  fg = "#343a40",
  primary = "#007bff",
  secondary = "#6c757d",
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins"),
  "navbar-bg" = "#007bff"
)

ui <- secure_app(
  fluidPage(
    shinythemes::themeSelector(),
    page_navbar(
      theme = my_theme,
      nav_panel("🏠 Accueil", 
                fluidRow(
                  column(12, 
                         h3(strong("Évaluation de la consommation énergétique des logements :"), 
                            align = "center", style = "color: #007BFF;"),
                         p("Le ", strong("Diagnostic de Performance Énergétique (DPE)"), " est un outil essentiel pour évaluer la ", strong("consommation énergétique des logements."),
                           br(),
                           "Cette application utilise des données sur les DPE pour", strong(" offrir une analyse claire et efficace de la performance énergétique des logements."), 
                           align = "center", style = "font-size: 16px; line-height: 1.5;"),
                         br(),
                         h4(strong(style = "border-bottom: 2px solid #007BFF; padding-bottom: 5px;",
                                   "Aperçu des Fonctionnalités :"), align = "center"),
                         br(),
                         div(class = "centered",
                             tags$ul(class = "no-bullet",
                                     tags$li(em("* Un tableau de bord interactif pour visualiser les données énergétiques.")),
                                     tags$li(em("* Des cartes géographiques pour analyser la répartition de la consommation énergétique.")),
                                     tags$li(em("* Des outils d'analyse prédictive pour anticiper les besoins énergétiques futurs.")),
                                     tags$li(em("* Une section de documentation pour comprendre les méthodologies utilisées.")),
                                     tags$li(em("* La possibilité d'exporter les données analysées en format CSV ou TXT."))
                             )
                         ),
                         br(),
                         p("Pour plus d'informations, n'hésitez pas à explorer les autres sections de l'application.", 
                           style = "text-align: center; font-size: 16px; font-weight: bold;"),
                         br(),
                         h4(strong(style = "border-bottom: 2px solid #007BFF; padding-bottom: 5px;",
                                   "Présentation des données :"), align = "center"),
                         br(),
                         p("Les données utilisées dans cette application comprennent les informations suivantes :", style = "text-align: center;"),
                         div(class = "centered",
                             tags$ul(class = "no-bullet",
                                     tags$li(strong("Etiquette_DPE :"), " La classification énergétique des logements."),
                                     tags$li(strong("Conso_5_usages_é_finale :"), " La consommation énergétique finale des logements."),
                                     tags$li(strong("Surface_Habitable :"), " La superficie habitable des logements."),
                                     tags$li(strong("Type_bâtiment :"), " Le type de bâtiment (maison, appartement, etc.)."),
                                     tags$li(strong("geopoint :"), " Coordonnées géographiques pour la cartographie."),
                                     div(style = "text-align: center; margin-top: 50px;",
                                         img(src = "https://humancoders-formations.s3.amazonaws.com/uploads/course/logo/1833/formation-r-shiny.png",
                                             style = "max-width: 100%; height: auto; border-radius: 8px;")
                                     )
                             )
                         )
                  )
                )
      ),
      nav_panel("📊 Statistiques",
                fluidPage(
                  h2("Visualisation des Données"),
                  selectInput("dpe_filter", "Sélectionnez une Étiquette DPE :", choices = NULL), # Options mises à jour dans le server
                  plotOutput("energyDistribution", height = "300px"), 
                  plotOutput("boxPlot", height = "300px"),
                  plotOutput("histogram", height = "300px"),  
                  plotOutput("scatterPlot", height = "300px"),  
                  dataTableOutput("tableData") 
                )
      ),
      nav_panel("🗺 Cartographie", fluidPage(
        selectInput("codePostal", "Choisir le code postal:", choices = NULL), # Options mises à jour dans le server
        leafletOutput("mapView"),
        br(),
        h4("Informations sur le Code Postal Sélectionné :"),
        textOutput("postalInfo")
      )),
      nav_panel("🗎 Documentation",
                fluidRow(
                  column(12,
                         div(style = "text-align: center; padding: 20px; background-color: #f8f9fa; border-radius: 8px;",
                             h3(strong("Documentation utilisateur :"), style = "color: #007BFF;"),
                             p("Cette section fournit des explications sur l'utilisation de l'application, ainsi que des guides pour les utilisateurs."),
                             br(),
                             h4(strong("Explication des pages :"), style = "color: #007BFF;"),
                             p(strong("Accueil"), ": Présente un résumé des fonctionnalités principales de l'application."),
                             p(strong("Tableau de bord"), ": Permet de visualiser les données DPE sous forme de graphiques interactifs."),
                             p(strong("Cartographie"), ": Fournit une carte interactive pour visualiser la répartition géographique des classes DPE."),
                             p(strong("Analyse prédictive"), ": Génère des régressions entre différentes variables et affiche les statistiques associées."),
                             p(strong("Exportation"), ": Offre la possibilité de télécharger les données au format CSV ou TXT."),
                             br(),
                             h4(strong("Utilisation des filtres :"), style = "color: #007BFF;"),
                             p("Le tableau de bord et la cartographie permettent de filtrer les données par surface habitable et classe DPE à l'aide de filtres interactifs."),
                             br(),
                             h3(strong("Documentation technique :"), style = "color: #007BFF;"),
                             p("Cette section est destinée aux développeurs qui souhaitent comprendre et déployer l'application."),
                             br(),
                             h4(strong("Installation et déploiement :"), style = "color: #007BFF;"),
                             p("Voici les étapes pour installer et déployer l'application localement :"),
                             div(style = "background-color: #e9ecef; border-radius: 5px; padding: 10px; margin: 10px 0;",
                                 pre("git clone https://github.com/mon-projet/shiny-app.git"),
                                 pre("install.packages(c('shiny', 'ggplot2', 'dplyr', 'leaflet', 'shinyWidgets', 'shinymanager'))"),
                                 pre("shiny::runApp('path/to/app')")
                             ),
                             br(),
                             h4(strong("Liste des packages utilisés :"), style = "color: #007BFF;"),
                             p("* shiny : Framework pour l'interface utilisateur"),
                             p("* ggplot2 : Création de graphiques"),
                             p("* dplyr : Manipulation des données"),
                             p("* leaflet : Cartographie interactive"),
                             p("* shinyWidgets : Widgets supplémentaires pour l'interface"),
                             p("* shinymanager : Authentification sécurisée")
                         )
                  )
                )
      ),
      nav_panel("📁 Fichiers",
                fluidRow(
                  column(12,
                         div(style = "text-align: center; padding: 40px; background-color: #f8f9fa; border-radius: 8px;",
                             h3(strong("Exportation des données :"), style = "color: #007BFF;"),
                             p("Téléchargez vos données au format CSV ou TXT en utilisant les boutons ci-dessous.", style = "margin-bottom: 40px;"),
                             
                             div(style = "margin-bottom: 40px;",
                                 downloadButton("downloadCSV", 
                                                label = tags$span(style = "color: white;", "Télécharger les données (CSV)"),
                                                style = "background-color: #28a745; color: white; width: 280px; height: 50px; font-size: 16px;"),
                                 br(),
                                 tags$img(src = "https://cdn-icons-png.flaticon.com/512/8242/8242984.png", height = "100px")
                             ),
                             
                             div(style = "margin-bottom: 40px;",
                                 downloadButton("downloadTXT", 
                                                label = tags$span(style = "color: white;", "Télécharger les données (TXT)"),
                                                style = "background-color: #007BFF; color: white; width: 280px; height: 50px; font-size: 16px;"),
                                 br(),
                                 tags$img(src = "https://cdn-icons-png.flaticon.com/512/8243/8243060.png", height = "100px")
                             )
                         )
                  )
                )
      ),
      nav_panel("⚙️ Paramètres",
                fluidRow(
                  column(12,
                         div(style = "display: flex; justify-content: center; align-items: center; height: 100%;",
                             div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center;",
                                 h3("Connexion", style = "color: #007BFF;"),
                                 textInput("username", "Nom d'utilisateur", value = "a", placeholder = "a"),
                                 passwordInput("password", "Mot de passe", value = "a", placeholder = "a"),
                                 actionButton("toggle_password", "Afficher le mot de passe", icon = icon("eye")),
                                 br(), br(),
                                 h4(strong("Informations"), style = "border-bottom: 2px solid #007BFF; padding-bottom: 5px;"),
                                 p("Créateurs : Gabriel CLAIRAC-VUITTON, Kévin COSTISOR", style = "font-style: italic;"),
                                 p("Heure de connexion : ", textOutput("current_time")),
                                 br(), br(),
                                 h4(strong("FAQ"), style = "border-bottom: 2px solid #007BFF; padding-bottom: 5px;"),
                                 p(strong("Comment utiliser l'application ?")),
                                 p("Consultez la section Documentation pour des instructions."),
                                 p(strong("Puis-je exporter les données ?")),
                                 p("Oui, vous pouvez exporter les données au format CSV ou TXT."),
                                 br(),
                                 actionButton("help_button", "Demander de l'aide", icon = icon("question-circle"), style = "background-color: #28a745; color: white;"),
                                 p("(pour toute autre question, n'hésitez pas à contacter notre support)")
                             )
                         )
                  )
                )
      ),
      title = strong("ANALYSE ÉNERGÉTIQUE DES LOGEMENTS"),
      id = "page"
    )
  ),
  language = "fr"
)

server <- function(input, output, session) {
  # Chargement des données
  data <- reactive({
    req(res_auth)  # Assurer que l'utilisateur est authentifié
    read.csv("C:/Users/OLIVIER PORTABLE/Downloads/Projet R SHINY/app/donnees_dpe_existant.csv", sep = ",", fileEncoding = "UTF-8")
  })
  
  observe({
    updateSelectInput(session, "dpe_filter", choices = unique(c("Tous", data()$Etiquette_DPE)))
    updateSelectInput(session, "codePostal", choices = unique(data()$Code_postal_BAN))
  })
  
  # Graphiques
  output$energyDistribution <- renderPlot({
    req(input$dpe_filter)
    filtered_data <- data()
    
    if (input$dpe_filter != "Tous") {
      filtered_data <- filtered_data[filtered_data$Etiquette_DPE == input$dpe_filter, ]
    }
    
    ggplot(filtered_data, aes(x = Etiquette_DPE)) +
      geom_bar(fill = "#007BFF") +
      labs(title = "Distribution des Étiquettes DPE", x = "Étiquette DPE", y = "Nombre de logements") +
      theme_minimal()
  })
  
  output$boxPlot <- renderPlot({
    filtered_data <- data()
    
    ggplot(filtered_data, aes(x = Etiquette_DPE, y = Surface_Habitable, fill = Etiquette_DPE)) +
      geom_boxplot() +
      labs(title = "Surface Habitable par Étiquette DPE", x = "Étiquette DPE", y = "Surface Habitable") +
      theme_minimal()
  })
  
  output$histogram <- renderPlot({
    filtered_data <- data()
    
    ggplot(filtered_data, aes(x = Conso_5_usages_é_finale)) +
      geom_histogram(binwidth = 5, fill = "#28a745") +
      labs(title = "Histogramme de la Consommation Énergétique", x = "Consommation Énergétique (kWh)", y = "Nombre de logements") +
      theme_minimal()
  })
  
  output$scatterPlot <- renderPlot({
    filtered_data <- data()
    
    ggplot(filtered_data, aes(x = Surface_Habitable, y = Conso_5_usages_é_finale)) +
      geom_point(aes(color = Etiquette_DPE), alpha = 0.5) +
      labs(title = "Relation entre Surface Habitable et Consommation Énergétique", x = "Surface Habitable", y = "Consommation Énergétique (kWh)") +
      theme_minimal()
  })
  
  output$tableData <- renderDataTable({
    filtered_data <- data()
    datatable(filtered_data)
  })
  
  output$mapView <- renderLeaflet({
    req(input$codePostal)
    filtered_data <- data()[data()$Code_postal_BAN == input$codePostal, ]
    req(nrow(filtered_data) > 0)  # Vérifier qu'il y a des données
    
    lat_lon <- strsplit(as.character(filtered_data$geopoint), ",")
    lat <- sapply(lat_lon, function(x) as.numeric(x[1]))
    lon <- sapply(lat_lon, function(x) as.numeric(x[2]))
    
    leaflet(filtered_data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lng = lon, lat = lat, popup = ~paste("Type:", Type_bâtiment, "<br>", "Etiquette DPE:", Etiquette_DPE))
  })
  
  output$postalInfo <- renderText({
    paste("Code Postal sélectionné :", input$codePostal)
  })
  
  # Téléchargement des données
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("data_export", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadTXT <- downloadHandler(
    filename = function() {
      paste("data_export", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(data(), file, row.names = FALSE, sep = "\t")
    }
  )
  
  # Authentification
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
}

shinyApp(ui = ui, server = server)


