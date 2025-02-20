# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Analyse des Installations Électriques en France"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analyse Temporelle", tabName = "numerical", icon = icon("chart-line")),
      menuItem("Analyse par Filière", tabName = "categorical", icon = icon("chart-bar")),
      menuItem("Analyse Géographique", tabName = "geographical", icon = icon("map")),
      menuItem("Exploration Personnalisée", tabName = "custom", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Vue d'ensemble
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Statistiques Générales",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("summary_table")
          )
        ),
        fluidRow(
          box(
            title = "Distribution de la Puissance Installée",
            status = "primary",
            plotOutput("power_dist")
          ),
          box(
            title = "Répartition par Filière",
            status = "primary",
            plotOutput("type_dist")
          )
        )
      ),
      
      # Analyse Temporelle
      tabItem(
        tabName = "numerical",
        fluidRow(
          box(
            title = "Évolution Temporelle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("time_var", "Type d'analyse:",
                        choices = c(
                          "Installations par date" = "count",
                          "Puissance cumulée" = "power"
                        )),
            plotOutput("temporal_plot")
          )
        )
      ),
      
      # Analyse par Filière
      tabItem(
        tabName = "categorical",
        fluidRow(
          box(
            title = "Analyse par Catégorie",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("cat_var", "Variable à analyser:",
                        choices = c(
                          "Filière" = "filiere",
                          "Région" = "region",
                          "Tension de Raccordement" = "tension"
                        )),
            plotOutput("categorical_plot")
          )
        )
      ),
      
      # Analyse Géographique
      tabItem(
        tabName = "geographical",
        fluidRow(
          box(
            title = "Carte des Installations",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("map_color", "Colorer par:",
                        choices = c(
                          "Filière" = "filiere",
                          "Tension" = "tensionRaccordement"
                        )),
            leafletOutput("map", height = 600)
          )
        )
      ),
      
      # Exploration Personnalisée
      tabItem(
        tabName = "custom",
        fluidRow(
          box(
            title = "Filtres",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("region_filter", "Région:",
                        choices = NULL,
                        multiple = TRUE),
            selectInput("filiere_filter", "Filière:",
                        choices = NULL,
                        multiple = TRUE),
            sliderInput("power_filter", "Puissance (kW):",
                        min = 0, max = 1000, value = c(0, 1000))
          ),
          box(
            title = "Résultats",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            plotOutput("custom_plot"),
            dataTableOutput("filtered_data")
          )
        )
      )
    )
  )
)