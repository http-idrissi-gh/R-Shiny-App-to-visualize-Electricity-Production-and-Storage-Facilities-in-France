# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(rAmCharts)

ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(title = "Analyse des Installations Électriques en France",
                  titleWidth = 451),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("  Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("  Analyse par Filière", tabName = "categorical", icon = icon("chart-bar")),
      menuItem("  Analyse Temporelle", tabName = "numerical", icon = icon("chart-line")),
      menuItem("  Exploration Personnalisée", tabName = "custom", icon = icon("search")),
      menuItem("  Analyse Géographique", tabName = "geographical", icon = icon("map"))
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
          status = "success",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput("summary_table")
        )
      ),
      fluidRow(
        box(
        title = "Distribution des instalations en France",
        status = "success",
        selectInput("var_an",
                    "Filtre",
                    choices = c(
                      "Installation par filière" = "filiere",
                      "Installation par energie renouvelable" = "Renouvelable"
                    )
        ),
        amChartsOutput("type_reg")
       ),
      box(
      title = "Répartition de la production électrique par filière",
      status = "success",
      amChartsOutput("type_dist")
      )
    )
  ),
        
  # Analyse par Filière
  tabItem(
    tabName = "categorical",
    fluidRow(
      box(
        title = "Analyse par Catégorie",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        selectInput("cat_var",
                    "Variable à analyser:",
                    choices = c(
                      "Région" = "region",
                      "Tension de Raccordement" = "tension",
                      "Installations" = "technologie"
                    )),
        amChartsOutput("categorical_plot")
      )
    )
  ),
      # Analyse Temporelle
      tabItem(
        tabName = "numerical",
        fluidRow(
          box(
            title = "Évolution Temporelle",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            selectInput("time_var", "Type d'analyse:",
                        choices = c(
                          "Installations par date" = "count",
                          "Puissance cumulée" = "power"
                        )),
            plotlyOutput("temporal_plot")  
          )
        )
      ),
    
      
      # Exploration Personnalisée
      tabItem(
        tabName = "custom",
        fluidRow(
          box(
            title = "Filtres",
            status = "success",
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
            status = "success",
            solidHeader = TRUE,
            width = 9,
            plotOutput("custom_plot"),
            dataTableOutput("filtered_data")
          )
        )
      ),
      # Analyse Géographique
      tabItem(
        tabName = "geographical",
        fluidRow(
          box(
            title = "Carte des Installations",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            selectInput("map_visualization", "Type de visualisation:",
                        choices = c(
                          "Progression vers l'objectif COP21" = "progression_cop21",
                          "Évolution temporelle depuis l'Accord de Paris" = "evolution_paris"
                        )),
            sliderInput("map_year_filter", "Année de référence:",
                        min = 2015, max = 2023, value = 2023, step = 1,
                        animate = TRUE),
            leafletOutput("map", height = 600)
          )
        )
      )
    )
  )
)
