# server.R
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(stringr)
library(scales)
library(lubridate)
library(sf)

server <- function(input, output, session) {
  
  # Charger les données
  data <- reactive({
    df <- read.csv("registre-national-installation-production-stockage-electricite-agrege.csv", 
                   sep = ";", 
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE) %>%
      select(
        commune,
        departement,
        region,
        dateMiseEnService = `dateMiseEnservice..format.date.`,
        tensionRaccordement,
        filiere,
        technologie,
        puissance = puisMaxInstallee,
        latitude = 0,  # Ces colonnes ne sont pas dans le dataset
        longitude = 0  # On utilisera une approche alternative pour la carte
      ) %>%
      filter(!is.na(puissance), puissance > 0) %>%
      mutate(
        dateMiseEnService = as.Date(dateMiseEnService),
        puissance = as.numeric(str_replace(puissance, ",", ".")),
        annee = year(dateMiseEnService)
      )
    View(df)
    return(df)
  })
  
  # Mettre à jour les filtres dynamiques
  observe({
    updateSelectInput(session, "region_filter",
                      choices = sort(unique(data()$region)))
    updateSelectInput(session, "filiere_filter",
                      choices = sort(unique(data()$filiere)))
  })
  
  # Vue d'ensemble 
  output$summary_table <- renderDataTable({
    data() %>%
      group_by(region) %>%
      summarise(
        `Nombre d'Installations` = n(),
        `Puissance Totale (MW)` = sum(puissance, na.rm = TRUE) / 1000,
        `Puissance Moyenne (kW)` = mean(puissance, na.rm = TRUE)
      ) %>%
      arrange(desc(`Puissance Totale (MW)`))
  }, options = list(pageLength = 5))
  
  output$power_dist <- renderPlot({
    ggplot(data(), aes(x = puissance)) +
      geom_histogram(fill = "#2c3e50", bins = 50) +
      scale_x_log10(labels = scales::comma) +
      labs(x = "Puissance Installée (kW) - Échelle logarithmique",
           y = "Nombre d'Installations") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  output$type_dist <- renderPlot({
    data() %>%
      group_by(filiere) %>%
      summarise(total_power = sum(puissance, na.rm = TRUE) / 1000) %>%
      ggplot(aes(x = reorder(filiere, total_power), y = total_power)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      coord_flip() +
      labs(x = "Filière",
           y = "Puissance Totale (MW)") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  # Analyse Temporelle
  output$temporal_plot <- renderPlot({
    if(input$time_var == "count") {
      data() %>%
        count(annee) %>%
        ggplot(aes(x = annee, y = n)) +
        geom_line() +
        geom_point() +
        labs(x = "Année",
             y = "Nombre d'installations") +
        theme_minimal()
    } else {
      data() %>%
        group_by(annee) %>%
        summarise(total_power = sum(puissance, na.rm = TRUE) / 1000) %>%
        ggplot(aes(x = annee, y = total_power)) +
        geom_line() +
        geom_point() +
        labs(x = "Année",
             y = "Puissance totale installée (MW)") +
        theme_minimal()
    }
  })
  
  # Analyse Catégorielle
  output$categorical_plot <- renderPlot({
    var <- input$cat_var
    
    if(var == "filiere") {
      data() %>%
        group_by(filiere, technologie) %>%
        summarise(count = n(), .groups = 'drop') %>%
        ggplot(aes(x = reorder(technologie, count), y = count, fill = filiere)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Technologie",
             y = "Nombre d'Installations",
             fill = "Filière") +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else if(var == "tension") {
      data() %>%
        group_by(tensionRaccordement) %>%
        summarise(
          puissance_totale = sum(puissance, na.rm = TRUE) / 1000,
          nombre = n()
        ) %>%
        ggplot(aes(x = reorder(tensionRaccordement, puissance_totale), 
                   y = puissance_totale)) +
        geom_bar(stat = "identity", fill = "#2c3e50") +
        coord_flip() +
        labs(x = "Tension de Raccordement",
             y = "Puissance Totale (MW)") +
        theme_minimal()
    } else {
      data() %>%
        group_by(region) %>%
        summarise(
          puissance_totale = sum(puissance, na.rm = TRUE) / 1000,
          nombre = n()
        ) %>%
        ggplot(aes(x = reorder(region, puissance_totale), y = puissance_totale)) +
        geom_bar(stat = "identity", fill = "#2c3e50") +
        coord_flip() +
        labs(x = "Région",
             y = "Puissance Totale (MW)") +
        theme_minimal()
    }
  })
  
  # Analyse Géographique
  # Note: Comme nous n'avons pas les coordonnées exactes,
  # nous utiliserons une visualisation alternative basée sur les régions
  
  # Charger les données géographiques des départements français une seule fois
  france_deps <- reactive({
    sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
                quiet = TRUE)
  })
  
  # Analyse Géographique
  output$map <- renderLeaflet({
    # Agréger les données par département
    dept_data <- data() %>%
      group_by(departement) %>%
      summarise(
        puissance_totale = sum(puissance, na.rm = TRUE) / 1000,  # Conversion en MW
        nombre = n()
      )
    
    # Joindre avec les données géographiques
    map_data <- france_deps() %>%
      left_join(dept_data, by = c("nom" = "departement"))
    
    # Palette de couleurs
    pal <- colorNumeric(
      palette = "viridis",
      domain = map_data$puissance_totale,
      na.color = "#CCCCCC"
    )
    
    # Créer la carte
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(puissance_totale),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>Département:</b>", nom, "<br>",
          "<b>Puissance Totale:</b>", 
          ifelse(is.na(puissance_totale), "0", round(puissance_totale, 1)), "MW<br>",
          "<b>Nombre d'installations:</b>", 
          ifelse(is.na(nombre), "0", nombre)
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~puissance_totale,
        title = "Puissance Totale (MW)",
        labFormat = labelFormat(suffix = " MW")
      )
  })
  
  
  # Exploration Personnalisée
  filtered_data <- reactive({
    df <- data()
    
    if(!is.null(input$region_filter) && length(input$region_filter) > 0) {
      df <- df %>% filter(region %in% input$region_filter)
    }
    
    if(!is.null(input$filiere_filter) && length(input$filiere_filter) > 0) {
      df <- df %>% filter(filiere %in% input$filiere_filter)
    }
    
    df <- df %>%
      filter(puissance >= input$power_filter[1],
             puissance <= input$power_filter[2])
    
    return(df)
  })
  
  output$custom_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = dateMiseEnService, y = puissance, color = filiere)) +
      geom_point(alpha = 0.6) +
      scale_y_log10(labels = scales::comma) +
      scale_color_viridis_d() +
      labs(x = "Date de mise en service",
           y = "Puissance (kW) - Échelle logarithmique",
           color = "Filière") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  })
  
  output$filtered_data <- renderDataTable({
    filtered_data() %>%
      select(filiere, technologie, region, departement, commune, puissance, dateMiseEnService) %>%
      rename(
        "Filière" = filiere,
        "Technologie" = technologie,
        "Région" = region,
        "Département" = departement,
        "Commune" = commune,
        "Puissance (kW)" = puissance,
        "Date de mise en service" = dateMiseEnService
      )
  }, options = list(pageLength = 5))
}
