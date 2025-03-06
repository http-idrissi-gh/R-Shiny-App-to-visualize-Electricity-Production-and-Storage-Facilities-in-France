# server.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(DT)
library(stringr)
library(scales)
library(lubridate)
library(sf)
library(plotly)
library(rAmCharts)

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
        annee = year(dateMiseEnService),
        Renouvelable = ifelse(filiere %in% c("Solaire", "Hydraulique", "Eolien", "Bioénergies", "Energies Marines", "Géothermie"),
                              "Oui", "Non"))
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
        `Puissance Moyenne (kW)` = mean(puissance, na.rm = TRUE),
        `Pourcentage Renouvelable ` = sum(ifelse(Renouvelable == "Oui", puissance, 0), na.rm = TRUE) /
          sum(puissance, na.rm = TRUE) * 100
      ) %>%
      arrange(desc(`Puissance Totale (MW)`))
  }, options = list(pageLength = 5))
  
  output$type_reg <- renderAmCharts({
    var <- input$var_an
    count_data <- data() %>%
      group_by(!!sym(var)) %>%
      count()
    
    total_n <- sum(count_data$n)
    
    count_data <- count_data %>%
      mutate(proportion = n / total_n)
    am_data <- data.frame(
      label  = count_data[[var]],
      value = count_data$proportion,
      n = count_data$n
    )
    amPie(
      data = am_data,
      balloonText = "Pourcentage: [[percents]]%, soit [[n]] instalations",
      fontSize = 9,
      radius = "30%",
      labelRadius = 1,
      marginTop = 110,
      creditsPosition = "bottom-right"
    )
  })
  output$type_dist <- renderAmCharts({
    data_bar <- data() %>%
      group_by(filiere, Renouvelable) %>%
      summarise(total_power = sum(puissance, na.rm = TRUE) / 1000, .groups = 'drop') %>%
      arrange(desc(total_power))
    
    
    pipeR::pipeline(            
      amBarplot(
        x = "filiere",
        y = "total_power",
        data = data_bar,
        horiz = TRUE,
        labelRotation = -45,  
        ylab = "Puissance Totale (MW)",  
        xlab = "Filière",
        creditsPosition = "bottom-right"
      ),
      setChartCursor()
    )
  })
  
  
  #Analyse temporelle
  
  output$temporal_plot <- renderPlotly({
    if (input$time_var == "count") {
      df <- data() %>%
        count(annee, Renouvelable) %>%
        mutate(Renouvelable = factor(Renouvelable, levels = c("Oui", "Non")))  
      plot_ly(df, x = ~annee, y = ~n, color = ~Renouvelable,
              colors = c("Oui" = "#2ECC71", "Non" = "#E74C3C"),  
              type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Évolution des Installations",
               xaxis = list(title = "Année"),
               yaxis = list(title = "Nombre d'installations"),
               legend = list(title = list(text = "Énergie Renouvelable")))
      
    } else {
      df <- data() %>%
        group_by(annee, Renouvelable) %>%
        summarise(total_power = sum(puissance, na.rm = TRUE) / 1000, .groups = 'drop') %>%
        mutate(Renouvelable = factor(Renouvelable, levels = c("Oui", "Non")))  
      
      plot_ly(df, x = ~annee, y = ~total_power, color = ~Renouvelable,
              colors = c("Oui" = "#2ECC71", "Non" = "#E74C3C"),  
              type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Puissance Cumulée des Installations",
               xaxis = list(title = "Année"),
               yaxis = list(title = "Puissance totale installée (MW)"),
               legend = list(title = list(text = "Énergie Renouvelable")))
    }
  })
  
  # Analyse Catégorielle
  
  output$categorical_plot <- renderAmCharts({
    var <- input$cat_var
    if(var == "tension") {
      data_bar <- data() %>%
        group_by(tensionRaccordement) %>%
        summarise(
          puissance_totale_r = sum(puissance[Renouvelable == "Oui"], na.rm = TRUE) / 1000,
          puissance_totale_nr = sum(puissance[Renouvelable == "Non"], na.rm = TRUE) / 1000,
          .groups = 'drop'
        ) %>%
        arrange(desc(puissance_totale_r + puissance_totale_nr))
      pipeR::pipeline(
        amBarplot(x = "tensionRaccordement",
                  y = c("puissance_totale_r", "puissance_totale_nr"),
                  data = data_bar,
                  horiz = TRUE,
                  stack_type = "regular",
                  groups_color = c("#2ECC71", "#E74C3C"),
                  main = "Puissance electrique en fonction de la tension de raccordement",
                  xlab = "Tension de Raccordement",
                  ylab = "Puissance Totale (MW)",
                  legend = TRUE,
                  creditsPosition = "bottom-right"),
        setChartCursor())
    } else if(var == "region") {
      data_bar <- data() %>%
        group_by(region) %>%
        summarise(
          puissance_totale_r = sum(puissance[Renouvelable == "Oui"], na.rm = TRUE) / 1000,
          puissance_totale_nr = sum(puissance[Renouvelable == "Non"], na.rm = TRUE) / 1000,
          .groups = 'drop'
        ) %>%
        arrange(desc(puissance_totale_r + puissance_totale_nr))  
      pipeR::pipeline(
        amBarplot(x = "region",
                  y = c("puissance_totale_r" , "puissance_totale_nr"),
                  groups_color = c("#2ECC71", "#E74C3C"),
                  data = data_bar,
                  horiz = TRUE,
                  xlab = "Région",
                  ylab = "Puissance Totale (MW)",
                  creditsPosition = "bottom-right",
                  stack_type = "regular",
                  main = "Puissance électrique par région",
                  legend = TRUE,
                  fontSize = 10),
        setChartCursor()
      )
    } else {
      data_bar <- data() %>%
        group_by(region) %>%
        summarise(
          n_r = sum(Renouvelable == "Oui", na.rm = TRUE),
          n_nr = sum(Renouvelable == "Non", na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(n_r + n_nr))
      pipeR::pipeline(
        amBarplot(x = "region",
                  y = c("n_r", "n_nr"),
                  groups_color = c("#2ECC71", "#E74C3C"),
                  data = data_bar,
                  horiz = TRUE,
                  xlab = "Région",
                  ylab = "Nombre d'installations",
                  creditsPosition = "bottom-right",
                  stack_type = "regular",
                  main = "Nombres d'installations par région",
                  legend = TRUE),
        setChartCursor()
      )
    }
  })
  
  # Charger les données géographiques des départements français
  france_deps <- reactive({
    sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
                quiet = TRUE)
  })
  
  # Préparation des données pour les cartes
  map_data_prepared <- reactive({
    selected_year <- input$map_year_filter
    
    # Données de base par département
    dept_data <- data() %>%
      mutate(post_cop21 = ifelse(annee >= 2015, "Oui", "Non")) %>%
      group_by(departement) %>%
      summarise(
        puissance_totale = sum(puissance, na.rm = TRUE) / 1000,
        puissance_renouvelable = sum(ifelse(Renouvelable == "Oui", puissance, 0), na.rm = TRUE) / 1000,
        nombre_total = n(),
        nombre_renouvelable = sum(Renouvelable == "Oui", na.rm = TRUE),
        pourcentage_renouvelable = puissance_renouvelable / puissance_totale * 100,
        .groups = 'drop'
      ) %>%
      # Objectif COP21 (40% d'énergie renouvelable)
      mutate(
        objectif_atteint = ifelse(pourcentage_renouvelable >= 40, "Oui", "Non"),
        progression = pmin(pourcentage_renouvelable / 40 * 100, 100)
      )
    
    # Données temporelles pour la visualisation de l'évolution depuis l'Accord de Paris
    dept_data_temporal <- data() %>%
      filter(annee <= selected_year) %>%
      group_by(departement) %>%
      summarise(
        # Données cumulatives jusqu'à l'année sélectionnée
        puissance_totale_cumul = sum(puissance, na.rm = TRUE) / 1000,
        puissance_renouvelable_cumul = sum(ifelse(Renouvelable == "Oui", puissance, 0), na.rm = TRUE) / 1000,
        
        # Installations post-COP21 (après 2015)
        puissance_post_cop21 = sum(ifelse(annee >= 2015, puissance, 0), na.rm = TRUE) / 1000,
        puissance_renouvelable_post_cop21 = sum(ifelse(annee >= 2015 & Renouvelable == "Oui", puissance, 0), na.rm = TRUE) / 1000,
        
        # Calcul des pourcentages
        pourcentage_renouvelable_cumul = puissance_renouvelable_cumul / puissance_totale_cumul * 100,
        pourcentage_renouvelable_post_cop21 = ifelse(puissance_post_cop21 > 0, 
                                                     puissance_renouvelable_post_cop21 / puissance_post_cop21 * 100, 
                                                     0),
        
        # Évolution du pourcentage d'énergies renouvelables depuis la COP21
        installations_post_cop21 = sum(annee >= 2015, na.rm = TRUE),
        installations_renouvelables_post_cop21 = sum(annee >= 2015 & Renouvelable == "Oui", na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        # Indicateur de transition énergétique
        taux_transition = ifelse(puissance_post_cop21 > 0, 
                                 pourcentage_renouvelable_post_cop21, 
                                 0)
      )
    
    
    # Joindre les données temporelles avec les autres données
    dept_data_complete <- dept_data %>%
      left_join(dept_data_temporal, by = "departement")
    
    # Joindre avec les données géographiques
    france_deps() %>%
      left_join(dept_data_complete, by = c("nom" = "departement"))
  })
  
  # Analyse Géographique - Carte avec options multiples
  output$map <- renderLeaflet({
    map_data <- map_data_prepared()
    visualization_type <- input$map_visualization
    selected_year <- input$map_year_filter
    
    # Définir les palettes de couleurs selon le type de visualisation
    if (visualization_type == "progression_cop21") {
      # Palette pour la progression vers l'objectif COP21
      values_vector <- map_data$progression
      values_vector[is.na(values_vector)] <- 0  # Remplacer NA par 0
      
      pal <- colorNumeric(
        palette = colorRampPalette(c("#E74C3C", "#F39C12", "#2ECC71"))(100),
        domain = c(0, 100),
        na.color = "#CCCCCC"
      )
      
      legend_title <- "Progression vers l'objectif COP21 (%)"
      popup_content <- paste(
        "<b>Département:</b>", map_data$nom, "<br>",
        "<b>Puissance Totale:</b>", 
        ifelse(is.na(map_data$puissance_totale), "0", round(map_data$puissance_totale, 1)), "MW<br>",
        "<b>Puissance Renouvelable:</b>", 
        ifelse(is.na(map_data$puissance_renouvelable), "0", round(map_data$puissance_renouvelable, 1)), "MW<br>",
        "<b>% Renouvelable:</b>", 
        ifelse(is.na(map_data$pourcentage_renouvelable), "0", round(map_data$pourcentage_renouvelable, 1)), "%<br>",
        "<b>Objectif COP21 (40%):</b>", 
        ifelse(map_data$objectif_atteint == "Oui", "Atteint ✓", 
               paste("En progression (", round(map_data$progression, 1), "%)")),
        "<br>",
        "<b>Nombre d'installations:</b>", 
        ifelse(is.na(map_data$nombre_total), "0", map_data$nombre_total)
      )
      
    } else if (visualization_type == "evolution_paris") {
      # Palette pour l'évolution depuis l'Accord de Paris
      values_vector <- map_data$taux_transition
      values_vector[is.na(values_vector)] <- 0  # Remplacer NA par 0
      
      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = c(0, 100),
        na.color = "#CCCCCC"
      )
      
      legend_title <- paste("% d'énergies renouvelables dans les nouvelles installations (2015-", selected_year, ")", sep="")
      popup_content <- paste(
        "<b>Département:</b>", map_data$nom, "<br>",
        "<b>Nouvelles installations (2015-", selected_year, "):</b> ", 
        ifelse(is.na(map_data$installations_post_cop21), "0", map_data$installations_post_cop21), "<br>",
        "<b>Dont renouvelables:</b> ", 
        ifelse(is.na(map_data$installations_renouvelables_post_cop21), "0", map_data$installations_renouvelables_post_cop21), 
        " (", ifelse(is.na(map_data$pourcentage_renouvelable_post_cop21), "0", round(map_data$pourcentage_renouvelable_post_cop21, 1)), "%)<br>",
        "<b>Nouvelle puissance renouvelable:</b> ", 
        ifelse(is.na(map_data$puissance_renouvelable_post_cop21), "0", round(map_data$puissance_renouvelable_post_cop21, 1)), " MW<br>",
        "<b>Nouvelle puissance totale:</b> ", 
        ifelse(is.na(map_data$puissance_post_cop21), "0", round(map_data$puissance_post_cop21, 1)), " MW"
      )
    }
    
    # Créer la carte avec la visualisation sélectionnée
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 2.5, lat = 46.5, zoom = 5.1) %>%
      addPolygons(
        fillColor = ~pal(values_vector),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = popup_content,
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
        values = values_vector,
        title = legend_title,
        labFormat = labelFormat(suffix = "%")
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
    ggplot(filtered_data(), aes(x = dateMiseEnService, y = puissance, color = filiere, shape = Renouvelable)) +
      geom_point(alpha = 0.6) +
      scale_y_log10(labels = scales::comma) +
      scale_color_viridis_d() +
      scale_shape_manual(values = c("Oui" = 16, "Non" = 4)) +
      labs(x = "Date de mise en service",
           y = "Puissance (kW) - Échelle logarithmique",
           color = "Filière",
           shape = "Renouvelable") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  })
  
  output$filtered_data <- renderDataTable({
    filtered_data() %>%
      select(filiere, technologie, Renouvelable, region, departement, commune, puissance, dateMiseEnService) %>%
      rename(
        "Filière" = filiere,
        "Technologie" = technologie,
        "Renouvelable" = Renouvelable,
        "Région" = region,
        "Département" = departement,
        "Commune" = commune,
        "Puissance (kW)" = puissance,
        "Date de mise en service" = dateMiseEnService
      )
  }, options = list(pageLength = 5))
}
