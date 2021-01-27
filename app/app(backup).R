####-----------------------------  US Election Visualizer  -----------------------------####
##                                                                                        ##
##            Will visualize the per county relative distribution of votes.               ##
##                                                                                        ##
####------------------------------------------------------------------------------------####


#### DEPENDENCIES

library(htmltools)
library(leaflet)
library(plotly)
library(shiny)
library(shinybusy)
library(shinythemes)
library(stringr)
library(tidyverse)



#### IMPORT DATA

election <- readRDS("election.RDS")
final_map <- readRDS("final.RDS")
unemployment <- readRDS("unemployment.RDS")
income <- readRDS("income.RDS")
income_quantile_plot <- readRDS("income_quantile_plot.RDS")
population <- readRDS("population.RDS")
leaflet_state_borders <- readRDS("state_borders.RDS")
leaflet_labels <- readRDS("leaflet_labels.RDS")



#### DONUT PLOT FUNCTION
##
## args: x = string, FIPS code
##       y = numerical, year

plot_donut <- function(x, y) {
  
  county_filtered <- election %>% 
    filter(FIPS == x, year == y) %>% 
    arrange(party) %>% 
    mutate(party = str_to_title(party))
  
  donut_plot <- county_filtered %>% 
    plot_ly(textinfo = "percent",
            textposition = "inside",
            textfont = list(size = 10, face = "bold"),
            hovertemplate = "%{percent}<br>%{value:,.0f} votes<extra></extra>") %>% 
    add_pie(labels = ~party, values = ~candidatevotes, hole = 0.7, direction = "clockwise", 
            rotation = 180, sort = FALSE, domain = list(x = c(0, 1), y = c(0, 1)),
            textinfo = "none",
            marker = list(colors =  c("#0B6BA6", "#E6E6E6", "#CD191D"),
                          line = list(color = "#FFFFFF", width = 1))) %>% 
    add_pie(labels = ~party, values = ~state_candidatevotes, hole = 0.8, direction = "clockwise", 
            rotation = 180, sort = FALSE, domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85)),
            marker = list(colors = c("#9FC4DC", "#F5F5F5", "#EBA4A6"),
                            line = list(color = "#FFFFFF", width = 1))) %>%
    add_annotations(x = 0, y = 0, xref = "x", yref = "y", text = ~unique(state),
                    xanchor = FALSE, showarrow = FALSE,  font = list(size = 16)) %>% 
    layout(showlegend = F, 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           margin = m <- list(l = 10, r = 10, b = 0, t = 0, pad = 20)) %>% 
    config(displayModeBar = FALSE)
  
}


#### TREND PLOT FUNCTION
##
## args: x = string, FIPS code
##       y = numerical, selected year
##       z = string, selected trend

plot_trend <- function(x, y, z) {
  
  county_election_filtered <- election %>% 
    filter(FIPS == x) %>% 
    arrange(party) %>% 
    mutate(party = str_to_title(party)) %>% 
    select(year, candidatevotes, ratio, party)
  
  if (z == "unemployment") {
    
    county_unemployment_filtered <- unemployment %>% 
      filter(FIPS == x) %>% 
      left_join(county_election_filtered, by = "year")
    
    trend_plot <- county_unemployment_filtered %>% 
      plot_ly(hovertemplate = "%{x}:<br>%{y:.2%} <extra></extra>") %>%
      add_trace(x = ~year, y = ~ratio, color = ~party, width = 1.5, type = "bar",
                colors = c("#0B6BA6", "#E6E6E6", "#CD191D"),
                marker = list(line = list(color = "#FFFFFF", width = 1)),
                showlegend = FALSE) %>% 
      add_trace(x = ~year, y = ~state_unemployment, name = "State Average", type = "scatter", mode = "lines",
                line = list(color = "#555555", width = 1.5, dash = "dash"),
                yaxis = "y2") %>% 
      add_trace(x = ~year, y = ~national_unemployment, name = "National Average", type = "scatter", mode = "lines",
                line = list(color = "#555555", width = 1.5, dash = "dot"),
                yaxis = "y2") %>% 
      add_trace(x = ~year, y = ~unemployment, name = "County", type = "scatter", mode = "lines",
                line = list(color = "#000000", width = 1.5),
                yaxis = "y2") %>% 
      layout(showlegend = TRUE, 
             title = "Unemployment Rate and Voting Trends",
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                           dtick = 4, tick0 = 2000, tickmode = "linear", title = "",
                           tickfont = list(size = 10)),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                           side = "right", title = ""),
             yaxis2 = list(showgrid = FALSE, zeroline = FALSE, overlaying = "y",
                           side = "left", title = "", tickformat = "%",
                           tickfont = list(size = 10)),
             legend = list(orientation = "h", yanchor = "bottom", y = -1,
                           font = list(size = 10)),
             barmode = "relative", 
             hoverdistance = 5,
             margin = m <- list(l = 50, r = 25, b = 0, t = 25, pad = 10)) %>% 
      config(displayModeBar = FALSE)
    
  } else {
    
    county_income_filtered <- income %>% 
      filter(FIPS == x) %>% 
      left_join(county_election_filtered, by = "year")
    
    trend_plot <- county_income_filtered %>% 
      plot_ly(hovertemplate = "%{x}:<br>%{y:.2%} <extra></extra>") %>%
      add_trace(x = ~year, y = ~ratio, color = ~party, width = 1.5, type = "bar",
                colors = c("#0B6BA6", "#E6E6E6", "#CD191D"),
                marker = list(line = list(color = "#FFFFFF", width = 1)),
                showlegend = FALSE) %>% 
      add_trace(x = ~year, y = ~state_median_income, name = "State Average", type = "scatter", mode = "lines",
                hovertemplate = "%{x}:<br>%{y} <extra></extra>",
                line = list(color = "#888888", width = 2, dash = "dash"),
                yaxis = "y2") %>% 
      add_trace(x = ~year, y = ~national_median_income, name = "National Average", type = "scatter", mode = "lines",
                hovertemplate = "%{x}:<br>%{y} <extra></extra>",
                line = list(color = "#888888", width = 2, dash = "dot"),
                yaxis = "y2") %>% 
      add_trace(x = ~year, y = ~median_income, name = "County", type = "scatter", mode = "lines",
                hovertemplate = "%{x}:<br>%{y} <extra></extra>",
                line = list(color = "#000000", width = 2),
                yaxis = "y2") %>% 
      layout(showlegend = TRUE, 
             title = "Median Income and Voting Trends",
             xaxis  = list(showgrid = FALSE, zeroline = FALSE,
                           dtick = 4, tick0 = 2000, tickmode = "linear", title = "",
                           tickfont = list(size = 10)),
             yaxis  = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                           side = "right", title = ""),
             yaxis2 = list(showgrid = FALSE, zeroline = FALSE, overlaying = "y",
                           side = "left", title = "", tickformat = "$",
                           tickfont = list(size = 10)),
             legend = list(orientation = "h", yanchor = "bottom", y = -1,
                           font = list(size = 10)),
             barmode = "relative",
             hoverdistance = 5,
             margin = m <- list(l = 50, r = 25, b = 0, t = 25, pad = 10)) %>% 
      config(displayModeBar = FALSE)
    
  }
  
}




#### SHINY (UI)

ui <- navbarPage(
  theme = shinytheme("flatly"), title = "US Election Results (2000 - 2016)", id = "main_page",
  
  tabPanel(
    "Map", icon = icon("map", lib = "font-awesome"),
    add_busy_spinner("scaling-squares", color = "#2C3E50",
                     timeout = 1000, position = "full-page", onstart = TRUE),
    
    div(
      class = "outer", tags$head(includeCSS("styles.css")),
      leafletOutput("map", height = "100%", width = "100%"),

      absolutePanel(
        id = "title_panel", class = "center", top = "10", right = "410",
        width = "300", height = "135", draggable = FALSE,
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        
        "Election Year",
        div(id = "year_selector_space",
            sliderInput("year_selector", label = NULL, sep = "", 
                        min = 2000, max = 2016, value = 2016, step = 4,
                        ticks = TRUE, round = TRUE)),
        div(id = "type_selector_space",
            radioButtons("type_selector", label = NULL, inline = TRUE, selected = "year",
                         c("Election Results" = "year", "Closeness" = "closeness")))
      ),
      
      absolutePanel(
        id = "data_panel", class = "center", right = "0", bottom = "0", top = "0",
        width = "400", draggable = FALSE,

        fluidRow(id = "county_title",
                 column(12, div(textOutput("county_value")))),
        
        div(
          plotlyOutput("donut_plot", height = "225px"),
          div(
            id = "democrat_percent_float", 
            div(
              id = "democrat_percent_text", 
              textOutput("democrat_percent", inline = TRUE), " %")),
          div(
            id = "republican_percent_float",
            div(
              id = "republican_percent_text", 
              textOutput("republican_percent", inline = TRUE), " %"))
        ),
        
        div(
          div(
            id = "democrat_candidate_text", 
            textOutput("democrat_candidate")),
          div(
            id = "republican_candidate_text", 
            textOutput("republican_candidate")),
        ),
        
        hr(),
        
        div(
          style = "height: 220px;",
          plotlyOutput("trend_plot", height = "200px")),
        span(actionLink("trend_selector", "Median Income", 
                        style = '
                            color: #2C3E50; 
                            text-decoration: underline; 
                            border-style: solid;
                            border-width: thin;
                            padding: 5px;')
          
        )
        
      )

    )
           
  ),
  
  tabPanel(
    "Income Quantiles", icon = icon("coins", lib = "font-awesome"),
    
    div(
      class = "outer", 
      
      div(
        id = "center_flex_container",
        div(
          id = "center_flex_box",
          span(id = "income_quantile_plot_title", "Income Quantile Plot"), br(),
          span(id = "income_quantile_plot_description", "The voting trends for each income quintiles in the US"), p(),
          plotlyOutput("income_quantile_plot", height = "85%", width = "100%"),
        )
        
      )
      
    )
    
  )
  
)



#### SHINY (SERVER)

server <- function(input, output, session) {
  
  ## Suppress warnings  
  
  storeWarn<- getOption("warn")
  options(warn = -1)

  
  ### LEAFLET
  
  ## Leaflet basemap
  
  output$map <- renderLeaflet({

    leaflet(final_map, 
            options = leafletOptions(zoomSnap = 1, zoomDelta = 1, wheelPxPerZoomLevel = 200)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -80, lat = 40, zoom = 4)

  })
  
  ## Leaflet layer control

  observe({

    leafletProxy("map", data = filtered_map$data) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~filtered_map$pallette(label),
                  label = lapply(filtered_map$label, htmltools::HTML),
                  labelOptions = labelOptions(style = list("padding" = "5px")),
                  color = "Grey", weight = 0.5, fillOpacity = 0.4, smoothFactor = 0,
                  highlightOptions = highlightOptions(color = "Black", weight = 0.75, bringToFront = TRUE),
                  layerId = ~GEOID) %>%
      addPolylines(data = leaflet_state_borders, color = "Grey", opacity = 1, weight = 1) 

  })
  
  ## Leaflet legend control
  
  observe({
    
    leafletProxy("map", data = final_map) %>%
      clearControls() %>%
      addLegend(position = c("bottomleft"), opacity = 1, title = filtered_map$legend_title,
                colors = filtered_map$colors,  
                labels = filtered_map$labels)
    
  })
  
  leaflet_colour_scale <- colorNumeric(palette = "RdBu", domain = c(0,3))
  
  
  ### REACTIVE VALUES
  
  filtered_map <- reactiveValues(
    data = final_map, 
    label = leaflet_labels[12933:16165],
    pallette = colorNumeric(palette = "RdBu", domain = c(0,3)),
    legend_title = "Legend",
    colors = c("#BF96A1", "#F9DFD2", "#D9E9F0", "#98A9BC"),
    labels = c("Flipped Republican", "Republican", "Democrat", "Flipped Democrat")
    )
  
  observeEvent(c(input$year_selector, input$type_selector), ignoreInit = TRUE, {
    
    filtered_map$data@data[["label"]] <- final_map@data[[paste0(input$type_selector, input$year_selector)]]
    
    label_index_s <- (((input$type_selector == "closeness") * 16165) 
                      + ((as.numeric(input$year_selector) - 2000) / 4) * 3233 + 1)
    label_index_e <- (((input$type_selector == "closeness") * 16165) 
                      + (((as.numeric(input$year_selector) - 2000) / 4) + 1) * 3233)
    filtered_map$label <- leaflet_labels[label_index_s:label_index_e]
    
    if(input$type_selector == "year") {
      filtered_map$pallette <- colorNumeric(palette = "RdBu", domain = c(0,3))
      filtered_map$legend_title <- "Legend"
      filtered_map$colors <- c("#BF96A1", "#F9DFD2", "#D9E9F0", "#98A9BC")
      filtered_map$labels <- c("Flipped Republican", "Republican", "Democrat", "Flipped Democrat")
    } else {
      filtered_map$pallette <- colorNumeric(palette = "viridis", domain = c(0,1))
      filtered_map$legend_title <- "Closeness Score"
      filtered_map$colors <- c("#EFF19F", "#C3E9B8", "#A2D1CD", "#B3A4C4")
      filtered_map$labels <- c("Close Result", "", "", "Polarised Result")
    }
    
  })
  
  filtered_data <- reactiveValues(
    data = election %>% 
      filter(FIPS == "36061")
  )
  
  observe(filtered_data$data <- election %>% 
            filter(FIPS == county_geoid()))
  

  ### INPUTS

  county_geoid <- reactive({

    if(is.null(input$map_shape_click) == TRUE) {
      "36061"
    } else {
      input$map_shape_click$id
    }

  })
  
  selected_year <- reactive({
    
    as.numeric(input$year_selector)
    
  })
  
  last_year <- reactive ({
    
    selected_year() == 2000
    
  })
  
  observeEvent(input$trend_selector, {
    
    if (input$trend_selector %% 2 == 0) {
      trend_selector_label <- "Median Income"
    } else {
      trend_selector_label <- "Unemployment Rate"
    }

    updateActionButton(session, "trend_selector", label = trend_selector_label)

  })
  
  selected_trend <- reactive ({
    
    ifelse(input$trend_selector %% 2 == 0, "unemployment", "income") 
    
  })

  
  ### OUTPUTS

  ## Text outputs for selected county, year

  output$county_value <- renderText({

    filtered_data$data %>%
      select(county) %>%
      head(1) %>%
      pull()

  })
  
  output$state_value <- renderText({
    
    filtered_data$data  %>%
      select(state) %>%
      head(1) %>%
      pull()
    
  })
  
  output$year_value <- renderText({
    
    selected_year()
    
  })

  output$democrat_percent <- renderText({

    filtered_data$data %>% 
      filter(party == "democrat",
             year == selected_year()) %>%
      select(ratio) %>%
      pull() %>% 
      round(digits = 4) * 100

  })

  output$republican_percent <- renderText({

    filtered_data$data %>% 
      filter(party == "republican",
             year == selected_year()) %>%
      select(ratio) %>%
      pull() %>% 
      round(digits = 4) * 100

  })
  
  output$democrat_percent_state <- renderText({
    
    { if (last_year() == TRUE) "-"
      else {
        filtered_data$data %>% 
          filter(party == "democrat",
                 year == selected_year()) %>%
          select(state_ratio) %>%
          pull() %>% 
          round(digits = 4) * 100
      }
    }
    
  })
  
  output$republican_percent_state <- renderText({
    
    { if (last_year() == TRUE) "-"
      else {
        filtered_data$data %>% 
          filter(party == "republican",
                 year == selected_year()) %>%
          select(state_ratio) %>%
          pull() %>% 
          round(digits = 4) * 100
      }
    }
    
  })
  
  output$democrat_candidate <- renderText({
    
    filtered_data$data %>% 
      filter(party == "democrat",
             year == selected_year()) %>%
      select(candidate) %>%
      pull()
    
  })
  
  output$republican_candidate <- renderText({
    
    filtered_data$data %>% 
      filter(party == "republican",
             year == selected_year()) %>%
      select(candidate) %>%
      pull()
    
  })
  

  ## Output donut plot

  output$donut_plot <- renderPlotly({

    plot_donut(county_geoid(), selected_year())

  })
  
  ## Output trend plot
  
  output$trend_plot <- renderPlotly({
    
    plot_trend(county_geoid(), selected_year(), selected_trend())
    
  })
  
  ## Output income quantile plot
  
  output$income_quantile_plot <- renderPlotly({
    
    income_quantile_plot
    
  })
}



#### SHINYAPP

shinyApp(ui = ui, server = server)
