####-----------------------------  US Election Visualizer  -----------------------------####
##                                                                                        ##
##        For visualizing the US presidential election results between 2000-2020          ##
##                                                                                        ##
####------------------------------------------------------------------------------------####


#### DEPENDENCIES

library(htmltools)
library(leaflet)
library(plotly)
library(shiny)
library(shinybusy)
library(shinythemes)
library(sp)
library(stringr)
library(tidyverse)



#### IMPORT DATA

election <- readRDS("election.RDS")
final_map <- readRDS("final.RDS")
unemployment <- readRDS("unemployment.RDS")
income <- readRDS("income.RDS")
income_quintile_plot <- readRDS("income_quintile_plot.RDS")
density_plots <- readRDS("density_plots.RDS")
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
    
    county_data_filtered <- unemployment %>% 
      filter(FIPS == x) %>% 
      mutate(county = unemployment,
             state = state_unemployment,
             national = national_unemployment) %>% 
      full_join(county_election_filtered, by = "year")
    
    plot_title = "Unemployment Rate and Voting Trends"
    plot_tick_format = "%"

  } else {
    
    county_data_filtered <- income %>% 
      filter(FIPS == x) %>% 
      mutate(county = median_income,
             state = state_median_income,
             national = national_median_income) %>% 
      full_join(county_election_filtered, by = "year")
    
    plot_title = "Median Income and Voting Trends"
    plot_tick_format = "$"
      
  }
    
  trend_plot <- county_data_filtered %>% 
    plot_ly(hovertemplate = "%{x}:<br>%{y:.2%} <extra></extra>") %>%
    add_trace(x = ~year, y = ~ratio, color = ~party, width = 1.5, type = "bar",
              colors = c("#0B6BA6", "#E6E6E6", "#CD191D"),
              marker = list(line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>% 
    add_trace(x = ~year, y = ~state, name = "State Average", type = "scatter", mode = "lines",
              line = list(color = "#555555", width = 1.5, dash = "dash"),
              yaxis = "y2") %>% 
    add_trace(x = ~year, y = ~national, name = "National Average", type = "scatter", mode = "lines",
              line = list(color = "#555555", width = 1.5, dash = "dot"),
              yaxis = "y2") %>% 
    add_trace(x = ~year, y = ~county, name = "County", type = "scatter", mode = "lines",
              line = list(color = "#000000", width = 1.5),
              yaxis = "y2") %>% 
    layout(showlegend = TRUE, 
           title = plot_title,
           xaxis = list(showgrid = FALSE, zeroline = FALSE,
                         dtick = 4, tick0 = 2000, tickmode = "linear", title = "",
                         tickfont = list(size = 10)),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                         side = "right", title = ""),
           yaxis2 = list(showgrid = FALSE, zeroline = FALSE, overlaying = "y",
                         side = "left", title = "", tickformat = plot_tick_format,
                         tickfont = list(size = 10)),
           legend = list(orientation = "h", yanchor = "bottom", y = -1,
                         font = list(size = 10)),
           barmode = "relative", 
           hoverdistance = 5,
           margin = m <- list(l = 50, r = 25, b = 0, t = 25, pad = 10)) %>% 
    config(displayModeBar = FALSE)
  
}



#### SHINY (UI)

ui <- navbarPage(
  theme = shinytheme("simplex"), title = "US Election Results (2000 - 2020)", id = "main_page",
  
  tabPanel(
    "Map", icon = icon("map", lib = "font-awesome"),
    add_busy_spinner("scaling-squares", color = "#2C3E50",
                     timeout = 1000, position = "full-page", onstart = TRUE),
    
    div(
      class = "outer", tags$head(includeCSS("styles.css")),
      leafletOutput("map", height = "100%", width = "100%"),

      absolutePanel(
        id = "title_panel", class = "center", top = "10", right = "410",
        width = "300", height = "185", draggable = FALSE,
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        
        "Election Year",
        div(id = "year_selector_space",
            sliderInput("year_selector", label = NULL, sep = "", 
                        min = 2000, max = 2020, value = 2020, step = 4,
                        ticks = TRUE, round = TRUE)),
        div(id = "type_selector_space",
            radioButtons("type_selector", label = NULL, inline = TRUE, selected = "closeness",
                         c("Election Results" = "closeness", "Flipped/Retained" = "year", 
                           "Median Income*" = "median_income"))),
        div(id = "subtext_space",
            "*Income data for 2019 used for 2020")
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
    "Income Quintiles", icon = icon("coins", lib = "font-awesome"),
    
    div(
      class = "outer", 
      
      div(
        class = "center_flex_container",
        div(
          class = "center_flex_box",
          span(class = "plot_title", "Income Quintile Plot"), br(),
          span(class = "plot_description", 
               "Voting trends for county median income quintiles", 
               "(*Income data for 2019 used for 2020)"), p(),
          plotlyOutput("income_quintile_plot", height = "85%", width = "100%"),
        )
        
      )
      
    )
    
  ),
  
  tabPanel(
    "Density Plot", icon = icon("chart-area", lib = "font-awesome"),
    
    div(
      class = "outer", 
      
      div(
        class = "center_flex_container",
        
        div(
          class = "center_flex_box",
          span(class = "plot_title", "Density Plot"), br(),
          span(class = "plot_description", 
               "Distribution of county unemployment rates/median incomes",
               "(*Income and unemployment data for 2019 used for 2020)"), p(),
          span(plotlyOutput("density_plot", height = "75%", width = "100%")),
          div(id = "density_selectors_float", sliderInput("density_year_selector", label = NULL, sep = "", 
                      min = 2000, max = 2020, value = 2020, step = 4, 
                      ticks = TRUE, round = TRUE),
               selectInput("density_type_selector", label = NULL, choices = list("Unemployment", "Median Income"),
                      selected = "Unemployment"))
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
    label = leaflet_labels[16166:19398],
    pallette = colorNumeric(palette = "RdBu", domain = c(0,5)),
    legend_title = "Legend",
    colors = c("#BD96A1", "#E8BCB3", "#FAEEE4", "#EAF2F5", "#B6D1E3", "#9CA9BC"),
    labels = c("70%", "60%", "", "", "60%", "70%")
  )
  
  observeEvent(c(input$year_selector, input$type_selector), ignoreInit = TRUE, {
    
    filtered_map$data@data[["label"]] <- final_map@data[[paste0(input$type_selector, input$year_selector)]]
    
    label_index_s <- (((input$type_selector == "closeness") * (3233 * 6)) 
                      + ((input$type_selector == "median_income") * (3233 * 6) * 2)
                      + ((as.numeric(input$year_selector) - 2000) / 4) * 3233 + 1)
    label_index_e <- (((input$type_selector == "closeness") * (3233 * 6))
                      + ((input$type_selector == "median_income") * (3233 * 6) * 2)
                      + (((as.numeric(input$year_selector) - 2000) / 4) + 1) * 3233)
    filtered_map$label <- leaflet_labels[label_index_s:label_index_e]
    
    if(input$type_selector == "year") {
      filtered_map$pallette <- colorNumeric(palette = "RdBu", domain = c(0,3))
      filtered_map$legend_title <- "Legend"
      filtered_map$colors <- c("#BF96A1", "#F9DFD2", "#D9E9F0", "#98A9BC")
      filtered_map$labels <- c("Flipped Republican", "Republican", "Democrat", "Flipped Democrat")
    } else if(input$type_selector == "closeness") {
      filtered_map$pallette <- colorNumeric(palette = "RdBu", domain = c(0,5))
      filtered_map$legend_title <- "Closeness Score"
      filtered_map$colors <- c("#BD96A1", "#E8BCB3", "#FAEEE4", "#EAF2F5", "#B6D1E3", "#9CA9BC")
      filtered_map$labels <- c("70%", "60%", "", "", "60%", "70%")
    } else {
      filtered_map$pallette <- colorQuantile(palette = "RdBu", domain = c(1, 10), n = 10)
      filtered_map$legend_title <- "Income Deciles"
      filtered_map$colors <- rev(c("#BD96A1", "#DA9FA6", "#E8BCB3", "#F5D8C9", "#FAEEE4", 
                               "#EAF2F5", "#D3E5EE", "#B6D1E3", "#A9BFDA", "#9CA9BC"))
      filtered_map$labels <- rev(c("Bottom decile", "2nd", "3rd", "4th", "5th", 
                               "6th", "7th", "8th", "9th", "Top decile"))
    }
    
  })
  
  
  filtered_data <- reactiveValues(
    data = election %>% 
      filter(FIPS == "36061")
  )
  
  observe(filtered_data$data <- election %>% 
            filter(FIPS == county_geoid()))
  
  
  filtered_density_plot <- reactiveValues(
    plot = density_plots[[6]]
  )
  
  observe(filtered_density_plot$plot <- density_plots[[(density_plot_type() * 6) + density_plot_year()]])
  

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
  
  density_plot_year <- reactive ({
    
    (input$density_year_selector - 1996) %/% 4
    
  })
  
  density_plot_type <- reactive ({
    
    (input$density_type_selector == "Median Income") 
    
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
  
  ## Output income quintile plot
  
  output$income_quintile_plot <- renderPlotly({
    
    income_quintile_plot
    
  })
  
  ## Output unemployment density plot
  
  output$density_plot <- renderPlotly({
    
    filtered_density_plot$plot
    
  })
}



#### SHINYAPP

shinyApp(ui = ui, server = server)
