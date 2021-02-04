####------------------------ US Election Visualizer Pre-Process ------------------------####
##                                                                                        ##
##      Visualizing the distribution of votes for US presidential election in the         ##
##    2000 - 2016 period. This script simplifies the original county level spatial        ##
##    data, wrangles the historical election data and merges it to output a spatial       ##
##   data frame with indicators used to generate shapes and colour levels in Leaflet.     ##
##                                                                                        ##
##                         Estimated processing time: ~10 minutes                         ##
##                                                                                        ##
####------------------------------------------------------------------------------------####


rm(list=ls())


#### DEPENDENCIES

library(htmltools)
library(rgdal)
library(rmapshaper)
library(stringr)
library(tidyverse)
library(readxl)
library(curl)
library(censusapi)
library(plotly)
library(openintro)
library(parallel)
library(scales)



#### SET CENSUS BUREAU API KEY

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="5ab0db73ed700fae9391185839f2a444106917b3")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")



#### SET WORKING DIRECTORY

setwd("D:/Google Drive/2.Study/ECON3170/US_Presidential_Election/")

#### SET TIMER

start <- Sys.time()



#### IMPORT AND SIMPLIFY SPATIAL DATA

county_map <- readOGR(
  dsn= "./spatial",
  layer="tl_2017_us_county",
  verbose=FALSE
)

county_map <- spTransform(county_map, CRS("+init=epsg:4326"))

county_map <- ms_simplify(county_map, keep = 0.001, keep_shapes = TRUE)



#### IMPORT AND WRANGLE ELECTION DATA

current_data <- read_csv("current_data/2020_US_County_Level_Presidential_Results.csv")
historical_data <- read_csv("historical_data/countypres_2000-2016.csv")

election <- historical_data %>% 
  mutate(FIPS = str_pad(FIPS, 5, pad = "0"),
         party = ifelse(candidate == "Other", "other", party),
         fullname = paste0(county, ", ", state_po)) %>% 
  drop_na()
  
others <- election %>% 
  mutate(candidatevotes = ifelse(party %in% c("republican", "democrat"), 
                                 0, candidatevotes)) %>% 
  group_by(year, FIPS) %>% 
  mutate(candidatevotes = sum(candidatevotes)) %>% 
  ungroup()

indices <- which(election$party == "other")
election[indices, "candidatevotes"] <- others[indices, "candidatevotes"]

election_current <- current_data %>% 
  select(state = state_name, county = county_name, FIPS = county_fips, votes_gop:total_votes) %>%
  mutate(votes_indep = total_votes - votes_gop - votes_dem) %>% 
  select(state, county, FIPS, votes_gop, votes_dem, votes_indep, totalvotes = total_votes) %>% 
  pivot_longer(votes_gop:votes_indep, names_to = "party", values_to = "candidatevotes") %>% 
  mutate(party = case_when(party == "votes_gop" ~ "republican",
                           party == "votes_dem" ~ "democrat",
                           party == "votes_indep" ~ "other"),
         candidate = case_when(party == "republican" ~ "Donald Trump",
                               party == "democrat" ~ "Joe Biden",
                               party == "other" ~ "Other"),
         county = str_remove(county, " County$"),
         version = 20201215,
         state_po = state2abbr(state),
         fullname = paste0(county, ", ", state_po),
         year = 2020,
         office = "president") %>%
  select(colnames(election))

election <- election %>% 
  filter(party %in% c("democrat", "republican", "other")) %>% 
  rbind(election_current) %>% 
  mutate(ratio = candidatevotes / totalvotes) %>% 
  group_by(year, state, party) %>% 
  mutate(state_candidatevotes = sum(candidatevotes)) %>% 
  ungroup() %>% 
  group_by(year, state) %>% 
  mutate(state_totalvotes = sum(candidatevotes)) %>% 
  ungroup() %>% 
  mutate(state_ratio = state_candidatevotes / state_totalvotes)

closeness <- election %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  select(year, FIPS, party, ratio) %>% 
  spread(party, ratio) %>% 
  mutate(diff = democrat - republican) %>% 
  mutate(closeness = case_when(
    diff < 0 & republican >= 0.7 ~ 0,
    diff < 0 & republican >= 0.6 & republican < 0.7 ~ 1,
    diff < 0 & republican < 0.6 ~ 2,
    diff > 0 & democrat < 0.6 ~ 3,
    diff > 0 & democrat >= 0.6 & democrat < 0.7 ~ 4,
    diff > 0 & democrat >= 0.7 ~ 5
  ))
  
election <- election %>% 
  left_join(closeness, by = c("year", "FIPS"))

closeness_spread <- closeness %>% 
  select(closeness = year, FIPS, score = closeness) %>% 
  spread(closeness, score, sep = "") %>% 
  select(FIPS, closeness2000, closeness2004, closeness2008, closeness2012, closeness2016, closeness2020)

election_indicator <- election %>% 
  spread(party, ratio) %>% 
  group_by(year, FIPS) %>% 
  summarise(democrat = sum(democrat, na.rm = TRUE), 
            republican = sum(republican, na.rm = TRUE), 
            other = sum(other, na.rm = TRUE),
            fullname = unique(fullname)) %>%
  mutate(red_blue = ifelse(democrat > republican, 1, 0)) %>% 
  ungroup() %>% 
  group_by(FIPS) %>% 
  mutate(l_red_blue = lag(red_blue, default = NA),
         diff_red_blue = red_blue - l_red_blue) %>% 
  ungroup() %>% 
  group_by(FIPS, year) %>% 
  mutate(indicator = sum(red_blue, diff_red_blue, na.rm = TRUE) + 1)
  
election_spread <- election_indicator %>% 
  select(year, FIPS, indicator) %>% 
  spread(year, indicator, sep = "") %>% 
  select(FIPS, year2000, year2004, year2008, year2012, year2016, year2020)



#### IMPORT AND WRANGLE COUNTY LEVEL UNEMPLOYMENT DATA

u_years <- str_pad(c(98:99, 0:19), 2, pad = "0")
u_url_prefix <- "https://www.bls.gov/lau/laucnty"
u_url_suffix <- ".xlsx"

u_urls <- paste0(u_url_prefix, u_years, u_url_suffix)

wrangle_unemployment <- function(index) {
  destfile <- paste0("unemployment_data/", u_years[index], ".xlsx")
  if(!file.exists(destfile)) {
    curl::curl_download(u_urls[index], destfile)
  }
  
  data <- suppressMessages(read_excel(destfile, skip = 2))
  data <- data[4:nrow(data), c(2, 3, 5, 10)]
  colnames(data) <- c("state_fips", "county_fips", "year", "unemployment")
  
  data_final <- data %>% 
    mutate(FIPS = paste0(state_fips, county_fips),
           unemployment = as.numeric(unemployment) / 100,
           year = as.numeric(year)) %>% 
    group_by(state_fips, year) %>% 
    mutate(state_unemployment = mean(unemployment, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    mutate(national_unemployment = mean(unemployment, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(FIPS, year, unemployment, state_unemployment, national_unemployment)
}

unemployment <- wrangle_unemployment(1)

for(i in 2:length(u_urls)) {
  unemployment <- rbind(unemployment, wrangle_unemployment(i))
}



#### IMPORT AND WRANGLE COUNTY LEVEL POPULATION ESTIMATES DATA

n_years <- str_pad(c(1996:1999), 2, pad = "0")
n_url_prefix <- "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen"
n_url_suffix <- ".txt"

n_urls <- paste0(n_url_prefix, n_years, n_url_suffix)

wrangle_population_1 <- function(index) {

  destfile <- paste0("population_data/", n_years[index], ".txt")
  if(!file.exists(destfile)) {
    curl::curl_download(n_urls[index], destfile)
  }

  data <- read_table2(destfile, col_names = FALSE, col_types = "iciiii", progress = FALSE)
  colnames(data) <- c("year", "FIPS", "age", "gender_race", "ethnic", "count")

  data_final <- data %>%
    group_by(year, FIPS) %>%
    summarise(population = sum(count)) %>%
    ungroup() %>%
    mutate(year = paste0("19", year),
           state_fips = str_extract(FIPS, "^..")) %>%
    group_by(state_fips) %>%
    mutate(state_population = sum(population)) %>%
    ungroup() %>%
    mutate(national_population = sum(population)) %>%
    select(year, FIPS, state_fips, population, state_population, national_population)

}

population <- wrangle_population_1(1)

for(i in 2:length(n_urls)) {
  population <- rbind(population, wrangle_population_1(i))
}

destfile <- "population_data/2000_2010.csv"
if(!file.exists(destfile)) {
  curl::curl_download("http://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv", destfile)
}

population_2000_2010 <- read_csv("population_data/2000_2010.csv", col_types = "iiiiicciiiiiiiiiiiii") %>%
  filter(COUNTY != 0) %>%
  mutate(FIPS = paste0(str_pad(STATE, 2, side = "left", pad = "0"),
                       str_pad(COUNTY, 3, side = "left", pad = "0"))) %>%
  gather(key = "year", "population", POPESTIMATE2000:CENSUS2010POP) %>%
  mutate(year = str_extract(year, "....$"),
         year = ifelse(year == "0POP", "2010", year),
         state_fips = str_pad(STATE, 2, side = "left", pad = "0")) %>%
  group_by(year, state_fips) %>%
  mutate(state_population = sum(population)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(national_population = sum(population)) %>%
  ungroup() %>%
  select(year, FIPS, state_fips, population, state_population, national_population)

population <- rbind(population, population_2000_2010)

destfile <- "population_data/2011_2019.csv"
if(!file.exists(destfile)) {
  curl::curl_download("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", destfile)
}

population_2011_2019 <- read_csv("population_data/2011_2019.csv") %>%
  select(STATE:POPESTIMATE2019, -CENSUS2010POP, -ESTIMATESBASE2010, -POPESTIMATE2010) %>%
  filter(COUNTY != "000") %>%
  mutate(FIPS = paste0(STATE, COUNTY)) %>%
  gather(key = "year", "population", POPESTIMATE2011:POPESTIMATE2019) %>%
  mutate(year = str_extract(year, "....$"),
         state_fips = STATE) %>%
  group_by(year, state_fips) %>%
  mutate(state_population = sum(population)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(national_population = sum(population)) %>%
  ungroup() %>%
  select(year, FIPS, state_fips, population, state_population, national_population)

population <- rbind(population, population_2011_2019) %>% 
  mutate(year = as.numeric(year))

population_growth <- population %>%
  pivot_longer(population:national_population) %>%
  group_by(name, FIPS) %>%
  mutate(growth_rate = value / lag(value) - 1) %>%
  select(-value) %>%
  pivot_wider(names_from = name, values_from = growth_rate)



#### IMPORT AND WRANGLE COUNTY LEVEL INCOME ESTIMATES DATA

saipe_county <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME", "GEOID", "SAEMHI_PT", "YEAR"), 
  region = "county:*")

saipe_state <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME", "GEOID", "SAEMHI_PT", "YEAR"), 
  region = "state:*")

median_national <- suppressMessages(read_excel("income_data/h08.xlsx", range = "A7:AX7", col_names = FALSE))
median_national <- median_national[, seq(2, 48, 2)]
median_national <- median_national[, c(-4, -9)]
colnames(median_national) <- c(2019:1998)
median_national <- pivot_longer(median_national, "2019":"1998", names_repair = "minimal")

income <- saipe_county %>% 
  filter(YEAR >= 1998 & YEAR <= 2019) %>% 
  left_join(saipe_state, by = c("YEAR", "state")) %>% 
  select(FIPS = GEOID.x, year = YEAR, median_income = SAEMHI_PT.x, state_median_income = SAEMHI_PT.y) %>% 
  mutate(national_median_income = 0,
         year = as.numeric(year)) %>% 
  group_by(year) %>% 
  mutate(quintiles = cut(median_income, breaks = quantile(median_income, seq(0, 1, 0.2), na.rm = TRUE),
                         labels = seq(1, 5)),
         deciles = cut(median_income, breaks = quantile(median_income, seq(0, 1, 0.1), na.rm = TRUE),
                       labels = seq(1, 10))) %>% 
  ungroup()

for(i in 1998:2019) {
  income[income$year == i, "national_median_income"] <- median_national[median_national$name == i, "value"]
}

income_spread <- income %>% 
  select(median_income = year, FIPS, score = median_income) %>% 
  group_by(median_income) %>%
  mutate(score = cut(score, breaks = quantile(score, seq(0, 1, 0.1), na.rm = TRUE),
                         labels = seq(1, 10))) %>% 
  ungroup() %>% 
  mutate(score = as.numeric(score)) %>% 
  spread(median_income, score, sep = "") %>% 
  select(FIPS, median_income2000, median_income2004, median_income2008, median_income2012, median_income2016,
         median_income2020 = median_income2019)
  
  

#### CREATE MEDIAN INCOME QUANTILES AND VOTING TREND PLOT (STATIC)

population_quantile_plot <- population %>% 
  mutate(year = ifelse(year == 2018, 2020, year))

income_quintile <- income %>% 
  mutate(year = ifelse(year == 2018, 2020, year)) %>% 
  inner_join(election, by = c("year", "FIPS")) %>% 
  inner_join(population_quantile_plot, by = c("year", "FIPS")) %>% 
  group_by(year, quintiles, party) %>% 
  summarise(ratio = weighted.mean(ratio, population)) %>% 
  ungroup() %>% 
  na.omit()


#### INCOME QUANTILE TREND PLOT FUNCTION
##
## args: x = Data Frame, quantile grouped

plot_quintile_subplot <- function(x) {
  
  ratio_lable <- x$ratio
  quintile_lable <- case_when(
    x$quintiles == 1 ~ "Bottom Quintile",
    x$quintiles == 2 ~ "Second Quintile",
    x$quintiles == 3 ~ "Third Quintile",
    x$quintiles == 4 ~ "Fourth Quintile",
    x$quintiles == 5 ~ "Top Quintile",
  )
  
  x %>% 
    plot_ly(hovertemplate = "%{x}:<br>%{y:.2%} <extra></extra>") %>% 
    add_trace(x = ~year, y = ~ratio, color = ~party, type = "bar", text = paste(round(ratio_lable, 4) * 100, "%"),
              textposition = "inside", textfont = list(color = "#FFFFFF", size = 10),
              colors = c("#0B6BA6", "#E6E6E6", "#CD191D"),
              marker = list(line = list(color = "#FFFFFF", width = 1.5)),
              showlegend = FALSE) %>% 
    layout(annotations = list(x = 0.5, y = -0.2, xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
                              showarrow = FALSE,
                              text = unique(quintile_lable),
                              font = list(size = 16)),
           shapes = list(list(type = "line", 
                              xref = "paper", x0 = -0.25, x1 = 1.25,
                              yref = "paper", y0 = 0.475, y1 = 0.475,
                              line = list(color = "black", dash = "dash", width = 0.5))))
  
}

yaxis_layout <- list(showgrid = FALSE, showticklabels = FALSE, title = "")
xaxis_layout <- list(showgrid = FALSE, zeroline = FALSE,
                     tickmode = "array", 
                     tickangle = 90,
                     tickvals = list(2000, 2004, 2008, 2012, 2016, 2020),
                     ticktext = list("2000", "2004", "2008", "2012", "2016", "2020*"),
                     tickfont = list(size = 12), title = "")

income_quintile_plot <- income_quintile %>% 
  group_by(quintiles) %>% 
  do(plots = plot_quintile_subplot(.)) %>% 
  subplot(shareX = TRUE, shareY = TRUE, margin = 0.04,
          widths = c(0.17, 0.22, 0.22, 0.22, 0.17)) %>% 
  layout(showlegend = TRUE, 
         xaxis = c(xaxis_layout),
         xaxis2 = c(xaxis_layout),
         xaxis3 = c(xaxis_layout),
         xaxis4 = c(xaxis_layout),
         xaxis5 = c(xaxis_layout),
         yaxis  = c(yaxis_layout),
         barmode = "relative", 
         hoverdistance = 5,
         margin = list(l = 25, r = 25, b = 100, t = 0, pad = 10)) %>% 
  config(displayModeBar = FALSE)

income_quintile_plot



#### CREATE UNEMPLOYMENT / INCOME DENSITY PLOTS (STATIC)

states_filter <- election_indicator %>% 
  mutate(party = ifelse(indicator == 2 | indicator == 3, "democrat", "republican")) %>% 
  select(year, FIPS, party, indicator, fullname)

unemployment_density_plot_data <- unemployment %>% 
  mutate(year = ifelse(year == 2019, 2020, year)) %>% 
  inner_join(states_filter, by = c("year", "FIPS")) %>%
  mutate(groups = cut(unemployment, breaks = seq(0, 0.20, 0.002), labels = FALSE)) %>% 
  mutate(groups = groups * 0.002 - 0.001) %>% 
  group_by(groups, year, indicator) %>% 
  mutate(id = row_number() / 2) %>% 
  ungroup()
   
income_density_plot_data <- income %>% 
  mutate(year = ifelse(year == 2019, 2020, year)) %>% 
  inner_join(states_filter, by = c("year", "FIPS")) %>% 
  mutate(groups = cut(median_income, breaks = seq(0, 160000, 1600), labels = FALSE)) %>% 
  mutate(groups = groups * 1600 - 800) %>% 
  group_by(groups, year, indicator) %>% 
  mutate(id = row_number() / 2) %>% 
  ungroup()


#### UNEMPLOYMENT / INCOME DENSITY PLOT FUNCTION
##
## args: x = string, party name
##       y = numerical, year
##       z = string, plot type

plot_density_subplot <- function(x, y, z) {
  
  if(z == "unemployment") {
    
    states_year_data <- unemployment_density_plot_data %>% 
      filter(year == y) %>% 
      mutate(data = unemployment) %>% 
      mutate(label = label_percent(accuracy = 0.01)(data))
    yaxis_range <- c(0, 40)
    xaxis_range <- c(0, 0.18)
    yend <- 40
    x_tickformat <- "%"
    
  } else {
    
    states_year_data <- income_density_plot_data %>% 
      filter(year == y) %>% 
      mutate(data = median_income) %>% 
      mutate(label = paste0("$ ", data))
    yaxis_range <- c(0, 0.00007)
    xaxis_range <- c(0, 160000)
    yend <- max(yaxis_range)
    x_tickformat <- "$"
    
  }
  
  party_states_year_data <- states_year_data %>% 
    filter(party == x)
  
  if(x == "republican") {
    
    flipped_states <- party_states_year_data %>% 
      inner_join(states_filter %>% filter(indicator == 0), by = c("year", "FIPS", "fullname"))
    
    line_colour <- "#CD191D"
    fill_colour <- "rgba(205, 25, 29, 0.6)"
    yaxis_range <- yaxis_range
    yaxis2_range <- c(0, 20)
    overlaying <- "y"
    dot_title <- "Flipped Republican Counties"
    autorange <- ""
    
  } else {
    
    flipped_states <- party_states_year_data %>% 
      inner_join(states_filter %>% filter(indicator == 3), by = c("year", "FIPS", "fullname"))
    
    line_colour <- "#0B6BA6"
    fill_colour <- "rgba(11, 107, 166, 0.6)"
    yaxis_range <- rev(yaxis_range)
    yaxis2_range <- c(20, 0)
    overlaying <- "y3"
    dot_title <- "Flipped Democrat Counties"
    autorange <- "reverse"
    
  }
  
  data_density <- density(party_states_year_data$data)
  data_median <- median(party_states_year_data$data)
  data_sd <- sd(party_states_year_data$data)
  
  unemployment_median_text <- paste0("Median: ", label_percent(accuracy = 0.01)(data_median), 
                                     "<br>Standard Dev.: ", label_percent(accuracy = 0.01)(data_sd))
  income_median_text <- paste0("Median: $", data_median, "<br>Standard Dev.: $", round(data_sd))
  
  density_plot_subplot <- party_states_year_data %>% 
    plot_ly() %>% 
    add_trace(x = ~data_density$x, y = ~data_density$y, type = "scatter", mode = "lines",
              fill = "tozeroy", fillcolor = fill_colour, line = list(color = line_colour, width = 1.5),
              showlegend = FALSE, hoverinfo = "skip") %>% 
    add_segments(x = data_median, y = 0,
                 xend = data_median, yend = yend,
                 line = list(color = "#000000", width = 0.5, dash = "dash"),
                 showlegend = FALSE, hoverinfo = "skip") %>%
    add_trace(x = ~flipped_states$groups, y = ~flipped_states$id, type = "scatter", mode = "markers", yaxis = "y2",
              name = dot_title, 
              text = ~paste0(flipped_states$fullname, "<br>", flipped_states$label),
              hovertemplate = "%{text}<extra></extra>",
              marker = list(size = 5, 
                            color = line_colour)) %>% 
    add_annotations(x = data_median * 1.1, y = yend * 0.8, xref = "x", yref = "y", align = "left",
                    text = ifelse(z == "unemployment", unemployment_median_text, income_median_text),
                    xanchor = "left", showarrow = FALSE,  font = list(size = 12)) %>%
    layout(yaxis = list(range = yaxis_range, title = "", showgrid = FALSE, showticklabels = FALSE,
                        zeroline = FALSE),
           yaxis2 = list(range = yaxis2_range, showgrid = FALSE, showticklabels = FALSE,
                         side = "right", title = "", overlaying = overlaying),
           xaxis = list(range = xaxis_range, title = "", tickformat = x_tickformat,
                        zeroline = FALSE),
           legend = list(xanchor = "right", x = 0.99, yanchor = "top", y = 0.99),
           hoverdistance = 5) %>% 
    config(displayModeBar = FALSE)
  
}

# test_republican_unemployment <- plot_density_subplot("republican", 2020, "income")
# test_democrat_unemployment <- plot_density_subplot("democrat", 2020, "income")
# 
# test_unemployment_plot <- subplot(test_republican_unemployment, test_democrat_unemployment,
#                                   shareX = TRUE, nrows = 2)
# 
# test_unemployment_plot


{ density_plots <- vector(mode = "list", 
                          length = length(seq(2000, 2020, 4)) * length(c("unemployment", "income")))
  k <- 1
  
  for(j in c("unemployment", "income")) {
    for(i in seq(2000, 2020, 4)) {
      
      republican_density_subplot <- plot_density_subplot("republican", i, j)
      democrat_density_subplot <- plot_density_subplot("democrat", i, j)
      
      combined_density_plot <- subplot(republican_density_subplot, democrat_density_subplot,
                                            shareX = TRUE, nrows = 2)
      
      density_plots[[k]] <- combined_density_plot
      
      k <- k + 1
      
    }
  }
}



#### CREATE UNEMPLYOMENT / MEDIAN INCOME TREND PLOTS

complete_states_filter <- states_filter %>% 
  slice(rep(1:n(), each = 4)) %>% 
  arrange(year, FIPS)

complete_years <- unlist(lapply(states_filter[["year"]], function(x) seq(x-3, x)))

complete_states_filter["year"] <- complete_years

median_income_trend_plot_data <- income %>% 
  left_join(population, by = c("year", "FIPS")) %>% 
  left_join(complete_states_filter, by = c("year", "FIPS"))





#### MERGE ELCTION DATA WITH SPATIAL DATA, CREATE STATE BORDERS

election_final <- election_spread %>% 
  full_join(closeness_spread, by = "FIPS") %>% 
  full_join(income_spread, by = c("FIPS"))

final_map <- merge(county_map, election_final, by.x = "GEOID", by.y = "FIPS")
final_map@data[["label"]] <- final_map@data[["closeness2020"]]

state_borders <- aggregate(final_map[, "STATEFP"], by = list(ID = final_map@data$STATEFP), 
                                   FUN = unique, dissolve = T)



#### GENERATE LABELS FOR LEAFLET POPUPS (MULTI-THREAD PROCESS)

{ leaflet_labels <- list()
  
  numbering <- c("Bottom", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "Top")
  
  # Prepare Multi-thread cluster and export libraries and objects
  cl <- makeCluster(4)
  clusterEvalQ(cl, library(tidyverse))
  clusterEvalQ(cl, library(sp))
  clusterExport(cl, "election")
  clusterExport(cl, "final_map")
  clusterExport(cl, "income")
  clusterExport(cl, "numbering")
  
  for(j in seq(2000, 2020, 4)) {
    
    clusterExport(cl, 'j')
    
    election_labels <- parLapply(
      cl, seq(nrow(final_map)), function(i) {
          paste0('<strong style="font-size:20px;">',
                 election %>% filter(FIPS == final_map[["GEOID"]][i]) %>% select(fullname) %>% head(1) %>% pull(), '</strong>', '<br>',
                 j,
                 '<table style="font-size:14px;">', '<tr>',
                 '<td style="background-color:#0B6BA6; width:5px;">', '</td>',
                 '<td>', ' Democrat: ', '</td>',
                 '<td>', round(election %>% filter(year == j, party == "democrat",
                                                   FIPS == final_map[["GEOID"]][i]) %>% select(ratio) %>% pull() * 100, 2), ' %', '</td>',
                 '</tr>',
                 '<tr>',
                 '<td style="background-color:#CD191D; width:5px;">', '</td>',
                 '<td>', ' Republican: ', '</td>',
                 '<td>', round(election %>% filter(year == j, party == "republican",
                                                   FIPS == final_map[["GEOID"]][i]) %>% select(ratio) %>% pull() * 100, 2), ' %', '</td>',
                 '</tr>', '</table>')
      })
    
    leaflet_labels <- append(leaflet_labels, election_labels)
  }
  
  for(j in seq(2000, 2020, 4)) {
    
    clusterExport(cl, 'j')
    
    closeness_labels <- parLapply(
      cl, seq(nrow(final_map)), function(i) {
        paste0('<strong style="font-size:20px;">',
               election %>% filter(FIPS == final_map[["GEOID"]][i]) %>% select(fullname) %>% head(1) %>% pull(), '</strong>', '<br>',
               j,
               '<table style="font-size:14px;">', '<tr>',
               '<td style="background-color:#0B6BA6; width:5px;">', '</td>',
               '<td>', ' Democrat: ', '</td>',
               '<td>', round(election %>% filter(year == j, party == "democrat",
                                                 FIPS == final_map[["GEOID"]][i]) %>% select(ratio) %>% pull() * 100, 2), ' %', '</td>',
               '</tr>',
               '<tr>',
               '<td style="background-color:#CD191D; width:5px;">', '</td>',
               '<td>', ' Republican: ', '</td>',
               '<td>', round(election %>% filter(year == j, party == "republican",
                                                 FIPS == final_map[["GEOID"]][i]) %>% select(ratio) %>% pull() * 100, 2), ' %', '</td>',
               '</tr>', '</table>')
      })
    
    leaflet_labels <- append(leaflet_labels, closeness_labels)
  }
  
  for(j in c(seq(2000, 2016, 4), 2019)) {
    
    clusterExport(cl, 'j')
    
    income_labels <- parLapply(
      cl, seq(nrow(final_map)), function(i) {
        paste0(
          '<strong style="font-size:20px;">', final_map[["NAME"]][i], ", ",
          election %>% filter(FIPS == final_map[["GEOID"]][i]) %>% select(state_po) %>% head(1) %>% pull(), '</strong>', '<br>', 
          j,
          '<table style="font-size:14px;">', '<tr>', 
          '<td>', 'County Median Income: ', '</td>',
          '<td>', '$ ', 
          income %>% filter(year == j, FIPS == final_map[["GEOID"]][i]) %>% 
            select(median_income) %>% head(1) %>% pull() %>% format(big.mark = ","), " (",
          numbering[income %>% filter(year == j, FIPS == final_map[["GEOID"]][i]) %>% select(deciles) %>% pull()], 
          ' decile)', '</td>', 
          '</tr>', 
          '<tr>',
          '<td>', 'State Median Income: ', '</td>',
          '<td>', '$ ', income %>% filter(year == j, FIPS == final_map[["GEOID"]][i]) %>% 
            select(state_median_income) %>% head(1) %>% pull() %>% format(big.mark = ","), '</td>', 
          '<tr>', '</table>')
      })
    
    leaflet_labels <- append(leaflet_labels, income_labels)
  }
  
  stopCluster(cl)
}



#### EXPORT WRANGLED DATA

saveRDS(election, "app/election.RDS")
saveRDS(final_map, "app/final.RDS")
saveRDS(unemployment, "app/unemployment.RDS")
saveRDS(income, "app/income.RDS")
saveRDS(income_quintile_plot, "app/income_quintile_plot.RDS")
saveRDS(density_plots, "app/density_plots.RDS")
saveRDS(population, "app/population.RDS")
saveRDS(state_borders, "app/state_borders.RDS")
saveRDS(leaflet_labels, "app/leaflet_labels.RDS")


#### END TIMER

(Sys.time() - start)

#### CLEAR MEMORY

# rm(list=ls())
