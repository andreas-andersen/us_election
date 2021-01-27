
## (shiny) Map of US Presidential Election results and economic data

https://andreasandersen.shinyapps.io/US_election/
![Map Preview](/images/preview.png)

This is my github repository for my shiny app.

The map displays election results and economic data (median income and unemployment) in the period 2000 - 2020 (2019 for economic data).
The income and density plots is an attempt at visualizing the distribution of "rich" and "poor" counties across the two major political parties.

The app is coded in R.
Map is created with leaflet and graphs with plotly.

Scripts and functions:
* __update.R__
  * Imports spatial data, simplifies polygons, and exports spatial data including values used for colouring the counties in leaflet.
  * Imports, wrangles and exports unemployment, income and population data
  * Generates static plotly plots (income quantile plots and density plots)
* __app/app.R__
  * Main shiny app
