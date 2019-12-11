#Let's start over using the gapminder data set!
library(ggplot2)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)


# We'll replace our styles with an external stylesheet 
# for simplicity
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# bar chart graph starts
# load the data for bar chart and make plot_bar() function
bar_data <- read.csv("data/clean_data_for_bar_chart.csv")
bar_data <- bar_data %>% 
    rename(Death_in_million = Death..in.million.)

plot_bar <- function(data){
    p <- ggplot(data, aes(x = reorder(Risk_factors, Death_in_million), y = Death_in_million)) +
    geom_bar(stat = 'identity', color = 'grey', fill = 'orange') +
    coord_flip() +
    theme_bw() +
    labs(x = 'Risk Factors', 
        y = 'Death (in million)',
        title = 'Global death by risk factors in 2017 (in million)')
    ggplotly(p)
}
# store the bar chart into bar_graph 
bar_graph <- dccGraph(
  id = 'bar-graph',
  figure=plot_bar(bar_data)
  )
# bar chart part ends

# map chart starts
show_map <- htmlIframe(src = "<//plot.ly/~mirohu/1.embed", 
                    width="900", height="800",
                    style=list(borderWidth = 0))

# layout starts
app$layout(
  htmlDiv(
    list(
      htmlH1('Exploring the Risk Factors of Death'),
      # htmlH2(''),
      bar_graph,
      htmlDiv(),
      show_map,
      htmlDiv(), #spacer
      htmlH3('Data source: "Institute for Health Metrics and Evaluation (IHME), 2018".')
    )
  )
)

app$run_server()
