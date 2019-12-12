#Let's start over using the gapminder data set!
library(ggplot2)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(sf)
library(scales)
options(warn=-1)


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

# map chart starts
geo_data <- st_read("data/map_data.geojson")
plot_map <-  function(column_name){
    p <- ggplot(data = geo_data, aes_string(fill = column_name)) +
    geom_sf(aes( text = paste("Country:", country, "<br>", str_replace_all(column_name, "_", " " ),':', 
                              percent(get(column_name), accuracy = 0.01)))) +
    scale_fill_distiller(palette = "Reds", trans = "reverse") +
    theme_bw() +
    labs(title = paste('Death percentage of',
                       str_replace_all(column_name, "_", " " ),
                       'among countries in 2017'),
        fill = 'Proportion of death') 
    
    ggplotly(p,tooltip = c("text"))   
}
# map ends

# line graph starts
factors_data=read.csv("data/clean_data_line_plot_new.csv")

plot_line <- function (cols){

    line_plot <- ggplot(factors_data, aes(x= year, y = get(cols), color=continent)) +
             geom_line(size=0.5)+
             geom_point(aes(text = paste("Continent:", continent, "<br>", "Year:", year, "<br>",
                                         str_replace_all(cols, "_", " "),':',
                              percent(get(cols), accuracy = 0.01
                                         ))))+
             theme(axis.text.x = element_text(angle = -60))+
             scale_x_continuous(breaks = seq(1990, 2020, 1))+
             theme_bw() +
             xlab("Years") +
             ylab("Death % over the total death in the continent")+
            scale_y_continuous(labels = scales::percent)+
            ggtitle(paste0("Trend of death due to ",str_replace_all(cols, "_", " "), " over time, 1990-2017"))
    
    ggplotly(line_plot,tooltip = c("text") )
    }

# store the charts into bar_graph 
bar_graph <- dccGraph(
  id = 'bar-graph',
  figure=plot_bar(bar_data)
  )


show_map <- dccGraph(
  id = 'map-graph',
  figure=plot_map('high_blood_pressure')
  )

line_chart <- dccGraph(
  id = 'line_chart',
  figure=plot_line('high_blood_pressure')
  )




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
      line_chart,
      htmlDiv(),
      htmlP('Data source: "Institute for Health Metrics and Evaluation (IHME), 2018".')
    )
  )
)

app$run_server()
