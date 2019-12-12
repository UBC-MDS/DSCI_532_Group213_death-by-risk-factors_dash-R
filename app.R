#Let's start over using the gapminder data set!
library(ggplot2)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(sf)
library(scales)


# We'll replace our styles with an external stylesheet 
# for simplicity
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# bar chart graph starts
# load the data for bar chart and make plot_bar() function
bar_data <- read.csv("data/clean_data_for_bar_chart.csv")
bar_data <- bar_data %>% 
    rename(Death_in_million = Death..in.million.)

plot_bar <- function(data){
    #' Draw a bar chart for all the death by risk factors in 2017 (descending order)
    #'
    #' @param data -- (string) the data source
    #'
    #' @return a bar chart of all the death by risk factors in 2017
    #'  (descending order)
    #'
    #' @examples plot_bar(bar_data)
    p <- ggplot(data, aes(x = reorder(Risk_factors, Death_in_million), y = Death_in_million,
                         text = paste("Risk factor: ",Risk_factors, "<br>", 
                                      'Death in million: ', round(Death_in_million,2)))) +
    geom_bar(stat = 'identity', color = 'grey', fill = 'orange') +
    coord_flip() +
    theme_bw() +
    labs(x = 'Risk Factors', 
        y = 'Death (in million)',
        title = 'Death (in million) by risk factors in 2017')
    ggplotly(p,tooltip = c('text')) 
    
}

# set y-axis key
yaxisKey <- tibble(label = c("High blood Pressure", "Smoking", "High blood sugar", "Air pollution outdoor & indoor", 'Obesity'),
                   value = c("high_bood_pressure", "smoking", "high_blood_sugar", "air_pollution_outdoor_._indoor", "obesity"))

yaxisDropdown <- dccDropdown(
  id = 'y-axis',
  options = map(
    1:nrow(yaxisKey), function(i){
      list(label=yaxisKey$label[i], value=yaxisKey$value[i])
    }),
  value = "high_blood_pressure"
)

# map chart starts
geo_data <- st_read("data/map_data.geojson")
plot_map <-  function(yaxis){
    #' Draw heatmap for given quantitative value in world map
    #'
    #' @param yaxis -- (string) columns in source, default: 
    #' 'high_blood_pressure'
    #'
    #' @return a headmap for givien quantitative value in the world map
    #'
    #' @examples draw_map('high_blood_sugar')
    y_axis_reassigned <- aes_string(fill = yaxis)
    p <- ggplot(data = geo_data, y_axis_reassigned) + 
    geom_sf(aes(text = paste("Country:", country, "<br>", str_replace_all(y_axis_reassigned, "_", " " ),':', 
                              percent(get(yaxis), accuracy = 0.01)))) +
    scale_fill_distiller(palette = "Reds", trans = "reverse") +
    theme_bw() +
    labs(title = paste('Death percentage of',
                       str_replace_all(yaxis, "_", " " ),
                       'among countries in 2017'),
        fill = 'Proportion of death') 
    
    ggplotly(p,tooltip = c("text"))   
}
# map ends

yaxisRadioButton <- dccRadioItems(
  id = 'y-axis-line-graph',
  options = map(
    1:nrow(yaxisKey), function(i){
      list(label=yaxisKey$label[i], value=yaxisKey$value[i])
    }),
  value = "high_blood_pressure"
)

# line graph starts
factors_data=read.csv("data/clean_data_line_plot_new.csv")

plot_line <- function (cols){
    #' Draw the interactie line graph for the trend of the 
    #' top five risk factors of death from 1990 to 2017 by continent
    #'
    #' @param yaxis  -- (string) columns in source, default: 
    #' 'high_blood_pressure'
    #'
    #' @return a interactive line chart
    #' 
    #' @examples plot_line('high_blood_sugar)
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
            ggtitle(paste0("Trend of death due to ",str_replace_all(cols, "_", " "), " over time, 1990-2017")) +
            theme(plot.margin = margin(.8,.8,1,1, "cm"))
    
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
  id = 'line-chart',
  figure=plot_line('high_blood_pressure')
  )


# layout starts
app$layout(
  htmlDiv(list(htmlH2('Exploring the Risk Factors of Death', style = list(textAlign = 'center')),
    htmlH5('This app explores the various death causing risk factors globally, geographically in 2017 and the trends of the top five risk factors over years across continents.', 
            style = list(textAlign = 'left')),
    dccTabs(id = 'tabs', value = 'tab-1', children = list(
      dccTab(label = '2017 Overview', value = 'tab-1', children = list(
          bar_graph,
          htmlP('Data source: "Institute for Health Metrics and Evaluation (IHME), 2018".')
      )),
      
      dccTab(label = '2017 World Spread', value = 'tab-2', children = list(
          yaxisDropdown,
          show_map
      )),

      dccTab(label = 'Trend over Continent', value = 'tab-3', children = list(
          yaxisRadioButton,
          line_chart
      ))
    ))
  ))
)

# Adding callbacks for interactivity
app$callback(
  # update figure of gap-graph
  output=list(id = 'map-graph', property='figure'),
  
  params=list(input(id = 'y-axis', property='value')),

  # this translates your list of params into function arguments
  function(yaxis_value) {
    plot_map(yaxis_value)
  })

app$callback(
  # update figure of gap-graph
  output=list(id = 'line-chart', property='figure'),
  
  params=list(input(id = 'y-axis-line-graph', property='value')),

  function(yaxis_value){
    plot_line(yaxis_value)
  })

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
