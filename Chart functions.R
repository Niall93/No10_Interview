#Create single line chart
library(tidyverse)
library(forcats)
library(gghighlight)

custom_ggplot_theme <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),  # Remove x-gridlines
      panel.grid.minor.x = element_blank(),  # Remove x-gridlines
      panel.grid.major.y = element_line(color = "lightgrey"),  # Keep y-gridlines
      panel.grid.minor.y = element_blank(),  # Remove minor y-gridlines
      axis.line = element_line(color = "darkgrey"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      axis.title.y = element_text(angle = 0, hjust = 0, vjust = 1),  # Y-axis label at top-left
      plot.title = element_text(hjust = 0.5,family="Arial",face="bold")
    )
}

one_line_timeseries <- function(.data,datecol,ycol,dateformat="%Y-%m-%d"){
  
  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))
  
  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }
  
  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }
  
  
  .data <- .data |>
    dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
    dplyr::mutate(value = {{ycol}})
  
  ggplot2::ggplot(data = .data,ggplot2::aes(x = Date,y = value)) +
    ggplot2::geom_line(size = 1.5, color = "#12436D") +
    custom_ggplot_theme()
}

multi_line_timeseries <- function(.data,datecol,ycol,groupcol,dateformat = "%Y-%m-%d"){
  
  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))
  
  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }
  
  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }
  
  
  .data <- .data |>
    dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
    dplyr::mutate(value = {{ycol}}) |>
    dplyr::mutate(variable = {{groupcol}})
  
  variable_count <- length(unique(.data$variable))
  
  
  graph <- .data |>
    ggplot2::ggplot(ggplot2::aes(x = Date,y = value)) +
    ggplot2::geom_line(size = 1, ggplot2::aes(color = variable)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    custom_ggplot_theme()
  
  if(variable_count == 2){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#F46A25"))
  }else if(variable_count == 3){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25"))
  }else if(variable_count == 4){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25","#801650"))
  }else if(variable_count == 5){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25","#801650","#3D3D3D"))
  }else{
    stop("This function only allows for plotting up to 5 categories (lines) on one graph. If your grouping variable has more than 5 categories, it is suggested you use the facet_timeseries or facet_highlight_timeseries function")
  }
  
  graph
  
}

facet_highlight_timeseries <-function(.data,datecol,ycol,groupcol,dateformat="%Y-%m-%d"){
    
    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))
    
    if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
      stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
    }
    
    if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
      stop("Value column contains non-numeric values. Check your data and try again")
    }
    
    
    .data <- .data |>
      dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
      dplyr::mutate(value = {{ycol}}) |>
      dplyr::mutate(GROUP = {{groupcol}})
    
    ggplot2::ggplot(.data, mapping=ggplot2::aes(x=Date, y=value)) +
      ggplot2::geom_line(show.legend = FALSE, col = "#012169" , size = 1.5) +
      ggplot2::geom_line(ggplot2::aes(Date,value,colour=GROUP), colour = "#012169")+
      ggplot2::facet_wrap(~ GROUP, scales='free',strip.position="top") +
      gghighlight::gghighlight(use_direct_label = FALSE) +
      custom_ggplot_theme()
}

highlight_timeseries <-function(.data,datecol,ycol,groupcol,highlights,dateformat="%Y-%m-%d"){
  
  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))
  
  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }
  
  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }
  
  
  .data <- .data |>
    dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
    dplyr::mutate(value = {{ycol}}) |>
    dplyr::mutate(GROUP = {{groupcol}})
  
  graph <- ggplot2::ggplot(.data) +
    ggplot2::geom_line(ggplot2::aes(x = Date,y=value,colour=GROUP),show.legend = FALSE, size = 1.5) +
    gghighlight::gghighlight(GROUP %in% highlights,use_direct_label = TRUE) +
    custom_ggplot_theme()
  
  variable_count <- length(highlights)
  
  if(variable_count == 2){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#F46A25"))
  }else if(variable_count == 3){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25"))
  }else if(variable_count == 4){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25","#801650"))
  }else if(variable_count == 5){
    graph <- graph +
      ggplot2::scale_color_manual(values=c("#12436D","#28A197","#F46A25","#801650","#3D3D3D"))
  }else{
    stop("This function only allows for plotting up to 5 categories (lines) on one graph. If your grouping variable has more than 5 categories, it is suggested you use the facet_timeseries or facet_highlight_timeseries function")
  }
  
  graph
  
 }

simple_barchart <- function(.data,xcol,ycol,ordered = "none"){
  
  if(!(ordered %in% c("none","asc","desc"))){
    stop("ordered must be one of none, asc or desc")
  }
  .data <- .data |>
    dplyr::mutate(value = {{ycol}}) |>
    dplyr::mutate(GROUP = {{xcol}})
  
  if(ordered=="asc"){
    .data <- .data %>%
      mutate(GROUP = fct_reorder(GROUP, value))
  }
  if(ordered=="desc"){
    .data <- .data %>%
      mutate(GROUP = fct_reorder(GROUP, desc(value)))
  }
  
  ggplot(.data) +
    geom_bar(aes(x = GROUP,y=value),fill="#12436D",stat="identity") +
    scale_y_continuous(expand=c(0,0)) 
}

stacked_barchart <- function(.data,Cat1_Col,Cat2_Col,ycol,perc=FALSE,flipped =TRUE){
  
  .data <- .data |>
    dplyr::mutate(value = {{ycol}}) |>
    dplyr::mutate(GROUP = {{Cat1_Col}}) |>
    dplyr::mutate(GROUP2 = {{Cat2_Col}})
  
  if(perc){
  graph <-  ggplot(.data) +
    geom_bar(aes(x = GROUP,y=value,fill=GROUP2),stat="identity",position="fill") +
    scale_y_continuous(expand=c(0,0),labels = scales::percent) +
    custom_ggplot_theme() 
  }else{
    graph <-  ggplot(.data) +
      geom_bar(aes(x = GROUP,y=value,fill=GROUP2),stat="identity") +
      scale_y_continuous(expand=c(0,0),labels = scales::comma) +
      custom_ggplot_theme() 
  }
  
  variable_count <- length(unique(.data$GROUP2))
  
  if(variable_count == 2){
    graph <- graph +
      ggplot2::scale_fill_manual(values=c("#12436D","#F46A25"))
  }else if(variable_count == 3){
    graph <- graph +
      ggplot2::scale_fill_manual(values=c("#12436D","#28A197","#F46A25"))
  }else if(variable_count == 4){
    graph <- graph +
      ggplot2::scale_fill_manual(values=c("#12436D","#28A197","#F46A25","#801650"))
  }else if(variable_count == 5){
    graph <- graph +
      ggplot2::scale_fill_manual(values=c("#12436D","#28A197","#F46A25","#801650","#3D3D3D"))
  }else{
    stop("This function only allows for plotting up to 5 categories (lines) on one graph. If your grouping variable has more than 5 categories, it is suggested you use the facet_timeseries or facet_highlight_timeseries function")
  }
  
  if(flipped){
    graph <- graph +
      coord_flip() +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "lightgrey")  
      )
  }
  graph
}

stacked_barchart(example_data,category1,category2,value,perc=F)

