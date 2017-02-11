suppressMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
})
  
getFacetPlots <- function(){
  data <- getData()

  return(list(
    genre_count = createFacetPlot(data$genre, "Year", "Count", "Genre"),
    genre_rating = createFacetPlot(data$genre, "Year", "Median Rating", "Genre"),
    genre_years = createFacetPlot(data$genre, "Year", "Median Number of Years", "Genre"),
    network_count = createFacetPlot(data$network, "Year", "Count", "Network"),
    network_rating = createFacetPlot(data$network, "Year", "Median Rating", "Network"),
    network_years = createFacetPlot(data$network, "Year", "Median Number of Years", "Network")
  ))
  
  
}

getData <- function(){
  df_genres <- read.csv("data/df_genres.csv", stringsAsFactors = F)
  df_genres["genre"] <- lapply(df_genres["genre"], factor)
  df_genres["original_network"] <- lapply(df_genres["original_network"], factor)
  
  df_final <- read.csv("data/df_final.csv", stringsAsFactors = F)
  df_final["original_network"] <- lapply(df_final["original_network"], factor)
  
  genre_year <- df_genres %>% group_by(genre, start_year) %>% summarise(`Median Number of Years`=median(num_years, na.rm=T), `Median Rating`=median(rating, na.rm=T), Count=n()) %>% filter(!is.na(sd))
  #bad_genres <- c("anthology", "biography", "news", "history", "horror", "music", "musical", "short", "sport", "talk-show", "western", "war", "game-show", "documentary", "fantasy")
  #bad_genres <- c("")
  bad_genres <- c("anthology", "biography", "short", "sport", "war", "musical", "western")
  
  genre_year <- genre_year %>% filter(!(genre %in% bad_genres))
  genre_year <- genre_year %>% rename(Year=start_year, Genre=genre)
  
  network_year <- df_final %>% group_by(original_network, start_year) %>% summarise(`Median Number of Years`=median(num_years, na.rm=T),`Median Rating`=median(rating, na.rm=T), Count=n()) %>% filter(!is.na(sd))
  top_networks_df <- (df_final %>% filter(original_network != "none") %>% group_by(original_network) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(20, count))
  top_networks_vec <- top_networks_df[["original_network"]]
  network_year <- network_year %>% filter(original_network %in% top_networks_vec)
  network_year <- network_year %>% rename(Network=original_network, Year=start_year)
  
  return(list(
    genre = genre_year,
    network = network_year
  ))
}

createFacetPlot <- function(data, x, y, by){
  x <- as.name(x)
  y <- as.name(y)
  by <- as.name(by)

  title <- paste0(y, " of Shows per ", x, " by ", by)
  
  plt <- ggplot(data, aes_string(x=x, y=y)) + 
    geom_point(aes_string(color=by)) + 
    facet_wrap(as.formula(paste("~", by))) + 
    ggtitle(title) + 
    theme_fivethirtyeight() + 
    theme(
      plot.title = element_text(
        hjust = 0.5,
        margin = margin(0,0,20,0),
        size = 14
      ),
      legend.position = "none",
      panel.background = element_rect(fill = "#ffffff"),
      plot.background = element_rect(fill = "#ffffff"),
      strip.background = element_rect(fill="#000000"),
      strip.text = element_text(color="#ffffff"),
      panel.spacing = unit(20, "pt"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  return(plt)
}

getDetailPlot <- function(data, title, x, y, xRange, yRange){
  
  ggplot(data, aes_string(x=x, y=y)) + 
    geom_point(size=3) + 
    ggtitle(title) + 
    theme_fivethirtyeight() + 
    theme(
      plot.title = element_text(
        hjust = 0.5,
        margin = margin(0,0,20,0),
        size = 14
      ),
      legend.position = "none",
      panel.background = element_rect(fill = "#ffffff"),
      plot.background = element_rect(fill = "#ffffff"),
      strip.background = element_rect(fill="#000000"),
      strip.text = element_text(color="#ffffff"),
      panel.spacing = unit(20, "pt"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    coord_cartesian(xlim = xRange, ylim = yRange)
}