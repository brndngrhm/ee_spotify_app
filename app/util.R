
library(dplyr)
library(lubridate)
library(highcharter)
library(DT)
library(spotifyr)
library(ggcorrplot)
library(ggridges)
library(factoextra)
library(viridisLite)
library(ggiraphExtra)
library(purrr)
library(tidyr)
library(flexclust)
library(ClusterR)

get_ee_data <- function(){
  
  access_token <- get_spotify_access_token()
  
  #EE uri's
  artist <- "540R9ampm8wv8ddRgCuzmu"
  ee1 <- "6d5fJJTFC7ye5ZU9PgYdZ6"
  ee2 <- "6uqZhqgs6v552QuFSsrCpC"
  split <- "3Sl0RHAzh2E5SwwJfNSMn0"
  ep <- "7uwPqfziaTdk7KbUr4VXQJ"

  ee_album_ids <- c(ee1, ee2, split, ep)

  songs <- purrr::map(ee_album_ids, ~get_album_tracks(.)) %>%
    set_names(nm = ee_album_ids) %>%
    purrr::map_dfr(., bind_rows, .id = "album_id") %>%
    mutate(album_name = case_when(
      album_id == "6d5fJJTFC7ye5ZU9PgYdZ6" ~ "Everyone Everywhere 2010",
      album_id == "6uqZhqgs6v552QuFSsrCpC" ~ "Everyone Everywhere 2012",
      album_id == "3Sl0RHAzh2E5SwwJfNSMn0" ~ "Split EP",
      album_id == "7uwPqfziaTdk7KbUr4VXQJ" ~ "A Lot of Weird People Standing Around",
      TRUE ~ "Other"
    ))
  
  features <- get_track_audio_features(songs$id, access_token) %>% 
    select(id, danceability:tempo)
  
  # popular <- get_track_popularity(songs$id, access_token)
  
  #combine and remove IIOI songs from the split
  ee <- songs %>%
    left_join(., features, by = "id") %>%
    mutate(iioi = ifelse(name %in% c("Raw Bar OBX 2002", "Augusta, GA") & album_name == "Split EP", 1, 0)) %>%
    filter(iioi == 0) %>%
    select(-iioi)
  
  return(ee)

}

feature_analysis <- function(df, feature){
  
  feature <- enquo(feature)
  
  df <- df %>%
    select(artist_name, album_name, track_name, !!feature)
  
  names(df)[4] <- 'param'
  
  df %>% 
    group_by(artist_name) %>%
    top_n(10, param) %>%
    ungroup()%>%
    arrange(desc(param)) %>%
    mutate(artist_color = case_when(
      artist_name == "Everyone Everywhere" ~ colorize(artist_name, "#4db6ac"),
      TRUE ~ colorize(artist_name, "#ffcc80")
    ),
    param_choice = !!feature)
}

reccomendation <- function(df, song){
  
  # get non-EE artist songs, and cluster
  other <-  df %>% filter(artist_name != "Everyone Everywhere")
  cluster_df <- other %>% select(-c(album_name, artist_name, track_name)) %>% as.data.frame()
  cluster <- ClusterR::KMeans_rcpp(cluster_df, clusters = 3, num_init = 4, max_iters = 100, initializer = 'kmeans++')
  
  # join cluster centroids 
  cluster_centroids <- data.frame(cluster = cluster$clusters)
  other <- bind_cols(other, cluster_centroids)
  
  # predict which cluster selected EE song belonds to
  ee <- df %>% filter(artist_name == "Everyone Everywhere") %>% filter(track_name == {{song}}) %>% 
    select(-c(album_name, artist_name, track_name)) %>% 
    as.data.frame()
  
  pred_cluster <- ClusterR::predict_KMeans(ee, cluster$centroids)
  
  # return 5 random songs from the predicted cluster
  other %>% 
    filter(cluster == pred_cluster[1]) %>% 
    ungroup()%>%
    sample_n(size = 5) %>%
    select(-c(artist_name, cluster))
  
}