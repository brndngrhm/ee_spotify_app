
# Call spotify API
get_artist_data <- function(artist_1, artist_2){
  
  artists <- c({{artist_1}}, {{artist_2}})
  
  purrr::map_dfr(artists, ~spotifyr::get_artist_audio_features(., 
                                                               include_groups = c("album", "single", "compilation", "appears_on"),
                                                               dedupe_albums = TRUE,
                                                               authorization = get_spotify_access_token())) %>%
    select(artist_name, album_name, track_name, danceability, energy, key, loudness, speechiness, acousticness,
           instrumentalness, liveness, valence, tempo, duration_ms) %>%
    # remove other EE from results
    filter(album_name != "Beerg Scary Monsters") 
}

# test <- get_artist_data("Everyone Everywhere", "311")

############################

# format data: returns dataframe with features avgs per artist - works with result of get_artist_data()
get_feature_avgs <- function(df, ...){
  
  df %>%
    mutate_if(is.numeric, scale) %>%
    group_by(...) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = T))) %>%
    pivot_longer(cols = 2:12)
}

# get_feature_avgs(test, artist_name)

############################

# generate dumbbell plot for feature comparisons - works with result of get_feature_avgs()
get_dumbbell_plot <- function(data){
  
  # data <- get_feature_avgs(test)
  
  #rename to work with plotly
  data_wide <- data %>%
    pivot_wider(names_from = artist_name, values_from = value) 
  
  #store names
  artist_1_name <- names(data_wide[2])
  artist_2_name <- names(data_wide[3])
  
  dumbbell_df <- data_wide %>%
    rename(artist_1_value = 2,
           artist_2_value = 3) %>%
    mutate(artist_1_value = round(artist_1_value, 2),
           artist_2_value = round(artist_2_value, 2),
           message = ifelse(artist_1_value > artist_2_value,
                            glue::glue("{name} is on average higher for {artist_1_name} than {artist_2_name}: {artist_1_value} vs {artist_2_value}"),
                            glue::glue("{name} is on average higher for {artist_2_name} than {artist_1_name}: {artist_2_value} vs {artist_1_value}")
                            ))
  
  hchart(dumbbell_df, "columnrange", hcaes(x = name, low = artist_1_value, high = artist_2_value, message = message), color = "lightgray") %>%
    hc_add_series(dumbbell_df, "point", name = artist_1_name, hcaes(x = name, y = round(artist_1_value, 2)), color = "#4db6ac", showInLegend = TRUE) %>%
    hc_add_series(dumbbell_df, "point", name = artist_2_name, hcaes(x = name, y = round(artist_2_value, 2)), color = "#ffcc80", showInLegend = TRUE) %>%
    hc_chart(inverted = TRUE) %>%
    hc_plotOptions(columnrange = list(pointWidth = 1)) %>%
    hc_tooltip(headerFormat = "{point.message}",
               pointFormat = "Normalized Value: {point.y} <br> {point.message}") %>%
    hc_xAxis(title = list(text = "Normalized Feature Value")) %>%
    # hc_title(text = "Average Feature Scores (Normalized)", align = "left") %>%
    # hc_subtitle(text = "shaded by artist", align = "left") %>%
    hc_chart(zoomType = "x")
  
}

# get_dumbbell_plot(get_feature_avgs(test, artist_name))

############################

# works with result of get_artist_data()
get_boxplot <- function(data){

boxplot_data <- data %>%
  mutate_if(is.numeric, scale) %>%
  pivot_longer(cols = 4:14) %>%
  rename(value = 5)

hcboxplot(x = round(boxplot_data$value,2), var = boxplot_data$name, boxplot_data$artist_name, outliers = TRUE) %>%
  hc_chart(type = "column") %>%
  hc_colors(c("#4db6ac", "#ffcc80")) %>%
  # hc_plotOptions(series=list(colorByPoint=TRUE)) %>%
  hc_chart(inverted = TRUE) %>%
  hc_yAxis(title = list(text = "Normalized Feature Value"),
           min = -5, max = 5) %>%
  # hc_title(text = "Distribution of Feature Scores (Normalized)", align = "left") %>%
  # hc_subtitle(text = "shaded by artist", align = "left") %>%
  hc_chart(zoomType = "x")
}

# get_boxplot(test)

############################

#distribution - works with result of get_artist_data()
get_ridgeplot <- function(df){
  
  #reshape data
  df <- df %>% 
    mutate_if(is.numeric, scale) %>%
    as_tibble() %>% 
    pivot_longer(cols = 4:14) %>%
    rename("value" = 5)
  
  #assign colors
  other_artist <- df %>% 
    select(artist_name) %>% 
    filter(artist_name != "Everyone Everywhere") %>% 
    distinct() %>% 
    pull(artist_name)
  
  group_colors <- tibble(`Everyone Everywhere` = "#4db6ac", 
                         artist_2 = "#ffcc80")
  
  names(group_colors)[2] <- other_artist
  
  #plot
  ggplot(df, aes(x = value, y = name, fill = artist_name)) + 
    scale_fill_manual(values = group_colors) + 
    theme_grey() + 
    geom_density_ridges(alpha = .75) + 
    labs(x = "Normalized Feature Score",
         y = "",
         fill = ''
         # title = "Distribution of Song Features"
         ) +
    theme(legend.position = 'bottom',
          legend.background = element_rect(fill="#f5f5f5"),
          # legend.position = c(0.87, 0.90),
          text = element_text(size = 16),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "#f5f5f5")
    )
}

# get_ridgeplot(test)

############################

# barplot of top 10 songs per artist, given feature input - works with result of get_artist_data()
get_ranked_features <- function(data, feature){
  
  feature <- enquo(feature)
  
  df_small <- data %>%
    select(artist_name, album_name, track_name, !!feature)
  
  names(df_small)[4] <- 'feature_value'
  
  df_bar <- df_small %>% 
    group_by(artist_name) %>%
    top_n(10, feature_value) %>%
    ungroup() %>%
    arrange(desc(feature_value)) %>%
    mutate(
      track_name = as.factor(track_name),
      track_name = forcats::fct_reorder(track_name, feature_value, .desc = FALSE),
      feature_choice = !!feature,
      artist_color = case_when(
        artist_name == "Everyone Everywhere" ~ colorize(artist_name, "#4db6ac"),
        TRUE ~ colorize(artist_name, "#ffcc80"))
    ) %>% 
    as.data.frame()
  
  other_artist <- df_bar %>% filter(artist_name != "Everyone Everywhere") %>% select(artist_name) %>% distinct() %>% pull(artist_name)
  
  hchart(df_bar, type = "column", hcaes(x = track_name, y = feature_value, alb = album_name, feature_choice = feature_choice)) %>%
    hc_chart(inverted = TRUE) %>%
    # hc_colors(c("#4db6ac", "#ffcc80")) %>%
    hc_add_series(df_bar, "line", name = "Everyone Everywhere", hcaes(x = 1, y = 1), visible = TRUE, color = "#4db6ac", showInLegend = TRUE) %>%
    hc_add_series(df_bar, "line", name = other_artist, hcaes(x = 1, y = 1), visible = TRUE, color = "#ffcc80", showInLegend = TRUE) %>%
    # hc_tooltip(pointFormat = "<b>{point.alb}</b> <br> Track: {point.x} <br> {point.feature_choice}: {point.y}") %>%
    hc_yAxis(title = list(text = "Normalized Feature Score")) %>%
    hc_xAxis(title = list(text = " ")) %>%
    hc_chart(zoomType = "x")
  
  # df_bar
  
}

# get_ranked_features(test, "energy") 

############################
# k means cluster recommendation - works with output of get_artist_data()
get_reccomendation <- function(df, song){
  
  # get non-EE artist songs, and cluster
  other <-  df %>% filter(artist_name != "Everyone Everywhere")
  cluster_df <- other %>% select(-c(album_name, artist_name, track_name)) %>% as.data.frame()
  cluster <- ClusterR::KMeans_rcpp(cluster_df, clusters = 5, num_init = 4, max_iters = 100, initializer = 'kmeans++')
  
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
    select(album_name, track_name) %>%
    # select(-c(artist_name, cluster)) %>%
    mutate_if(is.numeric, ~round(.x, 2)) %>%
    reactable(.,  highlight = TRUE)

  # plot_2d(other, cluster$clusters, cluster$centroids)
  # 
  # ggplot(other) %>%
  #          ggplot(aes(x=imr, y=tfr, group=cluster, color=cluster, cex=3)) + geom_point(
  
}

# get_reccomendation(test, "Tiny Town")
