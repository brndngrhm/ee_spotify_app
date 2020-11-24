library(shinycssloaders)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(FITfileR)
library(imputeTS)
library(slider)
library(changepoint)
library(highcharter)
library(leaflet)
library(shinythemes)
library(stringr)
library(emo)
library(recommenderlab)

# Define UI for application that draws a histogram
ui <- material_page(title = 'Everyone Everywhere Music Popularity Tool',
                  # nav_bar_color = 'blue-grey darken-2',
                  # font_color = 'blue-grey lighten-5',
                  # background_color = 'blue-grey darken-2',
                  material_row(
                      material_column(
                          width = 2,
                          material_card(
                              # color = 'blue-grey darken-2',
                              font_color = 'white',
                              title = 'Everyone Everywhere vs...',
                              depth = 0,
                              material_text_box('artist_search', label = 'type an artist or band name', value = "The Beatles"),
                              conditionalPanel("input.artist_search != ''", 
                                               material_dropdown('select_artist', 'Refine your search', '')),
                              uiOutput('select_artist_ui')
                          )
                      ),
                      material_column(
                          width = 8,
                          uiOutput('popularity_card')
                      ),
                      tags$br(),
                      material_column(
                          width = 8,
                              material_dropdown('feature_select',
                                                label = 'Explore Some Song Features',
                                                choices = c("danceability", "energy", "key", "loudness", "speechiness", "acousticness",
                                                            "instrumentalness", "liveness", "valence", "tempo", "duration_ms", "key_name", "mode_name", "key_mode"),
                                                selected = "energy")
                      ),
                      tags$br(),
                      material_column(
                          width = 8,
                          highchartOutput('ranked_features')
                      ),
                      tags$br(),
                      material_column(
                          offset = 2,
                          width = 8,
                          material_dropdown('song_select',
                                            label = 'Find Songs Similar to...',
                                            choices = c("$1,000,000,000", "Big Hat", "Big Hat", "Blown up Grown Up", 
                                            "Fervor & Indifference in the Bicameral Brian", "Fervor and Indifference in the Bica", 
                                            "Fld Ovr", "From the Beginning to the Tail", "I Feel Exhausted", 
                                            "I Feel Exhausted", "I Feel Fine", "Music Work Paper Work", "No Furniture", 
                                            "No Furniture", "Obama House, Fukui Prefecture", "Queen Mary II", 
                                            "Queen Mary II", "Raw Bar Obx 2002", "The Future", "The Future", 
                                            "Tiny Boat", "Tiny Planet", "Tiny Town", "Turn & Go & Turn", 
                                            "Turn and Go and Turn", "Wild Life", "Wild Life")
                                            )
                      ),
                      tags$br(),
                      material_column(
                          offset = 2,
                          width = 8,
                          formattable::formattableOutput("cluster_reccomendation")
                      )
                  )
)

server <- function(input, output, session){
    
    source(here::here("app", "util.R"))
    
    spotify_access_token <- reactive({
        get_spotify_access_token()
    })
    
    ######################################
    
    # sidebar stuff: sourced from https://github.com/charlie86/sentify/blob/14dab5b47aba147b2c6b8bd3192b35ec071c63d7/server.R
    
    artist_info <- reactive({
        req(input$artist_search != '')
        search_spotify(input$artist_search, 'artist', authorization = spotify_access_token())
    })
    
    observeEvent(input$artist_search, {
        choices <- artist_info()$name
        names(choices) <- choices
        update_material_dropdown(session, 'select_artist', value = artist_info()$name[1], choices = choices)
    })
    
    output$select_artist_ui <- renderUI({
        req(nrow(artist_info()) > 0)
        tagList(
            htmlOutput('artist_img'),
            shiny::actionButton('compare_bands', 'Compare Bands')
        )
    })
    
    observeEvent(input$select_artist, {
        
        req(nrow(artist_info()) > 0)
        
        artist_img <- ifelse(!is.na(artist_info()$images[[1]]$url[1][artist_info()$name == input$select_artist]),
                             artist_info()$images[[1]]$url[1][artist_info()$name == input$select_artist],
                             'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        output$artist_img <- renderText({
            HTML(str_glue('<img src={artist_img} height="200">'))
        })
        
    })
    
    ######################################
    #card data
    observeEvent(input$compare_bands, {

        Sys.sleep(3)
        
        output$popularity_card <- renderUI({
            material_card(
                title = HTML(paste("<span style='font-weight:bold'> and the winner is... Everyone Everywhere!!</span>",
                                   emo::ji("partying_face"),
                                   emo::ji("hundred_points"),
                                   emo::ji("dancing"))),
                HTML("<span style='font-size:14px'>Our proprietary multi-node single perceptron sideways propogation AI says <span style='font-weight:bold'>Everyone Everywhere</span> are the more popular than<span style='font-weight:bold'>", artist_info()$name[1], "</span>!</span>")
                
            )
        })
    })
    
    #####################################
    
    features <- eventReactive(input$compare_bands,{
        
        ee <- get_artist_audio_features(artist = "everyone everywhere") %>%
            select(artist_name, album_name, track_name, danceability, energy, key, loudness, speechiness, acousticness,
                   instrumentalness, liveness, valence, tempo, duration_ms)
        
       input_band <- get_artist_audio_features("the beatles") %>%
            select(artist_name, album_name, track_name, danceability, energy, key, loudness, speechiness, acousticness,
                   instrumentalness, liveness, valence, tempo, duration_ms)
        
        bind_rows(ee, input_band)
        
    })
    
    feature_data <- reactive(
        feature_analysis(features(), input$feature_select)
    )
    
    output$ranked_features <- renderHighchart({

        plot_title <- paste("Top 10 Tracks For Each Artist Ranked by", str_to_title(input$feature_select))
        
        hchart(feature_data(), type = "bar",  hcaes(x = track_name, y = param, color = artist_color, alb = album_name, param_choice = param_choice)) %>%
            hc_tooltip(headerFormat = '', pointFormat = "<b>{point.alb}</b> <br> Track: {point.track_name} <br> {point.param_choice}: {point.y}") %>%
            hc_yAxis(title = list(text = input$feature_select)) %>%
            hc_xAxis(title = list(text = " ")) %>%
            hc_title(text = plot_title, align = "left") %>%
            hc_subtitle(text = "shaded by artist", align = "left") %>%
            hc_chart(zoomType = "x")
        
    })
    
    observeEvent(input$song_select, {
        reccomendation_data <- reccomendation(features(), input$song_select)
        
    output$cluster_reccomendation <- formattable::renderFormattable({
        formattable::formattable(reccomendation_data)
    })
    })

}

# Run the application
shinyApp(ui, server)