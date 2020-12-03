library(shinycssloaders)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(highcharter)
library(shinythemes)
library(stringr)
library(emo)
library(shinymaterial)
library(reactable)
library(ClusterR)

# Define UI for application that draws a histogram
ui <- material_page(title = 'Everyone Everywhere AI Popularity Analyzer',
                    material_row(
                      material_column(
                          width = 2,
                          material_card(
                              # color = 'blue-grey darken-2',
                              font_color = 'white',
                              title = 'Everyone Everywhere vs...',
                              depth = 0,
                              material_text_box('artist_search', label = 'type an artist or band name', value = ''),
                              conditionalPanel("input.artist_search != ''", 
                                               material_dropdown('select_artist', 'Refine your search', '')),
                              uiOutput('select_artist_ui'),
                              # tags$br(),
                              # conditionalPanel("input.select_artist != ''", 
                              #                  material_dropdown('feature_select',
                              #                   label = 'Explore Some Song Features',
                              #                   choices = c("danceability", "energy", "key", "loudness", "speechiness", "acousticness",
                              #                               "instrumentalness", "liveness", "valence", "tempo", "duration_ms", "key_name", "mode_name", "key_mode"),
                              #                   selected = "energy")
                              #                  ),
                              tags$br(),
                              conditionalPanel("input.select_artist != ''",
                                               material_dropdown('song_select',
                                                                 label = 'Find Songs Similar to...',
                                                                 choices = c("$1,000,000,000", "Big Hat", "Blown up Grown Up", 
                                                                             "Fervor & Indifference in the Bicameral Brian",
                                                                             "Fld Ovr", "From the Beginning to the Tail", 
                                                                             "I Feel Exhausted", "I Feel Fine", "Music Work Paper Work",
                                                                             "No Furniture", "Obama House, Fukui Prefecture",
                                                                             "Queen Mary II", "Raw Bar Obx 2002", "The Future", 
                                                                             "Tiny Boat", "Tiny Planet", "Tiny Town", "Turn & Go & Turn", 
                                                                             "Turn and Go and Turn", "Wild Life")
                                               )
                              ),
                              tags$br(),
                              conditionalPanel("input.select_artist != ''",
                                               shiny::actionButton('get_reccomendation', 'Find Similar Songs')
                              ),
                              tags$br(),
                              conditionalPanel("input.select_artist != ''",
                                               shiny::actionButton('about', 'About',
                                                                   style="color: #fff; background-color: #ee6e73; border-color: #ee6e73")
                              )
                              
                      )),
                      material_column(
                          width = 7,
                          offset = 1,
                          uiOutput('popularity_card') %>% shinycssloaders::withSpinner(color = "#ee6e73")
                      ),
                      tags$br(),
                      material_column(
                          width = 7,
                          offset = 1,
                          uiOutput("dumbbell_title"),
                          highchartOutput('dumbbell_plot') #%>% shinycssloaders::withSpinner()
                      ),
                      tags$br(),
                      material_column(
                          width = 7,
                          offset = 1,
                          uiOutput("ridgeplot_title"),
                          plotOutput('ridgeplot') #%>% shinycssloaders::withSpinner()
                      ),
                      # tags$br(),
                      # material_column(
                      #     width = 10,
                      #     highchartOutput('feature_barplot') #%>% shinycssloaders::withSpinner()
                      # ),
                      tags$br(),
                      tags$br(),
                      material_column(
                          width = 7,
                          offset = 3,
                          uiOutput("reccomendation_title"),
                          reactableOutput('reccomendation_table') %>% shinycssloaders::withSpinner()
                      ),
                      tags$br()

                  )
)

server <- function(input, output, session){
    
    # sidebar stuff sourced from https://github.com/charlie86/sentify/blob/14dab5b47aba147b2c6b8bd3192b35ec071c63d7/server.R
    
    ############################################################################
    #load functions, save access token
    source("util.R")
    
    spotify_access_token <- reactive({
        get_spotify_access_token()
    })
    
    ############################################################################
    # reactive functions
    
    #searches spotofy for artists matching user input
    artist_info <- reactive({
        req(input$artist_search != '')
        search_spotify(input$artist_search, 'artist', authorization = spotify_access_token())
    })
    
    # stores the selected artist once the selection is made
    selected_artist <- reactive({
        req(nrow(artist_info()) > 0)
        artist_info() %>% 
            filter(name == input$select_artist) %>% 
            filter(popularity == max(popularity))
    })
    
    artist_data <- reactive(get_artist_data("Everyone Everywhere", selected_artist()$name))
    feature_avgs <- reactive(get_feature_avgs(artist_data(), artist_name))
    
    ############################################################################
    # Observers - used to update things in real time
    
    # updates the artist drop down list as user types
    observeEvent(input$artist_search, {
        choices <- artist_info()$name
        names(choices) <- choices
        update_material_dropdown(session, 'select_artist', value = artist_info()$name[1], choices = choices)
    })
    
    # updates album art once user makes artist selection
    observeEvent(input$select_artist, {
        req(nrow(artist_info()) > 0)
        
        img <- artist_info() %>% 
            filter(name == input$select_artist) %>%
            select(images) %>%
            unnest(cols = c(images)) %>%
            head(1) %>%
            pull(url)
        
        artist_img <- ifelse(!is.na(img),
                             img,
                             'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        # artist_img <- ifelse(!is.na(artist_info()$images[[1]]$url[1][artist_info()$name == input$select_artist]),
        #                      artist_info()$images[[1]]$url[1][artist_info()$name == input$select_artist],
        #                      'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        output$artist_img <- renderText({
            HTML(str_glue('<img src={artist_img} height="200">'))
        })
        
    })
    
    # About modal
    observeEvent(input$about, {
        show_alert(
            title = " Everyone Everywhere AI Popularity Analyzer",
            type = "info",
            text = tags$span(
                "The app uses AI to tell you if Everyone Everywhere is more popular than any other band on Spotify. To learn more about the features presented, click",
                tags$a(href = "https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/", "here."),
                tags$br(),
            ),
        html = FALSE,
        width = "60%")
    })
    
    ############################################################################
    # Event Reactives - used to create output after some action is taken
    popularity_card <- eventReactive(input$compare_bands,{
        material_card(
            title = HTML(paste("<span style='font-weight:bold'> and the winner is... Everyone Everywhere!!</span>",
                               emo::ji("partying_face"),
                               emo::ji("hundred_points"),
                               emo::ji("dancing"))),
            HTML("<span style='font-size:14px'>Our proprietary AI predicts <span style='font-weight:bold'>Everyone Everywhere</span> are more popular than<span style='font-weight:bold'>", selected_artist()$name, "</span>!</span>")
            
        )
    })
    
    dumbbell_title <- eventReactive(input$compare_bands, {
        shiny::tags$h4("Comparison of Song Features")
    })
    dumbbell_plot <- eventReactive(input$compare_bands, {get_dumbbell_plot(feature_avgs())})
    
    ridgeplot_title <- eventReactive(input$compare_bands, {
        shiny::tags$h4("Distribution of Song Features")
    })
    ridgeplot <- eventReactive(input$compare_bands, {get_ridgeplot(artist_data())})
    
    # feature_barplot <- eventReactive(input$compare_bands | input$feature_select, ignoreInit = TRUE, {get_ranked_features(artist_data(), input$feature_select)})
    
    reccomendation_table <- eventReactive(input$get_reccomendation, {get_reccomendation(artist_data(), input$song_select)})
    reccomendation_title <- eventReactive(input$get_reccomendation, {
            shiny::tags$h4(glue::glue("5 {selected_artist()$name} songs similar to {input$song_select}"))
    })
    
    ############################################################################
    # outputs
    output$select_artist_ui <- renderUI({
        req(nrow(artist_info()) > 0)
        tagList(
            htmlOutput('artist_img'),
            tags$br(),
            shiny::actionButton('compare_bands', 'Analyze')
        )
    })
    output$popularity_card <- renderUI(popularity_card())
    output$dumbbell_title <- renderUI(dumbbell_title())
    output$dumbbell_plot <- renderHighchart(dumbbell_plot())
    output$ridgeplot_title <- renderUI(ridgeplot_title())
    output$ridgeplot <- renderPlot(ridgeplot())
    # output$feature_barplot <- renderHighchart(feature_barplot())
    output$reccomendation_title <- renderUI(reccomendation_title())
    output$reccomendation_table <- renderReactable(reccomendation_table())

}

# Run the application
shinyApp(ui, server)