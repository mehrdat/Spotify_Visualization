########## Final ##############

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(randomForest)
library(plotly)
library(lubridate)
library(corrplot)
library(heatmaply)
library(DT)
library(readr) 
library(tidyr)
library(Metrics)
library(caret)
library(leaflet)
library(maps)
library(tools)
library(ggbiplot)
library(countrycode)
library(factoextra)
library(scales)
library(shinyjs)
library(googlesheets4)

country_codes_df <- data.frame(
  code = c(
    "US", "CA", "GB", "DE", "FR", "AU", "JP", "BR", "MX", "IN",
    "RU", "CN", "ZA", "NG", "EG", "ID", "PK", "BD", "TR", "IR",
    "ES", "IT", "KR", "SA", "AR", "CO", "PL", "UA", "MA", "KE",
    "GH", "CI", "SN", "TZ", "UG", "ZM", "ZW", "AO", "CM", "ET",
    "SD", "LY", "TN", "DZ", "IQ", "JO", "LB", "OM", "KW", "QA",
    "AE", "YE", "SY", "PS", "IL", "BE", "NL", "CH", "SE", "NO",
    "DK", "FI", "PT", "GR", "CZ", "HU", "RO", "BG", "HR", "SK",
    "SI", "LT", "LV", "EE", "IE", "AT", "BA", "MK", "AL", "RS",
    "XK", "ME", "IS", "AZ", "GE", "AM", "BY", "MD", "PL", "UA",
    "KZ", "UZ", "KG", "TJ", "TM", "MN", "KP", "HK", "TW", "VN",
    "TH", "MY", "SG", "PH", "ID", "NZ", "CL", "PE", "VE", "CU",
    "DO", "GT", "HN", "SV", "NI", "CR", "PA", "JM", "TT", "GY",
    "EC", "BO", "PY", "UY", "SR", "BB"
  ),
  country_name =c(
    "United States", "Canada", "United Kingdom", "Germany", "France", "Australia", "Japan", "Brazil", "Mexico", "India",
    "Russia", "China", "South Africa", "Nigeria", "Egypt", "Indonesia", "Pakistan", "Bangladesh", "Turkey", "Iran",
    "Spain", "Italy", "South Korea", "Saudi Arabia", "Argentina", "Colombia", "Poland", "Ukraine", "Morocco", "Kenya",
    "Ghana", "Cote d'Ivoire", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe", "Angola", "Cameroon", "Ethiopia",
    "Sudan", "Libya", "Tunisia", "Algeria", "Iraq", "Jordan", "Lebanon", "Oman", "Kuwait", "Qatar",
    "United Arab Emirates", "Yemen", "Syria", "Palestine", "Israel", "Belgium", "Netherlands", "Switzerland", "Sweden", "Norway",
    "Denmark", "Finland", "Portugal", "Greece", "Czech Republic", "Hungary", "Romania", "Bulgaria", "Croatia", "Slovakia",
    "Slovenia", "Lithuania", "Latvia", "Estonia", "Ireland", "Austria", "Bosnia and Herzegovina", "North Macedonia", "Albania", "Serbia",
    "Kosovo", "Montenegro", "Iceland", "Azerbaijan", "Georgia", "Armenia", "Belarus", "Moldova", "Poland", "Ukraine",
    "Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Mongolia", "North Korea", "Hong Kong", "Taiwan", "Vietnam",
    "Thailand", "Malaysia", "Singapore", "Philippines", "Indonesia", "New Zealand", "Chile", "Peru", "Venezuela", "Cuba",
    "Dominican Republic", "Guatemala", "Honduras", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Jamaica", "Trinidad and Tobago", "Guyana",
    "Ecuador", "Bolivia", "Paraguay", "Uruguay", "Suriname", "Barbados"
  )
)%>% distinct(code, .keep_all = TRUE) 

spotify_data_raw <- read_csv("spotify_01.csv") %>%
  slice_sample(prop = .1)%>%
  na.omit() %>%
  mutate(is_explicit = as.factor(is_explicit),
         country = as.factor(country),
         release_year=year(as.Date(album_release_date)))
spotify_data<-spotify_data_raw%>%
  select(-c(tempo,snapshot_date,weekly_movement,daily_movement,daily_rank,spotify_id,name, time_signature))

str(spotify_data)
summary(spotify_data$key)
ind_train<-sample(1:nrow(spotify_data),size=0.8*nrow(spotify_data))
train_data<- spotify_data[ind_train,]
test_data<-spotify_data[-ind_train,]

rf_model <- randomForest(popularity ~ speechiness+ duration_ms+ danceability+ energy+ liveness+ loudness+valence+ key+ acousticness + instrumentalness+mode,
                         data = train_data, 
                         ntree = 30, 
                         min.node.size = 5,
                         mtry = 3,
                         num.threads = parallel::detectCores() - 1,
                         verbose = TRUE  )
print(rf_model)
varImpPlot(rf_model)
varImp(rf_model)

feedback_file <- "nps_feedback.csv"

feedback_ss <- "https://docs.google.com/spreadsheets/d/1TroPoCHRu3LTVHemxOgV7qo5VOzfvYPnSu_33M6jpl0/edit?gid=0#gid=0"


############# UI Definition ###############
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Spotify Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Main Dashboard", tabName = "main", icon = icon("dashboard")),
      menuItem("Interactive Plot", tabName = "interactive", icon = icon("chart-bar")),
      menuItem("Static Plot", tabName = "static", icon = icon("chart-pie")),
      menuItem("Model Plots", tabName = "model", icon = icon("cogs")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Feedback / NPS", tabName = "nps", icon = icon("comment-dots")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    #tags$head(tags$style(HTML(".info-box-content { white-space: normal; }"))),
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                valueBoxOutput("totalSongsBox", width = 4),
                valueBoxOutput("avgPopularityBox", width = 4),
                valueBoxOutput("topArtistBox", width = 4)
              ),
              fluidRow(
                box(
                  title = "Top 7 Artists", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("topArtistsPlot")
                ),
                box(
                  title = "Countries by Popular Songs", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotlyOutput("popularityByCountries")
                )
              )
      ),
      tabItem(tabName = "interactive",
              fluidRow(
                box(
                  title = "Variables Changes Over Time", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  
                  selectInput("variableSelect", "Choose Metric:",
                              choices = c("popularity", "danceability", "energy", 
                                          "valence", "acousticness", "liveness", "loudness", "speechiness"),
                              selected = "popularity"),
                  dateRangeInput("dateRange", "Date Range:",
                                 start = min(spotify_data$album_release_date),
                                 end = max(spotify_data$album_release_date),
                                 min = min(spotify_data$album_release_date),
                                 max = max(spotify_data$album_release_date),
                                 format = "yyyy-mm-dd",
                                 startview = "month",
                                 separator = " - "),
                  plotlyOutput("overTimePlot")
                )
              )
      ),
      tabItem(tabName = "static",
              fluidRow(
                box(
                  title = "Correlation Between Variables", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("corrPlot")
                ),
                box(
                  title = "Principal Component Analysis: Principals", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("pcaPlot")
                )
                
                ),
                fluidRow(
                  
                  box(
                    title = "Representation of Variable on Principal Components: Cosine^2", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = 6,
                    plotOutput("fvizPlot")
                  ),
                  box(
                    title = "Correlation of PCs and Variables", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = 6,
                    plotOutput("pcaCorr")
                  )
                

              )
      ),
####################### Data Table Tab ########################
      
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Table Filters",
                  selectInput("countryFilter", "Filter by Country:",
                              choices = c("All", unique(country_codes_df$country_name))),
                  sliderInput("popularityFilter", "Popularity Range:",
                              min = 0, max = 100, value = c(0, 100)), width = 12)
              ),
              fluidRow(
                box(
                  DTOutput("dataTable"),
                  width = 12
                )
              )
      ),
      
####################### Model Tab ########################
      tabItem(tabName = "model",
              fluidRow(
                box(
                  title = "Song Features",
                  sliderInput("predDance", "Danceability:", 
                              min = 0.15, max =0.99, value = 0.70, step = 0.01),
                  sliderInput("predEnergy", "Energy:", 
                              min = 0.01, max = 1.0, value = 0.65, step = 0.01),
                  sliderInput("predLoudness", "Loudness (normalized):", 
                              min = -31.0, max = 3.24, value = -6.5, step = 0.1),
                  sliderInput("predValence", "Valence:", 
                              min = 0.01, max = 1, value = 0.53, step = 0.01),

                  sliderInput("speechiness", "Speechiness:",
                              min = 0.02, max = .93, value = 0.09, step = 0.01),
                  sliderInput("duration_ms", "duration_ms:",
                              min = 36826, max = 821631 , value = 193301, step = 10),
                  sliderInput("liveness", "Liveness:",
                              min = 0.01, max = .98, value = 0.175, step = 0.01),
                  
                  sliderInput("key", "Key:",
                              min = 0, max = 11, value = 5, step = 1),
                  sliderInput("acousticness", "acousticness:",
                              min = 0.01, max = .98, value = 0.175, step = 0.01),
                  sliderInput("instrumentalness", "instrumentalness:",
                              min = 0.01, max = .98, value = 0.175, step = 0.01),
                  sliderInput("mode", "Mode:",
                              min = 0, max = 1, value = 1, step =1),

                  width = 4,
                
                ),
                box(
                  title = "Predicted Popularity",
                  verbatimTextOutput("predictionOutput"),
                  plotOutput("predictionPlot"),
                  width = 8,
                  height = 20
                )
              ),
                tagList(
                  fluidRow(
                    box(
                      title = " 80% Used to Train Data ", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      plotOutput("splitData")
                    ),
                    box(
                      title = "Feature Importance for Predicting Popularity", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      plotOutput("featureImportancePlot")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Predicted vs. Actual Popularity", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 12,
                      plotOutput("predActualPlot")
                    )
                  )
                )
            
      ),
################## NPS ###########
tabItem(tabName="nps",
        fluidRow(
          box(
            title="FeedBack",
            p(),
            br(),
            sliderInput("nps_score",
                        label = "How was your experience?",
                        min=1,max =10 ,value =7 ,step=1,with="100%"),
            tags$div(),
            br(),
            textAreaInput(),
            
            
          )
        )
          ),


      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Dashboard", status = "info", solidHeader = TRUE, width = 12,
                  h4("Dataset Description"),
                  p("This dashboard visualizes the Spotify dataset. ",
                    strong("Please specify your data source and details here."),
                    " Key columns assumed include speechiness, duration_ms, danceability, energy, liveness, loudness, valence",
                    strong("Genre information was not available in the provided dataset. And for simplicity and demonstration purposes, the dataset was sampled to 10%.")
                  ),
                  tags$hr(),
                  h4("Dashboard Purpose"),
                  p("The purpose of this dashboard is to provide veriety o plots for exploring trends and patterns within the Spotify dataset. It allows users to get a glance, explore correlations between audio features, view trends over time, and understand relationships between different song attributes over time."),
                  tags$hr(),
                  h4("Methodology"),
                  p("Data was loaded and preprocessed using R and the Tidyverse. Preprocessing included parsing the 'album_release_date' to extract the release year, handling potential missing values, type conversions,. Visualizations were created primarily using ggplot2 and plotly for interactivity specifically in map."),
                  p(strong("Random Forest Model"), " is useed in the 'Model Plots' section to predict the popularity of a song based on its audio features. The model was trained on 80% of the data and tested on the remaining 20%. The model's performance is evaluated using RMSE and R². The model's feature importance is also visualized."),
                  tags$hr(),
                  h4("Contact / Source Code"),
                  p("For questions or feedback, please contact: d00242480@student.dkit.ie"),
                  p("The source code for this Shiny application can be found on GitHub:",
                    tags$a(href = "https://github.com/mehrdat/visualization_final_project", target = "_blank", "[https://github.com/mehrdat/visualization_final_project]"))
                )
              )
      )
    )
  )
)

#####################  Server function #################

server <- function(input, output, session) {
  
  pca_df <- spotify_data %>%
    select(danceability, energy, loudness, valence, popularity, speechiness, duration_ms, liveness) %>%
    na.omit()
  
  #pca_df <- scale(pca_df)
  pca_df <- prcomp(pca_df, scale = TRUE)
  
  var=get_pca_var(pca_df)
  var$coord
  var$contrib
  varcos<-var$cos2
  var$cor
  

  output$totalSongsBox <- renderValueBox({
    total_songs <- nrow(spotify_data)
    valueBox(
      "Songs",format(total_songs, big.mark = ","), icon = icon("music"),
      color = "purple"
    )
  })
  
  output$avgPopularityBox <- renderValueBox({

    pop_album<-spotify_data%>%
      group_by(album_name,artists)%>%
      summarise(avg_popularity=mean(popularity,na.rm=TRUE),total_songs = n(),)%>%
      arrange(desc(avg_popularity))%>%
      head(1)

    valueBox( "Album",
              paste0(toTitleCase(pop_album$album_name), " is the most popular album by ",  toTitleCase(pop_album$artists)), icon = icon("star"),
      color = "yellow"
    )
  })

  output$topArtistBox <- renderValueBox({
    
      top_artist_data <- spotify_data %>%
        count(artists, sort = TRUE) %>%
        slice_max(n, n = 1, with_ties = FALSE)
    
    valueBox(
      "Top Artist", if (nchar(top_artist_data$artists) > 30) paste0(substr(top_artist_data$artists, 1, 27), " by ",top_artist_data$n," Songs" ) else paste0(top_artist_data$artists, " by ",top_artist_data$n," Songs" ),
       icon = icon("user"),
      color ="green"
    )
  })

  output$topArtistsPlot <- renderPlot({
    
    top_artists <- spotify_data %>%
      group_by(artists) %>%
      summarise(count = n(),avg_popularity = mean(popularity,na.rm = TRUE),
                ind= n()/mean(popularity,na.rm = TRUE)
      ) %>%
      arrange(desc(avg_popularity)) %>%
      top_n(7, count)
    
    ggplot(top_artists, aes(x = ind, y = reorder(artists, ind), fill = ind)) +
      geom_bar(stat = "identity", show.legend = TRUE) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Popularity") +
      labs(
        title = "Song Count / Avg Popularity",
        x = "Count",
        y = "Artist"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(angle = 45, hjust = 0.5),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.5, "cm")
      )
  })
        
  output$popularityByCountries <- renderPlotly({
    
    
    country_codes_df <- country_codes_df %>%
      mutate(iso_a3 = countrycode(code, origin = 'iso2c', destination = 'iso3c'))
    
    spotify_data <- spotify_data_raw %>%
      select(popularity, country, album_release_date)%>%
      left_join(country_codes_df, by = c("country" = "code"))
    
    country_popularity <- spotify_data %>%
      filter(!is.na(iso_a3)) %>% 
      group_by(iso_a3) %>%       
      summarise(avg_popularity = mean(popularity, na.rm = TRUE),
                n_songs = n(), 
                country_display_name = first(country_name)
      ) %>%
      ungroup() # could use .groups = 'drop'
    
    l <- list(color = toRGB("white"), width = 0.5)
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'orthographic'),
      resolution = '100',
      showcountries = TRUE,
      countrycolor = '#d1d1d1',
      showocean = TRUE,
      oceancolor = '#c9d2e0',
      showlakes = TRUE,
      lakecolor = '#99c0db',
      showrivers = TRUE,
      rivercolor = '#99c0db')
    
    
    names(spotify_data)
    p <- plot_geo(country_popularity) %>%
      add_trace(z = ~avg_popularity, color = ~avg_popularity, colors = 'Blues',
                text = ~country_display_name, locations = ~iso_a3, marker = list(line = l)) %>%
      colorbar(title = 'popularity') %>%
      layout(title = '', geo = g)
    p
  })


################### Interactive Plot ###################
  
  output$overTimePlot <- renderPlotly({
 
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    
    filtered <- spotify_data %>%
      mutate(album_release_date =  floor_date(as.Date(album_release_date),unit="year") ) %>%
      filter(
        album_release_date >= start_date,
        album_release_date <= end_date
      ) %>%
      group_by(album_release_date) %>%
      summarise(
        avg_value = mean(get(input$variableSelect), na.rm = TRUE),
        .groups = 'drop'
      )
    
    filtered <- filtered %>%
      mutate(
        year = floor_date(as.Date(album_release_date), unit = "year") 
      )

      ggplot(filtered, aes(x = year, y = avg_value)) +
      geom_line(color = "#4E79A7", size = 1) + 
      geom_point(color = "#4E79A7", size = 2, stroke = 1, shape = 21, fill = "#4E79A7") +
      labs(
        title = paste("Trend of", toTitleCase(input$variableSelect), "Over Time"),
        x = "Date",
        y = paste("Average", input$variableSelect)
      ) +
      theme_minimal() + 
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),

      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") 

    #print(full_plot)
  })
########################## Correlation Plot ###########################
  
  output$corrPlot <- renderPlot({
    numeric_data <- spotify_data %>% 
      select(danceability, energy, loudness, valence, popularity, speechiness, duration_ms, liveness)
    
    cor_matrix <- cor(numeric_data)
    corrplot(cor_matrix, method = "color", type = "upper", 
             tl.col = "black", tl.srt = 45,
             addCoef.col = "black", number.cex = 0.7)
  })
  

########################## PCA Plots ###########################
 output$pcaPlot <- renderPlot({
    
    
    fviz_eig(pca_df,addlabels = TRUE, ylim=c(0,50))
      })
  
  output$fvizPlot <- renderPlot({
   
    fviz_pca_var(pca_df,
                 col.var = "cos2",
                 gradient.cols = c("red", "blue", "green"),
                 repel = TRUE) 
  })
  
  output$pcaCorr <- renderPlot({
    
    corrplot(varcos, is.corr=FALSE,col = colorRampPalette(c("blue", "white", "red"))(50))
  })
########################## Data Table ###########################
  output$dataTable <- renderDT({
    
    spotify_data_raw<- spotify_data_raw%>%
      left_join(country_codes_df, by = c("country" = "code"))%>%
                   mutate(country =country_name)
 head(spotify_data_raw,2)   
    filtered <- spotify_data_raw %>%
      select(-spotify_id, -snapshot_date, -weekly_movement, -daily_movement)
    
    if (input$countryFilter != "All") {
      filtered <- filtered %>% filter(country == input$countryFilter)
    }
    
    filtered <- filtered %>% 
      filter(popularity >= input$popularityFilter[1] & 
               popularity <= input$popularityFilter[2])
    
    datatable(filtered, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = 'top')
  })
  
############################  Model Server  #######################
  predicted_value <- reactive({
    
    new_data <- data.frame(
      danceability = input$predDance,
      energy = input$predEnergy,
      loudness = input$predLoudness,
      valence = input$predValence,
      #tempo = input$predTempo,
      speechiness=input$speechiness,
      duration_ms=input$duration_ms,
      liveness = input$liveness,
      key=input$key,
      acousticness=input$acousticness,
      instrumentalness=input$instrumentalness,
      mode=input$mode
    )
    pred <- predict(rf_model, newdata = new_data)
    return(pred)
  })
  
  output$predictionOutput <- renderPrint({
    pred <- predicted_value()
    cat("Predicted Popularity Score: ",round(pred, 1), "/ 100",sep="")
  })
  
  output$predictionPlot <- renderPlot({
    pred <- predicted_value()
    plot_data <- data.frame(label = "Predicted", value = pred)
    
    ggplot(plot_data, aes(x = label, y = value)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
      
      geom_hline(yintercept = mean(spotify_data$popularity, na.rm = TRUE),
                 linetype = "dashed", color = "red") +
      annotate("text", x = .5, y = mean(spotify_data$popularity, na.rm = TRUE) + 2,
               label = "Average", color = "red") +
      
      geom_hline(yintercept = spotify_data%>%filter(country == "IE")%>%summarise(avgp=mean(popularity))%>%pull(avgp),
                 linetype = "dashed", color = "green") +
      annotate("text", x = .5, y = spotify_data%>%filter(country == "IE")%>%summarise(avgp=mean(popularity))%>%pull(avgp)+2,
               label = "Ireland", color = "green") +
      
      geom_hline(yintercept = spotify_data%>%summarise(qu1=quantile(popularity, probs = 0.25))%>%pull(qu1),
                 linetype = "dashed", color = "purple") +
      annotate("text", x = .53, y = spotify_data%>%summarise(qu1=quantile(popularity, probs = 0.25))%>%pull(qu1)+2,
               label = "First Quantile", color = "purple") +
    
      coord_cartesian(ylim = c(40, 100))+
      labs(title = "Popularity Prediction",
           x = "", y = "Popularity Score") +
      theme_minimal()
  }, height = 600)

  
################### Split Data Plot ####################
  output$splitData <- renderPlot({
    
    train_data$set<-"train"
    test_data$set<-"test"
    

    combined_data<-rbind(train_data,test_data)
    
    ggplot(combined_data,aes(x=energy,y=popularity,color=set))+
      geom_point(alpha = 0.3, size = 2) +
      scale_color_manual(values = c("train" = "blue", "test" = "red")) + 
      labs(title = "Train vs. Test Data",x = "Danceability",y = "Popularity",
                     color = "Dataset") +
      theme_minimal()
  })
  
############## Importance Plot ####################
  output$featureImportancePlot <- renderPlot({
    

    importance_df <- as.data.frame(varImp(rf_model)) # Mean decrease in accuracy
    colnames(importance_df) <- c("Importance")
    importance_df$Feature <- rownames(importance_df)
    

    importance_df <- importance_df %>%
      arrange(desc(Importance))
    
    ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Feature Importance", x = "Feature", y = "Importance Score") +
      theme_minimal()
  })

  output$predActualPlot<-renderPlot({
    test_data$pred <- predict(rf_model, newdata = test_data)
    rmse_val<-round(rmse(test_data$popularity, test_data$pred), 2)
    r2_val<-round(cor(test_data$popularity, test_data$pred)^2, 3)

    ggplot(test_data, aes(x = popularity, y = pred)) +
      geom_point(alpha = 0.5, color = "darkblue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(
        title = paste("Actual vs. Predicted Popularity (RMSE:", rmse_val, ", R²:", r2_val, ")"),
        x = "Actual Popularity",
        y = "Predicted Popularity"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
  })
  
}

shinyApp(ui = ui, server = server)


