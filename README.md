# Spotify Music Data Explorer 🎵📊

An interactive R Shiny dashboard to explore trends, features, and predict the popularity of songs from Spotify data.

## 🚀 Live Demo

**You can run and explore the live application here:**
[https://cy21r8-mehrdad-lashgari.shinyapps.io/spotify_visualization/](https://cy21r8-mehrdad-lashgari.shinyapps.io/spotify_visualization/)

## 📂 GitHub Repository

**Find the source code for this project here:**
[https://github.com/mehrdat/visualization_final_project/](https://github.com/mehrdat/visualization_final_project/)

## 📸 Screenshots

*(Here, you'll add your screenshots. Make sure to upload them to your GitHub repo, perhaps in an `images` folder, and then link them. Below are examples of how to link them.)*

**Example: Main Dashboard**
![Main Dashboard](images/main_dashboard_screenshot.png)
*(Replace `images/main_dashboard_screenshot.png` with the actual path to your image)*

**Example: Model Prediction Plot**
![Model Prediction](images/model_plot_screenshot.png)
*(Replace `images/model_plot_screenshot.png` with the actual path to your image)*

**Example: Popularity by Country Map**
![Country Map](images/country_map_screenshot.png)
*(Replace `images/country_map_screenshot.png` with the actual path to your image)*

## 📖 About The Project

This dashboard is a tool for looking at data about songs from Spotify. You can:
*   See general numbers like how many songs are in the dataset.
*   Find out which artists and albums are popular.
*   See how popular songs are in different countries.
*   Look at how song features (like how 'danceable' a song is, or its energy) have changed over the years.
*   Understand how different song features are related to each other.
*   Try to guess how popular a new song might be based on its features using a simple prediction model.
*   Look at the data in a table and filter it.
*   Give feedback about the app.

## 📊 The Data

The app uses data about songs from Spotify. This data includes information like:
*   **Popularity:** A score from 0 to 100 showing how popular a song is.
*   **Audio Features:**
    *   `danceability`: How suitable a track is for dancing.
    *   `energy`: How intense and active a track feels.
    *   `loudness`: The overall loudness of a track in decibels (dB).
    *   `speechiness`: The presence of spoken words in a track.
    *   `acousticness`: How acoustic a track is (0 means not acoustic, 1 means very acoustic).
    *   `instrumentalness`: Whether a track contains no vocals.
    *   `liveness`: The presence of an audience in the recording.
    *   `valence`: The musical positiveness conveyed by a track (happy, cheerful).
*   **Other Details:**
    *   `duration_ms`: The length of the song in milliseconds.
    *   `album_release_date`: When the album was released (used to find the year).
    *   `country`: The country code related to the song's chart data.
    *   `artists`, `album_name`, `name` (song title).

The data was preprocessed, which means it was cleaned up. For example, the release year was taken from the `album_release_date`. Some missing data was removed.

## ✨ Key Features (App Structure)

The app is organized into several tabs:

*   **Main Dashboard:**
    *   Shows quick numbers: Total songs, average popularity score of the most popular album, and the top artist by song count.
    *   A bar chart of the top 7 artists by a mix of song count and average popularity.
    *   A world map showing average song popularity by country.
*   **Interactive Plot:**
    *   Lets you choose a song feature (like `popularity`, `danceability`, `energy`) and see how its average value has changed over the years using a line graph.
*   **Static Plot:**
    *   **Correlation Plot:** Shows how different song features are related to each other (e.g., if energy goes up, does loudness also go up?).
    *   **PCA Plots:** (Principal Component Analysis) These plots help to show the main patterns and variations in the song features data in a simpler way.
*   **Model Plots:**
    *   **Song Features Input:** You can use sliders to set values for different song features (like danceability, energy, etc.).
    *   **Predicted Popularity:** Based on your inputs, the app predicts a popularity score for a song with those features using a Random Forest model.
    *   **Feature Importance:** Shows which song features are most important for the model when it predicts popularity.
    *   **Predicted vs. Actual Popularity:** A plot showing how well the model's predictions matched the actual popularity scores on a test set of data.
*   **Data Table:**
    *   Shows the song data in a table. You can filter by country and popularity range.
*   **Feedback / NPS:**
    *   Lets you give a score (0-10) on how likely you are to recommend the app and leave comments. This feedback is saved to a Google Sheet.
*   **About:**
    *   Information about the dataset, the purpose of the dashboard, and the methods used.

## 🛠️ Built With

This project was built using R and the following main R packages:
*   [Shiny](https://shiny.posit.co/) & [shinydashboard](https://rstudio.github.io/shinydashboard/): For creating the web application.
*   [dplyr](https://dplyr.tidyverse.org/): For data manipulation.
*   [ggplot2](https://ggplot2.tidyverse.org/) & [plotly](https://plotly.com/r/): For creating interactive plots.
*   [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html): For the prediction model.
*   [DT](https://rstudio.github.io/DT/): For displaying interactive data tables.
*   [countrycode](https://cran.r-project.org/web/packages/countrycode/index.html): For converting country codes for the map.
*   [googlesheets4](https://googlesheets4.tidyverse.org/): For saving feedback to Google Sheets.
*   And other helpful packages like `readr`, `tidyr`, `lubridate`, `corrplot`, `heatmaply`, `Metrics`, `caret`, `maps`, `tools`, `ggbiplot`, `factoextra`, `scales`, `shinyjs`.
