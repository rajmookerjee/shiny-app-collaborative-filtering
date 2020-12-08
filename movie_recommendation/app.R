library(shiny)
library(shinydashboard)
library(tidyverse)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(varhandle)

source('functions/helpers.R')

print('####################### 00000000000')
print('Data steps - STARTING')

memory.size(max=TRUE)

print("Printing Memory Used")
print(gc())

# Download URL
# myurl = "https://liangfgithub.github.io/MovieData/"
####Download movies    
# movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
# movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
# movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
# movies = data.frame(movies, stringsAsFactors = FALSE)
# colnames(movies) = c('MovieID', 'Title', 'Genres')
# movies$MovieID = as.integer(movies$MovieID)
# 
# # convert accented characters
# movies$Title[73]
# movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# movies$Title[73]
# 
# # extract year
# # movies$Year = as.numeric(str_extract(str_extract(movies$Title, "\\((\\d)+\\)"), "\\d+"))
# # 
# small_image_url = "https://liangfgithub.github.io/MovieImages/"
# movies$image_url = sapply(movies$MovieID, 
#                           function(x) paste0(small_image_url, x, '.jpg?raw=true'))
# 
# write.csv(movies, 'data/movies.csv')
# 
####Read Movies from downloaded file
movies = read.csv('data/movies.csv')
print("Done loading movies")

print("Printing Memory Used")
print(gc())
# 
# ####Download ratings
# ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
#                    sep = ':',
#                    colClasses = c('integer', 'NULL'), 
#                    header = FALSE)
# colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
# 
# write.csv(ratings, 'data/ratings.csv')
#
#####Read Ratings from downloaded file
ratings_raw = read.csv('data/ratings.csv')
print("Done loading rating")

print("Printing Memory Used")
print(gc())
####Download users
# users = read.csv(paste0(myurl, 'users.dat?raw=true'),
#                  sep = ':', header = FALSE)
# users = users[, -c(2,4,6,8)] # skip columns
# colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
# 
# write.csv(users, 'data/users.csv')
#
####Read Users from downloaded file
# users = read.csv('data/users.csv')
# print("Done loading users")

print("Printing Memory Used")
print(gc())

####Preprocess ratings for prediction
# consl = ratings %>% 
#     group_by(MovieID) %>% 
#     summarise(num_ratings = n()) %>% 
#     group_by(num_ratings) %>%
#     summarise(num_movies = n()) %>% 
#     arrange(num_ratings) 

consl_dist = ratings_raw %>% 
    group_by(MovieID) %>% 
    summarise(num_ratings = n()) %>% 
    filter(num_ratings > 47)
quantile(consl_dist$num_ratings, probs = seq(0, 1, 0.05))

ratings_gt1rvw = ratings_raw %>% 
    inner_join(consl_dist, by = "MovieID")

#Pivot all movies by userid
# g = ratings_gt1rvw %>% 
#     pivot_wider(id_cols = UserID, names_from = MovieID, values_from = Rating)
# # Convert to matrix
# # g_matrix = as.matrix(g[,-1])

# rating_matrix = as(g_matrix, "realRatingMatrix")

movies_gt1rvw = movies %>%
    select(MovieID, image_url, Title, Genres) %>%
    inner_join(consl_dist, by = "MovieID")

print("All variables before")
print(ls())

rm(list = setdiff(ls(), c("movies", "ratings_gt1rvw", "movies_gt1rvw")))



print("All variables after")
print(ls())

print("Printing Memory Used")
print(gc())

# rm(ratings_raw)
# rm(movies)
# rm(consl_dist)
# rm(g)
# rm(rating_matrix)
# rm()


print("Done preprocessing step")
print("Printing Memory Used")
print(gc())

# movies <- movies_gt1rvw

# rec_mod_UBCF <- Recommender(rating_matrix, method = "UBCF",
#                        param = list(method = "pearson", nn = 500))
# rec_mod_IBCF <- Recommender(rating_matrix, method = "IBCF",
#                        param = list(method = "cosine"))
# rec_mod_popular <- Recommender(rating_matrix, method = "POPULAR",
#                        param = NULL)

# save(rec_mod_UBCF, file = 'model/trained_model_ubcf.rda')
# save(rec_mod_IBCF, file = 'model/trained_model_ibcf.rda')
# save(rec_mod_popular, file = 'model/trained_model_popular.rda')

# Load pretrained models
load(file = 'model/trained_model_ubcf.rda')
load(file = 'model/trained_model_ibcf.rda')
load(file = 'model/trained_model_popular.rda')

print("Printing Memory Used")
print(gc())
print('####################### 00000000000')
print('Data steps - DONE')


get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    dat
}

sidebar <- dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs",
                menuItem("Intelligent Filter/Sort - Genre (System 1)", tabName="system1", selected=TRUE),
                menuItem("Recommendation based on Sample Selection (System 2)", tabName="system2", selected=FALSE)
    )
)

body <-   dashboardBody(
    tabItems(
        tabItem(tabName = "system1",
                fluidPage(
                    titlePanel("Select Filter Type:"),
                    fluidRow(
                        sidebarLayout(
                            sidebarPanel(
                                h3("Select Movie Genre You Prefer:"),
                                radioButtons("inputGenre", "Genre:",
                                             c("Action"="Action", 
                                               "Adventure"="Adventure", 
                                               "Animation"="Animation", 
                                               "Children"="Children", 
                                               "Comedy"="Comedy", 
                                               "Crime"="Crime",
                                               "Documentary"="Documentary", 
                                               "Drama"="Drama", 
                                               "Fantasy"="Fantasy",
                                               "Film.Noir"="Film.Noir", 
                                               "Horror"="Horror", 
                                               "Musical"="Musical", 
                                               "Mystery"="Mystery",
                                               "Romance"="Romance",
                                               "Sci.Fi"="Sci.Fi", 
                                               "Thriller"="Thriller", 
                                               "War"="War", 
                                               "Western"="Western"
                                             )
                                ),
                                h3("Select Filter Type:"),
                                radioButtons("inputFilterType", "Filter Types:",
                                             c("All Time Most Popular - Highest Rating Among Most Reviewed"="cummRatingAllTime", 
                                               "Trending Movies (Recent reviews) - Highest Rating Among Most Reviewed"="cummRatingRecent"
                                             )
                                )                                      
                            ),
                            mainPanel(
                                textOutput("txt"),
                                tableOutput("system1Table") 
                            )
                        ),
                    )
                ),
            ),
        tabItem(tabName = "system2",
                dashboardPage(
                    skin = "blue",
                    dashboardHeader(title = "Movie Recommender - UBCF/IBCF"),
                    
                    dashboardSidebar(
                        disable = TRUE
                    ),
                    
                    dashboardBody(
                                fluidRow(
                                    h3("Select Type of Recommendation Algorithm:"),
                                    radioButtons("inputAlgoType", "Algorithm Type:",
                                                 c("UBCF"="UBCF",
                                                   "IBCF"="IBCF",
                                                   "Popular Items"="POPULAR"
                                                 )
                                    ),
                                ),
                                includeCSS("css/movies.css"),
                                  fluidRow(
                                      box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                          div(class = "rateitems",
                                              uiOutput('ratings')
                                          )
                                      )
                                  ),
                                  fluidRow(
                                      useShinyjs(),
                                      box(
                                          width = 12, status = "info", solidHeader = TRUE,
                                          title = "Step 2: Discover movies you might like",
                                          br(),
                                          withBusyIndicatorUI(
                                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                          ),
                                          br(),
                                          tableOutput("results")
                                      )
                                  )
                    )
                ),
            )        
        )
)
    
    
getSystemRecommendations <- function(genre, filterType) {
    
    if(filterType == "cummRatingAllTime"){
        
        movies_rating <- movies_gt1rvw %>% 
            filter(str_detect(Genres, genre)) %>%
            inner_join(ratings_gt1rvw, by = "MovieID")
        
        moviesCumm <- movies_rating %>%
            group_by(Genres, Title) %>%
            summarise(ratingsCumm = sum(Rating)) %>% 
            arrange(desc(ratingsCumm)) %>% 
            head(5)
        
    }
    
    if(filterType == "cummRatingRecent"){
        
        movies_rating <- movies_gt1rvw %>% 
            filter(str_detect(Genres, genre)) %>%
            inner_join(ratings_gt1rvw, by = "MovieID") %>% 
            arrange(desc(Timestamp)) %>% 
            head(10000) 
        
        moviesCumm <- movies_rating %>%
            group_by(Genres, Title) %>%
            summarise(ratingsCumm = sum(Rating)) %>% 
            arrange(desc(ratingsCumm)) %>% 
            head(5)
        
    }    
    
    moviesCumm
    
}
    
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$txt <- renderText({
        paste("Chosen Genre: ", input$inputGenre)
    })
    
    output$system1Table = renderTable({getSystemRecommendations(input$inputGenre, input$inputFilterType)})
    
    # show the books to be rated
    output$ratings <- renderUI({
        num_rows <- 20
        num_movies <- 6 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            
            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            user_ratings <- get_user_ratings(value_list)
            print('####################### user_ratings')
            print(user_ratings)
            if(nrow(user_ratings) > 1) {
                rating_vec <- movies_gt1rvw %>%
                    left_join(., user_ratings, by = "MovieID") %>% 
                    pull(Rating) 
                # rating_vec <- user_ratings$Ratin
                print('####################### rating_vec')# g
                print(rating_vec)
                rating_vec <- as.matrix(t(rating_vec))
                rating_vec <- as(rating_vec, "realRatingMatrix")
                if(input$inputAlgoType == 'UBCF') {
                    print('############# UBCF')
                    rec_mod <- rec_mod_UBCF  
                }
                else if(input$inputAlgoType == 'POPULAR') {
                    print('############# POPULAR')
                    rec_mod <- rec_mod_popular 
                }
                else {
                    print('############# IBCF')
                    rec_mod <- rec_mod_IBCF           
                    
                }
                print('####################### rec_mod')            
                print(rec_mod)
                top_5_prediction <- predict(rec_mod, rating_vec, n = 5)
                print('####################### top_5_prediction')            
                print(top_5_prediction)              
                top_5_list <- as(top_5_prediction, "list")
                print('####################### top_5_list')            
                print(top_5_list)               
                top_5_df <- data.frame(top_5_list)
                colnames(top_5_df) <- "MovieID"
                print('####################### top_5_df')            
                print(top_5_df)         
                print(str(top_5_df))
                user_results= as.numeric(top_5_df$MovieID)
                top_5_df$MovieID = as.numeric(as.character(top_5_df$MovieID))
                
                print('####################### top_5_df')            
                print(top_5_df)   
                user_predicted_ids = as.vector(top_5_df[, c("MovieID")])
                print('####################### user_predicted_ids')            
                print(user_predicted_ids)              
                # names <- left_join(top_5_df, movie_data, by="movieId")
                # top_5_df
                # user_predicted_ids <- as.data.frame(names) %>% select(movieId)
                # names            
                
                # user_results = (1:5)/5
                # user_results= as.numeric(top_5_df$MovieID)
                # user_predicted_ids = 1:10
                recom_results <- data.table(Rank = 1:5,
                                            MovieID = movies_gt1rvw[(movies_gt1rvw$MovieID %in% user_predicted_ids), "MovieID"],
                                            Title = movies_gt1rvw[(movies_gt1rvw$MovieID %in% user_predicted_ids), "Title"],
                                            Image_Url = movies_gt1rvw[(movies_gt1rvw$MovieID %in% user_predicted_ids), "image_url"],
                                            Predicted_rating = user_results)
                print('####################### recom_results')            
                print(recom_results)
            }
            else {
                recom_results <- data.table()
            }
            
            recom_results
            
        }) # still busy
        
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
        recom_result <- df()
        print(nrow(recom_result))
        if(!is.null(recom_result)) {
            if(nrow(recom_result) > 1) {
                num_rows <- 1
                num_movies <- 5
                
                lapply(1:num_rows, function(i) {
                    list(fluidRow(lapply(1:num_movies, function(j) {
                        print('####################### calculated value')            
                        print((i - 1) * num_movies + j)
                        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                        
                            div(style = "text-align:center", 
                                a(img(src = recom_result$Image_Url[(i - 1) * num_movies + j], height = 150))
                            ),
                            div(style="text-align:center; font-size: 100%", 
                                strong(recom_result$Title[(i - 1) * num_movies + j])
                            )
                            
                        )        
                    }))) # columns
                }) # rows
            }
            else {
                fluidRow(
                    h3("Please rate more than 1 movie.")
                )
            }
        }
        else {
            fluidRow(
                h3("Need different ratings for at least 2 movies.")
            )
        }
    }) # renderUI function
    
    
    
}


ui <- dashboardPage(
    dashboardHeader(title = "Movie Recommendation System"),
    sidebar,
    body
)

# Run the application 
shinyApp(ui = ui, server = server)




