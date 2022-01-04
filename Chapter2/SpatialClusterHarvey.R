

here::i_am("Chapter2/Harvey/SpatialClusterHarvey.R") # Declares the location of the current script


  # LIBRARIES ---------------------------------------------------------------
  library(here) # creates paths relative to the top-level directory
  library(tidyverse) # For ggplot. Nice plots.
  library(sf) # Spatial monster

  library(tibble)

  library(sp)
  
  # For spatial clustering
  library(dbscan)
  
  # Mapping tools
  library(leaflet) ####
  library(leaflet.esri)
  library(htmltools) ##
  library(raster)
  library(rmapshaper)
  library(tigris) # Loading tigris dataset/ Census
  library(htmlwidgets) ###
  library(classInt)
  library(RColorBrewer)
  library(mapview)
  library(classInt)
  library(tmap)
  
  # For text Mining
  library(tm) 
  library(wordcloud)
  library(tidytext) ####
  library(stringr) # For str_to_lower
  library(stringi)
  
  options(tigris_use_cache = TRUE)
  
  # IMPORTING DATA ----------------------------------------------------------
  
  ## Importing Tweets
  TweetsHarveyTexas_sf <- readRDS(file = here("Chapter2", "Data", "TweetsHarveyTexas_sf.rds"))  ## havent used this yet
  TweetsHarveyTexas <- readRDS(file = here("Chapter2", "Data", "TweetsHarveyTexas.rds"))
  
  # Importing Spotters
  HarveyNWS <- readRDS(file = here("Chapter2", "Data", "HarveyNWSTexas.rds")) 
  
  # Store tweets as simple features and project data
  HarveyNWS_sf <- HarveyNWS %>% 
    st_as_sf(coords = c("BEGIN_LON", "BEGIN_LAT"), crs = 4326) # Check the thing about begin
  
  # Importing allReports dataset
  allReports <- readRDS(file = here("Chapter2", "Data", "allReports.rds"))  ## havent used this yet
  
  
  
  # DATA CLEANING -----------------------------------------------------------
  
  TweetsHarveyTexas_sf <- TweetsHarveyTexas_sf %>% # Removing retweets
    filter(!str_detect(tweet, "^RT"))
  
  
  
  # FLOOD EXTENT ------------------------------------------------------------
  # 
  # FloodExtent <- st_read("Data/ObservedFloodExtent/ObservedFloodExtent.shp") %>% # Need to reproject in WGS84 datum. long lat format.
  #   st_transform(crs = 4326)
  # 
  # 
  # # In flood we save the "original" or the first simplified version resulting in QGIS. Unable to use the original file for this.
  # # But for the real version I just need to run simplifying steps used below with the original file 
  # # downloaded from: http://www.arcgis.com/home/item.html?id=826e2679f912443d9c7853d3addd59aa
  # # I can't do that now because it will take so much time I don't have.
  # 
  # flood <- st_read("Data/FloodSimplified/femaFloodProjSimplyCheck.shp")
  # 
  # # Here to simplify
  # flood_simply <- ms_simplify(input = flood,
  #                             keep = 0.025)
  # 
  # # Saving/exporting result
  # saveRDS(flood_simply, file = "Data/flood_simply.rds")
  # st_write(flood_simply, "Data/flood_simply.shp")
  
  # Load simplified file
  flood_simply <- readRDS(file = here("Chapter2", "Data","flood_simply.rds"))
  
  flood_simply <- flood_simply %>% # Need to reproject in WGS84 datum. long lat format.
    st_transform(crs = 4326)
  
  # Checking coordinate systems
  st_crs(flood_simply)
  
  # Fixing Topology error
  checker <- st_is_valid(flood_simply)
  good <- flood_simply[!is.na(checker),]
  good <- good[checker,] # "good" is the final version of the final flood file in vector format.
  
  # Creating raster Mike's way
  # bb = AOI::getBoundingBox(fvu) %>% st_as_sf()
  # r = raster(bb)
  # tmp = fasterize::fasterize(fv, r)
  
  # Make a raster of the flood areas. This is great for plotting
  # rraster <- raster()
  # extent(rraster) <- extent(good)
  # dim(rraster) = c(5000, 5000)
  # # res(rraster) <- 5 # set cell size to 2500 metres Also I can play with this later.
  # floodR <- fasterize::fasterize(good, rraster)
  # saveRDS(floodR, file = "Data/floodR.rds")
  
  #floodR <- readRDS(file = here("Chapter2", "Data", "floodR.rds"))
  
  
  
  # FLOOD FILTER ------------------------------------------------------
  
  TexasCounties <- readRDS(file = here("Chapter2", "Data", "TexasCounties.rds")) # Total area in meters: 128295306849
  
  texas <- TexasCounties  %>% 
    st_transform(crs = 4326)
  
  SpotterInCounties <- st_intersection(x = texas, y = HarveyNWS_sf) # Spotters in counties
  saveRDS(SpotterInCounties, file = here("Chapter2", "Data", "SpotterInCounties.rds")) # 157 tweets
  
  TweetsInCounties <- st_intersection(x = texas, y = TweetsHarveyTexas_sf) # Tweets in counties
  saveRDS(TweetsInCounties, file = here("Chapter2", "Data", "TweetsInCounties.rds")) # 3808 tweets
  
  outside <- sapply(st_intersects(TweetsHarveyTexas_sf, texas),function(x){length(x)==0}) 
  TweetsOut <- TweetsHarveyTexas_sf[outside, ] # Explanation here: https://gis.stackexchange.com/questions/245136/how-to-subset-point-data-by-outside-of-polygon-data-in-r  
  saveRDS(TweetsOut, file = here("Chapter2", "Data", "TweetsOutsideCounties.rds")) # 676 tweets outside counties
  
  TweetsInFlood <- st_intersection(x = good, y = TweetsHarveyTexas_sf) # Tweets in counties
  saveRDS(TweetsInFlood, file = here("Chapter2", "Data", "TweetsInFlood.rds")) #83 tweets
  
  
  
  # ANALYSIS IN HEXAGONS -------------------------------------------------------------------
  
  # # When using the whole state area
  # 
  # # Data to create basemap
  # states <- states(cb = FALSE, resolution = "500k") # needs tigris package
  # state_sf <- st_as_sf(states)
  # texas <- state_sf %>% 
  #   filter(NAME == "Texas") %>% 
  #   st_transform(crs = 4326)
  # 
  # # Checking CRS
  # st_crs(texas)
  # 
  # texas_sp <-  as(texas, "Spatial")
  
  texas_sp <-  as(texas, "Spatial")
  
  ## Defining number of hexagons works best to cover the whole area
  sp_hex <- HexPoints2SpatialPolygons(spsample(texas_sp,
                                               n=12500, #Try with 25000 first and it was probably too small. With 12500 polygons, hex area is around 10km2
                                               type="hexagonal")) # Create hexagons based on defined number of hex
  
  sf_hex <- st_as_sf(sp_hex) %>% 
    mutate(group = 1:nrow(.))
  
  # HexagonsWithTweets
  hexWithTweets <- st_join(sf_hex, TweetsInCounties) %>% 
    group_by(group) %>%
    summarise(total = sum(!is.na(group)))%>% 
    mutate(totRatio = total/sum(total))# Add column that normalize totals
  
  # HexagonsWithSpotters
  hexWithSpotters <- st_join(sf_hex, HarveyNWS_sf) %>% 
    group_by(group) %>%
    summarise(total = sum(!is.na(group))) %>% 
    mutate(totRatio = total/sum(total))# Add column that normalize totals
  
  classes <- 6
  style_method <- "fisher"
  pal1 <- brewer.pal(classes, "YlOrRd")
  palData <- classIntervals(hexWithSpotters$totRatio, n = classes, style=style_method, pal = pal1)
  hexWithSpotters$colores <- findColours(palData, pal1)%>%
    as.factor(.)
  pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)
  
  SandyHexSpotMap <- leaflet(hexWithSpotters) %>%
    setView(lng = -96.5, lat = 31.5, zoom = 7) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    addPolygons(fillColor = ~colores,
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "2",
                fillOpacity = 0.7,
                popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                            total))) %>%
    addLegend(pal = pal2,
              values = ~totRatio, 
              opacity = 0.7, 
              title = "Spotter Reports Density",
              position = "bottomright")
  
  SandyHexSpotMap
  #htmlwidgets::saveWidget(SandyHexSpotMap, file = "SandyHexSpotMap10km.html")
  
  classes <- 7
  style_method <- "fisher"
  pal1 <- brewer.pal(classes, "YlOrRd")
  palData <- classIntervals(hexWithTweets$totRatio, n = classes, style=style_method, pal = pal1)
  hexWithTweets$colores <- findColours(palData, pal1)%>%
    as.factor(.)
  pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)
  
  SandyHexTweetMap <- leaflet(hexWithTweets) %>%
    setView(lng = -96.5, lat = 31.5, zoom = 7) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    addPolygons(fillColor = ~colores,
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "2",
                fillOpacity = 0.7,
                popup = ~htmlEscape(sprintf("Reports per hexagon: %f",
                                            total))) %>%
    addLegend(pal = pal2,
              values = ~totRatio, 
              opacity = 0.7, 
              title = "Tweets Density",
              position = "bottomright")
  
  SandyHexTweetMap
  #htmlwidgets::saveWidget(SandyHexTweetMap, file = "SandyHexTweetMap10km.html")
  
  mix <- leafsync::sync(SandyHexSpotMap, SandyHexTweetMap)
  #save_html(mix, "mix.html", background = "white", libdir = "lib")
  mix
  
  
  
  ## LEVEL 2 TWEETS!!! ------------------------------------------------------
  
  # FloodTweetsInHex <- 
  #   st_join(TweetsInFlood, hexWithSpotters) %>% 
  #   filter(total == 2) # 3 tweets sent within flooded areas and sharing hex with Spotters. Content seems to be irrelevant.
  
  # InHexInCounty <- 
  #   st_join(TweetsInCounties, hexWithSpotters) 
  # 
  # saveRDS(InHexInCounty, file = "Data/InHexInCounty.rds") 
  
  InHexInCounty <- readRDS(file = here("Chapter2", "Data","InHexInCounty.rds"))
  
  CountyTweetsInHex <- InHexInCounty %>% 
    filter(total != 1) # 30 tweets sent within counties affected and sharing hex with Spotters. THESE ARE THE LEVEL 2 TWEETS
  
  level2 <- CountyTweetsInHex %>% 
    dplyr::select(text = tweet,
                  date = date,
                  total = total,
                  geometry = geometry)
  
  CountyTweetsOutHex <- InHexInCounty %>% 
    filter(total == 1) # 3775 tweets sent within counties affected and but not sharing hex with Spotters. They now need to be classified by content. 
  
  
  
  ## LEVEL 3 TWEETS!!! ------------------------------------------------------
  
  # First Joining CountyTweetsOutHex and TweetsOut for content analysis
  
  TweetsOutHex <- dplyr::select(CountyTweetsOutHex,date, tweet, geometry, total) # Select just a few columns from the original dataset.
  TweetsOutRiskA <- dplyr::select(TweetsOut, date, tweet, geometry) %>% 
    mutate(total = 0)
  
  TweetsForContent <- dplyr::union(TweetsOutHex, TweetsOutRiskA)
  
  # CONTENT ANALYSIS ------------------------------------------------------
  
  tweets_clean <- TweetsForContent %>% 
    mutate(text = tweet) %>% 
    mutate(id = row_number())
  
  tweets_clean  <- tweets_clean  %>% 
    mutate(text = str_remove_all(text, 'http[^ ]+'), #Finally removing urls in a nice way
           text = str_remove_all(text, '@[^ ]+'), # Removing usernames
           text = str_remove_all(text, "[:punct:]"), # Removing puntuation
           text = str_remove_all(text, "[:digit:]"), # Removing numbers
           text = str_remove_all(text, "|")) 
  
  tweets_tidy <- tweets_clean %>% # To keep a column with the original tweet
    unnest_tokens(word, text, token = "tweets") %>% # add a row for each token (word) and repeat the other information
    anti_join(stop_words) # remove stopwords
  
  tweets_tidy %>% 
    count(word, sort = TRUE) # Count words
  
  # Remove trending words
  keywords <- c("hurricaneharvey", "houston", "harvey", "texas", "hurricane", "houstonstrong", "repost")
  tweets_tidy <- tweets_clean %>% 
    mutate(tweet = text) %>% # To keep a column with the original tweet
    unnest_tokens(word, text, token = "tweets") %>% # add a row for each token (word) and repeat the other information
    anti_join(stop_words) %>%  # remove stopwords
    filter(!word %in% keywords) # Next two candidates to be removed are Im and boulderflood. TO CHECK LATER!
  tweets_tidy %>% 
    count(word, sort = TRUE) # Count words
  
  #saveRDS(tweets_clean, file = "tweets_clean.rds")
  #saveRDS(tweets_tidy, file = "tweets_tidy.rds")
  
  
  library(stm) #fast compared to other implementations of topic models. Base don C++
  library(quanteda)
  # Either one (of the following) works as input for the topic modelling
  # Create Document-Term Matrix
  tweets_dfm <- tweets_tidy %>%
    count(tweet, word, sort = TRUE) %>% 
    cast_dfm(tweet, word, n) # 4 documents (4 stages). Quanteda document frecuency. Special implementation for document term matrix
  # Create Sparce Matrix
  tweets_sparse <- tweets_tidy %>%
    count(id, word) %>%
    cast_sparse(id, word, n)
  
  k_result <- readRDS(file = here("Chapter2", "Data", "k_result.rds"))
  k_result %>%
    transmute(K,
              `Lower bound` = lbound,
              Residuals = map_dbl(residual, "dispersion"),
              `Semantic coherence` = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    gather(Metric, Value, -K) %>%
    ggplot(aes(K, Value, color = Metric)) +
    geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics",
         subtitle = "These diagnostics indicate that a good number of topics would be around 13")
  
  k_result %>%
    dplyr::select(K, exclusivity, semantic_coherence) %>%
    filter(K %in% c(7,19)) %>% # This values should be the same used when creating the model in the R script
    unnest() %>%
    mutate(K = as.factor(K)) %>%
    ggplot(aes(semantic_coherence, exclusivity, color = K)) +
    geom_point(size = 2, alpha = 0.7) +
    labs(x = "Semantic coherence",
         y = "Exclusivity",
         title = "Comparing exclusivity and semantic coherence",
         subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
  
  topic_model <- k_result %>% 
    filter(K == 19) %>% 
    pull(topic_model) %>% 
    .[[1]]
  # topic_model
  td_beta <- tidy(topic_model) # The beta matrix shows what are the words that contribute to each topic.
  td_beta %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup %>% 
    mutate(topic = paste0("Topic ", topic),
           term = reorder(term, beta)) %>% #reordering by tf_idf
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics") # To see which words contribute the most t
  
  library(ggthemes)
  library(extrafont)
  library(scales) # for function percet_format
  td_gamma <- tidy(topic_model, matrix = "gamma",                    
                   document_names = rownames(tweets_sparse))
  top_terms <- td_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(7, beta) %>%
    arrange(-beta) %>%
    dplyr::select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>% 
    unnest(cols = c(terms)) #unnest now requires argument cols
  gamma_terms <- td_gamma %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    arrange(desc(gamma)) %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic = paste0("Topic ", topic),
           topic = reorder(topic, gamma))
  gamma_terms %>%
    top_n(17, gamma) %>%
    ggplot(aes(topic, gamma, label = terms, fill = topic, alpha = 0.7)) +
    geom_col(show.legend = FALSE) +
    geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
              family = "CM Roman") +
    coord_flip() +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 0.30),
                       labels = percent_format()) +
    theme_tufte(base_family = "CM Roman",ticks = FALSE) +
    theme(plot.title = element_text(size = 14,
                                    family="CM Roman"),
          plot.subtitle = element_text(size = 11),
          legend.position = "none") +
    labs(x = NULL, y = expression(gamma),
         title = "Topics by prevalence in the dataset containing all Colorado tweets",
         subtitle = "With the top words that contribute to each topic")
  
  ## Prepare heatmap
  
  heat <- td_gamma %>% 
    rename(tweetid = document) %>% 
    mutate(id = as.integer(tweetid)) %>% 
    left_join(tweets_clean, by = "id") %>% 
    dplyr::select(id, topic, gamma, date)
  # Assigning a topic to each tweet based on which one was had the maximum Gamma
  heat <- heat %>% 
    group_by(id) %>% 
    filter(gamma == max(gamma)) %>% 
    ungroup()
  
  tweet_and_topic <- dplyr::left_join(heat, tweets_clean, by = "id") %>% 
    dplyr::select(tweet_id = id,
           topic = topic,
           gamma = gamma,
           text, text,
           date = date.x,
           geometry = geometry,
           total = total) %>% 
    mutate(topic = paste0("Topic ", topic))
  
  # Finally for levels 3 and 5.1
  
  tweet_and_topic %>% 
    group_by(topic) %>% 
    count()
  
  NonRelevantTopics <- c("Topic 4", "Topic 7", "Topic 10", "Topic 14", "Topic 18")
  
  tweet_and_topic %>%  # Total ==0 corresponds to CountyTweetsOutHex
    filter(total == 1) %>% 
    count()
  
  level5_1 <- tweet_and_topic %>% 
    filter(total == 1 & topic %in% NonRelevantTopics)
  
  `%out%` <- function(a,b) ! a %in% b # Had to define `out' because there is not the opposite of %in%
  
  level3 <- tweet_and_topic %>% 
    filter(total == 1 & topic %out% NonRelevantTopics)
  
  tweet_and_topic %>%  # Total ==0 corresponds to TweetsOutRiskA
    filter(total == 0) %>% 
    count()
  
  ToLevel4 <- tweet_and_topic %>% 
    filter(total == 0 & topic %out% NonRelevantTopics)
  
  Excluded <- tweet_and_topic %>% 
    filter(total == 0 & topic %in% NonRelevantTopics)
  
  ## LEVEL 4 TWEETS!!! ------------------------------------------------------
  
  # SPATIAL CLUSTERING -------------------------------------
  
  ToLevel4 <- st_as_sf(ToLevel4)
  
  set.seed(123)
  clusters <- hdbscan(ToLevel4  %>%
                        st_coordinates(), #This rounds coordinates
                      minPts = 15)
  
  level4_clusters <- ToLevel4 %>% 
    mutate(cluster = clusters$cluster)
  
  unique(level4_clusters$cluster)
  
  # Checking coordinate systems
  st_crs(level4_clusters)
  
  
  # PLOT: TWEETS SPATIAL CLUSTERS, NWS REPORTS AND FLOODED AREAS ------------
  
  level4_clusters <- level4_clusters %>% 
    mutate(color = case_when(cluster == "0" ~ "#bababa",
                             cluster == "1" ~ "#f46d43",
                             TRUE ~ "#762a83"))
  
  level4_clusters$cluster <- as.factor(level4_clusters$cluster) #Clusters as factors for coloring

  addLegendCustom <- function(map, color, labels, size = 10, opacity = 1, stroke = FALSE, title = "Spatial Cluster", position = 'bottomright'){
    colorAdditions <- paste0(color, "; border-radius: 50%; width:", size, "px; height:", size, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                             size, "px;margin-top: 4px;line-height: ", size, "px;'>", 
                             labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, 
                     labels = labelAdditions, 
                     opacity = opacity, 
                     title = title,
                     position = position))
  }
  

  clustersMap <- leaflet(data = level4_clusters) %>% # Interactive map to see resulting clusters
    addTiles()  %>%
    addProviderTiles(providers$CartoDB.Voyager) %>% #$Stamen.TonerLite
    addCircleMarkers(weight = 5,
                     radius= 3,
                     color= ~color,
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(cluster)) %>% 
    addLegendCustom(color = c("#bababa", "#f46d43", "#762a83"), 
                    labels = c("No Cluster",
                               "Cluster 1", 
                               "Cluster 2"))
  


  clustersMap
  
  saveWidget(clustersMap, file = here("harveyMapClustersNoOpacity.html"))
  
  
  level4 <- level4_clusters %>% 
    filter(cluster == 1)
  
  level5_2 <- level4_clusters %>% 
    filter(cluster != 1) 
  
  level5_2$color <- NULL # drop color column
  
  level5_1 <- level5_1 %>% 
    mutate(cluster = NA) # Just to make sure joining with the other dataset have same columns. Also is to identify these tweets do not come from spatial clusters
  
  level5 <- dplyr::union(level5_1, level5_2)
  
  
  
  ## VALIDATION!!! ------------------------------------------------------
  
  
  Level1InFlood <- st_intersection(x = good, y = HarveyNWS_sf) # Spotter reports in flooded areas
  
  Level2InFlood <- st_intersection(x = good, y = level2) # Spotter reports in flooded areas
  
  
  level3 <- level3 %>% # For some reason was not in sf format
    st_as_sf(crs = 4326)
  
  Level3InFlood <- st_intersection(x = good, y = level3) # Spotter reports in flooded areas
  
  Level4InFlood <- st_intersection(x = good, y = level4) # Spotter reports in flooded areas
  
  level5 <- level5 %>% # For some reason was not in sf format
    st_as_sf(crs = 4326)
  
  Level5InFlood <- st_intersection(x = good, y = level5) # Spotter reports in flooded areas
  
  Excluded <- Excluded %>% # For some reason was not in sf format
    st_as_sf(crs = 4326)
  
  ExcludedInFlood <- st_intersection(x = good, y = Excluded) # Spotter reports in flooded areas
  
 # Creating a unique final dataset 
  
  level1 <- dplyr::select(SpotterInCounties,
                          text = EVENT_TYPE) %>% 
    mutate(level = "1: High")
  
  level2 <- dplyr::select(level2, text) %>% 
    mutate(level = "2: High-Medium")
  
  level3 <- dplyr::select(level3, text) %>% 
    mutate(level = "3: Medium")
  
  level4 <- dplyr::select(level4, text) %>% 
    mutate(level = "4: Low-Medium")
  
  level5 <- dplyr::select(level5, text) %>% 
    mutate(level = "5: Low")
  
  levelss <- dplyr::bind_rows(level1, level2, level3, level4, level5)
  
  ## MAPPING RESULTS!!! ------------------------------------------------------
  
  harveyClassifiedMap <- leaflet() %>% # Interactive map to see resulting clusters
    addTiles()  %>%
    addProviderTiles(providers$Stamen.TonerLite) %>% #$Stamen.TonerLite  #CartoDB.DarkMatter
    # addRasterImage(floodR,
    #                col = '#4393c3', #
    #                opacity = 1) %>%  #0.5
    addCircleMarkers(data = SpotterInCounties,
                     group = "Group A",
                     weight = 5, 
                     radius = 5,
                     stroke = FALSE,
                     color = "#1a9641",
                     fill = TRUE,
                     #  fillColor = "#35978f",
                     fillOpacity = 1) %>% #With fillOpacity is less transparent #0.8
    addCircleMarkers(data = level2,
                     group = "Group B",
                     weight = 5,
                     radius= 3,
                     color= "#a6d96a",
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(text)) %>%
    addCircleMarkers(data = level3,
                     group = "Group C",
                     weight = 5,
                     radius= 3,
                     color= "#F4D03F",#ffffbf#ffeda0
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(text)) %>%
    addCircleMarkers(data = level4,
                     group = "Group D",
                     weight = 5,
                     radius= 3,
                     color= "#fd8d3c",#fdae61
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(text)) %>%
    addCircleMarkers(data = level5,
                     group = "Group E",
                     weight = 5,
                     radius= 3,
                     color= "#ef3b2c",#d7191c
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(text)) %>%
    setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995) %>% 
    addLegend(group = "Group E", 
              colors = "#ef3b2c",
              opacity = 1,
              labels = "Level 5",
              position = "bottomright") %>% 
    addLegend(group = "Group D", 
              colors = "#fd8d3c",
              opacity = 1,
              labels = "Level 4",
              position = "bottomright") %>% 
    addLegend(group = "Group C", 
              colors = "#F4D03F",
              opacity = 1,
              labels = "Level 3",
              position = "bottomright") %>% 
    addLegend(group = "Group B", 
              colors = "#a6d96a",
              opacity = 1,
              labels = "Level 2",
              position = "bottomright") %>% 
    addLegend(group = "Group A", 
              colors = "#1a9641",
              opacity = 1,
              labels = "Level 1",
              position = "bottomright") %>%
    addLayersControl(
      overlayGroups = c("Group A", "Group B", "Group C", "Group D", "Group E"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  harveyClassifiedMap
  
  # saveWidget(harveyClassifiedMap, file = "harveyClassifiedMap.html")
  
  
  ## MAPPING RESULTS 2!!! ------------------------------------------------------
  
  library(lubridate)

  pal <- colorFactor(c("#1a9641", "#a6d96a", "#F4D03F", "#fd8d3c", "#ef3b2c"), 
                     domain = c("1: High", "2: High-Medium", "3: Medium","4: Low-Medium","5: Low"))
 
  harveyClassifiedMap <- leaflet(data = levelss) %>% # Interactive map to see resulting clusters
    addProviderTiles(providers$Stamen.TonerLite) %>% #$Stamen.TonerLite  #CartoDB.DarkMatter
    # addRasterImage(floodR,
    #                col = '#4393c3', #
    #                opacity = 1) %>%  #0.5
    addCircleMarkers(weight = 5,
                     radius= 1.5,
                     color= ~pal(level),
                     label = ~levelss,
                     stroke = FALSE,
                     fill = TRUE,
                     fillOpacity = 1, #0.5
                     popup = ~htmlEscape(text)) %>%
    setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995) %>% 
    addLegend(pal = pal,
              opacity = 1,
              values = ~level,
              position = "bottomright",
              title = "Relevance Level") 
  
  harveyClassifiedMap

  