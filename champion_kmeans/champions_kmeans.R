###
###  Queries Riot Games API and performs a clustering analysis on in-game characters.
###
###  Note 1: you'll need to check for most recent version here: https://developer.riotgames.com/docs/lol#data-dragon_champions
###
###  No API key required.
###

###
###  Contact: Peter Adam Albrecht
###
###  E-mail: PALBRCHT@GMAIL.COM
###

adarooski_get_champion_api_data <- function() {
  
  require(RJSONIO)
  
  champion_static_data <- fromJSON(paste("http://ddragon.leagueoflegends.com/cdn/10.23.1/data/en_US/champion.json"))
  
  x <- length(champion_static_data$data)
  
  for (z in 1:x){
    
    temp_json <- head(data.frame(champion_static_data$data[z]),1)
    
    temp_data <- temp_json[2:4]
    
    colnames(temp_data)[1:3] <- c('id' , 'champ_id' , 'champ_name')
    
    if (exists(("compiled_static_data"))) {compiled_static_data <- rbind(compiled_static_data, temp_data)} else {compiled_static_data <- temp_data}
    
  }
  return(compiled_static_data)
}

adarooski_get_champion_stats <- function() {
  
  require(RJSONIO)
  require(tidyverse)
  
  champion_static_data <- fromJSON(paste("http://ddragon.leagueoflegends.com/cdn/10.23.1/data/en_US/champion.json"))
  
  x <- length(champion_static_data$data)

  for (z in 1:x){
    
    temp_champion_json <- champion_static_data$data[z]
    
    temp_data <- data.frame(temp_champion_json[[1]]$stats)
    
    champion_name <- temp_champion_json[[1]]$name
    
    temp_data$names <- rownames(temp_data)
    
    rownames(temp_data) <- 1:nrow(temp_data)
    
    colnames(temp_data)[1] <- 'value'
    
    colnames(temp_data)[2] <- 'stat_name'
    
    temp_data <- temp_data %>%
      spread(stat_name , value) %>%
      mutate(champ_name = champion_name)
    
    if (exists(("compiled_champ_data"))) {compiled_champ_data <- rbind(compiled_champ_data, temp_data)} else {compiled_champ_data <- temp_data}
  }
    
  return(compiled_champ_data)
    
}

adarooski_get_champion_cluster_data <- function() {
  
  require(Rtsne)
  
  training_set <- adarooski_get_champion_stats()
  
  set.seed(7788)
  
  tsne_output <- Rtsne(training_set, 
                dims = 2, 
                perplexity=30, 
                verbose=TRUE, 
                max_iter = 500)
  
  
  tsne_df <- data.frame(tsne_output$Y)
  
  k2 <- kmeans(tsne_df, centers = 6)
  
  cluster_data <- data.frame(k2$cluster)
  
  cluster_data <- cbind(cluster_data, tsne_df)
  
  return(cluster_data)
  
}

adarooski_compile_champion_clustering_data <- function(){
  
  require(tidyverse)
  
  champion_linking_data <- adarooski_get_champion_api_data()
  
  champion_stats <- adarooski_get_champion_stats()
  
  cluster_data <- adarooski_get_champion_cluster_data()
  
  temp_df <- cbind(champion_stats , cluster_data)
  
  ready_df <- left_join(temp_df , champion_linking_data , by = 'champ_name')
  
}

adarooski_plot_champion_clustering_data <- function(){
  
  require(ggplot2)
  
  plot_data <- adarooski_compile_champion_clustering_data()
  
  plot_data$k2.cluster <- as.factor(plot_data$k2.cluster)
  
  gg_output <- ggplot(data = plot_data, aes(x = X1 , y = X2 , color = k2.cluster , label = id)) +
    theme_bw() +
    geom_text( size = 3 , angle = 45, position=position_jitter(width=.5,height=.5)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank() ,
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank() ,
          legend.position = "none" , 
          panel.background = element_rect(fill = 'black')) 
  
  return(gg_output)
  
}

###
###  Functions you can call.
###

adarooski_get_champion_api_data()

adarooski_get_champion_stats()

adarooski_get_champion_cluster_data()

adarooski_compile_champion_clustering_data()  ##  Will produce a full data frame for you with stats and compiled metrics.

adarooski_plot_champion_clustering_data()  ## Will produce a crisp ggplot of the clusters.

###
###  END OF SCRIPT!
###
