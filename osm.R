library(osmar)
library(ggplot2)
library(colorspace)
#change directory which point osm directory
#setwd(dir = "C:/Users/han/Desktop/project/osm")
df_k_mapping <- read.delim("data/k-mapping.txt",header=TRUE,stringsAsFactors = FALSE, encoding = "UTF-8")
#df_kv_mapping <- read.delim("data/kv-mapping.txt",header=TRUE,stringsAsFactors = FALSE, encoding = "UTF-8")



#give the source, for now it's local file which was downloaded from OpenStreetMap
#source_osm <- osmsource_file("data/example2.osm")
#define the frame which limits the map for the example long lat values are as given:
#frame <- corner_bbox(left = 2.3381000, bottom = 48.8480000, right = 2.3599000, top = 48.8562000) #example 1
#frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1409000, top = 48.8069000) #example 2
#frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1309000, top = 48.8069000) #example 2 smaller
#read osm map // might take a while
#map_osm <- get_osm(x = frame, source = source_osm)
#source_osm <- osmsource_file("data/example2.osm")
#frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1309000, top = 48.8069000) #example 2 smaller
#map_osm <- get_osm(x = frame, source = source_osm)
load_map <- function(load_example=TRUE,coord_list){
  time_start<-proc.time()
  frame <- corner_bbox(left = as.numeric(coord_list$left), bottom = as.numeric(coord_list$bottom), right = as.numeric(coord_list$right), top = as.numeric(coord_list$top)) #example 2 smaller
  print(frame)
  if(load_example){
    source_osm <- osmsource_file("data/example2.osm")
    #frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1309000, top = 48.8069000) #example 2 smaller
    map_osm <- get_osm(x = frame, source = source_osm)
    map_osm_result<-map_osm 
  } else{
    #put some other things here to install via api
    map_osm <- get_osm(x = frame, source = osmsource_api() )
    map_osm_result<-map_osm 
  }
  print("Loading has been completed in seconds:")
  print(proc.time()-time_start)
  return(map_osm_result)
}


#levels(map_osm$nodes$tags$k) #k=key v=value
#summary(map_osm$nodes)
#summary(map_osm$ways)

#node_tags <- map_osm$nodes$tags # nrow= 16858 and 5549 unique osm id
#way_tags <- map_osm$ways$tags #nrow=12379 and 4199 unique osm id

#df_node_tags_k <- as.data.frame(table(node_tags$k))
#df_node_tags_k <- df_node_tags_k[order(df_node_tags_k$Freq, decreasing = TRUE),]
#plot barchart of 10 most frequent node tag
#ggplot(data = df_node_tags_k[1:10,]) + geom_bar(aes(x=Var1, y = Freq), stat = "identity")
#almost all the nodes have sources! also not easy to figure out type of a node 


#a<-merge(x = node_tags, y = df_k_mapping, by = c("k", "k"), sort = TRUE, all.x = TRUE)
#a<-a[order(a[,2]),]

#df_tags<-node_tags
match_entities<-function(df_tags, df_k_mapping ){ #df_kv_mapping
  nb_of_tags <- nrow(df_tags)
  nb_of_unique_osm_id <- length(unique(df_tags$id))
  k_base <- gsub(":.*", "",df_tags$k)
  df_tags <-  data.frame(df_tags, k_base)
  df_k_mapping$type <- "Class"
  k_base <- unique(k_base)
  merged<-merge(x = data.frame(k=k_base), y = df_k_mapping, by = c("k", "k"), all.x = TRUE)
  merged[is.na(merged$type),4] <- "property"
  df_tags <- merge(x = df_tags, y = merged, by.x = "k_base", by.y = "k", suffixes =c("","") )
  df_tags <- df_tags[c(2:4,1,5:7)]
  #a <- aggregate( id ~ k_base + property + object +type, df_tags,FUN = length)
  class_summary<-aggregate( id ~ k_base + property + object +type, df_tags,FUN = length)
  names(class_summary)[ncol(class_summary)] <- "frequency"
  class_summary <- class_summary[order(class_summary$frequency, decreasing = TRUE),]
  prop_summary<-aggregate( id ~ k_base + type, df_tags,FUN = length)
  names(prop_summary)[ncol(prop_summary)] <- "frequency"
  prop_summary<-prop_summary[prop_summary$type=="property",]
  prop_summary <- prop_summary[order(prop_summary$frequency, decreasing = TRUE),]
  result <- list(nb_of_tags = nb_of_tags, nb_of_unique_osm_id = nb_of_unique_osm_id, 
                 class_summary = class_summary, prop_summary = prop_summary)
  return(result)
}

#create pie chart to see dist. of classes in map
#ggplot(data = matched$class_summary, aes("", y = frequency, fill=k_base) ) + 
#  geom_bar(stat = "identity") + 
#  coord_polar("y", start=0) +
#  scale_fill_manual(values = rainbow_hcl(20) )#rainbow(n=20, s = 0.5, alpha = 0.8))


#df_summary<-matched$class_summary
draw_bar_chart <- function(df_summary, nbOfRecords=min(12, nrow(df_summary)), chart_title="Summary of OSM Tags" ){
  #is it class or prop
  type=unique(df_summary$type)[1]
  names(df_summary)[which(names(df_summary)=="k_base")]<-"OSM_Tag"
  df_summary<- df_summary[order(df_summary$frequency,decreasing = TRUE),]
  df_summary<-df_summary[1:max(nbOfRecords),]
  bar_chart <- ggplot(data = df_summary, aes(x=reorder(OSM_Tag, frequency, function(x) - sum(x) ), y = frequency, fill=OSM_Tag) ) + 
    geom_bar(stat = "identity")+
    scale_fill_manual(values = rainbow_hcl(nbOfRecords) )+
    labs(title = chart_title , x= type)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
  return(bar_chart)
}

#draw_bar_chart(matched$class_summary)
#draw_bar_chart(matched$prop_summary)


