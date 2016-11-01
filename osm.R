library(osmar)
library(ggplot2)
#change directory which point osm directory
#setwd(dir = "C:/Users/han/Desktop/project/osm")
df_k_mapping <- read.delim("data/k-mapping.txt",header=TRUE,stringsAsFactors = FALSE, encoding = "UTF-8")
df_kv_mapping <- read.delim("data/kv-mapping.txt",header=TRUE,stringsAsFactors = FALSE, encoding = "UTF-8")



#give the source, for now it's local file which was downloaded from OpenStreetMap
source_osm <- osmsource_file("data/example.osm")
#define the frame which limits the map for the example long lat values are as given:
frame <- corner_bbox(left = 2.3381000, bottom = 48.8480000, right = 2.3599000, top = 48.8562000)
#read osm map // might take a while
map_osm <- get_osm(x = frame, source = source_osm)

levels(map_osm$nodes$tags$k) #k=key v=value
summary(map_osm$nodes)
summary(map_osm$ways)

node_tags <- map_osm$nodes$tags # nrow= 16858 and 5549 unique osm id
way_tags <- map_osm$ways$tags #nrow=12379 and 4199 unique osm id

df_node_tags_k <- as.data.frame(table(node_tags$k))
df_node_tags_k <- df_node_tags_k[order(df_node_tags_k$Freq, decreasing = TRUE),]
#plot barchart of 10 most frequent node tag
ggplot(data = df_node_tags_k[1:10,]) + geom_bar(aes(x=Var1, y = Freq), stat = "identity")
#almost all the nodes have sources! also not easy to figure out type of a node 


a<-merge(x = node_tags, y = df_k_mapping, by = c("k", "k"), sort = TRUE, all.x = TRUE)
a<-a[order(a[,2]),]

df_tags<-node_tags
match_entities<-function(df_tags, df_k_mapping ){ #df_kv_mapping
  k_base <- gsub(":.*", "",df_tags$k)
  df_tags <-  data.frame(df_tags, k_base)
  df_k_mapping$type <- "Class"
  k_base <- unique(k_base)
  merged<-merge(x = data.frame(k=k_base), y = df_k_mapping, by = c("k", "k"), all.x = TRUE)
  merged[is.na(merged$type),4] <- "property"
  df_tags <- merge(x = df_tags, y = merged, by.x = "k_base", by.y = "k", suffixes =c("","") )
  df_tags <- df_tags[c(2:4,1,5:7)]
  a <- aggregate( id ~ k_base + property + object +type, df_tags,FUN = length)
  a<-aggregate( id ~ k_base, df_tags,FUN = length)
}

#write.csv(df_tags, "tags.csv")


