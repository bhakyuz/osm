library(osmar)
library(ggplot2)
#change directory which point osm directory
#setwd(dir = "C:/Users/han/Desktop/project/osm")

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



