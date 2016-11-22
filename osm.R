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
load_map <- function(load_example=TRUE,example="Versailles",coord_list){
  #time_start<-proc.time()
  if(missing(coord_list)) {
    frame<- frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1309000, top = 48.8069000)
  } else {
    frame <- corner_bbox(left = as.numeric(coord_list$left), bottom = as.numeric(coord_list$bottom), right = as.numeric(coord_list$right), top = as.numeric(coord_list$top))
  }
  #print(frame)
  if(load_example){
    #source_osm <- osmsource_file("data/example2.osm")
    source_osm <- osmsource_file( paste("data/",make.names(example),".osm",sep = "") )
    #frame <- corner_bbox(left = 2.1191000, bottom = 48.7987000, right = 2.1309000, top = 48.8069000) #example 2 smaller
    map_osm <- get_osm(x = frame, source = source_osm)
    map_osm_result<-map_osm 
  } else{
    #put some other things here to install via api
    print("dowloading specified map")
    map_osm <- get_osm(x = frame, source = osmsource_api() )
    map_osm_result<-map_osm 
  }
  #print("Loading has been completed in seconds:")
  #print(proc.time()-time_start)
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
match_entities<-function(df_tags, df_k_mapping, building_density_threshold=c(0.2,0.6,1) ){ #df_kv_mapping
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
  #building dens is the percentage of building among all unique osm id
  building_density<-class_summary$frequency[class_summary$k_base=="building"]/nb_of_unique_osm_id
  density_options=c("low","medium","high")
  settlement_options=c("rural","suburban","urban")
  current_density<-density_options[min(which(building_density<building_density_threshold))]
  current_settlement<-settlement_options[min(which(building_density<building_density_threshold))]
  settlement=list(building_density=building_density, settlement_type=current_settlement)
  result <- list(nb_of_tags = nb_of_tags, nb_of_unique_osm_id = nb_of_unique_osm_id, 
                 class_summary = class_summary, prop_summary = prop_summary,settlement=settlement)
  return(result)
}

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

filter_entities<-function(df_tags, df_attrs, filter_list= c("amenity", "shop", "tourism")){ 
  #df_tags$k_base <- gsub(":.*", "",df_tags$k)
  names(df_tags)<-c("id","entity","type")
  occurence<-table(df_tags$id, dnn = c("id"))
  df_tags<-merge(df_tags,occurence)
  filtered<-df_tags[gsub(":.*", "",df_tags$entity) %in% filter_list,]
  #names(df_tags)<- c("id","entity","type")
  #filtered<- filtered[order(filtered$Freq,decreasing = FALSE),]
  filtered<-merge(filtered, df_tags[df_tags$entity=="name",names(df_tags)%in% c("id", "type")],by="id", all.x=TRUE)
  names(filtered)[ncol(filtered)]<-"name"
  filtered<-filtered[!is.na(filtered$name),]
  filtered<-merge(filtered, df_tags[df_tags$entity=="website",names(df_tags)%in% c("id", "type")],by="id", all.x=TRUE)
  names(filtered)[ncol(filtered)]<-"website"
  df_attrs<-df_attrs[df_attrs$id %in% filtered$id,names(df_attrs)%in% c("id","lat","lon")]
  filtered<- merge(filtered, df_attrs)
  names(filtered)<-gsub("\\..*","",names(filtered))
  filtered$detail<-paste(filtered$name, " (",filtered$type,")",sep = "")
  filtered<-filtered[order(filtered$Freq,decreasing = TRUE),] 
  return(filtered)
}


#draw_bar_chart(matched$class_summary)
#draw_bar_chart(matched$prop_summary)

#df_summary<-matched$class_summary
#map_osm<- load_map()
#df_tags<-map_osm$nodes$tags
#df_attrs<-map_osm$nodes$attrs
#df_tags<-map_osm$ways$tags

#matched_nodes<-match_entities(df_tags = df_tags, df_k_mapping = df_k_mapping)
#matched_nodes

#filtered<-filter_entities(df_tags = df_tags,df_attrs =df_attrs )
#leaflet(data = filtered) %>% addTiles() %>% addMarkers(lng = ~lon, lat = ~lat, popup = ~detail)
extract_contribution <- function(df_attrs, graph=TRUE, nbofContr=20){
  days<-weekdays(df_attrs$timestamp)
  days<-ordered(days, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  days_freq<-as.data.frame(table(days))
  hours<-factor(format(df_attrs$timestamp, "%H"), levels = c(paste0(0, 0:9), 10:23))
  hours_freq<-as.data.frame(table(hours))
  users<-as.character(df_attrs$user)
  contributors_freq<-as.data.frame(table(users))
  contributors_freq<-contributors_freq[order(contributors_freq$Freq,decreasing = TRUE),]
  #contributors_freq<-contributors_freq[1:10,]
  if(graph) {
    days_graph<-ggplot(data = days_freq, aes(x=days, y=Freq, fill=days))+geom_bar(stat = "identity")+ scale_fill_manual(values = rainbow_hcl(7) )+ 
      guides(fill=FALSE)+labs(title = "Contribution according to days")
    hours_graph<-ggplot(data = hours_freq, aes(x=hours, y=Freq, fill=hours))+geom_bar(stat = "identity")+ scale_fill_manual(values = rainbow_hcl(24) )+ 
      guides(fill=FALSE)+labs(title = "Contribution according to hours")
    contributors_graph<-ggplot(data = contributors_freq[1:nbofContr,], aes(x=reorder(users, Freq, function(x) - sum(x) ), y=Freq, fill=users))+geom_bar(stat = "identity")+ scale_fill_manual(values = rainbow_hcl(nbofContr) )+ 
      guides(fill=FALSE)+labs(title = "Contribution according to the most active users", x= "users")
    result<-list(days_freq=days_freq, hours_freq=hours_freq, days_graph=days_graph,contributors_graph=contributors_graph, hours_graph=hours_graph)
  } else{
    result<-list(days_freq=days_freq, hours_freq=hours_freq)
  }
  return(result)
} 

