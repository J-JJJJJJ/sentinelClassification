rm(list = ls())
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(igraph)
library(data.table)
source("graphFunctions.r")

# add community information to edgelist and remove repeat edges
improveEdgelist<-function(oldEdges, clusters){
  names(clusters)<-c("node", "community")
  # add communities to edgelist
  oldEdges$community1<-left_join(oldEdges, clusters, by=c("V1"="node"))$community
  oldEdges$community2<-left_join(oldEdges, clusters, by=c("V2"="node"))$community
  
  # add inter / intra community booleans
  oldEdges<- oldEdges %>%  mutate(intercommunity =case_when((community1 == community2) ~FALSE, TRUE ~ TRUE) )
  oldEdges<- oldEdges %>%  mutate(intracommunity =case_when((community1 == community2) ~TRUE, TRUE ~ FALSE) )
  
  # remove loops
 oldEdges<-oldEdges %>% filter(V1 != V2)
  
  return(oldEdges)
  
}


# process an original GTC graph
original<-function(graphName, oldEdges, clusters){
  
  # rename columns in clusters
  names(clusters)<-c("node", "community")
  
 # add edge type, intercommunity and intracommunity to edgelist
 oldEdges<-improveEdgelist(oldEdges, clusters)
 
 
 # Create the graph
 graph<-oldEdges %>% select(V1, V2, intercommunity, intracommunity) %>% graph_from_data_frame(directed = FALSE)
 # create a data frame of the vectices (one vector)
 vertex<-graph %>% vertex_attr("name") %>% as.integer() %>%data.frame()
 names(vertex)<-"vertex"
 
  # create the community vector
  community<-left_join(vertex, clusters, by=c("vertex"="node"))$community
  
  graph<-set_vertex_attr(graph, "community", value = community)
  graph<-set_vertex_attr(graph, "graphName", value = graphName)
  graph<-set_vertex_attr(graph, "type", value = "original")
  graph<-set_vertex_attr(graph, "version", value = "")
  
  writeGraph(graph)
  return(graph)
}

# process according to HCM
HCM<-function(graphName, oldEdges, clusters, version){
  
  # rename columns in clusters
  names(clusters)<-c("node", "community")
  
  # add edge type, intercommunity and intracommunity to edgelist  
  oldEdges<-improveEdgelist(oldEdges, clusters)
  
  # split edges in inter and intracommunity and keep only the edgelist part
  inter<-oldEdges %>% dplyr::filter(intercommunity) %>% select(V1, V2, community1, community2)
  oldEdges<-oldEdges %>% dplyr::filter(intracommunity) %>% select(V1, V2)
  
  # get the half edges that need to be rearranged and rewire them to new edges
  inter<-rewire(inter, TRUE)

  
  # combine them back together and add other properties
  oldEdges<-bind_rows(oldEdges, inter)
  oldEdges<-improveEdgelist(oldEdges, clusters)
  
  #
  #  Create the graph
  #
  graph<-oldEdges %>% select(V1, V2, intercommunity, intracommunity) %>% graph_from_data_frame(directed = FALSE)
  # create a data frame of the vectices (one vector)
  vertex<-graph %>% vertex_attr("name") %>% as.integer() %>%data.frame()
  names(vertex)<-"vertex"
  
  
  # create the community vector
  community<-left_join(vertex, clusters, by=c("vertex"="node"))$community
  
  graph<-set_vertex_attr(graph, "community", value = community)
  graph<-set_vertex_attr(graph, "graphName", value = graphName)
  graph<-set_vertex_attr(graph, "type", value = "HCM")
  graph<-set_vertex_attr(graph, "version", value = version)
  
  writeGraph(graph)
  return(graph)
}

# process according to HCM*
HCMStar<-function(graphName, oldEdges, clusters, version){
  
  # rename columns in clusters
  names(clusters)<-c("node", "community")
  
  # add edge type, intercommunity and intracommunity to edgelist  
  oldEdges<-improveEdgelist(oldEdges, clusters)
  
  # split edges in inter and intracommunity and keep only the edgelist part
  inter<-oldEdges %>% dplyr::filter(intercommunity) %>% select(V1, V2 , community1, community2)
  intra<-oldEdges %>% dplyr::filter(intracommunity) %>% select(V1, V2, community1)
  oldEdges<-data.frame(V1=vector(), V2=vector())
  # get the half edges that need to be rearranged and rewire them to new edges
  inter<-rewire(inter, TRUE)
  
  cl <- makeCluster(6)
  registerDoParallel(cl)
  
  oldEdges<-foreach(i=unique(intra$community1), .combine = rbind)%dopar%{
    source("graphFunctions.r")
    temp=rewire(intra %>% filter(community1==i), FALSE)
    temp
    }
  
  stopCluster(cl)

  
  # combine them back together and add other properties
  oldEdges<-bind_rows(oldEdges, inter)
  oldEdges<-improveEdgelist(oldEdges, clusters)
  
  #
  #  Create the graph
  #
  graph<-oldEdges %>% select(V1, V2, intercommunity, intracommunity) %>% graph_from_data_frame(directed = FALSE)
  # create a data frame of the vectices (one vector)
  vertex<-graph %>% vertex_attr("name") %>% as.integer() %>%data.frame()
  names(vertex)<-"vertex"
  
  
  # create the community vector
  community<-left_join(vertex, clusters, by=c("vertex"="node"))$community
  
  graph<-set_vertex_attr(graph, "community", value = community)
  graph<-set_vertex_attr(graph, "graphName", value = graphName)
  graph<-set_vertex_attr(graph, "type", value = "HCMStar")
  graph<-set_vertex_attr(graph, "version", value = version)
  
  writeGraph(graph)
  return(graph)
}

# Write  graphs as graphML
writeGraph<-function(graph){
  name<-V(graph)$graphName[1]
  type<-V(graph)$type[1]
  version<-V(graph)$version[1]
  path<-paste0("graphs without prop/", name,"_", type, version, ".xml")
  write_graph(graph, path, format = "graphml")
}

# read the graph files and create a list of edges, clusters and louvain
importGraphFiles<-function(folderPath){
  # create file paths
  clusterPath<-paste0(folderPath, "clusters")
  edgePath<-paste0(folderPath, "edges")
  louvainPath<-paste0(folderPath, "louvain")
  
  clusters<-read.table(clusterPath, header = FALSE, sep="\t")
  edges<-read.table(edgePath)
  louvain<-read.table(louvainPath)
  list<-list(clusters, edges, louvain)
  names(list)<-c("communities", "edges", "louvain")
  return(list)
}


# import the files in the folders
dolphins<-importGraphFiles("graphs/dolphins/dolphins.")
football<-importGraphFiles("graphs/football/football.")
karate<-importGraphFiles("graphs/karate/karate.")
polbooks<-importGraphFiles("graphs/polbooks/polbooks.")

# # original graphs
original("dolphins", dolphins$edges, dolphins$communities)
original("football", football$edges, football$communities)
original("karate", karate$edges, karate$communities)
original("polbooks", polbooks$edges, polbooks$communities)

# HCM graphs
HCM("dolphins", dolphins$edges, dolphins$communities,1)
HCM("football", football$edges, football$communities,1)
HCM("karate", karate$edges, karate$communities,1)
HCM("polbooks", polbooks$edges, polbooks$communities,1)

# some more HCM
HCM("dolphins", dolphins$edges, dolphins$communities,2)
HCM("football", football$edges, football$communities,2)
HCM("karate", karate$edges, karate$communities,2)
HCM("polbooks", polbooks$edges, polbooks$communities,2)

# HCMstar graphs
HCMStar("dolphins", dolphins$edges, dolphins$communities,1)
HCMStar("football", football$edges, football$communities,1)
HCMStar("karate", karate$edges, karate$communities,1)
HCMStar("polbooks", polbooks$edges, polbooks$communities,1)
# some more HCMstar
HCMStar("dolphins", dolphins$edges, dolphins$communities,2)
HCMStar("football", football$edges, football$communities,2)
HCMStar("karate", karate$edges, karate$communities,2)
HCMStar("polbooks", polbooks$edges, polbooks$communities,2)



#
# Non- ground truth community graphs
#
processNonCommunityGraph<-function(graph){
  graph<-set_vertex_attr(graph, "community", value = "")
  graph<-set_vertex_attr(graph, "version", value = "")
  return(graph)
}
# random graphs Erdos-Renyi model, n vertices and p=2*ln(n)/n
erdosModel<-function(n){
  graph<-erdos.renyi.game(n, p=2*log(n)/n, type ="gnp", directed = FALSE,loops = FALSE)
  graph<-set_vertex_attr(graph, "graphName", value = paste0("erdosrenyi",n))
  graph<-set_vertex_attr(graph, "type", value = "erdosrenyi")
  graph<-processNonCommunityGraph(graph)
  writeGraph(graph)
  return(graph)
}



set.seed(123)
for(k in 30:65){erdosModel(k)}



#
#
#   Self-constructed training graphs
#
artFileNames<-list.files("graphs/artificial", pattern="*.csv", full.names=TRUE)
artGraphs<-artFileNames %>% lapply(fread)


processArtGraphs<-function(graphName, frame){
  
  frame<-frame %>% mutate(intercommunity=ifelse(from%/%100==to%/%100, TRUE, FALSE), intracommunity=ifelse(from%/%100==to%/%100, FALSE, TRUE))
  
  # Create the graph
  graph<-frame %>% select(from, to, intercommunity, intracommunity) %>% graph_from_data_frame(directed = FALSE)

  community<-as.numeric(V(graph)$name) %/% 100
  
  graph<-set_vertex_attr(graph, "community", value = community)
  graph<-set_vertex_attr(graph, "graphName", value = graphName)
  graph<-set_vertex_attr(graph, "type", value = "artificial")
  graph<-set_vertex_attr(graph, "version", value = "")
  
  writeGraph(graph)
  return(graph)
}


for(i in 1:18){
  name<-basename(artFileNames[i])
  name<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(name))
  processArtGraphs(name,artGraphs[[i]])
  
}








