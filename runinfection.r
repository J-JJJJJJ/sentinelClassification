get# multicore support and other packages 
library(foreach)
library(doParallel)
library(data.table)
library(plyr)
library(dplyr)
library(igraph)
source("infection.r")


monteCarloSirListCont<-function(graph, nRuns){
  infectedGraphs<-list()
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  infectedGraphs1 <- foreach(i= 1:nRuns) %dopar% {
    # add infection functions to cluster environment
    source("infection.r")
    # add tables to tableList  
    temp = runContSIR(graph, 0.5) 
    temp
    
  }
  
  infectedGraphs2 <- foreach(i= 1:nRuns) %dopar% {
    # add infection functions to cluster environment
    source("infection.r")
    # add tables to tableList  
    temp = runContSIR(graph, 1) 
    temp
    
  }
  
  infectedGraphs3 <- foreach(i= 1:nRuns) %dopar% {
    # add infection functions to cluster environment
    source("infection.r")
    # add tables to tableList  
    temp = runContSIR(graph, 5) 
    temp
    
  }
  
  
  stopCluster(cl)
  
  # return average
  # tableList<-rbindlist(tableList)[,lapply(.SD,mean), list(id, graphName)]
  infectedGraphs<-c(infectedGraphs1, infectedGraphs2, infectedGraphs3)
  tableList<-ldply(infectedGraphs, igraph::as_data_frame, what="vertices")
  
  
  return(tableList)
  
}



# read the graphs
# get the file names
graphs<-list.files("graphs with prop/", pattern="*.xml", full.names=TRUE)
# read them as igraph objects and put them in a list
graphList<-lapply(graphs, read_graph, format="graphml")


# for each graph, run the infections
for(g in graphList){

    n<-500
  infectionTable<-monteCarloSirListCont(g, n)
  
  # remove columns that are no longer interesting
  dropColumns<-c("id", "community", "currentState", "wasInfected" )
  infectionTable<-infectionTable %>% dplyr::select(-dplyr::one_of(dropColumns)) %>% mutate(Iduration=SIduration-Sduration)
  
  # Export the table with all results
  name<-V(g)$graphName[1]
  type<-V(g)$type[1]
  version<-V(g)$version[1]
  path<-paste0("fullinfection/", name,"_", type, version, ".csv")
  fwrite(infectionTable, file=path, append = FALSE, quote = "auto", eol="\n", nThread =6)
  
  
  # remove columns that are no longer interesting
  dropColumns<-c("SIduration", "maxTime", "percentageInfected", "Iduration")
  infectionTable<-infectionTable %>% dplyr::select(-dplyr::one_of(dropColumns))
  
  infectionTable<-infectionTable %>% 
    group_by_at(names(infectionTable)[-grep("Sduration", names(infectionTable))]) %>% 
    dplyr::summarise(sentinel=Sduration %>% mean() %>% round(5), sentinelVar=var(Sduration)%>%round(5))
  
  
  
  # Export the graph
  path<-paste0("infectionresult/", name,"_", type, version, ".csv")
  
  fwrite(infectionTable, file=path, append = FALSE, quote = "auto", eol="\n", nThread = 6)
}

