library(igraph)
library(dplyr)
library(data.table)

setwd("C:/Users/JB/Google Drive/uni/bep/new workspace")

# turns an igraph into an edge list with community column
graphToEdgelist<-function(graph, community, k){
  

  frame<-igraph::as_data_frame(graph, what="edges")
  
  # sample vertex with lowest degree in the graph
  halfEdges<-c(frame$from, frame$to)
  endpoint<-data.frame(table(halfEdges))
  endpoint<-100* community + as.numeric(endpoint[which.min(endpoint$Freq),1])
  
  # k is the number of branches per arm
  starPoint<-ceiling(community/k)
  newEdge<-data.table(from=endpoint, to=starPoint, community=community)
  
  frame<-frame %>% mutate(from=as.numeric(from), to=as.numeric(to))
  frame<-frame %>% mutate(from=from+100*community %>% as.numeric(), to=to+100*community%>% as.numeric(), community=community%>% as.numeric())
  frame<-bind_rows(frame, newEdge)
  
  
  return(frame)
}
# create the centre as edgelist with community column
starList<-function(arms){
  frame<-data.table(from=rep(0,arms), to=1:arms, community=rep(0,arms))
  
  return(frame)
}

writeFinal<-function(frame, name){

  fwrite(frame, paste0("graphs/artificial/", name, ".csv"))
}


processArtGraphstest<-function(frame, graphName){
  
  frame<-frame %>%  mutate(intercommunity=ifelse(from%/%100==to%/%100, TRUE, FALSE), intracommunity=ifelse(from%/%100==to%/%100, FALSE, TRUE))
  
  # Create the graph
  graph<-frame %>% select(from, to, intercommunity, intracommunity) %>% graph_from_data_frame(directed = FALSE)
  
  community<-as.numeric(V(graph)$name) %/% 100
  
  graph<-set_vertex_attr(graph, "community", value = community)
  graph<-set_vertex_attr(graph, "graphName", value = graphName)

  path<-paste0("test graphs/", graphName, ".xml")
  write_graph(graph, path, format = "graphml")
  
  return(graph)
}


full<-function(n, community, k=2){
  edgelist<-make_full_graph(n, directed=FALSE) %>% graphToEdgelist(community, k)
  return(edgelist)
}
bipartite<-function(m,n, community, k=2){
edgelist<-make_full_bipartite_graph(m,n, directed=FALSE) %>% graphToEdgelist(community, k)
  return(edgelist)
}


# add item to list
addTo<-function(item, list){
  list[[length(list)+1]]<-item
  return(list)
  
}



#
#
# Test graphs
#
#

test1<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-6
  b<-3
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-full(10,b*(arms-1)+1, b)    %>% addTo(branchlist)
    branchlist<-full(10,b*(arms-1)+2, b)    %>% addTo(branchlist)
    branchlist<-full(10,b*(arms-1)+3, b)    %>% addTo(branchlist)  
    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test1") 
  processArtGraphstest(frame, "test1")
  return(frame)
}
test2<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-7
  b<-6
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-full(15,b*(arms-1)+1, b)    %>% addTo(branchlist)
    branchlist<-full(15,b*(arms-1)+2, b)    %>% addTo(branchlist)
    branchlist<-full(15,b*(arms-1)+3, b)    %>% addTo(branchlist)
    branchlist<-full(15,b*(arms-1)+4, b)    %>% addTo(branchlist)
    branchlist<-full(15,b*(arms-1)+5, b)    %>% addTo(branchlist)
    branchlist<-full(15,b*(arms-1)+6, b)    %>% addTo(branchlist)
    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test2")
  processArtGraphstest(frame, "test2")
  return(frame)
}
test3<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-8
  b<-7
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-bipartite(8,3,b*(arms-1)+1, b)    %>% addTo(branchlist)
    branchlist<-bipartite(8,3,b*(arms-1)+2, b)    %>% addTo(branchlist)  
    branchlist<-bipartite(8,3,b*(arms-1)+3, b)    %>% addTo(branchlist)
    branchlist<-bipartite(18,2,b*(arms-1)+4, b)    %>% addTo(branchlist) 
    branchlist<-bipartite(18,2,b*(arms-1)+5, b)    %>% addTo(branchlist)  
    branchlist<-bipartite(18,2,b*(arms-1)+6, b)    %>% addTo(branchlist)
    branchlist<-bipartite(18,2,b*(arms-1)+7, b)    %>% addTo(branchlist) 

  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test3")
  processArtGraphstest(frame, "test3")
  
  return(frame)
}
test4<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-10
  b<-4
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-bipartite(10,4,b*(arms-1)+1, b)    %>% addTo(branchlist)
    branchlist<-bipartite(10,4,b*(arms-1)+2, b)    %>% addTo(branchlist)  
    branchlist<-full(6,b*(arms-1)+3, b)    %>% addTo(branchlist)
    branchlist<-full(6,b*(arms-1)+4, b)    %>% addTo(branchlist) 

    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test4")
  processArtGraphstest(frame, "test4")
  
  return(frame)
}
test5<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-12
  b<-4
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-bipartite(6,3,b*(arms-1)+1, b)    %>% addTo(branchlist) 
    branchlist<-bipartite(6,3,b*(arms-1)+2, b)    %>% addTo(branchlist)  
    branchlist<-bipartite(6,3,b*(arms-1)+3, b)    %>% addTo(branchlist)
    branchlist<-bipartite(6,3,b*(arms-1)+4, b)    %>% addTo(branchlist) 
    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test5")
  processArtGraphstest(frame, "test5")
  
  return(frame)
}
test6<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-15
  b<-6
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-bipartite(10,10,b*(arms-1)+1, b)    %>% addTo(branchlist) 
    branchlist<-bipartite(10,10,b*(arms-1)+2, b)    %>% addTo(branchlist)  
    branchlist<-bipartite(10,10,b*(arms-1)+3, b)    %>% addTo(branchlist)
    branchlist<-full(8,b*(arms-1)+4, b)    %>% addTo(branchlist)
    branchlist<-full(8,b*(arms-1)+5, b)    %>% addTo(branchlist)  
    branchlist<-full(8,b*(arms-1)+6, b)    %>% addTo(branchlist)

    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test6")
  processArtGraphstest(frame, "test6")
  
  return(frame)
}
test7<-function(void){
  
  # a is the number of arms, b is the number of branches per arm
  a<-40
  b<-2
  star<-starList(a)
  
  branchlist<-list()
  
  for(arms in 1:a){
    branchlist<-full(5,b*(arms-1)+1, b)    %>% addTo(branchlist)
    branchlist<-full(5,b*(arms-1)+2, b)    %>% addTo(branchlist)  
    
  }
  
  frame<-rbindlist(branchlist) %>% bind_rows(star)
  frame<-frame %>% mutate(graphName="test7")
  processArtGraphstest(frame, "test7")
  
  return(frame)
}




a<-test1(1)
b<-test2(1)
c<-test3(1)
e<-test4(1)
f<-test5(1)
g<-test6(1)
h<-test7(1)
#
#
#   Training graphs
#
#

art1<-function(void){
  star<-starList(6)
  b11<-full(5,1)
  b12<-full(5,2)
  b21<-full(5,3)
  b22<-full(5,4)
  b31<-full(5,5)
  b32<-full(5,6)
  b41<-full(5,7)
  b42<-full(5,8)
  b51<-full(5,9)
  b52<-full(5,10)
  b61<-full(5,11)
  b62<-full(5,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art1")
  writeFinal(frame, "art1")
  return(frame)
}
art2<-function(void){
  star<-starList(6)
  b11<-full(6,1)
  b12<-full(6,2)
  b21<-full(6,3)
  b22<-full(6,4)
  b31<-full(6,5)
  b32<-full(6,6)
  b41<-full(6,7)
  b42<-full(6,8)
  b51<-full(6,9)
  b52<-full(6,10)
  b61<-full(6,11)
  b62<-full(6,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art2")
  writeFinal(frame, "art2")
  
  return(frame)
}
art3<-function(void){
  star<-starList(6)
  b11<-full(7,1)
  b12<-full(7,2)
  b21<-full(7,3)
  b22<-full(7,4)
  b31<-full(7,5)
  b32<-full(7,6)
  b41<-full(7,7)
  b42<-full(7,8)
  b51<-full(7,9)
  b52<-full(7,10)
  b61<-full(7,11)
  b62<-full(7,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art3")
  writeFinal(frame, "art3")
  return(frame)
}
art4<-function(void){
  star<-starList(6)
  b11<-full(8,1)
  b12<-full(8,2)
  b21<-full(8,3)
  b22<-full(8,4)
  b31<-full(8,5)
  b32<-full(8,6)
  b41<-full(8,7)
  b42<-full(8,8)
  b51<-full(8,9)
  b52<-full(8,10)
  b61<-full(8,11)
  b62<-full(8,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art4")
  writeFinal(frame, "art4")
  return(frame)
}
art5<-function(void){
  star<-starList(6)
  b11<-full(5,1)
  b12<-full(8,2)
  b21<-full(5,3)
  b22<-full(8,4)
  b31<-full(5,5)
  b32<-full(8,6)
  b41<-full(5,7)
  b42<-full(8,8)
  b51<-full(5,9)
  b52<-full(8,10)
  b61<-full(5,11)
  b62<-full(8,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art5")
  writeFinal(frame, "art5")
  return(frame)
}
art6<-function(void){
  star<-starList(6)
  b11<-full(5,1)
  b12<-full(10,2)
  b21<-full(6,3)
  b22<-full(9,4)
  b31<-full(7,5)
  b32<-full(8,6)
  b41<-full(8,7)
  b42<-full(7,8)
  b51<-full(9,9)
  b52<-full(6,10)
  b61<-full(10,11)
  b62<-full(5,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art6")
  writeFinal(frame, "art6")
  return(frame)
}
art7<-function(void){
  star<-starList(6)
  b11<-full(5,1)
  b12<-full(10,2)
  b21<-full(5,3)
  b22<-full(10,4)
  b31<-full(5,5)
  b32<-full(10,6)
  b41<-full(5,7)
  b42<-full(10,8)
  b51<-full(5,9)
  b52<-full(10,10)
  b61<-full(5,11)
  b62<-full(10,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art7")
  writeFinal(frame, "art7")
  return(frame)
}
art8<-function(void){
  star<-starList(6)
  b11<-bipartite(4,5,1)
  b12<-bipartite(6,3,2)
  b21<-bipartite(7,2,3)
  b22<-bipartite(3,4,4)
  b31<-bipartite(8,1,5)
  b32<-bipartite(4,2,6)
  b41<-bipartite(4,5,7)
  b42<-bipartite(3,7,8)
  b51<-full(6,9)
  b52<-full(8,10)
  b61<-full(5,11)
  b62<-full(10,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art8")
  writeFinal(frame, "art8")
  return(frame)
}
art9<-function(void){
  star<-starList(6)
  b11<-bipartite(1,3,1)
  b12<-bipartite(1,3,2)
  b21<-bipartite(2,2,3)
  b22<-bipartite(2,2,4)
  b31<-bipartite(2,3,5)
  b32<-bipartite(2,3,6)
  b41<-bipartite(4,1,7)
  b42<-bipartite(3,1,8)
  b51<-full(7,9)
  b52<-full(5,10)
  b61<-full(5,11)
  b62<-full(6,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art9")
  writeFinal(frame, "art9")
  return(frame)
}
art10<-function(void){
  star<-starList(6)
  b11<-bipartite(1,6,1)
  b12<-bipartite(7,2,2)
  b21<-bipartite(7,2,3)
  b22<-bipartite(4,3,4)
  b31<-bipartite(8,1,5)
  b32<-bipartite(9,1,6)
  b41<-bipartite(4,2,7)
  b42<-bipartite(2,2,8)
  b51<-full(3,9)
  b52<-full(11,10)
  b61<-full(7,11)
  b62<-full(5,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art10")
  writeFinal(frame, "art10")
  return(frame)
}
art11<-function(void){
  star<-starList(6)
  b11<-bipartite(1,1,1)
  b12<-bipartite(7,7,2)
  b21<-bipartite(2,2,3)
  b22<-bipartite(6,6,4)
  b31<-bipartite(3,3,5)
  b32<-bipartite(5,5,6)
  b41<-bipartite(4,4,7)
  b42<-bipartite(4,4,8)
  b51<-full(14,9)
  b52<-full(5,10)
  b61<-full(10,11)
  b62<-full(6,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art11")
  writeFinal(frame, "art11")
  return(frame)
}
art12<-function(void){
  star<-starList(6)
  b11<-bipartite(4,5,1)
  b12<-bipartite(6,3,2)
  b21<-bipartite(7,2,3)
  b22<-bipartite(3,4,4)
  b31<-bipartite(8,1,5)
  b32<-bipartite(4,2,6)
  b41<-bipartite(4,5,7)
  b42<-bipartite(3,7,8)
  b51<-full(6,9)
  b52<-full(8,10)
  b61<-full(5,11)
  b62<-full(10,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art12")
  writeFinal(frame, "art12")
  return(frame)
}
art13<-function(void){
  star<-starList(6)
  b11<-bipartite(1,9,1)
  b12<-bipartite(3,5,2)
  b21<-bipartite(1,9,3)
  b22<-bipartite(3,5,4)
  b31<-bipartite(1,9,5)
  b32<-bipartite(3,5,6)
  b41<-bipartite(1,9,7)
  b42<-bipartite(3,5,8)
  b51<-bipartite(1,9,9)
  b52<-bipartite(3,5,10)
  b61<-bipartite(1,9,11)
  b62<-bipartite(3,5,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art13")
  writeFinal(frame, "art13")
  return(frame)
}
art14<-function(void){
  star<-starList(6)
  b11<-bipartite(3,7,1)
  b12<-bipartite(2,4,2)
  b21<-bipartite(3,7,3)
  b22<-bipartite(2,4,4)
  b31<-bipartite(3,7,5)
  b32<-bipartite(2,4,6)
  b41<-bipartite(3,7,7)
  b42<-bipartite(2,4,8)
  b51<-bipartite(3,7,9)
  b52<-bipartite(2,4,10)
  b61<-bipartite(3,7,11)
  b62<-bipartite(2,4,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art14")
  writeFinal(frame, "art14")
  return(frame)
}
art15<-function(void){
  star<-starList(6)
  b11<-bipartite(1,8,1)
  b12<-bipartite(4,2,2)
  b21<-bipartite(1,8,3)
  b22<-bipartite(4,2,4)
  b31<-bipartite(1,8,5)
  b32<-bipartite(4,2,6)
  b41<-bipartite(1,8,7)
  b42<-bipartite(4,2,8)
  b51<-bipartite(1,8,9)
  b52<-bipartite(4,2,10)
  b61<-bipartite(1,8,11)
  b62<-bipartite(4,2,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art15")
  writeFinal(frame, "art15")
  return(frame)
}
art16<-function(void){
  star<-starList(6)
  b11<-bipartite(2,2,1)
  b12<-bipartite(2,2,2)
  b21<-bipartite(2,2,3)
  b22<-bipartite(2,2,4)
  b31<-bipartite(2,2,5)
  b32<-bipartite(2,2,6)
  b41<-bipartite(2,2,7)
  b42<-bipartite(2,2,8)
  b51<-bipartite(2,2,9)
  b52<-bipartite(2,2,10)
  b61<-bipartite(2,2,11)
  b62<-bipartite(2,2,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art16")
  writeFinal(frame, "art16")
  return(frame)
}
art17<-function(void){
  star<-starList(6)
  b11<-bipartite(1,4,1)
  b12<-bipartite(1,4,2)
  b21<-bipartite(1,4,3)
  b22<-bipartite(1,4,4)
  b31<-bipartite(1,4,5)
  b32<-bipartite(1,4,6)
  b41<-bipartite(1,4,7)
  b42<-bipartite(1,4,8)
  b51<-bipartite(1,4,9)
  b52<-bipartite(1,4,10)
  b61<-bipartite(1,4,11)
  b62<-bipartite(1,4,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art17")
  writeFinal(frame, "art17")
  return(frame)
}
art18<-function(void){
  star<-starList(6)
  b11<-bipartite(3,3,1)
  b12<-bipartite(4,4,2)
  b21<-bipartite(3,3,3)
  b22<-bipartite(4,4,4)
  b31<-bipartite(3,3,5)
  b32<-bipartite(4,4,6)
  b41<-bipartite(3,3,7)
  b42<-bipartite(4,4,8)
  b51<-bipartite(3,3,9)
  b52<-bipartite(4,4,10)
  b61<-bipartite(3,3,11)
  b62<-bipartite(4,4,12)
  frame<-bind_rows(star, b11, b12, b21, b22, b31, b32, b41, b42, b51, b52, b61, b62)
  frame<-frame %>% mutate(graphName="art18")
  writeFinal(frame, "art18")
  return(frame)
}


art1()
art2()
art3()
art4()
art5()
art6()
art7()
art8()
art9()
art10()
art11()
art12()
art13()
art14()
art15()
art16()
art17()
art18()
art19()







