library(igraph)
# infect node by id
infectIDCont <- function(graph, idVector) {
  for (i in idVector) {
    # infect nodes in vector
    V(graph)[i]$currentState <- 1
    V(graph)[i]$wasInfected <- TRUE
  }
  return(graph)
}

# randomly infect nodes
randomlyInfectCont <- function(graph, number) {
  nodes <- as.vector(sample(V(graph), number, replace = FALSE))
  graph <- infectIDCont(graph, nodes)
  return(graph)
}

# run the infection
runContSIR <- function(graph, tau) {
  graph <- set_vertex_attr(graph, "tau", value = tau)
  graph <- set_vertex_attr(graph, "currentState", value = 0)
  graph <- set_vertex_attr(graph, "wasInfected", value = FALSE)
  graph <- set_vertex_attr(graph, "Sduration", value = 0)
  graph <- set_vertex_attr(graph, "SIduration", value = 0)
  graph <- randomlyInfectCont(graph, 1)
  
  # start with t=0
  t <- 0
  
  # while there are infected nodes
  while (length(V(graph)[V(graph)$currentState == 1]) > 0) {
    # neighbourList consists of nodes which may be infected in this turn
    # nodes occur multiple times if they share multiple edges with infected nodes
    neighbourList <-
      unlist(adjacent_vertices(graph, V(graph)[V(graph)$currentState == 1]), FALSE, FALSE)
    neighbourList <-
      neighbourList[!V(graph)[neighbourList]$wasInfected]
    
    # TAU is total infection rate
    TAU <- length(neighbourList) * tau
    GAMMA <- length(V(graph)[V(graph)$currentState == 1])
    rate <- TAU + GAMMA
    deltaT <- rexp(1, rate)
    t <- t + deltaT
    
    # infection is true if next event is infection, false if it is recovery
    infection <- sample(c(T, F), 1, prob = c(TAU / rate, GAMMA / rate))
    
    if (infection) {
      newInfected <- sample(neighbourList, 1)
      V(graph)[newInfected]$currentState <- 1
      V(graph)[newInfected]$Sduration <- round(t, 6)
      V(graph)[newInfected]$wasInfected <- TRUE
    }
    
    else{
      newRecovered <- sample(V(graph)[V(graph)$currentState == 1], 1)
      V(graph)[newRecovered]$currentState <- 2
      V(graph)[newRecovered]$SIduration <- round(t, 6)
    }
    
  }
  
  V(graph)$SIduration[V(graph)$currentState == 0] <- round(t, 6)
  V(graph)$Sduration[V(graph)$currentState == 0] <- round(t, 6)
  
  # Set some additional simulation properties
  graph <- set_vertex_attr(graph, "maxTime", value = round(t, 6))
  percentageInfected <-
    round(100 * sum(V(graph)$wasInfected) / vcount(graph), digits = 2)
  graph <-
    set_vertex_attr(graph, "percentageInfected", value = percentageInfected)
  
  
  return(graph)
}
