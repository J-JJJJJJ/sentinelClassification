library(dplyr)
library(tidyverse)
library(data.table)


#
#  Rewires a set of vertices
#
rewire <- function(edges, inter) {
  # if only 3 or fewer nodes in community, return immediately
  if (nrow(edges) < 4) {
    return(edges %>% select(V1, V2))
  }
  # turn into datatable to speed up
  edges <- data.table(edges)
  
  # add index column
  edges <- edges %>% mutate(index = 1:nrow(edges))
  
  # cap the number of iterations for really large (sub)edgelists, maximum 60k iterations
  nruns <- 100 * nrow(edges)
  for (i in 1:nruns) {
    # sample 2 edges with different index
    sample <- sample_n(edges, 2, replace = FALSE)
    
    sample1 <- sample %>% top_n(1, wt = index)
    sample2 <- sample %>% top_n(-1, wt = index)
    
    
    # with 50% probability, swap V1 and V2 in sample1
    if (sample(c(T, F), 1)) {
      sample1 <- sample1 %>% dplyr::rename(V1 = V2, V2 = V1)
      
      # if inter community, also swap the community columns
      if (inter) {
        sample1 <-
          sample1 %>% dplyr::rename(community1 = community2, community2 = community1)
      }
    }
    
    # with 50% probability, swap V1 and V2 in sample2
    if (sample(c(T, F), 1)) {
      sample2 <- sample2 %>% dplyr::rename(V1 = V2, V2 = V1)
      
      # if inter community, also swap the community columns
      if (inter) {
        sample2 <-
          sample2 %>% dplyr::rename(community1 = community2, community2 = community1)
      }
    }
    
    # bind the back together
    sample <- bind_rows(sample1, sample2)
    
    # The new edges
    sample <- sample %>% dplyr::mutate(V2 = rev(V2))
    
    # make sure no self loops are introduced, add column V3=V1-V2, this should not contain 0
    sample <- sample %>% dplyr::mutate(V3 = V1 - V2)
    if (0 %in% sample$V3) {
      next
    }
    
    
    # make sure neither of the new edges already exist
    #if(nrow(match_df(edges, sample, on=c("V1", "V2")))>0){next}
    #if(nrow(match_df(edges, sample%>%dplyr::rename(V1=V2, V2=V1), on=c("V1", "V2")))>0){next}
    
    test <-
      edges %>% dplyr::filter(
        (V1 == sample$V1[1] && V2 == sample$V2[1]) ||
          (V1 == sample$V1[2] &&
             V2 == sample$V2[2]) ||
          (V1 == sample$V2[1] &&
             V2 == sample$V1[1]) ||
          (V1 == sample$V2[2] &&
             V2 == sample$V1[2])
      )
    if (nrow(test) > 0) {
      next
    }
    
    # extra check for intercommunity edges, they should remain intercommunity
    if (inter) {
      #next
      # if intercommunity edges also swap the second community column and
      # add community3=community1-community2 and this should not contain 0 (otherwise new edge is intra)
      sample <-
        sample %>% dplyr::mutate(community2 = rev(community2))
      
      #if(nrow(sample %>% filter(community1 == community2))>0 ){next}
      sample <-
        sample %>% dplyr::mutate(community3 = community1 - community2)
      if (0 %in% sample$community3) {
        next
      }
      
      
      # remove old edges
      edges <- edges %>% filter(!index %in% sample$index)
      # add the new edges
      sample <-
        sample %>% dplyr::select(V1, V2, community1, community2, index)
      edges <- bind_rows(edges, sample)
      
    }
    
    
    else{
      # remove old edges
      edges <- edges %>% dplyr::filter(!index %in% sample$index)
      # add the new edges
      sample <- sample %>% dplyr::select(V1, V2, index)
      edges <- bind_rows(edges, sample)
      
    }
    
    
    
    
  }
  
  edges <- edges %>% select(V1, V2)
  return(edges)
}