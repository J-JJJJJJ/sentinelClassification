library(foreach)
library(doParallel)
library(dplyr)
library(data.table)

# if list of tables not in workspace, read the individual csv files
if (!exists("smallTable")) {
  smallTable <-
    list.files("infectionresult/",
               pattern = "*.csv",
               full.names = TRUE)
  
  infectedGraphs <- list()
  no_cores <- detectCores() - 2
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  smallTable <- foreach(i = 1:length(smallTable)) %dopar% {
    library(data.table)
    table = fread(
      smallTable[[i]],
      stringsAsFactors = TRUE,
      na.strings = c("NA", "", NULL)
    )
    table
  }
  stopCluster(cl)
}


# combine the rows in the list of tables
smallTable <- smallTable %>% bind_rows()



# add the sentClass column
smallTable <- smallTable %>%
  group_by(graphName, type, version, tau) %>%
  dplyr::mutate(sentClass = case_when(
    rank(sentinel, ties.method = "random") <= ceiling(0.15 * n()) ~ T,
    T ~ F
  )) %>%
  ungroup()

# write to file
smallTable %>% fwrite("smallTable.csv", append = FALSE, nThread = 6)
