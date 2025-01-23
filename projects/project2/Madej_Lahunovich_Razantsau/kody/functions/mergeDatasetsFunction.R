mergeDatasets <- function(ludwiksDataset, maximsDataset, yahorsDataset) {
  # requires loaded dplyr's library
  ludwiksDatasetChanged   <- ludwiksDataset %>% mutate("Owner" = "Ludwik")
  maximsDatasetChanged    <- maximsDataset %>% mutate("Owner" = "Maxim")
  yahorsDatasetChanged    <- yahorsDataset %>% mutate("Owner" = "Yahor")
  
  dplyr::bind_rows(ludwiksDatasetChanged, maximsDatasetChanged, yahorsDatasetChanged)
}