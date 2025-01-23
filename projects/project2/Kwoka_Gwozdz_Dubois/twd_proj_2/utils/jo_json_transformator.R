library(rjson)

lok_joz_raw <- fromJSON(file = "../data/Os_czasu_jo_raw.json")
lok_joz_raw <- lok_joz_raw$semanticSegments

k <- 0
arr <- c()
j <- 0
for(i in 1:length(lok_joz_raw)){
  if(is.null(lok_joz_raw[[i]]$timelinePath)){
    if(ceiling(as.numeric(difftime(as.POSIXct(substr(lok_joz_raw[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                   as.POSIXct("2024-12-08", format = "%Y-%m-%d"), units = "weeks"))) > 0){
    if(!is.null(lok_joz_raw[[i]]$visit)){
      lok_joz_raw[[i]]$visit$topCandidate$placeID = lok_joz_raw[[i]]$visit$topCandidate$placeId
      #lok_joz[[i]]$visit$topCandidate$placeID = NULL
    } else if(!is.null(lok_joz_raw[[i]]$activity)){
      activityType <- lok_joz_raw[[i]]$activity$topCandidate$type
      lok_joz_raw[[i]]$activity$topCandidate$type <- case_when(
        activityType == "IN_TRAM" ~ "in tram",
        activityType == "WALKING" ~ "walking",
        activityType == "IN_SUBWAY" ~ "in subway",
        activityType == "IN_PASSENGER_VEHICLE" ~ "in passenger vehicle",
        activityType == "IN_TRAIN" ~ "in train",
        activityType == "CYCLING" ~ "cycling",
        TRUE ~ activityType
      )
    }
    j <- j+1
    arr[j] <- i
    }
  }
  
}


lok_joz_new <- lok_joz_raw[arr]

write(toJSON(lok_joz_new), file = "../data/Os_czasu_jo.json")

###
