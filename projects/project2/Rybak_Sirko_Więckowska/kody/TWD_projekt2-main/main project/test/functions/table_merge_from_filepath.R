#takes a character vector of filepaths
#returns a table with combined data from multiple json files (multiple people)

library(dplyr)

source(file.path("functions", "table_from_relative_path.R"))

table_merger <- function(filepaths){
    df <- table_from_relative_path(filepaths[1])
    for (filepath in filepaths[-1]){
        df <- bind_rows(df, table_from_relative_path(filepath))
    }
    return(df)
}