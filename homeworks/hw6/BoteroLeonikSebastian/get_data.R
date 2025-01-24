library(stringi)
library(dplyr)
s <- httr::GET("https://ww2.mini.pw.edu.pl/badania/publikacje/wszystkie-publikacje/")
s <- rawToChar(s$content)
articles <- stri_match_all(s, regex = "(?<=<li>)(?:(?:<strong>)?\\p{Upper}\\p{Alpha}+ \\p{Upper}(?:</strong>)?\\.,[\\s])+")[[1]]

extracted <- apply(articles, MARGIN = 1, FUN = (\(x)stri_match_all(x, regex = "\\p{Upper}\\p{Alpha}+ \\p{Upper}")[[1]]))
extracted <- Filter((\(x) length(x) > 1), extracted)

authors <- tibble(author = unlist(extracted)) |>
  group_by(author) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  head(30)

pairings <- list()
for (article in extracted) {
  for (author in article) {
    pairings[[author]] <- c(pairings[[author]], article)
    if (author %in% authors$author) {
    }
  }
}

dt <- tibble()
for (name in names(pairings)) {
  pairings[[name]] <- pairings[[name]][pairings[[name]] != name]
  dt <- bind_rows(dt,  tibble(
    author = name,
    coauthor = pairings[[name]]
  ))
}

dt <- dt |>
  filter(author %in% authors$author) |>
  filter(coauthor %in% authors$author) |>
  group_by(author, coauthor) |>
  summarise(count = n()) 
# |>
#   ungroup() |>
#   arrange(desc(count)) |>
#   head(40)

write.csv(dt, file = "authors.csv", row.names = FALSE)
