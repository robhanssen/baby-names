library(tidyverse)

sources <-
    list.files(path = "source", pattern = "*.txt$", full.names = TRUE)

years <- as.integer(str_extract(sources, "\\d{4}"))

names <-
    map_df(
        sources,
        ~ read_csv(.x, col_types = "cci", col_names = c("name", "gender", "count")),
        .id = "year"
    ) %>%
    mutate(year = years[as.integer(year)])

save(names, file = "Rdata/names.Rda")