library(tidyverse)

load("Rdata/names.Rda")

names %>%
    group_by(year, gender) %>%
    slice_max(count, n = 500) %>%
    mutate(
        r = rank(-count, ties.method = "first")
    ) %>% 
    pivot_wider(id_cols = !count, names_from = c(gender, year), values_from = name) %>% 
    arrange(r) %>%
    view()
