library(tidyverse)

load("Rdata/names.Rda")

names %>%
    group_by(year, gender) %>%
    slice_max(count, n = 500) %>%
    mutate(
        r = rank(-count, ties.method = "first")
    ) %>%
    ungroup() %>%
    split(.$gender) %>%
    map(\(df) {
        g <- distinct(df, gender)

        df2 <- pivot_wider(df, id_cols = !count, names_from = year, values_from = name) %>%
            arrange(r)

        df2[is.na(df2)] <- ""

        write_csv(df2, paste0("export/top500names-", g, ".csv"))
    })
