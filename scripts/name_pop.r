library(tidyverse)
theme_set(
    theme_light()
)

load("Rdata/names.Rda")


rank_over_years <- function(s_name, s_gender) {
    names %>%
        group_by(year, gender) %>%
        mutate(
            r = rank(-count, ties.method = "first")
        ) %>%
        ungroup() %>%
        filter(name %in% s_name, gender %in% s_gender)
}


rank_graph <- function(rank_df) {
    ggplot(
        rank_df,
        aes(x = year, y = r, color = name)
    ) +
        geom_point() +
        scale_y_continuous(
            limits = c(1000, 1),
            transform = scales::reverse_trans()
        ) +
        labs(
            title = glue::glue("Rank of the name ", paste(unique(rank_df$name), collapse = " and ")),
            y = "Rank",
            x = "Year"
        ) +
        facet_wrap(vars(gender))
}

rank_df <- rank_over_years(s_name = c("Brayden", "Hayden"), "M")

rank_over_years("Alexander", "M") %>% rank_graph() + geom_vline(xintercept = 2009)
rank_over_years("Nova", "F") %>% rank_graph() + geom_vline(xintercept = 2011)

rank_over_years(c("Alexander", "Nova"), c("M", "F")) %>% rank_graph()

rank_over_years(
    c("Hayden", "Haiden", "Hayden", "Haydan", "Hadan", "Heyden", "Heden"), "M"
) %>%
    rank_graph()

