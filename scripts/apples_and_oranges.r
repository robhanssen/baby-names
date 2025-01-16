library(tidyverse)

load("Rdata/names.Rda")

theme_set(
    theme_light() +
        theme(
            panel.grid.minor.y = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = 5, margin = margin(t = 7, b = 2))
        )
)

apples <- names %>%
    filter(name == "Apple")


oranges <- names %>%
    filter(name == "Orange")

combined <- bind_rows(apples, oranges) %>% arrange(year)


combined %>%
    reframe(
        total = sum(count),
        .by = c(name, gender)
    )


apl <- "<span style = 'color: #EE10BE'>Apples</span>"
ora <- "<span style = 'color: dodgerblue'>Oranges</span>"

combined %>%
    mutate(name = fct_reorder(name, desc(name))) %>%
    ggplot(aes(x = year, y = count, fill = name)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
        values = c("Orange" = "dodgerblue", "Apple" = "#EE10BE")
        # values = c("M" = "dodgerblue", "F" = "#EE10BE")
    ) +
    labs(
        x = NULL, #"Birth year",
        y = NULL, # "Count",
        fill = "Name",
        caption = "Source: Social Security Administration baby name database",
        title = glue::glue("it is very hard to compare {apl} to {ora} as the large majority of {apl} are girls born from 1971 onwards while the large majority of {ora} are boys born before 1972.")
    ) +
    annotate("text",
        x = c(1940, 2000),
        y = c(18, 18),
        label = c("Orange", "Apple"),
        hjust = c(0, 1), color = c("dodgerblue", "#EE10BE")
    ) +
    theme(legend.position = "none") +
    geom_vline(
        xintercept = 1971, linetype = 3, linewidth = .5, color = "black"
    ) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            margin = margin(b = 20, t = 5),
            face = "bold", 
            size = 10
            ),
        
    )

ggsave("export/apples_oranges.png", width = 4.25, height = 4.25)