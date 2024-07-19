library(tidyverse)

load("Rdata/names.Rda")

name_filter <- function(dat = names, reg_exp, tag = "name") {
    filtered_df <-
        dat %>%
        distinct(name) %>%
        filter(str_detect(name, reg_exp)) %>%
        arrange(name) %>%
        mutate(first_letter = substr(name, 1, 1), x = 1) %>%
        mutate(
            y = cumsum(x), .by = first_letter
        ) %>%
        select(-x) %>%
        pivot_wider(names_from = "first_letter", values_from = "name") %>%
        select(-y)

    filtered_df[is.na(filtered_df)] <- ""

    filtered_df %>%
        write_csv(
            paste0("export/", tag, "_names.csv")
        )
}

name_filter_df <-
    tribble(
        ~tag, ~reg,
        "aiden", ".*(a|e).*(i|y).*(t|d)(e|o|a)(n|nn)$",
        "lees", ".*(ee|eigh|aigh|ey|ehy|egh|agh|aigh|ehg|eigh|ai|aei|aie|ae|iae)$",
        "ellon", ".*(a|e|o|i|y)(lon|lin|lan|lyn|len|lun)(n|)(e|)$",
        "isha", ".*(ish|esh|ysh|ysh|osh|ash)(e|o|a|y|i|ee)$",
        "yce", ".*(a|e|o|i|y|u)yc(ee|ie|ye|ey|oy|iy|yi)$",
        "xyz", "(xx|yy|zz|xy|yx|xz|zx|zy|yz)",
        "vy", "vy(a|o|e|u|i)",
        "ht", "(a|e|i|o|u|y)ht$"
    )

nnn <- map2(
    name_filter_df$reg,
    name_filter_df$tag,
    ~ name_filter(names, reg_exp = .x, tag = .y)
) %>% list_rbind()

nnn_df <-
    nnn %>%
    pivot_longer(
        everything(),
        names_to = "first",
        values_to = "names"
    ) %>%
    filter(!is.name(names), names != "") %>%
    arrange(names) %>%
    mutate(x = 1) %>%
    mutate(y = cumsum(x), .by = first) %>%
    select(-x) %>%
    pivot_wider(
        names_from = "first",
        values_from = "names"
    ) %>%
    select(-y)

nnn_df[is.na(nnn_df)] <- ""

write_csv(nnn_df, "export/traghedies.csv")
