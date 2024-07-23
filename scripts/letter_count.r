library(tidyverse)

load("Rdata/names.Rda")

names_trans <-
    names %>%
    mutate(
        name_t = tolower(name),
        first = substr(name_t, 1, 1)
    ) %>%
    distinct(name_t)

let_count <-
    map(
        letters,
        \(x) {
            str_count(names_trans$name_t, x)
        }
    ) %>% purrr::set_names(letters)


max_length_name <-
    map(
        letters,
        \(x) names_trans$name_t[which(let_count[[x]] == max(let_count[[x]]))]
    ) %>% purrr::set_names(letters)

longest_letter_range_names <-
    map_df(
        letters,
        \(l) {
            tibble(
                max_letter = l,
                name = unlist(max_length_name[l]),
            )
        }
    ) %>%
    mutate(
        letter_count = stringr::str_count(name, max_letter)
    ) %>%
    relocate(max_letter, letter_count, name) %>%
    arrange(desc(letter_count))

write_csv(longest_letter_range_names, "export/names_with_max_letters.csv")
