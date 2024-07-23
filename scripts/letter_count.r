library(tidyverse)

load("Rdata/names.Rda")

names_trans <-
    names %>%
    mutate(
        name_t = tolower(name),
        first = substr(name_t, 1, 1)
    ) %>%
    distinct(name_t)

let_count <- map(
    letters,
    \(x) {
        str_count(names_trans$name_t, x)
    }
)

max_count <- map_int(let_count, max)
names(max_count) <- letters

max_length <- map(
    letters,
    \(x) {
        stringr::str_extract_all(names_trans$name_t, x) %>% map_int(length) == max_count[x]
    }
)

max_length_index <- map(max_length, which)

names_max <- map(
    max_length_index,
    \(n) names_trans$name_t[n]
)

names(names_max) <- letters

longest_letter_range_names <- map_df(
    letters,
    \(l) {
        tibble(
            max_letter = l,
            name = unlist(names_max[l]),
        )
    }
) %>%
    mutate(
        letter_count = stringr::str_extract_all(name, max_letter) %>% map_dbl(length)
    ) %>%
    relocate(max_letter, letter_count, name) %>%
    arrange(desc(letter_count))

write_csv(longest_letter_range_names, "export/names_with_max_letters.csv")