source("import.r")

purrr::walk(
    list.files(
        path = "./scripts", pattern = ".*.r$", full.names = TRUE
    ), source
)
