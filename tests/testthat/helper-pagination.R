make_wdi_page <- function(page, pages = 10L) {
    list(
        list(lastupdated = "2026-08-16", pages = pages),
        list(
            country = data.frame(
                value = "United States",
                id = "US",
                stringsAsFactors = FALSE),
            countryiso3code = "USA",
            date = as.character(2001 - page),
            value = as.numeric(page),
            indicator = data.frame(
                value = "Synthetic indicator",
                stringsAsFactors = FALSE)
        )
    )
}
