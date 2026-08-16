test_that("pagination stops after the first unusable page, and warns", {
    requests <- character()
    from_json <- function(url) {
        requests <<- c(requests, url)
        page <- as.integer(sub(".*[&]page=([0-9]+).*", "\\1", url))
        if (page > 2) {
            stop("no more pages")
        }
        make_wdi_page(page)
    }

    expect_warning(
        out <- WDI:::wdi.dl(
            indicator = "SYNTH",
            country = "US",
            start = 1990,
            end = 2000,
            .fromJSON = from_json),
        "Retrieved 2 of 10 pages")

    expect_length(requests, 3)
    expect_equal(out$data$SYNTH, c(1, 2))
    expect_equal(out$data$year, c(2000L, 1999L))
    expect_equal(attr(out$data, "label"), "Synthetic indicator")
})

test_that("pagination keeps all ten available pages", {
    requests <- character()
    from_json <- function(url) {
        requests <<- c(requests, url)
        page <- as.integer(sub(".*[&]page=([0-9]+).*", "\\1", url))
        make_wdi_page(page)
    }

    out <- WDI:::wdi.dl(
        indicator = "SYNTH",
        country = "US",
        start = 1990,
        end = 2000,
        .fromJSON = from_json)

    expect_length(requests, 10)
    expect_equal(nrow(out$data), 10)
})

test_that("pagination uses API page-count metadata", {
    requests <- character()
    from_json <- function(url) {
        requests <<- c(requests, url)
        page <- as.integer(sub(".*[&]page=([0-9]+).*", "\\1", url))
        make_wdi_page(page, pages = 2L)
    }

    out <- WDI:::wdi.dl(
        indicator = "SYNTH",
        country = "US",
        start = 1990,
        end = 2000,
        .fromJSON = from_json)

    expect_length(requests, 2)
    expect_equal(out$data$SYNTH, c(1, 2))
    expect_null(attr(out$data, "pages"))
})

test_that("pagination falls back when page-count metadata is malformed", {
    requests <- character()
    from_json <- function(url) {
        requests <<- c(requests, url)
        page <- as.integer(sub(".*[&]page=([0-9]+).*", "\\1", url))
        if (page > 1) {
            stop("no more pages")
        }
        make_wdi_page(page, pages = "unknown")
    }

    out <- WDI:::wdi.dl(
        indicator = "SYNTH",
        country = "US",
        start = 1990,
        end = 2000,
        .fromJSON = from_json)

    expect_length(requests, 2)
    expect_equal(nrow(out$data), 1)
})

test_that("pagination stops after a first-page failure with an informative error", {
    requests <- 0L
    from_json <- function(url) {
        requests <<- requests + 1L
        stop("synthetic API failure")
    }

    expect_error(
        WDI:::wdi.dl(
            indicator = "SYNTH",
            country = "US",
            start = 1990,
            end = 2000,
            .fromJSON = from_json),
        "did not return usable data")
    expect_equal(requests, 1L)
})
