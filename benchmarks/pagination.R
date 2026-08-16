#!/usr/bin/env Rscript

# Offline benchmark for WDI pagination.
#
# The parent mode compares two independently installed WDI packages. Worker
# mode loads one package, mocks the JSON boundary, exercises exported WDI(),
# and records request counts and elapsed distributions. The delay models a
# fixed remote round trip; it is not a claim about World Bank latency.

args <- commandArgs(trailingOnly = TRUE)

argument <- function(name, default = NULL) {
    idx <- match(name, args)
    if (is.na(idx)) {
        return(default)
    }
    args[[idx + 1]]
}

make_response <- function(page, pages) {
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

if ("--worker" %in% args) {
    lib <- argument("--library")
    label <- argument("--label")
    output <- argument("--output")
    output_dir <- argument("--output-dir")
    repetitions <- as.integer(argument("--repetitions", "20"))
    delay_ms <- as.numeric(argument("--delay-ms", "10"))
    page_counts <- c(1L, 2L, 5L, 9L)

    library(WDI, lib.loc = lib)

    results <- lapply(page_counts, function(data_pages) {
        request_count <- 0L
        from_json <- function(url) {
            request_count <<- request_count + 1L
            if (delay_ms > 0) {
                Sys.sleep(delay_ms / 1000)
            }
            page <- as.integer(sub(".*[&]page=([0-9]+).*", "\\1", url))
            if (page > data_pages) {
                stop("no more pages")
            }
            make_response(page, data_pages)
        }
        run <- function() {
            WDI::WDI(
                country = "US",
                indicator = "SYNTH",
                start = 1990,
                end = 2000)
        }

        testthat::with_mocked_bindings(
            {
                invisible(run())
                request_count <- 0L
                elapsed <- numeric(repetitions)
                requests <- integer(repetitions)
                first_output <- NULL
                for (i in seq_len(repetitions)) {
                    before <- request_count
                    elapsed[[i]] <- system.time(value <- run())[["elapsed"]]
                    requests[[i]] <- request_count - before
                    if (is.null(first_output)) {
                        first_output <- value
                    }
                }
            },
            fromJSON = from_json,
            .package = "jsonlite")

        saveRDS(
            first_output,
            file.path(output_dir, paste0(label, "-pages-", data_pages, ".rds")))

        data.frame(
            implementation = label,
            data_pages = data_pages,
            repetitions = repetitions,
            delay_ms = delay_ms,
            requests = median(requests),
            median_ms = 1000 * median(elapsed),
            min_ms = 1000 * min(elapsed),
            max_ms = 1000 * max(elapsed),
            stringsAsFactors = FALSE)
    })

    utils::write.csv(do.call(rbind, results), output, row.names = FALSE)
    quit(save = "no", status = 0L)
}

baseline_lib <- argument("--baseline-lib")
candidate_lib <- argument("--candidate-lib")
repetitions <- argument("--repetitions", "20")
delay_ms <- argument("--delay-ms", "10")

if (is.null(baseline_lib) || is.null(candidate_lib)) {
    stop(
        "Usage: Rscript benchmarks/pagination.R --baseline-lib PATH ",
        "--candidate-lib PATH [--repetitions 20] [--delay-ms 10]",
        call. = FALSE)
}

script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
output_dir <- tempfile("wdi-pagination-")
dir.create(output_dir)
on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

run_worker <- function(label, lib) {
    output <- file.path(output_dir, paste0(label, ".csv"))
    status <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            shQuote(script),
            "--worker",
            "--library", shQuote(lib),
            "--label", label,
            "--output", shQuote(output),
            "--output-dir", shQuote(output_dir),
            "--repetitions", repetitions,
            "--delay-ms", delay_ms))
    if (!identical(status, 0L)) {
        stop(label, " benchmark failed", call. = FALSE)
    }
    utils::read.csv(output, stringsAsFactors = FALSE)
}

baseline <- run_worker("baseline", baseline_lib)
candidate <- run_worker("candidate", candidate_lib)

equivalent <- vapply(baseline$data_pages, function(data_pages) {
    identical(
        readRDS(file.path(output_dir, paste0("baseline-pages-", data_pages, ".rds"))),
        readRDS(file.path(output_dir, paste0("candidate-pages-", data_pages, ".rds"))))
}, logical(1))

comparison <- merge(
    baseline,
    candidate,
    by = c("data_pages", "repetitions", "delay_ms"),
    suffixes = c("_baseline", "_candidate"))
comparison$requests_saved <- comparison$requests_baseline - comparison$requests_candidate
comparison$request_reduction_pct <- 100 * comparison$requests_saved / comparison$requests_baseline
comparison$elapsed_saved_ms <- comparison$median_ms_baseline - comparison$median_ms_candidate
comparison$speedup <- comparison$median_ms_baseline / comparison$median_ms_candidate
comparison$identical_output <- equivalent[match(comparison$data_pages, baseline$data_pages)]

print(comparison, row.names = FALSE, digits = 4)
cat("\nR:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
