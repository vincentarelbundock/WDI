# Pagination benchmark

This benchmark compares an installed pre-change WDI package with an installed
candidate. It uses the exported `WDI()` workflow and replaces only the remote
JSON boundary with deterministic responses. The reproducible benchmark makes
no live World Bank requests; a deliberately limited illustrative check is
reported separately below.

The fixture varies the number of available data pages (1, 2, 5, and 9). Each
request has a fixed 10 ms simulated boundary delay so elapsed results expose
the effect of request count without claiming a specific real-world network
latency. The primary metric is request count; elapsed time is secondary.

Run:

```sh
Rscript benchmarks/pagination.R \
  --baseline-lib /path/to/baseline/library \
  --candidate-lib /path/to/candidate/library \
  --repetitions 20 \
  --delay-ms 10
```

The script runs each installation in a separate R process, reports median and
range, and requires complete output objects to be identical for every fixture.

## Recorded result

Baseline: commit `22ed43eaee843b286d1e9ad5309b9145c0e2f1b0` installed from a
clean `git archive`. Candidate: the working tree using validated API page-count
metadata with an unusable-page fallback. Both were built and installed into
isolated libraries.

Environment: R 4.5.0, `aarch64-apple-darwin20`, 20 repetitions after one
warm-up, 10 ms simulated delay per request. Speedup is baseline median divided
by candidate median.

| Data pages | Baseline requests | Candidate requests | Request reduction | Baseline median | Candidate median | Speedup | Identical output |
|---:|---:|---:|---:|---:|---:|---:|:---:|
| 1 | 10 | 1 | 90% | 125.0 ms | 13.0 ms | 9.615x | yes |
| 2 | 10 | 2 | 80% | 125.5 ms | 26.0 ms | 4.827x | yes |
| 5 | 10 | 5 | 50% | 127.0 ms | 65.0 ms | 1.954x | yes |
| 9 | 10 | 9 | 10% | 127.0 ms | 115.0 ms | 1.104x | yes |

Ranges were 123–128, 123–127, 123–128, and 123–129 ms for the baseline;
13–14, 25–27, 62–65, and 110–116 ms for the candidate, respectively.

These elapsed results model a fixed request boundary and must not be presented
as measured World Bank latency. The demonstrated resource improvement is the
request reduction. Real elapsed savings depend on network and server latency.
The candidate makes exactly the number of requests declared by valid metadata;
even the nine-page fixture avoids the baseline's unused tenth request.

## Illustrative live check

On 2026-08-16, one live baseline run and one final-candidate run were made
against the World Bank API for two real `NY.GDP.PCAP.KD` workloads. Intermediate
candidate measurements brought total live traffic during development to 26
requests. The baseline and final-candidate observations were made at different
times and are not matched repetitions.

| Workload | Baseline | Candidate | Absolute change | Requests | Rows | Identical output |
|---|---:|---:|---:|---:|---:|:---:|
| United States, 2000–2020 | 5.906 s | 0.208 s | -5.698 s | 10 → 1 | 21 | yes |
| All countries, 1960–2025 | 9.532 s | 0.398 s | -9.134 s | 10 → 1 | 17,490 | yes |

These are observations, not stable benchmark distributions. CDN state, server
load, connection setup, upstream caching, and the time between installations
were uncontrolled. The live check supports a material absolute latency benefit
for these one-page responses, but the offline request-count benchmark remains
the reproducible performance evidence.

## Validation

- Focused offline pagination tests: 13 passed.
- Full local test suite, including the existing live API tests: 54 passed,
  0 failed, 0 warned, and 0 skipped.
- Fresh source-package check: `Status: OK` with
  `_R_CHECK_FORCE_SUGGESTS_=false`, `--no-manual`, and
  `--no-build-vignettes`. The optional `altdoc` Suggest was unavailable.
