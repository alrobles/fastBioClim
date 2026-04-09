# fastBioClim — Comprehensive Implementation Analysis

> **Repository**: `alrobles/fastBioClim`
> **Package**: `fastbioclim` v0.5.0
> **Analysis date**: 2026-04-09

---

## Table of Contents

1. [Repository Structure](#1-repository-structure)
2. [Input Data Structures](#2-input-data-structures)
3. [Raster Processing Pattern (Block I/O)](#3-raster-processing-pattern-block-io)
4. [Complete Mathematical Pipeline for BIO1–BIO19](#4-complete-mathematical-pipeline-for-bio1bio19)
   - [C++ Primitives](#c-primitives)
   - [BIO1–BIO12 (Production)](#bio1bio12-production)
   - [BIO13–BIO19 (Sandbox Prototypes)](#bio13bio19-sandbox-prototypes)
5. [Computation Dependency Graph](#5-computation-dependency-graph)
6. [NA Handling](#6-na-handling)
7. [Performance Architecture](#7-performance-architecture)
8. [CHELSA Dataset Integration](#8-chelsa-dataset-integration)
9. [Test Coverage Summary](#9-test-coverage-summary)
10. [Production vs. Sandbox Status](#10-production-vs-sandbox-status)

---

## 1. Repository Structure

The package is organized as a standard R package. Key directories:

| Directory | Contents |
|-----------|----------|
| `R/` | Production R functions (BIO1–BIO12, primitives, mock data generators) |
| `R_sandbox/` | Exploratory/prototype R code covering all 19 BIOs (not installed) |
| `src/` | Production C++ (RcppParallel workers) compiled into the shared library |
| `src_sandbox/` | Prototype C++ functions (not compiled into the production build) |
| `tests/testthat/` | Unit tests for each function |

**`DESCRIPTION`** declares dependencies: `terra`, `Rcpp`, `RcppParallel`, `checkmate`.
**SystemRequirements**: OpenMP.

---

## 2. Input Data Structures

All 19 BIO functions accept `terra::SpatRaster` objects with **exactly 12 layers** (one per calendar month, January = layer 1 through December = layer 12). There are four input variable types:

| Variable | CHELSA naming | Description | Units |
|---|---|---|---|
| `tas` | `tas_01` … `tas_12` | Monthly mean temperature | °C |
| `tasmax` | `tasmax_01` … `tasmax_12` | Monthly maximum temperature | °C |
| `tasmin` | `tasmin_01` … `tasmin_12` | Monthly minimum temperature | °C |
| `pr` | `pr_01` … `pr_12` | Monthly total precipitation | mm (CHELSA stores ×100; `chelsa_bio_12.R:23` applies `/100` before passing to bio functions) |

**Mock data conventions** (`R/mock_*.R`):

| Function | Layer *m* value | CRS | Extent |
|---|---|---|---|
| `mock_tas()` | *m* (Jan=1, …, Dec=12) | EPSG:4326 | −180/180/−90/90 |
| `mock_tasmax()` | *m* + 1 (Jan=2, …, Dec=13) | EPSG:4326 | −180/180/−90/90 |
| `mock_tasmin()` | *m* − 1 (Jan=0, …, Dec=11) | EPSG:4326 | −180/180/−90/90 |
| `mock_pr()` | *m* × 100 (Jan=100, …, Dec=1200) | EPSG:4326 | −180/180/−90/90 |

All mock grids are 20×20 cells.

---

## 3. Raster Processing Pattern (Block I/O)

Every production function in `R/` follows an identical terra block-processing pattern:

```r
terra::readStart(raster)                          # open raster for reading
out <- terra::rast(template, nlyr = 1)            # allocate single-layer output
b   <- terra::writeStart(out, filename)           # open output; get block schedule
for (i in 1:b$n) {
  v <- terra::readValues(raster,
         row = b$row[i], nrows = b$nrows[i],
         col = 1, ncols = ncols, mat = TRUE)
  # v is a matrix: rows = cells in block, columns = 12 months
  r <- <C++ computation>(v)
  terra::writeValues(out, r, b$row[i], b$nrows[i])
}
terra::writeStop(out)
terra::readStop(raster)                           # via on.exit()
```

**Key details:**

- `mat = TRUE` in `readValues` returns a **matrix** with rows = spatial cells and columns = months. This is the fundamental data structure passed to all C++ workers.
- `terra::writeStart` with default `n` (or `n = block_factor * nlyr(pr)` in the optimized BIO12) controls block size. The scheduler tries to keep blocks within memory budget.
- `on.exit()` guards ensure `readStop`/`writeStop` are called even on error.
- **BIO3** (`R/bio_03.R:62–77`) has an extra optimization: if `tasmax` and `tasmin` come from the same source file (checked via `terra::sources()`), `readStart` is only called once.
- Output always inherits spatial geometry (extent, resolution, CRS) from the template raster via `terra::rast(input, nlyr = 1)`.

---

## 4. Complete Mathematical Pipeline for BIO1–BIO19

### C++ Primitives

All C++ workers use **`RcppParallel::parallelFor`** over rows, splitting row ranges across OS threads. The `Makevars` file (`src/Makevars:1`) enables `-fopenmp -O3 -march=native -ffast-math` for maximum throughput.

---

#### `rcpp_parallel_average`
**File**: `src/rcpp-parallel-average.cpp:8–34`

```
row → sum(row) / length(row)    [std::accumulate, no explicit NA handling]
```

---

#### `rcpp_parallel_difference`
**File**: `src/rcpp-parallel-difference.cpp:8–40`

```
(mat_1, mat_2) → mat_1 - mat_2  [element-wise, std::transform]
```

---

#### `rcpp_parallel_which_max_row` / `rcpp_parallel_which_min_row`
**File**: `src/rcpp-parallel-which-max-min.cpp:13–95`

```
row → value of max/min element   [std::max_element / std::min_element]
NA in row → NA_REAL (strict na.rm = FALSE policy)
```

---

#### `rcpp_parallel_sd` / `rcpp_parallel_variance`
**File**: `src/rcpp_parallel_sd.cpp`

```
row → Welford one-pass algorithm for sample variance (denominator n−1)
SD  = sqrt(M2 / (n−1))
NA in row → NA_REAL
```

---

#### `rcpp_parallel_sum`
**File**: `src/rcpp_parallel_sum.cpp`

```
row → sum of all elements
na_rm = FALSE: any NA → NA_REAL
na_rm = TRUE:  skip NAs; all-NA → NA_REAL
```

---

#### `rcpp_parallel_which_max_rolling_quarter` / `rcpp_parallel_which_min_rolling_quarter`
**File**: `src/rcpp-parallel-which-max-min-rolling-quarter.cpp:15–185`

```
For each row, scan all 3-month windows:
  wrap = FALSE: windows [j, j+1, j+2] for j in 0..(ncol−3) → ncol−2 windows
  wrap = TRUE:  windows [j, (j+1)%ncol, (j+2)%ncol] for j in 0..ncol−1 → ncol windows

Score = mean of the 3 values:
  na_rm = FALSE: window invalid if any NA
  na_rm = TRUE:  mean over non-NA; all-NA window is invalid

Returns 1-based starting column index of the max/min-score window.
Ties: earliest index wins (strictly greater/less update).
```

---

#### `rcpp_parallel_average_quarter`
**File**: `src/rcpp_parallel_average_quarter.cpp`

```
Given (idx, mat):
  For each row i, starting index j0 = idx[i]−1 (0-based)
  Read mat[i, j0], mat[i, j1], mat[i, j2]
    (j1, j2 = j0+1, j0+2; wrap modulo ncol if wrap = TRUE)
  na_rm = FALSE: any NA → NA_REAL
  na_rm = TRUE:  mean over non-NA; all-NA → NA_REAL
Returns nrow×1 matrix of averages.
```

---

#### `rcpp_get_average_quarter`
**File**: `src/rcpp-get-average-quarter.cpp:12–41`

Single-threaded version of the above (no `parallelFor`). Uses 0-based index from `ixQuarter`. NA-skipping via `nansum`/`count_if`.

---

### BIO1–BIO12 (Production)

#### BIO1 — Annual Mean Temperature
**File**: `R/bio_01.R:43–80`  
**Inputs**: `tas` (12 layers)

$$\text{BIO1} = \frac{1}{12}\sum_{m=1}^{12} \text{tas}_m$$

**C++ call**: `rcpp_parallel_average(v)` → `std::accumulate / length`

---

#### BIO2 — Mean Diurnal Range
**File**: `R/bio_02.R:49–96`  
**Inputs**: `tasmax`, `tasmin` (each 12 layers)

$$\text{BIO2} = \overline{\text{tasmax}} - \overline{\text{tasmin}}$$

*Note*: Mathematically equivalent to `mean(tasmax − tasmin)` — this is the mean of monthly diurnal temperature ranges.

**C++ calls**: `rcpp_parallel_average(v_tasmax)`, `rcpp_parallel_average(v_tasmin)`, `rcpp_parallel_difference(r_max, r_min)`

---

#### BIO3 — Isothermality
**File**: `R/bio_03.R:42–137`  
**Inputs**: `tasmax`, `tasmin`

$$\text{BIO3} = 100 \times \frac{\text{BIO2}}{\text{BIO7}} = 100 \times \frac{\overline{\text{tasmax}} - \overline{\text{tasmin}}}{\max(\text{tasmax}) - \min(\text{tasmin})}$$

**Division-by-zero guard** (`R/bio_03.R:126`): denominator ≤ 1×10⁻⁹ → NA; non-finite result → NA

**C++ calls**: `rcpp_parallel_average`, `rcpp_parallel_difference`, `rcpp_parallel_which_max_row`, `rcpp_parallel_which_min_row`

---

#### BIO4 — Temperature Seasonality
**File**: `R/bio_04.R:52–90`  
**Inputs**: `tas`

$$\text{BIO4} = 100 \times \text{sd}(\text{tas}_{1\ldots12}) \quad \text{(sample SD, denominator } n-1\text{)}$$

**C++ call**: `rcpp_parallel_sd(v)` via Welford algorithm

---

#### BIO5 — Max Temperature of Warmest Month
**File**: `R/bio_05.R:54–93`  
**Inputs**: `tasmax`

$$\text{BIO5} = \max(\text{tasmax}_{1\ldots12})$$

**C++ call**: `rcpp_parallel_which_max_row(v)` → `std::max_element` value

---

#### BIO6 — Min Temperature of Coldest Month
**File**: `R/bio_06.R:52–91`  
**Inputs**: `tasmin`

$$\text{BIO6} = \min(\text{tasmin}_{1\ldots12})$$

**C++ call**: `rcpp_parallel_which_min_row(v)` → `std::min_element` value

---

#### BIO7 — Temperature Annual Range
**File**: `R/bio_07.R:55–107`  
**Inputs**: `tasmax`, `tasmin`

$$\text{BIO7} = \max(\text{tasmax}_{1\ldots12}) - \min(\text{tasmin}_{1\ldots12})$$

**C++ calls**: `rcpp_parallel_which_max_row(v_tasmax)`, `rcpp_parallel_which_min_row(v_tasmin)`, `rcpp_parallel_difference(...)`

---

#### BIO8 — Mean Temperature of Wettest Quarter
**File**: `R/bio_08.R:59–110`  
**Inputs**: `pr`, `tas`

$$q_{\text{wet}} = \underset{j}{\arg\max}\; \sum_{k=0}^{2} \text{pr}_{j+k}$$
$$\text{BIO8} = \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{q_{\text{wet}}+k}$$

**C++ calls**: `rcpp_parallel_which_max_rolling_quarter(v_pr, wrap, na_rm)` → index, then `rcpp_parallel_average_quarter(idx, v_tas, wrap, na_rm)`

---

#### BIO9 — Mean Temperature of Driest Quarter
**File**: `R/bio_09.R:84–135`  
**Inputs**: `pr`, `tas`

$$q_{\text{dry}} = \underset{j}{\arg\min}\; \sum_{k=0}^{2} \text{pr}_{j+k}$$
$$\text{BIO9} = \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{q_{\text{dry}}+k}$$

**C++ calls**: `rcpp_parallel_which_min_rolling_quarter(v_pr, ...)`, `rcpp_parallel_average_quarter(idx, v_tas, ...)`

---

#### BIO10 — Mean Temperature of Warmest Quarter
**File**: `R/bio_10.R:82–119`  
**Inputs**: `tas` only

$$q_{\text{warm}} = \underset{j}{\arg\max}\; \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{j+k}$$
$$\text{BIO10} = \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{q_{\text{warm}}+k}$$

**C++ calls**: `rcpp_parallel_which_max_rolling_quarter(v_tas, ...)`, `rcpp_parallel_average_quarter(idx, v_tas, ...)`

---

#### BIO11 — Mean Temperature of Coldest Quarter
**File**: `R/bio_11.R:85–122`  
**Inputs**: `tas` only

$$q_{\text{cold}} = \underset{j}{\arg\min}\; \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{j+k}$$
$$\text{BIO11} = \frac{1}{3}\sum_{k=0}^{2} \text{tas}_{q_{\text{cold}}+k}$$

**C++ calls**: `rcpp_parallel_which_min_rolling_quarter(v_tas, ...)`, `rcpp_parallel_average_quarter(idx, v_tas, ...)`

---

#### BIO12 — Annual Precipitation
**File**: `R/bio_12.R:94–138`  
**Inputs**: `pr`

$$\text{BIO12} = \sum_{m=1}^{12} \text{pr}_m$$

**C++ call**: `rcpp_parallel_sum(v_pr, na_rm)` (uses `RVector<double>` output, fast NA handling)

**Optimized variant** (`R/bio_12_optim.R`): `parallel_sum(v_pr, na_rm, nthreads)` with configurable thread count and larger block factor (`block_factor = 4`).

---

### BIO13–BIO19 (Sandbox Prototypes)

These are prototype implementations in `R_sandbox/` with the correct algorithms but using an older C++ API (three separate shifted matrices rather than a single rolling-window matrix). They are **not exported** from the installed package.

| BIO | File | Inputs | Formula |
|---|---|---|---|
| **BIO13** Precip Wettest Month | `R_sandbox/bio_13.R` | `pr` | `max(pr_Jan…Dec)` per cell |
| **BIO14** Precip Driest Month | `R_sandbox/bio_14.R` | `pr` | `min(pr_Jan…Dec)` per cell |
| **BIO15** Precipitation Seasonality | `R_sandbox/bio_15.R` | `pr` | `100 × sd(pr) / mean(pr)` = coefficient of variation |
| **BIO16** Precip Wettest Quarter | `R_sandbox/bio_16.R` | `pr` | Sum of pr in argmax 3-month rolling window of pr |
| **BIO17** Precip Driest Quarter | `R_sandbox/bio_17.R` | `pr` | Sum of pr in argmin 3-month rolling window of pr |
| **BIO18** Precip Warmest Quarter | `R_sandbox/bio_18.R` | `pr`, `tasmax` | Sum of pr in argmax 3-month rolling window of tasmax |
| **BIO19** Precip Coldest Quarter | `R_sandbox/bio_19.R` | `pr`, `tasmin` | Sum of pr in argmin 3-month rolling window of tasmin |

Matrix-only `_m.R` variants call C++ functions directly on pre-extracted matrices, bypassing the terra block loop — useful for benchmarking and embedding in other workflows.

> **Known bugs in sandbox code:**
> - `bio_18.R:31` reads from `tasmax` instead of `pr` when computing the precipitation sum.
> - `bio_13.R:24` uses `rcpp_parallel_which_max_row` which returns the **value** (correct for BIO13), not an index — the function name is misleading but the result is correct.

---

## 5. Computation Dependency Graph

```
Inputs
  tas     (12 layers: monthly mean temperature)
  tasmax  (12 layers: monthly max temperature)
  tasmin  (12 layers: monthly min temperature)
  pr      (12 layers: monthly precipitation)

─────────────────────────────────────────────────────
Simple aggregations
─────────────────────────────────────────────────────
  mean(tas)                    → BIO1
  sd(tas) × 100                → BIO4
  max(tasmax)                  → BIO5
  min(tasmin)                  → BIO6
  sum(pr)                      → BIO12
  max(pr)                      → BIO13 (sandbox)
  min(pr)                      → BIO14 (sandbox)
  coef_var(pr) × 100           → BIO15 (sandbox)

─────────────────────────────────────────────────────
Shared intermediates
─────────────────────────────────────────────────────
  BIO2_val = mean(tasmax) − mean(tasmin)
  BIO7_val = max(tasmax)  − min(tasmin)

  BIO2 = BIO2_val
  BIO7 = BIO7_val
  BIO3 = 100 × BIO2_val / BIO7_val   [recomputed independently in bio_03.R]

─────────────────────────────────────────────────────
Rolling quarter indices (per cell)
─────────────────────────────────────────────────────
  q_wet_pr     = argmax 3-month rolling sum(pr)       → BIO8, BIO16
  q_dry_pr     = argmin 3-month rolling sum(pr)       → BIO9, BIO17
  q_warm_tas   = argmax 3-month rolling mean(tas)     → BIO10, BIO18
  q_cold_tas   = argmin 3-month rolling mean(tas)     → BIO11, BIO19

─────────────────────────────────────────────────────
Quarter aggregations
─────────────────────────────────────────────────────
  BIO8  = mean(tas[ q_wet_pr  : q_wet_pr+2  ])
  BIO9  = mean(tas[ q_dry_pr  : q_dry_pr+2  ])
  BIO10 = mean(tas[ q_warm_tas: q_warm_tas+2])
  BIO11 = mean(tas[ q_cold_tas: q_cold_tas+2])
  BIO16 = sum (pr [ q_wet_pr  : q_wet_pr+2  ])   (sandbox)
  BIO17 = sum (pr [ q_dry_pr  : q_dry_pr+2  ])   (sandbox)
  BIO18 = sum (pr [ q_warm_tas: q_warm_tas+2])   (sandbox)
  BIO19 = sum (pr [ q_cold_tas: q_cold_tas+2])   (sandbox)
```

Each BIO function recalculates its needed intermediates from scratch (no shared state between functions). This is intentional for memory efficiency — no large intermediate rasters are retained.

---

## 6. NA Handling

Two policies coexist across the codebase:

| Policy | Parameter | Behaviour |
|---|---|---|
| **Strict** | `na_rm = FALSE` (default) | Any NA in a row/window → NA output |
| **NA-removing** | `na_rm = TRUE` | Skip NAs; all-NA row/window → NA |

**Important caveat**: The older C++ functions `rcpp_parallel_average` (production, `src/rcpp-parallel-average.cpp`) and `rcpp_parallel_difference` do **not** handle NA explicitly — they use raw `std::accumulate`/`std::transform` which propagate `NaN` implicitly.

The R-level guard `checkmate::assert_matrix(..., any.missing = FALSE)` in `parallel_average()` (`R/parallel_average.R:13`) enforces that no NAs are present before BIO1 and BIO2 call this function. BIO3 and BIO7 rely on the same function.

---

## 7. Performance Architecture

### Parallelism

- **`RcppParallel::parallelFor`** partitions the row dimension of the block matrix across Intel TBB worker threads automatically. Thread count is controlled by `RcppParallel::setThreadOptions()` at runtime, or explicitly in `bio_12_optim.R:8` via `nthreads = parallel::detectCores()`.
- **`src/Makevars`** enables OpenMP (`-fopenmp`) plus aggressive compiler optimization (`-O3 -march=native -ffast-math`).

### Block Processing

- Rasters are never fully loaded into memory. Terra's scheduler decides block sizes based on available RAM.
- `bio_12_optim.R:20` requests smaller blocks (`n = block_factor * nlyr(pr) = 48`) for better memory locality.

### Data Layout

`terra::readValues(..., mat = TRUE)` returns a cells × months matrix. With rows as spatial units, C++ `RMatrix<double>::row(i)` accesses a contiguous row — cache-friendly for per-cell operations.

### Efficiency Notes

| Concern | Detail |
|---|---|
| BIO3 source deduplication | `R/bio_03.R:62–77` avoids double `readStart` on identical sources |
| Rolling quarter scan | O(12) per row with early exits for NA — fast for 12-month data |
| BIO8–BIO11 two-pass reads | Two block passes required (index computation, then average) |
| `rcpp_get_average_quarter` | **Single-threaded** (no `parallelFor`) — legacy; replaced by `rcpp_parallel_average_quarter` in all current production code |

---

## 8. CHELSA Dataset Integration

The sandbox helpers `chelsa_bio_1.R` and `chelsa_bio_12.R` illustrate the intended real-data workflow:

```r
chelsa_bio_12 <- function(year, targetDir, outputDir) {
  varFiles <- list.files(
    path    = paste0(targetDir, year, "/"),
    pattern = "pr_", full.names = TRUE
  )
  pr <- terra::rast(varFiles)   # stack 12 monthly files by pattern
  pr <- pr / 100                # CHELSA stores pr in units of 0.01 mm
  filename <- paste0(outputDir, year, "/bio_12.tif")
  fastBioClim::bio_12(pr, filename = filename)
}
```

**Key integration points:**

| Aspect | Detail |
|---|---|
| File organization | CHELSA V2.1 files per year in a directory, named with variable pattern (`tas_`, `pr_`, etc.) |
| Stacking | `terra::rast(varFiles)` automatically stacks 12 monthly files into a 12-layer `SpatRaster` |
| Precipitation scaling | CHELSA stores precipitation as integers ×100 → divide by 100 before calling bio functions |
| Temperature scaling | No rescaling applied in sandbox code; values assumed to already be in °C |
| CRS / resolution | Output inherits from CHELSA input rasters (typically 30-arcsecond global grids for CHELSA V2.1) |

---

## 9. Test Coverage Summary

Tests reside in `tests/testthat/` and cover all production functions (BIO1–BIO12 plus primitives). Each test file verifies:

1. **Return type**: single-layer `SpatRaster` with correct layer name
2. **Numerical correctness**: against known analytic values using mock data with predictable structure
3. **NA propagation**: both `na_rm = FALSE` and `na_rm = TRUE` paths
4. **Wrap-around quarter**: for BIO8–BIO11 (`wrap = TRUE` / `wrap = FALSE`)
5. **Input validation errors**: wrong class, wrong layer count, dimension mismatch, invalid filename
6. **File write capability**: `filename = tempfile(fileext = ".tif")`

---

## 10. Production vs. Sandbox Status

| BIOs | Status | Location |
|---|---|---|
| BIO1–BIO12 | **Production** (exported, tested) | `R/` + `src/` |
| BIO12 optimized (`bio_12_optim`) | **Production** (exported) | `R/bio_12_optim.R` |
| BIO13–BIO19 | **Prototype** (not exported from package) | `R_sandbox/`, `src_sandbox/` |
| Matrix-only `_m` variants (all BIOs) | **Prototype** | `R_sandbox/` |
| CHELSA data wrappers (`chelsa_bio_*`) | **Prototype** | `R_sandbox/chelsa_bio_*.R` |

The sandbox BIO13–BIO19 use an older rolling-quarter API (three aligned shifted matrices passed as separate arguments) that has been superseded in production by the unified `rcpp_parallel_which_max_rolling_quarter(full_matrix)` API introduced with BIO8–BIO11.

---

## References

- Karger, D.N., et al. (2022). CHELSA V2.1: High-resolution monthly and annual climatologies for the earth land surface areas. *Earth System Science Data*, 14, 5573–5610. <https://essd.copernicus.org/articles/14/5573/2022/>
- Hijmans, R.J., et al. (2005). Very high resolution interpolated climate surfaces for global land areas. *International Journal of Climatology*, 25(15), 1965–1978. <https://doi.org/10.1002/joc.1276>
- Nix, H.A. (1986). A biogeographic analysis of Australian elapid snakes. In: Longmore, R. (ed.), *Atlas of Elapid Snakes of Australia*, Australian Flora and Fauna Series 7, Bureau of Flora and Fauna, Canberra.
- CHELSA Bioclim dataset: <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
