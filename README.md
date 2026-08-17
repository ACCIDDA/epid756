# epid756: Infectious Disease Modeling Exercises and Materials

`epid756` is an R package designed for the EPID 756 Infectious Disease Modeling course. It packages student exercise scripts, solutions, reference materials, and datasets, ensuring all required R package dependencies are automatically installed.

---

## Prerequisites & Setup

Before installing the `epid756` package, make sure R and RStudio are installed on your machine:

1. **Install R**: Download and install R (>= 3.5.0) from [CRAN Cloud](https://cloud.r-project.org/).
2. **Install RStudio**: Download and install RStudio Desktop from the [Posit RStudio IDE Downloads Guide](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads).

---

## Installation

You can install `epid756` directly from GitHub using the `remotes` package:

```r
# Install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install epid756 from GitHub
remotes::install_github("ACCIDDA/epid756")
```

---

## Basic Usage

### 1. Copy Assignment Scripts

To copy all course exercise scripts and reference materials to a directory of your choice:

```r
library(epid756)

# Copy assignments to a local folder named "assignments"
copy_assignments(path = "assignments")
```

This creates the specified folder and populates it with all `.R`, `.Rmd`, and `.qmd` exercise files.

### 2. List Available Assignments

To list all exercise and reference files contained in the package:

```r
list_assignments()
```

### 3. Copy Solutions

To copy solution scripts to a local folder:

```r
# Copy solution scripts to a local folder named "solutions"
copy_solutions(path = "solutions")

# List available solutions
list_solutions()
```

### 4. Access Package Datasets

The package includes simulated epidemiological data (`simdata`) used across course exercises:

```r
library(epid756)

# Load simdata
data("simdata", package = "epid756")
head(simdata)
```

---

## Alternative: RStudio Git Checkout

If you prefer to check out the repository directly using RStudio:
1. Open RStudio and select **File -> New Project**.
2. Select **Version Control**, then **Git**.
3. Enter the Repository URL: `https://github.com/ACCIDDA/epid756`.

For detailed visuals, see the [Intro Slides](https://docs.google.com/presentation/d/1_0eno21uN9Do_9H7i6LBPrlLSlV2x8rk/?slide=id.p9#slide=id.p9).

---

## Local Development & Quality Assurance

If you are developing or modifying the package, you can run quality checks using `just`:

```bash
# Run roxygen2, lintr, testthat unit tests, and R CMD check --as-cran
just
```

Available `just` commands:
- `just docs`: Regenerate documentation and `NAMESPACE`
- `just lint`: Run package linting (`lintr`)
- `just test`: Run `testthat` unit test suite
- `just check-cran`: Run `R CMD check --as-cran`
- `just clean`: Clean up build artifacts
