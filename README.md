
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Patterns of sedentary time accumulation according to age in the United States: A 2003-2006 NHANES analysis

<!-- badges: start -->
<!-- badges: end -->

<img src="img/icampam-2024-logo.jpg" width="200px" style="display: block; margin: auto;" />

This repository contains the materials relating to the abstract
submission to 2024 ICAMPAM conference. The code implements an analytical
pipeline using R programming language. The pipeline performs the
followings:

- Download NHANES 2003-2006 demographic, anthropometric, and
  accelerometer data.
- Process accelerometer data.
- Join data.
- Generate a report of the analyses.

The analyses of accelerometer data are based on the `{activAnalyzer}`
package. If you want to reproduce the analytical pipeline on your
machine using an RStudio environment, please follow the steps below.

- Step 1: [Install R](https://cran.rstudio.com/) (recommended versions:
  \>=4.3.1), [RStudio](https://posit.co/download/rstudio-desktop/), and
  the latest version of
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (if your are
  a Windows user) on your machine. To retrieve previous releases of R,
  you can go [here](https://cran.r-project.org/bin/windows/base/old/)
  for Windows, and [here](https://cran.r-project.org/bin/macosx/) for
  Mac.

- Step 2: Go to the GitHub repository of the project and click on the
  green button called `Code`. Then click on `Download ZIP`.

- Step 3: Unzip the zipped folder on your machine, open it, and
  double-click on the `activAnalyzer.nhanes.Rproj` file to open the
  project in RStudio.

- Step 4: Restore the package dependencies of the project with `{renv}`
  using in the Console the command line shown below and then following
  the instructions proposed in the Console.

``` r
renv::restore()
```

This may take several minutes so that all the required packages are
downloaded from the web and then installed in the project directory.

- Step 5: Run the analytical pipeline with `{targets}` using the
  following command line in the Console:

``` r
targets::tar_make()
```

This last step will produce all the objects relating to the analytical
process.

Running the whole pipeline will take several hours (likely between 5 and
7 hours depending on the features of your machine). This is due to the
high volume of data to be downloaded from the web, as well as to an
important number of accelerometer data files to be processed (more than
12,000 files). Processing accelerometer data here is pretty slow, even
using parallel processing, because some functions of the
`{activAnalyzer}` package are pretty time consumming and cannot easily
be improved as they depend on other packages. There are some packages
available on the web that allow very fast analyses of NHANES
accelerometer data thanks to the implementation of C++ code and a
smarter structure of the data (e.g., `{accelerometry}`). So why the
`{activAnalyzer}` package ? This is mainly due to a lack of awareness of
other packages available to compute particular metrics as activity
accumulation metrics, which was required for the present work.

Source of the logo: <https://ismpb.org/>.
