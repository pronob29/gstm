# Group-specific Structural Topic Model (gSTM)

## Project Overview

This repository hosts the development version of the **Group-specific Structural Topic Model (gSTM)**, an extension of the **Structural Topic Model (STM)** designed for topic modeling with covariates. The **gSTM** incorporates group-specific structures, allowing for more nuanced analysis of text data by accounting for both covariates and group-specific characteristics.

This project is inspired by the foundational work done on the **`stm`** R package by **Molly Roberts, Brandon Stewart, and Dustin Tingley**. We extend their concepts to incorporate group-specific modifications, improving the flexibility and specificity of the topic modeling process.

For more information on the original STM model, visit [www.structuraltopicmodel.com](http://www.structuraltopicmodel.com).

## Key Features

- **Group-Specific Topic Modeling**: Enhances the original STM to allow for group-specific topics and covariate effects.
- **Text Data Ingestion**: Tools for preprocessing and managing text data for topic modeling.
- **Covariate and Group Effects on Topics**: Explore the combined influence of covariates and group structures on topic prevalence and content, with uncertainty quantification.
- **Visualization Tools**: Generate comprehensive visualizations, including topic correlation graphs and group-specific topic models.

## Credit and Acknowledgements

This project draws heavily from the original **`stm` R package**. Full credit for the original STM model goes to:
- **Molly Roberts** (Harvard University)
- **Brandon Stewart** (Princeton University)
- **Dustin Tingley** (Harvard University)

For inquiries related to the original **STM** package, please contact **Brandon Stewart** at **bms4 [AT] princeton.edu**.

My name is **Pronob Kumar Barman**, and this repository is an implementation and extension of their work. You can find this project under my GitHub account **[pronob29](https://github.com/pronob29/gstm)**.

## Installation

To get started, follow the steps below:

1. Ensure R is installed on your system. If not, download it from [http://www.r-project.org/](http://www.r-project.org/).
2. Install the CRAN version of the original `stm` package:

```r
install.packages("stm")
```

3. Install the development version of the gSTM package from this GitHub repository using `devtools`. First, install `devtools`:

```r
if(!require(devtools)) install.packages("devtools")
```

Then install gSTM:

```r
library(devtools)
install_github("pronob29/gstm", dependencies=TRUE)
```

4. This will install the package and its required dependencies.

## Getting Started

To begin using **gSTM**, explore the primary function `gstm()` for estimating group-specific structural topic models. You can preprocess raw text data using `textProcessor()` or ingest term-document matrices with `readCorpus()`.

Refer to the vignette for a detailed walkthrough of example analyses and further documentation on how to use this package.

## Contribution

We welcome contributions and suggestions to further improve **gSTM**. Please feel free to open issues or submit pull requests on this repository to help enhance the implementation.

Thank you for using **gSTM**!
