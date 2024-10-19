# Group-specific Structural Topic Model (gSTM)

## Project Overview

The **Group-specific Structural Topic Model (gSTM)** is an R package designed to extend the Structural Topic Model (STM) by introducing group-specific structures. This model allows for the estimation of topics with covariates and group-specific characteristics, enabling more detailed and flexible topic modeling.

This project is inspired by the original **`stm`** R package developed by **Molly Roberts, Brandon Stewart, and Dustin Tingley**, and builds upon their foundational work by incorporating group-specific extensions.

## Key Features

- **Group-Specific Topic Modeling**: Extends the Structural Topic Model by allowing for group-specific covariate effects on topics.
- **Text Data Processing**: Provides tools for ingesting and preprocessing text data.
- **Covariate and Group Effects on Topics**: Explore how covariates and group structures influence topic prevalence and content.
- **Visualization Tools**: Functions for visualizing group-specific topics, topic correlations, and covariate effects.

## Credit and Acknowledgements

This project is built on the concepts of the **`stm`** package, developed by:
- **Molly Roberts** (Harvard University)
- **Brandon Stewart** (Princeton University)
- **Dustin Tingley** (Harvard University)

This repository is maintained by **Pronob Kumar Barman**, and it can be found on GitHub under **[pronob29/gstm](https://github.com/pronob29/gstm)**.

## Installation

To install and use the **gSTM** package, follow these steps:

1. First, ensure that **R** is installed on your system. You can download it from [https://www.r-project.org/](https://www.r-project.org/).

2. Install the **gSTM** package directly from GitHub using the following commands:

    ```r
    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("pronob29/gstm")
    ```

3. This will install **gSTM** along with any required dependencies.

## Getting Started

Once installed, you can start using the **gSTM** package for your topic modeling needs. Below is an example to get you started:

```r
library(gstm)

# Preprocess text data
processed_data <- textProcessor(documents = your_raw_text_data)

# Fit the group-specific structural topic model
gstm_model <- gstm(documents = processed_data$documents, 
                   vocab = processed_data$vocab, 
                   K = 20, 
                   prevalence = ~ covariates, 
                   groups = your_group_variable)

# Summarize and visualize the model
summary(gstm_model)
plot(gstm_model)
