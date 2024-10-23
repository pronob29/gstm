# Group-specific Structural Topic Model (gSTM)

## Project Overview

The **Group-specific Structural Topic Model (gSTM)** is an R package designed to extend the **Structural Topic Model (STM)** by introducing group-specific structures. This model enables the estimation of topics with covariates and group-specific characteristics, allowing for more detailed and flexible topic modeling. The **gSTM** model is particularly suited for automating the formation of personalized support groups in online health forums, leveraging both user-generated content and demographic data.

This project is inspired by the original **`stm`** R package developed by **Molly Roberts**, **Brandon Stewart**, and **Dustin Tingley**, and builds upon their foundational work by incorporating group-specific extensions. The addition of group-specific parameters enhances the model's ability to capture demographic and interaction-based differences across user groups, making it ideal for applications like online health community support group formation.

## Key Features

- **Group-Specific Topic Modeling**: Extends the **Structural Topic Model** by introducing group-specific covariate effects on topics.
- **Text Data Processing**: Provides tools for ingesting and preprocessing text data for topic modeling.
- **Covariate and Group Effects on Topics**: Explore how covariates and group structures influence both topic prevalence and content.
- **Visualization Tools**: Includes functions to visualize group-specific topics, topic correlations, and covariate effects.

## Credit and Acknowledgements

This project is built on the concepts from the **`stm`** R package, developed by:

- **Molly Roberts** (Harvard University)
- **Brandon Stewart** (Princeton University)
- **Dustin Tingley** (Harvard University)

Full credit for the original **STM** model goes to them, and this package builds upon their ideas with group-specific extensions. The **gSTM** package is maintained by **Pronob Kumar Barman**, and can be found on GitHub at **[pronob29/gstm](https://github.com/pronob29/gstm)**.

## Installation

To install and use the **gSTM** package, follow these steps:

1. First, ensure that **R** is installed on your system. You can download it from [https://www.r-project.org/](https://www.r-project.org/).
   
2. Install the **gSTM** package directly from GitHub using the following commands:

    ```r
    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("pronob29/gstm")
    ```

This will install **gSTM** along with any required dependencies.

## Getting Started

Once installed, you can start using the **gSTM** package for your topic modeling needs. Below is a simple example to get you started:

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
