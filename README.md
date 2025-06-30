# AutoXplainR

[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

An R package for automated machine learning explanation and comparative reporting. AutoXplainR provides an efficient, automated, and standardized method for generating comparative model explanations from AutoML outputs with clear reports suitable for diverse audiences.

🔗 **Repository**: [https://github.com/Matt17BR/AutoXplainR](https://github.com/Matt17BR/AutoXplainR)

## Key Features

- **Automated ML Pipeline**: Seamless integration with H2O AutoML
- **Custom Explanations**: Permutation importance and partial dependence plots implemented from scratch
- **Interactive Visualizations**: Custom plotly-based plots for all explanation types
- **Comparative Analysis**: Side-by-side model comparison and explanation
- **Natural Language Reports**: LLM-generated summaries using Google Generative AI
- **HTML Dashboards**: Comprehensive interactive dashboards using flexdashboard
- **Self-Reliant**: No dependencies on existing XAI packages like DALEX or iml

## Installation

### From GitHub (Recommended)

```r
# Install from GitHub
devtools::install_github("Matt17BR/AutoXplainR")

# Load the package
library(AutoXplainR)
```

### From Source

```r
# Download and install from local source
devtools::install_local("path/to/autoxplainr")

# Load the package
library(AutoXplainR)
```

### Prerequisites

Ensure you have the required dependencies installed:

```r
# Install devtools if not already installed
if (!require(devtools)) install.packages("devtools")

# H2O installation (if not already installed)
if (!require(h2o)) {
  install.packages("h2o")
}
```

## Quick Start

```r
# Basic usage
data(iris)
result <- autoxplain(iris, "Species", max_models = 5, max_runtime_secs = 300)

# Generate explanations
importance <- calculate_permutation_importance(result$models[[1]], iris, "Species")
pdp_data <- calculate_partial_dependence_multi(result$models[[1]], iris, c("Petal.Length", "Sepal.Width"))

# Create visualizations
plot_permutation_importance(importance)
plot_partial_dependence_multi(pdp_data)

# Generate comprehensive dashboard
generate_dashboard(result, "my_dashboard.html")

# Create natural language report (requires GEMINI_API_KEY)
report <- generate_natural_language_report(result, importance_data = importance)
```

## Core Functions

### AutoML Integration
- `autoxplain()`: Main function for automated ML with explanation pipeline

### Explanation Methods
- `calculate_permutation_importance()`: Feature importance via permutation
- `calculate_partial_dependence()`: Partial dependence for single features
- `calculate_partial_dependence_multi()`: Partial dependence for multiple features

### Visualization
- `plot_permutation_importance()`: Interactive importance bar charts
- `plot_partial_dependence()`: Interactive PDP line plots
- `plot_partial_dependence_multi()`: Multi-feature PDP subplots
- `plot_model_comparison()`: Model performance scatter plots

### Dashboard & Reporting
- `generate_dashboard()`: Comprehensive HTML dashboard with flexdashboard
- `create_simple_dashboard()`: Lightweight HTML dashboard
- `generate_natural_language_report()`: LLM-powered report generation

## Configuration

### Google Generative AI Setup
Set your API key for natural language report generation:

```r
# Set environment variable
Sys.setenv(GEMINI_API_KEY = "your_api_key_here")

# Or pass directly to function
generate_natural_language_report(result, api_key = "your_api_key_here")
```

## Example Workflow

```r
library(AutoXplainR)

# 1. Run AutoML
data(mtcars)
result <- autoxplain(mtcars, "mpg", max_models = 3, max_runtime_secs = 180)

# 2. Generate explanations for best model
model <- result$models[[1]]
importance <- calculate_permutation_importance(model, mtcars, "mpg")
top_features <- head(importance$feature, 3)
pdp_data <- calculate_partial_dependence_multi(model, mtcars, top_features)


# 4. Generate comprehensive dashboard
generate_dashboard(
  result, 
  output_file = "mtcars_analysis.html",
  top_features = 5,
  sample_instances = 3,
  include_llm_report = TRUE
)

# 5. Create individual plots
plot_model_comparison(result)
plot_permutation_importance(importance)
plot_partial_dependence_multi(pdp_data)
```

## Architecture

AutoXplainR follows a modular architecture:

1. **AutoML Engine**: H2O AutoML integration for model training
2. **Explanation Engine**: Custom implementations of:
   - Permutation feature importance
   - Partial dependence plots
3. **Visualization Engine**: Interactive plotly-based charts
4. **Reporting Engine**: Dashboard generation and LLM integration
5. **Testing Suite**: Comprehensive test coverage

## Dependencies

### Required
- h2o (>= 3.40.0)
- plotly (>= 4.10.0) 
- data.table (>= 1.14.0)
- jsonlite (>= 1.8.0)
- httr (>= 1.4.0)
- stringr (>= 1.4.0)

### Suggested
- flexdashboard (>= 0.6.0)
- rmarkdown (>= 2.0.0)
- DT (>= 0.20.0)
- testthat (>= 3.0.0)

## Development

This package was developed following rigorous engineering practices:
- Comprehensive testing after each component implementation
- Custom explanations implemented from scratch
- Extensive input validation and error handling
- Modular, extensible architecture
- Complete documentation with examples and vignettes

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## Issues and Support

If you encounter any issues or have questions:

1. Check the [package vignette](vignettes/autoxplainr-introduction.Rmd) for detailed usage examples
2. Search existing [GitHub Issues](https://github.com/Matt17BR/AutoXplainR/issues)
3. Create a new issue with a reproducible example

## Citation

If you use AutoXplainR in your research, please cite:

```
Mazzarelli, M. (2024). AutoXplainR: Automated Machine Learning Explanation and Comparative Reporting. 
R package version 0.1.0. https://github.com/Matt17BR/AutoXplainR
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

**Matteo Mazzarelli**  
📧 matteo.mazzarelli@gmail.com  
🐙 [GitHub Profile](https://github.com/Matt17BR)