% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard_generation.R
\name{generate_dashboard}
\alias{generate_dashboard}
\title{Generate AutoXplainR Dashboard}
\usage{
generate_dashboard(
  autoxplain_result,
  output_file = "autoxplainr_dashboard.html",
  top_features = 5,
  sample_instances = 3,
  include_llm_report = TRUE,
  llm_api_key = NULL,
  open_browser = TRUE
)
}
\arguments{
\item{autoxplain_result}{Object from autoxplain() function}

\item{output_file}{Character. Path for output HTML file (default: "autoxplainr_dashboard.html")}

\item{top_features}{Integer. Number of top features to analyze in detail (default: 5)}

\item{sample_instances}{Integer. DEPRECATED - no longer used (default: 3)}

\item{include_llm_report}{Logical. Whether to include LLM-generated report (default: TRUE)}

\item{llm_api_key}{Character. Google Generative AI API key (optional)}

\item{open_browser}{Logical. Whether to open dashboard in browser (default: TRUE)}
}
\value{
Character. Path to generated HTML file
}
\description{
Creates a comprehensive HTML dashboard with comparative model explanations,
visualizations, and natural language summaries.
}
