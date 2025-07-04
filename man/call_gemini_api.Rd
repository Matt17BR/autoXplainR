% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm_integration.R
\name{call_gemini_api}
\alias{call_gemini_api}
\title{Call Google Generative AI API}
\usage{
call_gemini_api(prompt, api_key, model, max_tokens, temperature)
}
\arguments{
\item{prompt}{Character. Input prompt}

\item{api_key}{Character. API key}

\item{model}{Character. Model name}

\item{max_tokens}{Integer. Maximum tokens}

\item{temperature}{Numeric. Temperature parameter}
}
\value{
Character. Generated text
}
\description{
Call Google Generative AI API
}
\keyword{internal}
