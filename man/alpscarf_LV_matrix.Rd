% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alpscarf_statistics.R
\name{alpscarf_LV_matrix}
\alias{alpscarf_LV_matrix}
\title{Calculate Levenshtein distance of each pair of the given set of sequences}
\usage{
alpscarf_LV_matrix(alp_df = NULL, expected_order = NULL)
}
\arguments{
\item{alp_df}{a data frame with Alpscarf info, at least two columns "p_name" "AOI"}

\item{expected_order}{expected visit order, two columns "AOI" and "AOI_order"}
}
\value{
LV_matrix_df, a Levenshtein-distance matrix of each pair of "p_name"
}
\description{
Calculate Levenshtein distance of each pair of the given set of sequences
}
