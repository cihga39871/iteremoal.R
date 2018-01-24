#' @title Group 0 data
#'
#' @description Probability data of control samples.
#'
#' @format A data frame with 22 variables: \code{G0Sample1}, \code{G0Sample2}, ...,
#'   \code{G0Sample22}.
#' @family data
"SWRG0"

#' @title Group 1 data
#'
#' @description Probability data of positive samples.
#'
#' @format A data frame with 22 variables: \code{G0Sample1}, \code{G0Sample2}, ...,
#'   \code{G0Sample22}.
#' @family data
"SWRG1"

#' @title Summarized data
#'
#' @description RangedSummarizedExperiment object contains the probability data of the control and positive samples. It is the integration of \code{SWRG0} and \code{SWRG1}.
#'
#' @format RangedSummarizedExperiment. 
#' @family data
"SummarizedData"