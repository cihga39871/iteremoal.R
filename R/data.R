#' @title Group 0 data
#'
#' @description Probability data of control samples. Column represents samples,
#' row represents genome ranges related to a disease.
#'
#' @details We identified 299 genomic regions related to the methylation status
#' of a disease. Then, we sequenced the cfDNA of 44 subjects. 22 were malignant,
#'  while others were normal. We built a statistical model to compute the
#'  probability of the disease for each region and subject, and this dataframe
#'  contains the probabilities of 299 regions and 22 normal samples.
#'
#' @format A data frame with 22 variables: \code{G0Sample1},
#' \code{G0Sample2}, ..., \code{G0Sample22}.
#' @family data
"SWRG0"

#' @title Group 1 data
#'
#' @description Probability data of positive samples. Column represents samples,
#' row represents genome ranges related to a disease.
#'
#' @details We identified 299 genomic regions related to the methylation status
#' of a disease. Then, we sequenced the cfDNA of 44 subjects. 22 were malignant,
#'  while others were normal. We built a statistical model to compute the
#'  probability of the disease for each region and subject, and this dataframe
#'  contains the probabilities of 299 regions and 22 malignant samples.
#'
#' @format A data frame with 22 variables: \code{G0Sample1},
#' \code{G0Sample2}, ..., \code{G0Sample22}.
#' @family data
"SWRG1"

#' @title Summarized data
#'
#' @description RangedSummarizedExperiment object contains the probability
#' data of the control and positive samples. It is the integration of
#' \code{SWRG0} and \code{SWRG1}. \code{colData} in \code{SummarizedData}
#' contains a column 'Group' indicating normal(0) from malignant(1). In assay,
#' column represents samples,
#' row represents genome ranges related to a disease.
#'
#' @details We identified 299 genomic regions related to the methylation status
#' of a disease. Then, we sequenced the cfDNA of 44 subjects. 22 were malignant,
#'  while others were normal. We built a statistical model to compute the
#'  probability of the disease for each region and subject, and this dataframe
#'  contains the probabilities of 299 regions and 44 samples.
#'
#' @format RangedSummarizedExperiment.
#' @family data
"SummarizedData"
