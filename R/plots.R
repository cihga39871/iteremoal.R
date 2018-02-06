
#' @title Iteration trace of removed scores
#' @description plot the score of removed feature in each iteration.
#' @family plot
#' @return ggplot2 object.
#' @param li the list result of \code{feature_removal}.
#' @import utils
#' @import ggplot2
#' @export
#' @examples
#' g1 <- SWRG1; g0 <- SWRG0
#'
#' result.complex <- feature_removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925,
#'     offset=c(0.5, 1, 2))
#'
#' # it is a ggplot2 object, so plus sign is available
#' ggiteration_trace(result.complex) + theme_bw()
ggiteration_trace <- function(li) {
    # check li a valid list
    if (is.null(li$removed.scores))
        stop("`li` do not contain 'removed.scores'. Generate `li` with function `feature_removal`.")

    stacked.removed.scores <- stack(li$removed.scores, select = -1)
    stacked.removed.scores$Index <- li$removed.scores$Index

    ggplot(stacked.removed.scores) +
        geom_line(aes(stacked.removed.scores$Index, values, color=stacked.removed.scores$ind)) +
        labs(x = "Index", y = "Minimum Prediction Value", color="Offset")
}


#' @import graphics
NULL

#' @title Feature prevalence
#' @family prevalencestat plot
#' @description Compute the feature prevalence after removing the features of
#' the first \code{index} iterations.
#' @param li the list result of \code{feature_removal}.
#' @param index removing the features of the first \code{index} iterations. It
#' allows a positive integer or a proper fraction. If inproper fraction, it is
#' regarded as \code{as.integer(index)}.
#' @param hist.plot bool. A switch to plot the histogram of the remaining
#' features.
#' @export
#' @return Matrix
#' @examples
#' g1 <- SWRG1; g0 <- SWRG0
#'
#' result.complex <- feature_removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925,
#'     offset=c(0.5, 1, 2))
#'
#' # index is a proportion in 0-1
#' prevalence.result <- feature_prevalence(result.complex, 0.5, hist.plot=TRUE)
#'
#' # index is a positive integer
#' prevalence.result <- feature_prevalence(result.complex, 233, hist.plot=TRUE)
feature_prevalence <- function(li, index, hist.plot=TRUE) {
    # check li a valid list
    if (is.null(li$removed.feature_names))
        stop("`li` do not contain 'removed.feature_names'. Generate `li` with function `feature_removal`.")

    nfeature <- nrow(li$removed.feature_names)

    # check: index >= 1 ? real index : percent of index
    if (0 <= index && index < 1) {
        index <- as.integer(nfeature * index) + 1L
    } else if (index < 0) {
        stop("`index` < 0. `index` is either a positive integer or a decimal in [0,1) as a quantile.")
    } else if (index > nfeature)
        stop("`index` > the feature number.")

    features.mt <- li$removed.feature_names[as.integer(index):nfeature,
                                            2:ncol(li$removed.feature_names)]
    features.all <- features.mt %>% as.vector %>% as.matrix(ncol=1) %>% sort

    Features <- table(unlist(features.all)) %>% sort
    if (hist.plot) hist(Features)
    return(Features)
}

#' @title Plot histogram of feature prevalence
#' @family plot prevalencestat
#' @description Compute the feature prevalence (present in different cutoffs)
#' after removing the features of
#' the first \code{index} iterations, and then plot the histogram of remaining
#' features. It calls \code{feature_prevalence(..., hist.plot=TRUE)}.
#' @param li the list result of \code{feature_removal}.
#' @param index removing the features of the first \code{index} iterations. It
#' allows a positive integer or a proper fraction. If inproper fraction, it is
#' regarded as \code{as.integer(index)}.
#' @export
#' @return histogram
#' @examples
#' g1 <- SWRG1; g0 <- SWRG0
#'
#' result.complex <- feature_removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925,
#'     offset=c(0.5, 1, 2))
#'
#' # index is a proportion in 0-1
#' feature_hist(result.complex, 0.5)
#'
#' # index is a positive integer
#' feature_hist(result.complex, 233)
feature_hist <- function(li, index) {
    Features <- feature_prevalence(li, index, hist.plot=FALSE)
    hist(Features)
}

#' @title Screening feature using prevalence
#' @family prevalencestat
#' @description Return the screened feature names.
#' @param features result of \code{feature_prevalence(...)}
#' @param prevalence the prevalence cutoff of features. The features with
#' prevalence less than \code{prevalence} are removed.
#' @export
#' @return Vector
#' @examples
#' g1 <- SWRG1; g0 <- SWRG0
#'
#' result.complex <- feature_removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925,
#'     offset=c(0.5, 1, 2))
#'
#' prevalence.result <- feature_prevalence(result.complex, 233, hist.plot=TRUE)
#'
#' feature.list <- feature_screen(prevalence.result, 3)
feature_screen <- function(features, prevalence) {
    which(features >= prevalence) %>% names
}

