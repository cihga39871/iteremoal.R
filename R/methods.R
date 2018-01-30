#' @title Using function or parsing expression for normal class
#'
#' @description Evaluating a function or expression by the type of
#' \code{method}.
#' @param method if the class of method is "character", regarding method as a
#' function and evaluating \code{THE_METHOD(...)}. If the class of method is
#' "function", return the result of \code{method(...)}. If the class of method
#' is "expression", return the result of \code{eval(method)}.
#' @param ... arguments passed onto method if the class of method is
#' "character" or "function".
#' @return Evaluated result.
#' @export
#' @examples
#' funcOrExp(sample, 5)
#' funcOrExp('sample', 5)
#' funcOrExp(parse(text='sample(5)'))
funcOrExp <- function(method, ...) {
    if (class(method) == "character") {
        res <- eval(parse(text = sprintf("%s(...)", method)))
    }
    else if (class(method) == "expression") {
        res <- eval(method)
    }
    else if (class(method) == "function") {
        res <- method(...)
    }
    else {
        stop("`method` is not a function or an expression; to use expression, please convert your character string using parse(text=\"...\").")
    }
    res
}


######################
#   Weight methods   #
######################

#' @title Weight methods
#' @description Weight methods of function \code{feature.removal}.
#'
#' @param gx.signal The first parameter of the weight method used in
#' \code{feature.removal} must be the exact word \code{gx.signal}.
#' @name weight_methods
NULL

#' @rdname weight_methods
#' @return \code{1 / (1 + colSums(gx.signal, na.rm=T))}.
#' @export
#' @examples
#' gx.singal <- data.frame(x=1:5, t=2:6, k=c(3:6, NA))
#' reciprocal_colSums(gx.singal)
reciprocal_colSums <- function(gx.signal) {
    1 / (1 + colSums(gx.signal, na.rm=TRUE))
}

#' @rdname weight_methods
#' @return \code{1}.
#' @export
#' @examples
#' 1 == ones(2)
#' 1 == ones(c(4,6))
ones <- function(gx.signal) {
    1
}


#################################
# Score standardization methods #
#################################

#' @title Score standardization methods
#' @description Score standardization methods of function 
#' \code{feature.removal}.
#'
#' @param x The first parameter of score standardization method used in
#' \code{feature.removal} is the sum-up dataframe. See details in the help page
#' of function \code{feature.removal}.
#' @name score_standardization_methods
NULL

#' @rdname score_standardization_methods
#' @param na.rm Bool. Remove NA or not.
#' @return Numeric, normalized \code{x} to 0-1 range.
#' @export
#' @examples
#' min_max(1:5)
#' min_max(c(1:5, NA), na.rm=TRUE)
min_max <- function(x, na.rm=TRUE){
    x.max <- max(x, na.rm = na.rm)
    x.min <- min(x, na.rm = na.rm)
    return((x - x.min) / (x.max - x.min))
}


#################################
#     Score combine methods     #
#################################

#' @title Score combine methods
#' @description Score combine methods of function 
#' \code{feature.removal}.
#' To combine the feature score vectors of g1 and g0. The method
#' used in \code{feature.removal} must have three parameters in order,
#' \code{g1.score.feature}, \code{g0.score.feature}, and \code{offset}.
#' See details in the help page of function \code{feature.removal}.
#'
#' @param g1.score.feature feature score vector for g1.
#' @param g0.score.feature feature score vector for g0.
#' @param offset adjusts the proportion of \code{g1.score.feature}.
#' Default is \code{1}.
#' @name score_combine_methods
NULL

#' @rdname score_combine_methods
#' @return \code{g1.score.feature * offset + g0.score.feature}.
#' @export
#' @examples
#' linear_combine(0.2, 0.3, 2)
#' linear_combine(1:2, 3:4, 1)
linear_combine <- function(g1.score.feature, g0.score.feature, offset=1) {
    g1.score.feature * offset + g0.score.feature
}
