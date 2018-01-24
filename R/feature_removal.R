
#' @title Stepwise feature removal method internal
#'
#' @description This function screens features iteratively in consideration of
#'  limiting overfitting and overall performance.
#' @inheritParams feature.removal
#' @param g1.signal a dataframe with the row of feature, and the column of observation.
#' Cells are numeric or bool.
#' @param g0.signal a dataframe with the same row names as \code{g1.signal}. Normally, the observations
#' in \code{g0.signal} are in the distinct group of \code{g1.signal}.
#' @family iteration
#' @details See details in \code{feature.removal}
#' @return a list with names "offset", "removed.feature_names", "removed.scores", and "max.scores".
.feature.removal.iteration <- function(g1.signal, g0.signal,
									  offset=1,
									  weight.method="reciprocal.colSums",
									  scoreStandardization.method="min_max",
									  scoreCombine.method="linear.combine", ...) {
	# multiple offset
	if (length(offset) > 1){
		.multiple.feature.removal.iteration(g1.signal, g0.signal,
										   offs=offset,
										   wei=weight.method,
										   scoreSt=scoreStandardization.method,
										   scoreCo=scoreCombine.method, ...)
	} else {

		# data check
		if (!identical(row.names(g1.signal), row.names(g0.signal)))
			stop("Row names of g1 and g2 not identical")

		removed.scores = c()
		removed.feature_names = c()
		max.scores = c()

		while (length(nrow(g1.signal)) > 0 && nrow(g1.signal) > 0) {

			# weight
			g1.weight = funcOrExp(weight.method, g1.signal) %>% as.numeric()
			g0.weight = funcOrExp(weight.method, g0.signal) %>% as.numeric()

			g1.score = apply(g1.signal, 1, function(x) x * g1.weight) %>% t
			g0.score = apply(g0.signal, 1, function(x) x * g0.weight) %>% t

			# scoreStandardization
			g1.score.feature = rowSums(g1.score, na.rm=TRUE) %>% funcOrExp(scoreStandardization.method, .)
			g0.score.feature = rowSums(g0.score, na.rm=TRUE) %>% funcOrExp(scoreStandardization.method, .)

			# scoreCombine
			score.feature = funcOrExp(scoreCombine.method, g1.score.feature, g0.score.feature, offset)

			score.min = min(score.feature)
			score.max = max(score.feature)

			score.min.feature = which(score.min == score.feature)
			# score.max.feature = which(score.max == score.feature)


			n.removed = length(score.min.feature)
			removed.scores = c(removed.scores, rep(score.min, n.removed))
			removed.feature_names   = c(removed.feature_names  , names(score.min.feature  ))
			max.scores     = c(max.scores    , rep(score.max, n.removed))

			g1.signal = g1.signal[-1 * score.min.feature, ]
			g0.signal = g0.signal[-1 * score.min.feature, ]

		}
		return(list(offset=offset,
					removed.feature_names   = removed.feature_names  ,
					removed.scores = removed.scores,
					max.scores     = max.scores   ))
	}
}



# vector of offsets
#' @title Stepwise feature removal method internal wrapper
#'
#' @description This function screens features iteratively in consideration of
#' limiting overfitting and overall performance.
#' @inheritParams .feature.removal.iteration
#' @family iteration
#' @details See details in \code{feature.removal}
#' @return a list with names "offset", "removed.feature_names", "removed.scores", and "max.scores".
.multiple.feature.removal.iteration <- function(g1.signal, g0.signal,
									  offset=1,
									  weight.method="reciprocal.colSums",
									  scoreStandardization.method="min_max",
									  scoreCombine.method="linear.combine", ...) {

	# data check
	if (!identical(row.names(g1.signal), row.names(g0.signal)))
		stop("Row names of g1 and g2 not identical")

	df.removed.feature_names = data.frame(Index = seq_len(nrow(g1.signal)))
	df.removed.scores = data.frame(Index = seq_len(nrow(g1.signal)))
	df.max.scores = data.frame(Index = seq_len(nrow(g1.signal)))

	for (the.offset in offset) {
		writeLines(sprintf("Current offset: %s", the.offset))
		removing.stat <- .feature.removal.iteration(g1.signal, g0.signal,
													of=the.offset,
													we=weight.method,
													scoreSt=scoreStandardization.method,
													scoreCo=scoreCombine.method, ...)

		n.removed = length(removing.stat$removed.feature_names)
		the.removed.feature_names = c(rep(NA, nrow(g1.signal) - n.removed), removing.stat$removed.feature_names)
		the.removed.scores = c(rep(NA, nrow(g1.signal) - n.removed), removing.stat$removed.scores)
		the.max.scores     = c(rep(NA, nrow(g1.signal) - n.removed), removing.stat$max.scores    )

		df.removed.feature_names$NEW   = the.removed.feature_names
		df.removed.scores$NEW = the.removed.scores
		df.max.scores$NEW     = the.max.scores

		names(df.removed.feature_names  )[ncol(df.removed.feature_names  )] <- sprintf("Offset%s", the.offset)
		names(df.removed.scores)[ncol(df.removed.scores)] <- sprintf("Offset%s", the.offset)
		names(df.max.scores    )[ncol(df.max.scores    )] <- sprintf("Offset%s", the.offset)
	}

	return(list(offset = offset,
				removed.feature_names   = df.removed.feature_names  ,
				removed.scores = df.removed.scores,
				max.scores     = df.max.scores   ))

}



#' @title Stepwise feature removal method
#'
#' @description This function screens features iteratively in consideration of
#' limiting overfitting and overall performance.
#'
#' @section Other usages:
#' feature.removal(g1, g0, cutoff1, cutoff0, lt = ">", offset = 1,
#' weight.method = reciprocal.colSums, scoreStandardization.method = min_max,
#' scoreCombine.method = linear.combine, ...)
#' 
#' feature.removal(SE, g0.filter, cutoff1, cutoff0, lt = ">", offset = 1,
#' weight.method = reciprocal.colSums, scoreStandardization.method = min_max,
#' scoreCombine.method = linear.combine, ...)
#'
#' @details The method removes one feature/row in each iteration, and requires (A)
#' two dataframes, \code{g1} and \code{g0}, with identical row names; OR (B)
#' A SummarizedExperiment object \code{SE}, and a logical vector \code{g0.filter} to
#' define \code{SE}'s columns that belong to \code{g0}. Normally, \code{g0} is the
#' control set. \code{SE} will be devided to \code{g1} and \code{g0} automatically.
#'
#' @details
#' In each iteration, first, \code{g1} and \code{g0} are converted to dataframes
#' of 1 or 0 by \code{cutoff1}, \code{cutoff0}, and \code{lt}. The converted
#' dataframes are called \code{gx.singal}, and \code{x} stands for 1 and 0. If
#' you do not want the conversion, let \code{lt="skip"}, and cutoffs will be ignored.
#'
#' @details
#' Second, \code{gx.weight}, weight of gx, is computed using \code{weight.method}.
#' The weight is for the observations/columns, not the features/rows. The
#' default weight method is \code{reciprocal.colSums}, ie.
#' \code{1 / (1 + colSums(gx.signal, na.rm=T))}. You can specify your own
#' function, and the first parameter of the function should be the exact word of
#' \code{gx.signal}.
#'
#' @details
#' Third, \code{gx.score}, the score dataframe for observations and features, is
#' computed. It is the result of dot product of \code{gx.signal} and \code{gx.weight}.
#'
#' @details
#' Then, Summing up \code{gx.score} by row, and the result is standardized with
#' function \code{scoreStandardization.method}. Default standardization method is
#' Min-Max, ie. normalizing the vector to 0-1 range. You can specify your own
#' function, and the first parameter of the function is the sum-up dataframe.
#'
#' @details
#' After that, \code{gx.score.feature}, the feature scores of gx are calculated.
#' Now using \code{scoreCombine.method} to combine the feature score vectors of
#' g1 and g0. This method must have three parameters in order, \code{g1.score.feature},
#' \code{g0.score.feature}, and \code{offset}. Default method is \code{linear.combine}.
#' \code{offset} in the default method adjusts the proportion of \code{g1.score.feature}.
#' Specifically, \code{g1.score.feature * offset + g0.score.feature}. Besides,
#' \code{offset} can be a number or a vector. If it is a vector, the overall iteration
#' is done for each offset respectively.
#'
#' @param g1 a dataframe with the row of feature, and the column of observation.
#' Cells are numeric or bool.
#' @param g0 a dataframe with the same row names as \code{g1}. Normally, the observations
#' in \code{g0} are in the distinct group of \code{g1}.
#' @param SE a SummarizedExperiment object.
#' @param g0.filter a logical vector \code{g0.filter} to
#' define \code{SE}'s columns that belong to \code{g0}. 
#' @param cutoff1 \code{g1} is converted to a dataframe filled with 1 or 0 by \code{cutoff1}
#' and \code{lt}. The result is called \code{g1.signal}. For example, if \code{lt=">"},
#' the result of the step is \code{g1.signal <- g1 > cutoff1}. If you do not want
#' the conversion, let \code{lt="skip"}.
#' @param cutoff0 \code{g0} is converted to dataframes of 1 or 0 by \code{cutoff0} and \code{lt}.
#' It has the same usage as \code{cutoff1}. Different \code{cutoff1} and \code{cutoff0}
#' influence overfitting.
#' @param lt An operator to compare \code{gx} and \code{cutoffx}. Default is ">".
#' Other options include ">=", "<=", "<", etc. Additionally, \code{lt="skip"} skips the
#' comparation and \code{cutoffx} will be ignored.
#' @param offset a parameter in \code{scoreCombine.method}. It adjusts the score proportion
#' of g1 and g2. Besides, \code{offset} can be a number or a numeric vector.
#' If it is a vector, the overall iteration is done for each offset respectively.
#' See more in parameter \code{scoreCombine.method}.
#' @param weight.method \code{gx.weight}, weight of gx, is computed using
#' \code{weight.method}. The weight is for the observations/columns, not the
#' features/rows. The default weight method is \code{reciprocal.colSums}, ie.
#' \code{1 / (1 + colSums(gx.signal, na.rm=T))}. You can specify your own function,
#' and the first parameter of the function should be the exact word of \code{gx.signal}.
#' @param scoreStandardization.method Default standardization method is Min-Max,
#' ie. normalizing the vector to 0-1 range. You can specify your own function,
#' and the first parameter of the function is the sum-up dataframe. See more in
#' Details section.
#' @param scoreCombine.method to combine the feature score vectors of g1 and g0.
#' This method must have three parameters in order, \code{g1.score.feature},
#' \code{g0.score.feature}, and \code{offset}. Default method is \code{linear.combine}.
#' \code{offset} in the default method adjusts the proportion of \code{g1.score.feature}.
#' Specifically, \code{g1.score.feature * offset + g0.score.feature}. Besides,
#' \code{offset} can be a number or a vector. If it is a vector, the overall
#' iteration is done for each offset respectively.
#' @param ... Other parameter passed to method of expression class.
#' @return a list with names "offset", "removed.feature_names", "removed.scores",
#' and "max.scores".
#' @export
#' @author Jiacheng CHUAN
#' @examples
#'
#' g1 <- SWRG1; g0 <- SWRG0
#' result.simple.A <- feature.removal(g1, g0, cutoff1=0.95, cutoff0=0.95)
#'
#' result.simple.B <- feature.removal(SummarizedData, SummarizedData$Group==0,
#'     cutoff1=0.95, cutoff0=0.95)
#'
#' result.complex <- feature.removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925, lt=">",
#'     offset=c(0.5, 2),
#'     weight.method="reciprocal.colSums",
#'     scoreStandardization.method="min_max",
#'     scoreCombine.method="linear.combine")
#'
feature.removal <- function(g1=NULL, g0=NULL, cutoff1, cutoff0, lt=">",
							offset=1,
							weight.method=reciprocal.colSums,
							scoreStandardization.method=min_max,
							scoreCombine.method=linear.combine,
							SE=NULL, g0.filter=NULL, ...) {

	# check conflict between (SE, g0.filter) and (g1, g0)
	if (is.null(g1) && is.null(g0)) {
		if (is.null(SE) && is.null(g0.filter)) {
			stop("Please enter your dataset.")
		}
		else {
			# passing parameters
			g1 <- SE
			g0 <- g0.filter
		}
	}

	# check if `g1` belongs to SummarizedExperiment
	if (grepl("SummarizedExperiment", class(g1))) {

		# sanity check:
		if (class(g0) != "logical" || length(g0) != ncol(g1)){
			stop("Detected `g1` belonging to class SummarizedExperiment, but the second parameter is not a vector of TRUE/FALSE with the same length as `ncol(g1)`. TRUE means column belongs to g0, while FALSE means column belongs to g1.")
		}

		subset0 = assays(g1[,  g0])[[1]]
		subset1 = assays(g1[, !g0])[[1]]
		g0 <- as.data.frame(subset0)
		g1 <- as.data.frame(subset1)
	}

	# convert to TRUE/FALSE table
	if (lt != "skip") {
		g1.signal = eval(parse(text = sprintf("'%s'(g1, cutoff1)", lt)))
		g0.signal = eval(parse(text = sprintf("'%s'(g0, cutoff1)", lt)))
	} else {
		g1.signal = g1
		g0.signal = g0
	}

	.multiple.feature.removal.iteration(g1.signal, g0.signal,
									   of=offset,
									   we=weight.method,
									   scoreSt=scoreStandardization.method,
									   scoreCo=scoreCombine.method, ...)

}


