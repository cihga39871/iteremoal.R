
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
feature.removal.iteration <- function(g1.signal, g0.signal,
									  offset=1,
									  weight.method="reciprocal.colSums",
									  scoreStandardization.method="min_max",
									  scoreCombine.method="linear.combine", ...) {
	# multiple offset
	if (length(offset) > 1){
		multiple.feature.removal.iteration(g1.signal, g0.signal,
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
}}



# vector of offsets
#' @title Stepwise feature removal method internal wrapper
#'
#' @description This function screens features iteratively in consideration of
#' limiting overfitting and overall performance.
#' @inheritParams feature.removal.iteration
#' @family iteration
#' @details See details in \code{feature.removal}
#' @return a list with names "offset", "removed.feature_names", "removed.scores", and "max.scores".
multiple.feature.removal.iteration <- function(g1.signal, g0.signal,
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
		removing.stat <- feature.removal.iteration(g1.signal, g0.signal,
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
#' @family iteration
#' @details The method removes one feature/row in each iteration, and requires
#' two dataframes, \code{g1} and \code{g0}, with identical row names. Normally,
#' \code{g0} is the control set.\\
#' In each iteration, first, \code{g1} and \code{g0} are converted to dataframes
#' of 1 or 0 by \code{cutoff1}, \code{cutoff0}, and \code{lt}. The converted
#' dataframes are called \code{gx.singal}, and \code{x} stands for 1 and 0. If
#' you do not want the conversion, let the parameters \code{cutoff1=g1,
#' cutoff0=g0, lt="<-"}, or just use an alternative function
#' \code{iteremoval::multiple.feature.removal.iteration}.\\
#' Second, \code{gx.weight}, weight of gx, is computed using \code{weight.method}.
#' The weight is for the observations/columns, not the features/rows. The
#' default weight method is \code{reciprocal.colSums}, ie.
#' \code{1 / (1 + colSums(gx.signal, na.rm=T))}. You can specify your own
#' function, and the first parameter of the function should be the exact word of
#' \code{gx.signal}.\\
#' Third, \code{gx.score}, the score dataframe for observations and features, is
#' computed. It is the result of dot product of \code{gx.signal} and \code{gx.weight}.\\
#' Then, Summing up \code{gx.score} by row, and the result is standardized with
#' function \code{scoreStandardization.method}. Default standardization method is
#' Min-Max, ie. normalizing the vector to 0-1 range. You can specify your own
#' function, and the first parameter of the function is the sum-up dataframe.\\
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
#' @param cutoff1 \code{g1} is converted to a dataframe filled with 1 or 0 by \code{cutoff1}
#' and \code{lt}. The result is called \code{g1.signal}. For example, if \code{lt=">"},
#' the result of the step is \code{g1.signal <- g1 > cutoff1}. If you do not want
#' the conversion, let \code{cutoff1} be \code{g1} dataframe, and \code{lt="<-"}.
#' @param cutoff0 \code{g0} is converted to dataframes of 1 or 0 by \code{cutoff0} and \code{lt}. 
#' It has the same usage as \code{cutoff1}. Different \code{cutoff1} and \code{cutoff0} 
#' influence overfitting.
#' @param lt An operator to compare \code{gx} and \code{cutoffx}. Default is ">".
#' Other options include ">=", "<=", "<", etc.
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
#' g1 = SWR1[,5:ncol(SWR1)]
#' g0 = SWR0[,5:ncol(SWR0)]
#'
#' row.names(g1) <- SWR1[,4]
#' row.names(g0) <- SWR0[,4]
#'
#' result.simple <- feature.removal(g1, g0, cutoff1=0.95, cutoff0=0.95)
#'
#' result.complex <- feature.removal(g1, g0,
#'     cutoff1=0.95, cutoff0=0.925, lt=">",
#'     offset=c(0.5, 1, 2),
#'     weight.method="reciprocal.colSums",
#'     scoreStandardization.method="min_max",
#'     scoreCombine.method="linear.combine")
#'
feature.removal <- function(g1, g0, cutoff1, cutoff0, lt=">",
							offset=1,
							weight.method=reciprocal.colSums,
							scoreStandardization.method=min_max,
							scoreCombine.method=linear.combine) {

	# convert to TRUE/FALSE table
	g1.signal = eval(parse(text = sprintf("'%s'(g1, cutoff1)", lt)))
	g0.signal = eval(parse(text = sprintf("'%s'(g0, cutoff1)", lt)))

	multiple.feature.removal.iteration(g1.signal, g0.signal,
									   of=offset,
									   we=weight.method,
									   scoreSt=scoreStandardization.method,
									   scoreCo=scoreCombine.method)

}

