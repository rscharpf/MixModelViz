#' @include AllClasses.R
NULL


#' @include help.R
NULL


#' @rdname ObservedNames-method
#' @aliases ObservedNames,MixtureSummary-method
setMethod("ObservedNames", "MixtureSummary",
          function(object) c("x.val", "batch", "component"))

#' @rdname TheoreticalNames-method
#' @aliases TheoreticalNames,MixtureSummary-method
setMethod("TheoreticalNames", "MixtureSummary",
          function(object) c("batch", "component", "x", "y"))

#' @rdname getObserved-method
#' @aliases getObserved,MixtureSummary-method
setMethod("getObserved", "MixtureSummary", function(object) object@observed)

#' @rdname getTheoretical-method
#' @aliases getTheoretical,MixtureSummary-method
setMethod("getTheoretical", "MixtureSummary", function(object) object@theoretical)

#' @rdname nBins-method
#' @aliases nBins,MixtureSummary-method
setMethod("nBins", "MixtureSummary", function(object) object@nBins)

#' @rdname addMarginalBatch-method
#' @aliases addMarginalBatch,MixtureSummary-method
setMethod("addMarginalBatch", "MixtureSummary", function(summ) {
  obs.df <- getObserved(summ)
  theor.df <- getTheoretical(summ)

  stopifnot(is.factor(obs.df$batch))
  stopifnot(is.factor(theor.df$batch))
  batch_levels <- levels(obs.df$batch)
  stopifnot(identical(levels(theor.df$batch), batch_levels))
  stopifnot(!("marginal" %in% batch_levels))

  if (length(batch_levels) > 1) {  # MultiBatch
    # Append marginal batch to observed summary
    obs.marginal.df <- obs.df
    obs.marginal.df$batch <- "marginal"
    obs.df <- rbind(obs.df, obs.marginal.df)
    obs.df$batch <- factor(obs.df$batch,
                           c("marginal", batch_levels),
                           ordered=TRUE)


    # Calculate marginal batch for theoretical summary
    theor_fields <- names(theor.df)
    required_fields <- TheoreticalNames(summ)
    by_fields <- required_fields[!(required_fields %in% c("y", "batch"))]
    marginal.df <- aggregate(theor.df$y, theor.df[, by_fields], sum)
    names(marginal.df) <- c(by_fields, "y")
    marginal.df$batch <- "marginal"

    # Fill in NA for fields not returned by `TheoreticalNames(summ)`
    extra_fields <- theor_fields[!(theor_fields %in% required_fields)]
    if(length(extra_fields)) {
      extra.df <- lapply(setNames(nm=extra_fields), function(x) NA)
      marginal.df <- cbind(marginal.df, extra.df)
    }

    # Append marginal batch to theoretical summary
    marginal.df <- marginal.df[,sort(names(marginal.df))[rank(theor_fields)]]
    theor.df <- rbind(theor.df, marginal.df)
    theor.df$batch <- factor(theor.df$batch, levels(obs.df$batch), ordered=TRUE)
  } else {  # Single Batch
      # Recode batch to "marginal" for both summaries
      obs.df$batch <- factor("marginal", ordered=TRUE)
      theor.df$batch <- factor("marginal")
  }

  summ@observed <- obs.df
  summ@theoretical <- theor.df
  summ
})

#' @rdname addMarginalModel-method
#' @aliases addMarginalModel,MixtureSummary-method
setMethod("addMarginalModel", "MixtureSummary", function(summ) {
  theor.df <- getTheoretical(summ)

  stopifnot(is.factor(theor.df$component))
  component_levels <- levels(theor.df$component)
  stopifnot(!("marginal" %in% component_levels))

  if (length(component_levels) > 1) {
    # Calculate marginal component for theoretical summary
    marginal.df <- aggregate(theor.df$y, theor.df[, c("x", "batch")], sum)
    names(marginal.df) <- c("x", "batch", "y")
    marginal.df$component <- "marginal"

    # Fill in NA for fields besides "component", x", and "y"`
    theor_fields <- names(theor.df)
    extra_fields <- theor_fields[!(theor_fields %in% c("batch", "component", "x", "y"))]
    if(length(extra_fields)) {
      extra.df <- lapply(setNames(nm=extra_fields), function(x) NA)
      marginal.df <- cbind(marginal.df, extra.df)
    }

    # Append marginal model to theoretical summary
    marginal.df <- marginal.df[,sort(names(marginal.df))[rank(theor_fields)]]
    theor.df <- rbind(theor.df, marginal.df)
    theor.df$component <- factor(theor.df$component,
                                 c("marginal", component_levels),
                                 ordered=TRUE)
  } else {
    # For clarity, do not recode k=1 to "marginal"
    warning(sprintf(cat("Only one component detected in %s object,",
                        "marginal model was not added to summary.\n")),
            class(summ))
  }

  summ@theoretical <- theor.df
  summ
})

setMethod("show", "MixtureSummary", function(object) {
  cat("An object of class '", class(object), "'\n")
  cat("  Observed Model:\n")
  show(head(getObserved(object)))
  cat("  Theoretical Model:\n")
  show(head(getTheoretical(object)))
  cat("  nBins: ", object@nBins, "\n")
})

setValidity("MixtureSummary", function(object) {
  observed.names <- ObservedNames(object)
  theoretical.names <- TheoreticalNames(object)

  result <- c()

  observed.missing <- ! observed.names %in% names(getObserved(object))
  if (any(observed.missing)) {
    result <- sprintf("Column \"%s\" is missing from the observed mixture data frame.",
                      observed.names[observed.missing])
  }

  theoretical.missing <- ! theoretical.names %in% names(getTheoretical(object))
  if (any(theoretical.missing)) {
    result <- c(result,
                sprintf("Column \"%s\" is missing from the theoretical mixture data frame.",
                        theoretical.names[theoretical.missing]))
  }

  if (length(result)) {
    result
  } else {
    TRUE
  }
})
