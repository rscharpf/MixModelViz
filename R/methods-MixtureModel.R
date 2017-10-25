#' @include AllClasses.R
NULL


#' @include help.R
NULL


#' @rdname getThetaMatrix-method
#' @aliases getThetaMatrix,MixtureModel-method
setMethod("getThetaMatrix", "MixtureModel", function(model) {
  unname(theta(model))
})

#' @rdname getThetaMatrix-method
#' @aliases getThetaMatrix,SingleBatchModel-method
setMethod("getThetaMatrix", "SingleBatchModel", function(model) {
  matrix(callNextMethod(model), ncol=k(model))
})

#' @rdname getSigmaMatrix-method
#' @aliases getSigmaMatrix,MixtureModel-method
setMethod("getSigmaMatrix", "MixtureModel", function(model) {
  unname(sigma(model))
})

#' @rdname getSigmaMatrix-method
#' @aliases getSigmaMatrix,SingleBatchModel-method
setMethod("getSigmaMatrix", "SingleBatchModel", function(model) {
  matrix(rep_len(callNextMethod(model), k(model)), nrow=1)
})

#' @rdname getSigmaMatrix-method
#' @aliases getSigmaMatrix,MultiBatchPooled-method
setMethod("getSigmaMatrix", "MultiBatchPooled", function(model) {
  matrix(rep(callNextMethod(model), k(model)), ncol=k(model))
})

#' @rdname getSigmaMatrix-method
#' @aliases getSigmaMatrix,MultiBatchCopyNumberPooled-method
setMethod("getSigmaMatrix", "MultiBatchCopyNumberPooled", function(model) {
  matrix(rep(callNextMethod(model), k(model)), ncol=k(model))
})


#' @rdname summarizeObserved-method
#' @aliases summarizeObserved,MixtureModel-method
setMethod("summarizeObserved", "MixtureModel", function(model) {
  data.frame(
    x.val=oned(model),
    batch=factor(batch(model), seq(max(batch(model))), ordered=TRUE),
    component=factor(map_z(model), seq(k(model)), ordered=TRUE)
  )
})

#' @rdname summarizeObserved-method
#' @aliases summarizeObserved,SingleBatchCopyNumber-method
setMethod("summarizeObserved", "SingleBatchCopyNumber", function(model) {
  result <- callNextMethod(model)
  result$copynumber <- factor(copyNumber(model), levels=seq(max(mapping(model))), ordered=TRUE)
  result
})

#' @rdname summarizeObserved-method
#' @aliases summarizeObserved,MultiBatchModel-method
setMethod("summarizeObserved", "MultiBatchModel", function(model) {
  result <- callNextMethod(model)
  nBatch <- length(levels(result$batch))
  result <- rbind(result, transform(result, batch="marginal"))
  result$batch <- factor(result$batch, levels=c("marginal", seq(nBatch)), ordered=TRUE)
  result
})

#' @rdname summarizeObserved-method
#' @aliases summarizeObserved,MultiBatchCopyNumber-method
setMethod("summarizeObserved", "MultiBatchCopyNumber", function(model) {
  result <- callNextMethod(model)
  result$copynumber <- factor(copyNumber(model), levels=seq(max(mapping(model))), ordered=TRUE)
  result
})

#' @rdname summarizeObserved-method
#' @aliases summarizeObserved,MultiBatchCopyNumberPooled-method
setMethod("summarizeObserved", "MultiBatchCopyNumberPooled", function(model) {
  result <- callNextMethod(model)
  result$copynumber <- factor(copyNumber(model), levels=seq(max(mapping(model))), ordered=TRUE)
  result
})


#' @rdname summarizeTheoretical-method
#' @aliases summarizeTheoretical,MixtureModel-method
setMethod("summarizeTheoretical", c("MixtureModel", "data.frame"), function(model, obs.df) {
  theta.mat <- getThetaMatrix(model)
  sigma.mat <- getSigmaMatrix(model)
  stopifnot(identical(dim(theta.mat), dim(sigma.mat)))

  x.range <- range(obs.df$x.val)
  nBins <- ceiling(sqrt(length(obs.df$x.val)))
  binSize <- diff(x.range)/nBins

  crosstab.mat <- as.matrix(table(factor(obs.df$batch, seq(nrow(theta.mat))),
                                  obs.df$component))

  pred.df <- Reduce(rbind, lapply(seq(nrow(theta.mat)), function(b) {
    Reduce(rbind, lapply(seq(ncol(theta.mat)), function(k) {
      transform(
        data.frame(
          theta=theta.mat[b,k],
          sigma=sigma.mat[b,k],
          batch=factor(b, seq(nrow(theta.mat)), ordered=TRUE),
          component=factor(k, seq(ncol(theta.mat)), ordered=TRUE),
          x=seq(x.range[1], x.range[2], length=1000)),
        y=dnorm(x, theta, sigma)*binSize*crosstab.mat[b,k]
      )
    }))
  }))
  list(theoretical=pred.df, nBins=as.integer(nBins))
})

#' @rdname summarizeTheoretical-method
#' @aliases summarizeTheoretical,SingleBatchCopyNumber-method
setMethod("summarizeTheoretical", c("SingleBatchCopyNumber", "data.frame"), function(model, obs.df) {
  result <- callNextMethod(model, obs.df)
  result$theoretical$copynumber <- factor(mapping(model), seq(max(mapping(model))))[result$theoretical$component]
  result
})

#' @rdname summarizeTheoretical-method
#' @aliases summarizeTheoretical,MultiBatchCopyNumber-method
setMethod("summarizeTheoretical", c("MultiBatchCopyNumber", "data.frame"), function(model, obs.df) {
  result <- callNextMethod(model, obs.df)
  result$theoretical$copynumber <- factor(mapping(model), seq(max(mapping(model))))[result$theoretical$component]
  result
})

#' @rdname summarizeTheoretical-method
#' @aliases summarizeTheoretical,MultiBatchCopyNumberPooled-method
setMethod("summarizeTheoretical", c("MultiBatchCopyNumberPooled", "data.frame"), function(model, obs.df) {
  result <- callNextMethod(model, obs.df)
  result$theoretical$copynumber <- factor(mapping(model), seq(max(mapping(model))))[result$theoretical$component]
  result
})


#' @rdname summarize-method
#' @aliases summarize,MixtureModel-method
setMethod("summarize", c("MixtureModel", "tbl_df"), function(model, ds.tbl) {
  obs.df <- summarizeObserved(model)

  obs.df <- obs.df[match(ds.tib$tile, tileSummaries(ds.tib)$tile), ]
  if (max(cn.df$batch) > 1)  # Cannot be tested with SingleBatch/Pooled models
    stopifnot(ds.tib$batch == obs.df$batch)
  obs.df$x.val <- ds.tib$logratio
  obs.df$batch.var <- ds.tib$batch.var

  result <- summarizeTheoretical(model, subset(obs.df, batch != "marginal"))
  new("MixtureSummary",
      observed=obs.df,
      theoretical=results$theoretical,
      nBins=results$nBins)
})

#' @rdname summarize-method
#' @aliases summarize,MixtureModel-method
setMethod("summarize", c("MixtureModel", "missing"), function(model, ds.tbl) {
  obs.df <- summarizeObserved(model)
  results <- summarizeTheoretical(model, subset(obs.df, batch != "marginal"))
  new("MixtureSummary",
      observed=obs.df,
      theoretical=results$theoretical,
      nBins=results$nBins)
})

#' @rdname summarize-method
#' @aliases summarize,SingleBatchCopyNumber-method
setMethod("summarize", c("SingleBatchCopyNumber", "tbl_df"), function(model, ds.tbl) {
  result <- callNextMethod(model, ds.tbl)
  as(result, "CopyNumberMixtureSummary")
})

#' @rdname summarize-method
#' @aliases summarize,MultiBatchCopyNumber-method
setMethod("summarize", c("MultiBatchCopyNumber", "tbl_df"), function(model, ds.tbl) {
  result <- callNextMethod(model, ds.tbl)
  as(result, "CopyNumberMixtureSummary")
})

#' @rdname summarize-method
#' @aliases summarize,MultiBatchCopyNumberPooled-method
setMethod("summarize", c("MultiBatchCopyNumberPooled", "tbl_df"), function(model, ds.tbl) {
  result <- callNextMethod(model, ds.tbl)
  as(result, "CopyNumberMixtureSummary")
})

