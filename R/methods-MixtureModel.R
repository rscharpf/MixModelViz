#' @include AllClasses.R
NULL



#' @include help.R
NULL


setMethod("getThetaMatrix", "TempMix", function(model) {
  unname(theta(model))
})

setMethod("getThetaMatrix", "TempSBM", function(model) {
  matrix(callNextMethod(model), ncol=k(model))
})


setMethod("getSigmaMatrix", "TempMix", function(model) {
  unname(sigma(model))
})

setMethod("getSigmaMatrix", "TempSBM", function(model) {
  matrix(rep_len(callNextMethod(model), k(model)), nrow=1)
})

setMethod("getSigmaMatrix", "TempMBP", function(model) {
  matrix(rep(callNextMethod(model), k(model)), ncol=k(model))
})

setMethod("getSigmaMatrix", "TempMBCNP", function(model) {
  matrix(rep(callNextMethod(model), k(model)), ncol=k(model))
})


setMethod("summarizeObserved", "TempMix", function(model) {
  data.frame(
    x.val=oned(model),
    batch=batch(model),
    component=factor(map_z(model), seq(k(model)), ordered=TRUE)
  )
})

setMethod("summarizeObserved", "TempMixCN", function(model) {
  result <- callNextMethod(model)
  result$copynumber <- factor(copyNumber(model), levels=seq(max(mapping(model))), ordered=TRUE)
  result
})


setMethod("summarizeTheoretical", "TempMix", function(model) {
  theta.mat <- getThetaMatrix(model)
  sigma.mat <- getSigmaMatrix(model)
  stopifnot(identical(dim(theta.mat), dim(sigma.mat)))

  x.range <- range(oned(model))
  nBins <- ceiling(sqrt(length(oned(model))))
  binSize <- diff(x.range)/nBins

  crosstab.mat <- as.matrix(table(batch(model), map_z(model)))  # TODO: replace with method that will be indempotent so that crosstab.mat matches crosstab of `summarizeObserved`

  Reduce(rbind, lapply(seq(nrow(theta.mat)), function(b) {
    Reduce(rbind, lapply(seq(ncol(theta.mat)), function(k) {
      transform(
        data.frame(
          theta=theta.mat[b,k],
          sigma=sigma.mat[b,k],
          batch=factor(b, seq(nrow(theta.mat)), ordered=TRUE),
          component=factor(k, seq(ncol(theta.mat)), ordered=TRUE),
          x=seq(min(oned(model)), max(oned(model)), length=1000)),
        y=dnorm(x, theta, sigma)*binSize*crosstab.mat[b,k]
      )
    }))
  }))
})

setMethod("summarizeTheoretical", "TempMixCN", function(model) {
  result <- callNextMethod(model)
  result$copynumber <- factor(mapping(model), seq(max(mapping(model))))[result$component]
  result
})

setMethod("summarizeTheoretical", "TempMBM", function(model) {
  result <- callNextMethod(model)
  result <- rbind(result, transform(result, batch="marginal"))
  result$batch <- factor(result$batch, levels=c("marginal", seq(nBatches)), ordered=TRUE)
  result
})


#' @rdname summarize-method
#' @aliases summarize,MixtureModel-method
setMethod("summarize", "TempMix", function(model) {
  new("MixtureSummary",
      observed=summarizeObserved(model),
      theoretical=summarizeTheoretical(model),
      nBins=as.integer(ceiling(sqrt(length(oned(model))))))
})

setMethod("summarize", "TempMixCN", function(model) {
  new("CopyNumberMixtureSummary",
      observed=summarizeObserved(model),
      theoretical=summarizeTheoretical(model),
      nBins=as.integer(ceiling(sqrt(length(oned(model))))))
})

