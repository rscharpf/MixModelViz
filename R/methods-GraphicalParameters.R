#' @include AllClasses.R
NULL


#' @include help.R
NULL


#' @rdname getHistogramParams-method
#' @aliases getHistogramParams,GraphicalParameters-method
setMethod("getHistogramParams", c("GraphicalParameters"), function(params) {
  result <- list()
  if(!is.null(params@histogram.alpha))
    result <- append(result, list(alpha=params@histogram.alpha))
  if(!is.null(params@histogram.linetype))
    result <- append(result, list(linetype=params@histogram.linetype))
  if(!is.null(params@histogram.size))
    result <- append(result, list(size=params@histogram.size))
  result
})


#' @rdname getLineParams-method
#' @aliases getLineParams,GraphicalParameters-method
setMethod("getLineParams", c("GraphicalParameters"), function(params) {
  result <- list()
  if(!is.null(params@line.alpha))
    result <- append(result, list(alpha=params@line.alpha))
  if(!is.null(params@line.linetype))
    result <- append(result, list(linetype=params@line.linetype))
  if(!is.null(params@line.size))
    result <- append(result, list(size=params@line.size))
  result
})


setMethod("fill_palette", "GraphicalParameters", function(params) params@fill.palette)


setMethod("color_palette", "GraphicalParameters", function(params) params@color.palette)

