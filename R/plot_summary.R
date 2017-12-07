#  TODO: include links to GraphicalParameters in plot_summary documentation


setMethod("init_plot", c("MixtureSummary"), function(summ) {
  ggplot(mapping=aes_string(color="component", fill="component"))
})

setMethod("init_plot", c("CopyNumberMixtureSummary"), function(summ) {
  ggplot(mapping=aes_string(color="copynumber", fill="copynumber"))
})

setMethod("init_histogram", c("MixtureSummary"), function(summ, ...) {
  do.call("geom_histogram",
          modifyList(
            list(
              data=getObserved(summ),
              mapping=aes_string("x.val", "..count.."),
              bins=nBins(summ),
              position=position_stack(),
              linetype=0),
            list(...)))
})

setMethod("init_histogram", c("CopyNumberMixtureSummary"), function(summ, ...) {
  do.call("geom_histogram",
          modifyList(
            list(
              data=getObserved(summ),
              mapping=aes_string("x.val", "..count.."),
              bins=nBins(summ),
              position=position_stack(),
              linetype=0),
            list(...)))
})

setMethod("init_density", c("MixtureSummary"), function(summ, ...) {
  do.call("geom_line",
          modifyList(
            list(
              data=getTheoretical(summ),
              mapping=aes_string("x", "y", group="component"),
              size=1),
            list(...)))
})

setMethod("init_density", c("CopyNumberMixtureSummary"), function(summ, ...) {
  do.call("geom_line",
          modifyList(
            list(
              data=getTheoretical(summ),
              mapping=aes_string("x", "y", group="component"),
              size=1),
            list(...)))
})

setMethod("init_scales", c("MixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            if(is.null(color_palette(params))) {
              result <- scale_color_manual(name="Component")
            } else {
              result <- scale_color_manual(name="Component", values=color_palette(params))
            }

            if(is.null(fill_palette(params))) {
              list(result, scale_fill_manual(name="Component"))
            } else {
              list(result, scale_fill_manual(name="Component", values=fill_palette(params)))
            }
          })

setMethod("init_scales", c("CopyNumberMixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            if(is.null(color_palette(params))) {
              result <- scale_color_manual(name="Copy Number")
            } else {
              result <- scale_color_manual(name="Copy Number", values=color_palette(params))
            }

            if(is.null(fill_palette(params))) {
              list(result, scale_fill_manual(name="Copy Number"))
            } else {
              list(result, scale_fill_manual(name="Copy Number", values=fill_palette(params)))
            }
          })

init_facetting <- function(summ, nBatches) {
  batch_labels <- batch_counts <- table(getObserved(summ)$batch)
  batch_labels <- paste("batch", names(batch_labels))
  marginal_batch <- names(batch_labels) == "marginal"
  if(any(marginal_batch)) {
    batch_labels[marginal_batch] <- "marginal"
  }
  batch_labels <- sprintf("%s (n=%d)", batch_labels, batch_counts)
  names(batch_labels) <- names(batch_counts)

  result <- facet_wrap("batch", scales="free_y",
                          ncol=round(sqrt(nBatches + 1)),
                          labeller=labeller(batch=batch_labels))

  if((nBatches / round(sqrt(nBatches))) %% 1 != 0) {
    result <- list(result, theme(legend.position = c(1, 0),
                                 legend.justification = c(1, 0),
                                 legend.box="horizontal"))
  }
  result
}

#' @rdname plot_summary-method
#' @aliases plot_summary,MixtureSummary,GraphicalParameters-method
setMethod("plot_summary", c("MixtureSummary", "GraphicalParameters"), function(summ, params) {

  ggp <- init_plot(summ) +
    do.call("init_histogram", append(summ, getHistogramParams(params))) +
    do.call("init_density", append(summ, getLineParams(params))) +
    init_scales(summ, params) +
    scale_y_sqrt()

  nBatches <- length(levels(getTheoretical(summ)$batch))
  if (nBatches > 1) {
    ggp <- ggp + init_facetting(summ, nBatches)
  }
  ggp
})


#' @rdname plot_summary-method
#' @aliases plot_summary,MixtureSummary,missing-method
setMethod("plot_summary", c("MixtureSummary", "missing"),
          function(summ, params) plot_summary(summ, new("GraphicalParameters")))
