#  TODO: include links to GraphicalParameters in plot_summary documentation


setMethod("init_plot", c("CopyNumberMixtureSummary"), function(summ) {
  ggplot(mapping=aes_string(color="copynumber", fill="copynumber"))
})


setMethod("init_plot", c("MixtureSummary"), function(summ) {
  ggplot(mapping=aes_string(color="component", fill="component"))
})

setMethod("init_scales", c("MixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            result <- list()
            if(!is.null(color_palette(params))) {
              result <- append(result,
                               scale_color_manual(name="Component",
                                                  values=color_palette(params)))
            }

            if(!is.null(fill_palette(params))) {
              result <- append(result,
                               scale_fill_manual(name="Component",
                                                 values=fill_palette(params)))
            }
            result
})

setMethod("init_scales", c("CopyNumberMixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            result <- list()
            if(!is.null(color_palette(params))) {
              result <- append(result,
                               scale_color_manual(name="CopyNumber",
                                                  values=color_palette(params)))
            }

            if(!is.null(fill_palette(params))) {
              result <- append(result,
                               scale_fill_manual(name="CopyNumber",
                                                 values=fill_palette(params)))
            }
            result
})

#' @rdname plot_summary-method
#' @aliases plot_summary,MixtureSummary,GraphicalParameters-method
setMethod("plot_summary", c("MixtureSummary", "GraphicalParameters"),
          function(summ, params) {

        ggp <- init_plot(summ) +
          do.call("geom_histogram",
                  modifyList(
                    list(
                      data=getObserved(summ),
                      mapping=aes_string("x.val", "..count.."),
                      bins=nBins(summ),
                      position=position_stack()),
                    getHistogramParams(params))) +
          do.call("geom_line",
                  modifyList(
                    list(
                      data=getTheoretical(summ),
                      mapping=aes_string("x", "y", group="component")),
                    getLineParams(params))) +
          init_scales(summ, params) +
          scale_y_sqrt()


  nBatches <- length(levels(getTheoretical(summ)$batch))
  # Faceting and legend positioning
  if (nBatches > 1) {
    batch_labels <- batch_counts <- table(getObserved(summ)$batch)
    batch_labels <- c("marginal", paste("batch", names(batch_labels[-1])))
    batch_labels <- sprintf("%s (n=%d)", batch_labels, batch_counts)

    names(batch_labels) <- names(batch_counts)

    ggp <- ggp + facet_wrap("batch", scales="free_y",
                            ncol=round(sqrt(nBatches + 1)),
                            labeller=labeller(batch=batch_labels))


    if((nBatches / round(sqrt(nBatches))) %% 1 != 0) {
      ggp <- ggp + theme(legend.position = c(1, 0),
                         legend.justification = c(1, 0),
                         legend.box="horizontal")
    }
  }

  ggp
})


#' @rdname plot_summary-method
#' @aliases plot_summary,MixtureSummary,missing-method
setMethod("plot_summary", c("MixtureSummary", "missing"),
          function(summ, params) plot_summary(summ, new("GraphicalParameters")))
