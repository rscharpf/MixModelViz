#  TODO: add force_singlebatch to collapse densities for multibatch/pooled models


#' Plotting function for CopyNumberDataFrames
#'
#' Generates normal densities from theoretical theta and sigma values and plots
#'   them over a histogram of observed data.
#'
#' @param obs.df a data.frame of an observed mixture model for copy number polymorphisms
#'    `x.val` - a continuous value, such as LRR or a principal component
#'    `batch` - integer batch identifier, used for facetting
#'    `component` - integer value for a latent variable assignment corresponding
#'        to components described by the `theta` and `sigma` parameters, may be
#'        specified as the field used to group and fill the histogram
#'        ( see @param fill_aes ).
#'    `copynumber` - An integer value for a predicted copy state. This is the
#'        default field used to group and fill the histogram.
#'    `batch.var` - An (optional) character vector for improved facet labeling,
#'      values should map 1-to-1 to @param obs.df$batch .
#' @param thetas An integer-vector or matrix of theta values for a mixture model
#'      of normal distributions of the different batches. An integer-vector of
#'      length `k` will indicate a single-batch model with `k` predicted componets.
#'      A matrix of dimenions `c(k, b)` indicates a multi-batch model with `k`
#'      predicted components (even if k == 1) and `b` batches.
#' @param sigmas an integer-vector or matrix of sigma values for a mixture model
#'      of normal distributions of the different batches. An integer-vector of
#'      length `s` will indicate EITHER a single-batch model with `s` predicted
#'      componets or a multi-batch model with pooled variances and `s` batches.
#'      This ambiguity is resovled by a correctly-dimensioned `theta` matrix.
#'      A matrix of dimenions `c(k, b)` indicates a multi-batch (non-pooled) model
#'      with `k` predicted components (even if k == 1) and `b` batches.
#' @param copynumber_mapping An integer-vector of length `k` that indicates the
#'      predicted copy number for theoretical components `1:k`
#' @param fill_aes should be "copynumber" or "component", but can be any value
#'      that can be evaluated by `aes_(fill=as.name(fill_aes))` where `data=obs.df`
#' @param palette A vector of colors. To specify separate vectors for fill and
#    color aesthetics, provide a list named vectors, `fill` and `color`.
#' @param fixed_aes a list of alpha, linetype, and size values that will be
#'      passed on to geom_hist and geom_line as fixed aesthetic parameters.
#'      The named list should contain named vectors "hist" and "line".
#' @return An object of class `ggplot`
#' @export
#'
plot_model <- function(obs.df, thetas, sigmas, copynumber_mapping,
                       fill_aes="copynumber",
                       palette=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73",
                                 "4"="#F0E442", "5"="#0072B2"),
                       fixed_aes=list(hist=list(alpha=0.5, linetype=0,  size=0),
                                      line=list(alpha=1,   linetype=1,  size=1))
                       ) {

  # Coerce `thetas` and `sigmas` into Batch x Component matrices,
  #  determine the number of batches and components (`nBatches`, `nComponents`),
  #  establish the default 1:k `copynumber_mapping` if not provided, and
  #  build `bc.tab` - a Batch x Component contingency table
  thetas <- unname(thetas)
  multibatch <- is.matrix(thetas)
  if(!multibatch) {  # SingleBatch or SingleBatchPooled
    thetas <- matrix(thetas, nrow=1)
  }
  stopifnot(xor(multibatch, nrow(thetas) == 1))

  nComponents <- ncol(thetas)
  if(missing(copynumber_mapping)) copynumber_mapping <- seq(nComponents)
  nBatches <- nrow(thetas)

  sigmas <- unname(sigmas)
  if(!is.matrix(sigmas)) { # SingleBatch, SingleBatchPooled, or MultiBatchPooled
    sigmas <- matrix(sigmas, nrow=nrow(thetas))
    if(ncol(sigmas) == 1 & nComponents > 1) {  # SingleBatchPooled or MultiBatchPooled
      sigmas <- matrix(replicate(nComponents, c(sigmas)), ncol=nComponents)
    }
  }
  stopifnot(identical(dim(thetas), dim(sigmas)))

  if(multibatch) {
    bc.tab <- as.matrix(table(obs.df$batch, obs.df$component))

    obs.df <- rbind(obs.df, transform(obs.df, batch="marginal"))
    obs.df$batch <- factor(obs.df$batch, levels=c("marginal", seq(nBatches)), ordered=TRUE)
  } else {
    bc.tab <- matrix(table(obs.df$component), nrow=1)
  }


  # Generate points for density plots
  nBins <- ceiling(sqrt(nrow(obs.df)))
  binSize <- diff(range(obs.df$x.val))/nBins  # used to scale density to histogram

  predicted.df <- Reduce(rbind, lapply(seq(nBatches), function(b) {
    Reduce(rbind, lapply(seq(nComponents), function(k) {
      transform(
        data.frame(
          theta=thetas[b,k],
          sigma=sigmas[b,k],
          batch=factor(b, levels(obs.df$batch), ordered=TRUE),
          component=factor(k, levels(obs.df$component), ordered=TRUE),
          copynumber=factor(copynumber_mapping[k], levels(obs.df$copynumber), ordered=TRUE),
          x=seq(min(obs.df$x.val), max(obs.df$x.val), length=1000)),
        y=dnorm(x, theta, sigma)*binSize*bc.tab[b,k]
      )
    }))
  }))


  if(is.list(palette)) {
    stopifnot(all(c("color", "fill") %in% names(palette)))
  } else {
    palette <- list(color=palette, fill=palette)
  }

  ggp <- ggplot(obs.df, aes_(color=as.name(fill_aes), fill=as.name(fill_aes))) +
    geom_histogram(aes(x.val, ..count..), alpha=fixed_aes$hist$alpha,
                   linetype=fixed_aes$hist$linetype, size=fixed_aes$hist$size,
                   bins=nBins, position=position_stack()) + #reverse=TRUE)) +
    geom_line(data=predicted.df, aes(x,y, group=component),
              alpha=fixed_aes$line$alpha, linetype=fixed_aes$line$linetype,
              size=fixed_aes$line$size) +
    scale_color_manual(name="Component", values=palette$color) +
    scale_y_sqrt()

  if(fill_aes=="component") {
    ggp <- ggp + scale_fill_manual(name="Component", values=palette$fill)
  } else {
    fill_name <- fill_aes
    if (fill_aes=="copynumber") fill_name <- "Copy Number"
    ggp <- ggp + scale_fill_manual(name=fill_name, values=palette$fill) +
      guides(color=guide_legend(override.aes=list(fill=NA)))
  }


  # Faceting and legend positioning
  if (multibatch) {
    batch_labels <- paste("batch", seq(nBatches))
    if("batch.var" %in% names(obs.df)) {
      batch_names <- unique(obs.df[,c("batch", "batch.var")])
      batch_labels <- paste(batch_labels, ":",
                            batch_names$batch.var[order(batch_names$batch)])
    }
    names(batch_labels) <- seq(nBatches)
    batch_labels["marginal"] = "marginal"

    ggp <- ggp + facet_wrap("batch", scales="free_y",
                            ncol=round(sqrt(nBatches + 1)),
                            labeller=labeller(batch=batch_labels))

    if(sqrt(nBatches + 1) %% 1 != 0) {
      ggp <- ggp + theme(legend.position = c(1, 0),
                         legend.justification = c(1, 0),
                         legend.box="horizontal")
    }
  }

  ggp
}
