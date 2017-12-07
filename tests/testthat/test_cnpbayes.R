context("Visualization of CNPBayes models")

refactor_na <- function(x) {
  if(is.factor(x)) {
    x.levels <- levels(x)
    x.ordered <- is.ordered(x)
    x <- factor(x, c(".na.", x.levels), ordered=x.ordered)
  }
  x[is.na(x)] <- ".na."
  x
}

sort_df <- function(x) {
  x <- as.data.frame(lapply(x, refactor_na))
  x <- x[do.call(order, x),]
  row.names(x) <- NULL
  x
}

summary_test <- function(model.obj) {
  summ <- summarize(model.obj)
  summ.b <- addMarginalBatch(summ)
  summ.m <- addMarginalModel(summ)
  summ.b.m <- addMarginalModel(summ.b)
  summ.m.b <- addMarginalBatch(summ.m)

  b.m.t <- summ.b.m@theoretical
  b.m.t$y <- round(b.m.t$y, 5)
  b.m.t.sorted <- sort_df(b.m.t)

  m.b.t <- summ.m.b@theoretical
  m.b.t$y <- round(m.b.t$y, 5)
  m.b.t.sorted <- sort_df(m.b.t)

  expect_identical(b.m.t.sorted, m.b.t.sorted)

  plot_summary(summ)
  plot_summary(summ.b)
  plot_summary(summ.m)
  plot_summary(summ.b.m)
  plot_summary(summ.m.b)
}

test_that("SB", {
  library(CNPBayes)
  summary_test(SingleBatchModelExample)
})

test_that("SBP", {
  summary_test(CNPBayes_SBP)
})


test_that("SB copy number", {
  library(CNPBayes)
  summary_test(CopyNumberModel(SingleBatchModelExample))
})

test_that("SBP copy number", {
  library(CNPBayes)
  summary_test(CopyNumberModel(CNPBayes_SBP))
})

test_that("MB", {
  library(CNPBayes)
  summary_test(MultiBatchModelExample)
})

test_that("MBP", {
  library(CNPBayes)
  summary_test(MultiBatchPooledExample)
})


test_that("MB copy number", {
  library(CNPBayes)
  summary_test(CopyNumberModel(MultiBatchModelExample))
})

test_that("MBP copy number", {
  library(CNPBayes)
  summary_test(CopyNumberModel(MultiBatchPooledExample))
})



