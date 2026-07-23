# .de_best_parameter_set() is the pure helper extracted from run_lhc_wq()'s
# DE phase. It's what makes run_lhc_wq() report DE's actual optimum (instead
# of silently falling back to the LHC seed's best) once DE has run -- see the
# "best_parameter_set" attribute discussion in R/run_lhc_wq.r.

test_that(".de_best_parameter_set un-flips the sign for KGE (a maximized metric)", {
  # DEoptim always minimizes; .make_de_objective() negates KGE/NSE so that
  # minimizing bestval corresponds to maximizing the metric. bestval = -0.75
  # means the actual KGE achieved was 0.75.
  de_result <- list(optim = list(bestval = -0.75, bestmem = c(1.5, 2.5)))

  out <- .de_best_parameter_set(de_result, param_names = c("p1", "p2"), best_metric = "KGE")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(out$best_metric, "KGE")
  expect_equal(out$objective_score, -0.75)
  expect_equal(out$objective_value, 0.75)
  expect_equal(out$p1, 1.5)
  expect_equal(out$p2, 2.5)
  expect_true(is.na(out$sample_index))
})

test_that(".de_best_parameter_set does not flip the sign for RMSE (already minimized)", {
  de_result <- list(optim = list(bestval = 1.23, bestmem = c(0.1)))

  out <- .de_best_parameter_set(de_result, param_names = "p1", best_metric = "rmse")

  expect_equal(out$best_metric, "RMSE")
  expect_equal(out$objective_score, 1.23)
  expect_equal(out$objective_value, 1.23)
})

test_that(".de_best_parameter_set reports PBIAS magnitude directly (no sign flip)", {
  # .make_de_objective() already returns mean(abs(PBIAS)) for this metric.
  de_result <- list(optim = list(bestval = 4.5, bestmem = c(0.2)))

  out <- .de_best_parameter_set(de_result, param_names = "p1", best_metric = "PBIAS")

  expect_equal(out$objective_value, 4.5)
})

test_that(".de_best_parameter_set labels parameter columns with param_names, not DEoptim's own names", {
  de_result <- list(optim = list(bestval = -1, bestmem = setNames(c(9, 8), c("x", "y"))))

  out <- .de_best_parameter_set(de_result, param_names = c("a", "b"), best_metric = "NSE")

  expect_true(all(c("a", "b") %in% names(out)))
  expect_equal(out$a, 9)
  expect_equal(out$b, 8)
})

test_that(".de_best_parameter_set is case-insensitive on best_metric", {
  de_result <- list(optim = list(bestval = -0.5, bestmem = c(1)))

  out <- .de_best_parameter_set(de_result, param_names = "p1", best_metric = "kge")

  expect_equal(out$best_metric, "KGE")
})
