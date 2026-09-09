# With tree_id_style = "baysf" trees are matched on their exact polar
# coordinates. Two trees of one plot sharing a position would make the join a
# Cartesian product, so a tree could enter the increment more than once. The
# documented rule: a shared position counts as no position - the trees are
# treated like trees recorded without coordinates.

make_plot_with_shared_position <- function() {
  p <- ForestElementsR::spruce_pine_ccircle_spatial
  tp <- p$tree_positions
  # Give tree 2 exactly the position of tree 1.
  tp$R[2]     <- tp$R[1]
  tp$angle[2] <- tp$angle[1]
  p$tree_positions <- tp
  p
}

test_that("a shared position makes both trees unmatchable, not a cross product", {
  plot_1st <- make_plot_with_shared_position()
  plot_2nd <- make_plot_with_shared_position()

  shared <- plot_1st$tree_positions$tree_id[1:2]

  # One warning per affected survey pool, so both are collected here.
  warns <- character(0)
  res <- withCallingHandlers(
    match_trees_on_plot_repsurv_ccirc(plot_1st, plot_2nd,
                                      tree_id_style = "baysf"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 2)
  expect_true(all(grepl("position", warns)))
  # The report names the affected trees, so the case is findable in the data.
  expect_true(all(vapply(shared, function(id) any(grepl(id, warns, fixed = TRUE)),
                         logical(1))))

  # No Cartesian product: each tree appears at most once per side.
  id1 <- res$tree_id_1st[!is.na(res$tree_id_1st)]
  id2 <- res$tree_id_2nd[!is.na(res$tree_id_2nd)]
  expect_false(any(duplicated(id1)))
  expect_false(any(duplicated(id2)))

  # The two trees sharing the position are flagged unmatchable on both sides
  # and never form a matched pair.
  row_1st <- res[res$tree_id_1st %in% shared & !is.na(res$tree_id_1st), ]
  expect_true(all(row_1st$is_unmatchable_1st))
  expect_true(all(is.na(row_1st$tree_exists_2nd)))

  row_2nd <- res[res$tree_id_2nd %in% shared & !is.na(res$tree_id_2nd), ]
  expect_true(all(row_2nd$is_unmatchable_2nd))
  expect_true(all(is.na(row_2nd$tree_exists_1st)))
})

test_that("unshared positions are unaffected by the rule", {
  plot_a <- ForestElementsR::spruce_pine_ccircle_spatial
  # No duplicated positions here, so no warning and normal matching.
  expect_no_warning(
    res <- match_trees_on_plot_repsurv_ccirc(plot_a, plot_a,
                                             tree_id_style = "baysf")
  )
  matched <- res[res$tree_exists_1st %in% TRUE & res$tree_exists_2nd %in% TRUE, ]
  expect_gt(nrow(matched), 0)
  expect_false(any(duplicated(matched$tree_id_1st)))
})
