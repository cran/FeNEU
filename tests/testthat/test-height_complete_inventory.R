


test_that("height_complete inventory() calculates predictably", {

  # Test with NFI method
  expect_equal(
    processed_pulled_sub10 |>
      height_complete_inventory(method = "NFI") |>
      suppressWarnings() |>
      suppressMessages() |>
      dplyr::arrange(plot_id, tree_id),
    processed_heights_nfi_sub10 |>
      dplyr::arrange(plot_id, tree_id)
  )

  # Test with Bavaria method
  expect_equal(
    processed_pulled_sub10 |>
      height_complete_inventory(method = "Bavaria") |>
      suppressWarnings() |>
      suppressMessages() |>
      dplyr::arrange(plot_id, tree_id),
    processed_heights_bav_sub10 |>
      dplyr::arrange(plot_id, tree_id)
  )
})
