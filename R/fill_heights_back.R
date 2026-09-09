# Copyright (C) 2026  Peter Biber, Astor Torano Caicoya
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Write Estimated Heights at Inventory Level Back Into the Original
#' fe_inventory Object
#'
#' This procedure can be time consuming with large inventories, therefore
#' parallel processing is used (see parameter \code{free_cores}).
#'
#' @param inv_dat Object of class \code{fe_inventory}
#'
#' @param pulled_est_height tibble which was pulled from an \code{fe_inventory}
#'   object with \code{\link{pull_trees}}, and where missing heights were
#'   completed with \code{\link{height_complete_inventory}}.
#'
#' @param small_trees `r lifecycle::badge("experimental")` logical, default is
#'   FALSE. Only users who absolutely know what they are doing, should use the
#'   setting TRUE. If TRUE also small trees (i.e. trees with heights < 1.3 m)
#'   will be dealt with.
#'
#' @param .progress Logical, if \code{TRUE} (default) a progress bar is
#'   displayed during execution.
#'
#' @param free_cores Integer, indicating the number of processor cores which
#'   will not be assigned to parallel processing tasks. If \code{free_cores} is
#'   equal or greater than the number of available cores, one core will be
#'   used (i.e. no parallel processing). Defaults to 4.
#'
#'
#' @return The input \code{fe_inventory} object with updated heights
#'
#' @export
#'
#' @examples
#'
#' # Estimate heights - only the pulled tree tibble is affected so far
#' suppressWarnings(
#' # Warnings come from an intentional species code casts - no problem here
#'   inventory_est_heights <- data_ex3_sample_fe_inventory |>
#'     pull_trees() |>
#'     height_complete_inventory(method = "NFI")
#' )
#'
#' # Now write the heights back into the original object or, as in this case,
#' # an otherwise identical copy of it
#' peg_fe_inventory_updated <- fill_heights_back(
#'   data_ex3_sample_fe_inventory, inventory_est_heights
#' )
#'
fill_heights_back <- function(
    inv_dat, pulled_est_height, small_trees = FALSE, .progress = TRUE,
    free_cores = 4
) {

  # Number of worker cores (absolute cap, plot count, small-inventory threshold
  # and CRAN limit -- see .fe_worker_cores)
  numCores <- .fe_worker_cores(free_cores, n_tasks = nrow(inv_dat))

  if (numCores > 1L) {
    cl <- parallel::makeCluster(numCores)
    doSNOW::registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else {
    # One worker means no cluster at all: %dopar% then runs in this process, and
    # the inventory is not serialised anywhere. Starting a cluster of one would
    # pay the whole transfer for no parallelism.
    foreach::registerDoSEQ()
  }

  iterations <- nrow(inv_dat)
  if (.progress)
    pb <- utils::txtProgressBar(min = 1, max = iterations, style = 1)


  # function for the progress bar and foreach combination
  f <- function(iterator) {
    if (.progress)
      pb <- utils::txtProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0
    function(...) {
      count <<- count + length(list(...)) - 1
      if(.progress) {
        utils::setTxtProgressBar(pb, count)
        utils::flush.console()
      }
      rbind(...) # this can feed into .combine option of foreach
    }
  }

  # substitute the height column in each plot from the fe_inventory object
  # using foreach

  redo_inv <- foreach::`%dopar%`(
    foreach::foreach(i = 1:iterations, .combine = f(iterations)),
    {
      heights_vect <- pulled_est_height |>
        dplyr::filter(.data$plot_id == inv_dat$plot_id[i]) |>
        dplyr::select("height_m", "h_est_m", "tree_id")

      row_dat <- inv_dat[i, ]

      row <- row_dat[[2]][[1]][["trees"]]$tree_id

      # make sure trees are in matching order
      heights_vect <- heights_vect[match(row, heights_vect$tree_id), ]
      # find positions where trees were flagged as not measured
      not_hm_tree <- which(row_dat[[2]][[1]][["trees"]]$h_m_tree == FALSE)
      # only replace trees that were not flagged as measured
      row_dat[[2]][[1]][["trees"]]$height_m[not_hm_tree] <-
        heights_vect$h_est_m[not_hm_tree]


      if(small_trees == TRUE) {

        row <- row_dat[[2]][[1]][["small_trees"]]$tree_id

        # re-pull from pulled_est_height: heights_vect was already reordered
        # for the regular trees above and cannot be reused for small trees
        heights_vect_sm <- pulled_est_height |>
          dplyr::filter(.data$plot_id == inv_dat$plot_id[i]) |>
          dplyr::select("height_m", "h_est_m", "tree_id")
        heights_vect_sm <- heights_vect_sm[match(row, heights_vect_sm$tree_id), ]

        not_hm_tree <- which(
          row_dat[[2]][[1]][["small_trees"]]$h_m_tree == FALSE
        )
        row_dat[[2]][[1]][["small_trees"]]$height_m[not_hm_tree] <-
          heights_vect_sm$h_est_m[not_hm_tree]

      }

      return(row_dat)
    }
  )

  if (.progress) close(pb)
  # The cluster, if there is one, is stopped by the on.exit() set up above.

  # call the constructor to deliver an fe_inventory object back
  # this should be safe without calling the validator as we do not modify the
  # structure of the object in the previous step
  fe_iventory_back <- new_fe_inventory(redo_inv)
}


# Below this many plots the write-back runs in this process. Every PSOCK worker
# receives a serialised copy of the whole inventory, sf geometries included, and
# that transfer is a fixed cost of roughly 1.7 seconds, while the work itself is
# about 0.006 seconds per plot. Measured on the ex3 example (10 plots): 1.78 s
# through the cluster against 0.06 s in this process, a factor of thirty. The
# two costs meet somewhere around 300 to 400 plots; the threshold sits below
# that, so that no inventory loses parallelism it would actually benefit from.
# On Nordhalben (4014 plots) the cluster is clearly worth it: 22 s on one core
# against 13 s on four.
.fe_parallel_min_plots <- 200L

# Number of worker cores for the parallel height write-back.
#
# Four limits apply, smallest wins:
#
#  * Fewer plots than .fe_parallel_min_plots: one worker, see above.
#
#  * An absolute cap of four. The work per plot is light (a filter and an
#    assignment), while every PSOCK worker reloads the packages and receives a
#    copy of the inventory, so the setup cost dominates quickly. Four proved
#    favourable on real management-unit data of enterprise size: on test data
#    with 4014 plots, the elapsed time fell from 22 s on one core to 14 s on
#    two and 13 s on four, then rose again to 16 s on six -- more than a handful
#    of workers is wasted or counter-productive.
#  * The number of tasks (plots): more workers than plots is pointless.
#  * Under a CRAN-style check the cap drops to two. R itself reads
#    _R_CHECK_LIMIT_CORES_ as tolower(x) != "false" and makeCluster() aborts
#    with "N simultaneous processes spawned" above two cores. The earlier guard
#    compared the raw value to the literal "TRUE" and so missed win-builder's
#    lower-case value; on a 96-core check machine that let it ask for
#    detectCores() - free_cores = 92 cores.
#
# free_cores keeps its meaning of leaving cores idle on small machines; on a
# large machine the absolute cap is what bounds the count. detectCores() may
# return NA (documented), treated here as a single core.
.fe_worker_cores <- function(free_cores = 4, n_tasks = NULL) {
  detected <- parallel::detectCores()
  if (is.na(detected)) detected <- 1L

  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  cap <- if (nzchar(chk) && chk != "false") 2L else 4L

  cores <- min(max(1L, detected - free_cores), cap)
  if (!is.null(n_tasks)) {
    if (n_tasks < .fe_parallel_min_plots) return(1L)
    cores <- min(cores, max(1L, n_tasks))
  }
  cores
}

