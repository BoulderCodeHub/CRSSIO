#' Create Natural Flow Files for Each Site
#' 
#' `write_nfd()` creates csv files for each site, flow type, and time step. 
#' In the directory provided, there will be one folder for each flow type-time 
#' step combination, and it each of those folders there will a csv files for
#' each site. The csv files will be named based on the site names in the [nfd]
#' object. Each csv is in time by trace format
#' 
#' Folders will be created inside path e.g.:
#' | path/
#'     | monthly_intervening/
#'         | site1.csv
#'         | site2.csv
#'         | ...
#'     | annua_total/
#'         | site1.csv
#'         | site2.csv
#'         | ...
#' 
#' @param x [nfd] or [crss_nf] object.
#' 
#' @param path Directory to save files to.
#' 
#' @param overwrite Boolean. If `TRUE` and files already exists in 
#'   `path`/traceN, the files will be overwritten.
#' 
#' @export
write_nfd <- function(x, path, overwrite = FALSE) {
  assert_that(is_crss_nf(x) || is_nfd(x))
  assert_that(dir.exists(path))
  
  if (has_annual(x)) {
    if (has_total(x, "annual")) {
      flow_ts_to_csv(x, "total", "annual", path = path, overwrite = overwrite)
    }
    
    if (has_intervening(x, "annual")) {
      flow_ts_to_csv(x, "intervening", "annual", path = path, 
                     overwrite = overwrite)
    }
  }
  
  if (has_monthly(x)) {
    if (has_total(x, "monthly")) {
      flow_ts_to_csv(x, "total", "monthly", path = path, overwrite = overwrite)
    }
    
    if (has_intervening(x, "monthly")) {
      flow_ts_to_csv(x, "intervening", "monthly", path = path, 
                     overwrite = overwrite)
    }
  }
  
  invisible(x)
}

flow_ts_to_csv <- function(x, flow_space, time_step, path, overwrite) {
  
  path <- file.path(path, paste0(time_step, "_", flow_space))
  if (!dir.exists(path)) {
    dir.create(path)
  }
  
  ss <- sites(x)
  if (is.null(ss)) {
    ss <- paste0("s", seq_len(n_sites(x)))
  }
  
  if (!overwrite) {
    # check that files do not exist
    all_files <- file.path(path, paste0(ss, ".csv"))
    tmp <- file.exists(all_files)
    assert_that(
      !any(tmp), 
      msg = paste0(
        "The following files exist and `overwrite` is `FALSE`:\n  ", 
        paste(all_files[tmp], collapse = "\n  ")
      )
    )
  }
  
  cc <- paste0("Trace", seq_len(n_trace(x)))
  
  # write all csv files:
  tmp <- lapply(seq_along(ss), function(i) {
    tmp_data <- nfd_get_site(x, i, flow_space, time_step)
    colnames(tmp_data) <- cc
    f_name <- file.path(path, paste0(ss[i], ".csv"))
    
    write.csv(tmp_data, f_name, row.names = zoo::index(tmp_data))
  })
  
  invisible(x)
}
