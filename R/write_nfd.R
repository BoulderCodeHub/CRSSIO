#' Create Natural Flow Files for Each Site
#' 
#' `write_nfd()` creates csv files or an Excel file for `nfd` objects. 
#' For csv files, a file is created for each site, flow type, and time step (see
#' details below). For Excel files, there will be one Excel file for each flow
#' type and time step.  
#' 
#' For csv files, in the directory provided, there will be one folder for each 
#' flow type-time 
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
#'     | annual_total/
#'         | site1.csv
#'         | site2.csv
#'         | ...
#'         
#' For Excel files, there will be one file for each flow space, timestep 
#' combination. Each file will have the sites as sheets, and each sheet will 
#' be in time x trace format. 
#' 
#' @param x [nfd] or [crss_nf] object.
#' 
#' @param path Directory to save files to.
#' 
#' @param overwrite Boolean. If `TRUE` and files already exists in 
#'   `path`/traceN, the files will be overwritten.
#'   
#' @param format "csv" or "excel".
#' 
#' @param trace_names Overwrite the default trace names. If not `NULL`, then 
#'   must be the same length as the number of traces. 
#'   
#' @param insert_name String that is inserted in file name. 
#' 
#' @export
write_nfd <- function(x, path, overwrite = FALSE, format = "csv", 
                      trace_names = NULL, insert_name = "") {
  assert_that(is_crss_nf(x) || is_nfd(x))
  assert_that(dir.exists(path))
  assert_that(format %in% c("csv", "excel") & length(format) == 1)
  assert_that(length(insert_name) == 1)
  
  if (!is.null(trace_names)) {
    assert_that(
      length(trace_names) == n_trace(x),
      msg = "length of `trace_names` must match the number of traces in nfd object."
    )
  }
  
  if (insert_name != '') {
    insert_name <- paste0(insert_name, "_")
  }
  
  if (format == "csv") {
    write_func <- flow_ts_to_csv
  } else {
    write_func <- flow_ts_to_excel
  }
  
  if (has_annual(x)) {
    if (has_total(x, "annual")) {
      write_func(x, "total", "annual", path = path, overwrite = overwrite,
                 trace_names = trace_names, insert_name = insert_name)
    }
    
    if (has_intervening(x, "annual")) {
      write_func(x, "intervening", "annual", path = path, overwrite = overwrite, 
                 trace_names = trace_names, insert_name = insert_name)
    }
  }
  
  if (has_monthly(x)) {
    if (has_total(x, "monthly")) {
      write_func(x, "total", "monthly", path = path, overwrite = overwrite,
                 trace_names = trace_names, insert_name = insert_name)
    }
    
    if (has_intervening(x, "monthly")) {
      write_func(x, "intervening", "monthly", path = path, 
                 overwrite = overwrite, trace_names = trace_names, 
                 insert_name = insert_name)
    }
  }
  
  invisible(x)
}

flow_ts_to_excel <- function(x, flow_space, time_step, path, overwrite, 
                           trace_names, insert_name) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
  
  ofile <- file.path(
    path, 
    paste0(insert_name, time_step, "_", flow_space, ".xlsx")
  )
  
  if (!overwrite && file.exists(ofile)) {
    stop(ofile, "\nexists and `overwrite` is `FALSE`.")
  }
  
  # convert x (nfd) from it's format to a list, where each entry in the list
  # is a site, and each entry is time x traces
  # TODO: convert this to as.list.nfd at some point
  if (is.null(trace_names)) {
    trace_names <- paste0("Trace", seq_len(n_trace(x)))
  }
  
  ss <- sites(x)
  
  x_lst <- lapply(ss, function(s) {
    tmp <- nfd_get_site(x, s, flow_space, time_step)
    colnames(tmp) <- trace_names
    tmp <- as.data.frame(tmp)
    tmp <- tibble::rownames_to_column(tmp, var = "timestep")
  })
  names(x_lst) <- ss
  
  writexl::write_xlsx(x_lst, path = ofile)
}

flow_ts_to_csv <- function(x, flow_space, time_step, path, overwrite, 
                           trace_names, insert_name) {
  
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
    all_files <- file.path(path, paste0(insert_name, ss, ".csv"))
    tmp <- file.exists(all_files)
    assert_that(
      !any(tmp), 
      msg = paste0(
        "The following files exist and `overwrite` is `FALSE`:\n  ", 
        paste(all_files[tmp], collapse = "\n  ")
      )
    )
  }
  
  # if trace_names are provided, use them. If not, then will call them 
  # Trace1 - TraceN
  # already checked that trace_names is correct length if they were provided
  if (is.null(trace_names)) {
    trace_names <- paste0("Trace", seq_len(n_trace(x)))
  }
  
  # write all csv files:
  tmp <- lapply(seq_along(ss), function(i) {
    tmp_data <- nfd_get_site(x, i, flow_space, time_step)
    colnames(tmp_data) <- trace_names
    f_name <- file.path(path, paste0(insert_name, ss[i], ".csv"))
    
    utils::write.csv(tmp_data, f_name, row.names = zoo::index(tmp_data))
  })
  
  invisible(x)
}
