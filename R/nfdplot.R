#' Print and save nfdplots
#'
#' `save_nfdplot()` is a convenient function for saving `nfdplots`. While 
#' `print.nfdplot()` prints covers showing the plots when R is in interactive
#' mode. `nfdplot` objects are returned by [plot.nfd()], [plot.nfd_stats()], 
#' and [plot.nfd_pdf()].
#' 
#' @param x An object inheriting from `nfdplot`.
#' 
#' @param ... Other parameters not used by this method.
#' 
#' @return `x` is are invisibly returned.
#' 
#' @export
#' @rdname nfdplot
print.nfdplot <- function(x, ...)
{
  if (interactive()) {
    
    print(x[[1]])
    
      if (length(x) > 1) {
      
      oask <- grDevices::devAskNewPage(TRUE)
      on.exit(grDevices::devAskNewPage(oask))
      
      for (i in seq(2, length(x))) {
        grDevices::dev.hold()
        print(x[[i]])
        grDevices::dev.flush()
      }
    }
  } else {
    for (gg in x) {
      print(gg)
    }
  }
  
  invisible(x)
}

#' @param filename File name to create on disk. Should be a .png or .pdf file.
#'   If it is a .png file, then it will create multiple files with `%02d`
#'   inserted at the end of the file to ensure unique file names. Directory
#'   should already exist.
#'
#' @param width,height Plot size in inches. If not specified, defaults to 7.
#'
#' @return Invisibly returns `nfdplot`
#'
#' @export
#' @rdname nfdplot
save_nfdplot <- function(x, filename, width = NA, height = NA)
{
  assert_that(inherits(x, "nfdplot"))
  
  # check that base directory exists
  assert_that(dir.exists(dirname(filename)))
  
  # check that file extension is .png or .pdf
  ftype <- tools::file_ext(filename)
  assert_that(
    ftype %in% c("png", "pdf"),
    msg = "`filename` should be either a png or pdf."
  )
  
  if (is.na(width))
    width <- 7
  
  if (is.na(height))
    height <- 7
  
  if (ftype == "png") {
    # add in the number after the provided file name
    filename <- gsub(paste0(".", ftype), "", filename)
    
    for (i in seq(x)) {
      
      tmp_f <- paste0(filename, sprintf("%02d", i), ".png")
      ggsave(tmp_f, plot = x[[i]], width = width, height = height,
             units = "in")
    }
    
  } else {
    # its a pdf - one file
    grDevices::pdf(filename, width = width, height = height)
    on.exit(grDevices::dev.off())
    for (gg in x) {
      if (exists(p, where = x) && !is.null(x[[p]]))
        print(gg)
    }
  }
  
  invisible(x)
}

#' @export
c.nfdplot <- function(...)
{
  x <- NextMethod()
  class(x) <- "nfdplot"
  x
}