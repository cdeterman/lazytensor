

#' @importFrom utils install.packages menu
pkg_check <- function(pkg){
  tested <- try(find.package(pkg), silent = TRUE)
  if(class(tested)[1] == "try-error"){
    msg <- paste("The ", pkg, " package is not installed. ",
                 "Would you like to try to install it now",
                 sep = "")
    cat(msg)

    if(interactive()) {
      installChoice <- menu(c("yes", "no"))
      if(installChoice == 1){
        install.packages(pkg)
      }
    } else stop("Required package is missing", call. = FALSE)
  }
}


#' @title Set Backend Type
#' @description Select type of tensor (i.e. matrix, vector, etc.) to
#' have lazytensor to use internally
#' @param backend A character scalar defining the backend (e.g.)
#' @export
setBackend <- function(backend){

  switch(backend,
         "base" = options(lazytensor.backend = "base"),
         "gpuR" = {
           pkg_check(backend)
           if(!"gpuR" %in% .packages()){
             attachNamespace("gpuR")
           }
           options(lazytensor.backend = "gpuR")
         },
         stop(paste0("Unimplemented backend: ", backend))
  )
}



