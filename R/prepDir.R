#' prepDir: Create A New Directory
#'
#' prepDir() checks whether a directory exists and tries to creates it if not
#' 
#' This function will check whether the specified directory exists.  If it does
#' not, it will try to create it; it will fail with an error if it can not.  If
#' it already exists, it will return a value of TRUE.
#' 
#' @param outdir 
#'        character value with path of new directory to create 
#' @param recurs 
#'        logical; should parent directories be created?
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
prepDir <- 
function(outdir, recurs=T){
   success <- !is.na(file.info(outdir)$isdir) 
   if(!success){
      success <- dir.create(outdir, recursive=recurs)
      if(!success){
         stop(paste("Unable to create output directory:\n   ",outdir))
      }
   }
   success
}
