#' yyyymmdd: Get A Time Stamp
#'
#' yyyymmdd() returns current date, and optionally time, in YYYYMMDD format
#'   
#' @param hms
#'        logical; if TRUE append hour, minutes and seconds 
#' @param sep
#'        character value to separate date and time if hms is TRUE
#'        
#' @return 
#'        Returns the current data (and time if hms==TRUE) as a character value
#'        formatted as YYYYMMDD(_HHMMSS).
#'        
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
yyyymmdd <-
function(hms=FALSE, sep="") {
	if (hms) fmt <- paste("%Y%m%d", sep=sep, "%H%M%S") else fmt <- "%Y%m%d"
	format(Sys.time(), fmt)
}
