#' Programmatically Generate Expression Objects
#' 
#' express() converts a character vector into a set of expressions.
#' 
#' This function makes it possible to use plotmath symbols, but stored as
#' characters, which means they can be created and manipulated programmatically.
#' 
#' The text-drawing functions (text, mtext, axis, legend, \emph{etc.}) can
#' accept an expression for their text argument, which is then interpreted 
#' and drawn as a mathematical expression.
#' 
#' However, expressions are usually entered literally, which means you can't
#' manipulate them programmatically.  If the text argument is a character
#' object, it gets printed literally.  This function gets around that.
#' 
#' Ridiculously simple once you figure out how to do it, but it took me a
#' while; hence this function.  as.expression() doesn't do this, by the way...
#' 
#' Unfortunately, you still have to build valid plotmath expressions; plotmath
#' doesn't handle spaces well, for example.
#
#' @param char.expressions
#'    A character vector where each element is an expression as a character 
#'    string that you wish to evaluate.
#
#' @return
#'    Returns a length one compound expression object.
#' 
#' @examples
#' par(mar=c(6, 6, 1, 1))
#' plot(0, 0, xlim=sym(), ylim=sym(), xaxt="n", yaxt="n", mgp=c(4,0.2,0),
#'    xlab="axis(1, (-9:9)/10, tick.labels, las=2, cex.axis=0.8)",
#'    ylab="axis(2, (-9:9)/10, express(tick.labels), las=1, cex.axis=0.8)")
#' tick.labels <- paste("x >=", (-9:9)/10)
#' # this is what you get if you just use tick.labels the regular way:
#' axis(1, (-9:9)/10, tick.labels, las=2, cex.axis=0.8)
#' # but if you express() them... voila!
#' axis(2, (-9:9)/10, express(tick.labels), las=1, cex.axis=0.8)
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
express <- 
function(char.expressions){
   return(parse(text=paste(char.expressions, collapse=";")))
}
