#' Draw A Venn Plot With Proportional Areas
#' 
#' VennPlot draws a Venn diagram based on the overlap of three sets, where the
#' area are proportional to the size of each set an the overlaps approximate
#' the relative overlap to the extent possible.
#' 
#' Only the unique values in each set will be counted (replicate values will be 
#' removed). A plot will generated with overlapping circles approximating the 
#' overlap between the sets.  (It is not always possible to accurately 
#' represent the overlap of three sets using circles.)
#' 
#' Labels for each set may be placed in one of three ways using the labels.at 
#' argument:
#' \itemize{
#'   \item If labels.at is set to missing (NA; default), the function will 
#'     place the labels automatically near the centers of each set.
#'   \item If labels.at is set to NULL, the user will prompted to select 
#'      locations for each label by clicking on the plot.  This is useful for
#'      fine-tune label placement for legibility.  \strong{Do not close the plot
#'      window before placing the labels or it will screw up you session!  Do
#'      not use this option if plotting to a file (no interactive window).}
#'   \item You can also supply a data.frame with values specifying the location.
#'     You can get the values for this data frame by running the function with
#'     labels.at=NULL to manually place the labels; the list returned will
#'     include these values: \code{<list>$Circles[, c("x.label", "y.label")]}.  
#'     This can be useful if you want to be able to rerun a script where you 
#'     have already determined optimal placement manually.
#' }
#' 
#' @param set1
#'    Either a vector of identifiers (may be numeric or text) to define a 
#'    single set, \strong{or} a list of two or three such vectors.  If set is a
#'    list, the names of the elements will be used as the names if the labels
#'    argument (below) is not supplied.  Also, \strong{if set1 is a list, all 
#'    remaining arguments must be named.} Only the unique values in each set 
#'    will be counted (replicate values will be removed).
#' @param set2,set3 
#'    Vectors of identifiers; may be numeric or text.  If set1 is a vector, set2
#'    is required and set3 is optional.  If set1 is a list, the set2 and set3 
#'    arguments will be ignored if supplied.
#' @param labels
#'    Optional character vector with names of each set.
#' @param lwd 
#'    A number specifying line width of the circles.
#' @param cex.text
#'    A number specifying character size for labels.
#' @param col
#'    A character vector of named colors for each set; length should be equal to
#'    the number of sets.
#' @param mar
#'    4-element vector of positive numbers specifying outer margin of plot.
#' @param main
#'    A character string specifying title of plot.
#' @param cex.main
#'    A number specifying font size to use for title
#' @param labels.at
#'    \strong{If NA (default), or any of  the values are NA, the function will 
#'    attempt to place the labels automatically. If omitted, the user will be 
#'    prompted to manual select locations by clicking on the plot.}
#'    
#'    Otherwise, a 2-row column matrix with x- and y- coordinates indicating 
#'    placement of the set labels.  Number of rows must equal number of sets. 
#'    On subsequent calls, may be extracted from the "Circles" element of the 
#'    list returned by this function: \code{Circles[, c("x.label", "y.label")]}
#' @param xscale,yscale
#'    Numeric value that controls the range of the data shown.
#'    Default is 1.01; larger values will leave more space around the margins
#'    (sometimes useful for making labels fit), while smaller values could be
#'    used to remove extra space when the aspect ratio differs from one.
#' @param return.sets
#'    Logical; if TRUE, return a data.frame with each unique item's
#'    assignment to an overlap region.
#
#' @return 
#'   Returns a list object with named elements:
#' 
#'   Areas: data.frame specifying number of unique items in various regions of
#'      the Venn diagram and the midpoints of each region on the plot (i.e.,
#'      where the labels are placed).
#'   Circles : data.frame with the centers (x and y) and radii of the circles
#'      for each set
#'      
#' @seealso 
#'   \code{\link[graphics]{par}} and \code{\link[graphics]{plot.default}} give
#'   more detailed descriptions of the plotting parameters lwd, cex.text, col
#'   mar, main and cex.main.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @import graphics
#' @export
VennPlot <-
function(set1, set2, set3=vector(), labels=paste("Set", 1:3), lwd=2, cex.text=1, 
         col=c("blue", "green", "red"), mar=rep(1, 4), main="", cex.main=1.2, 
         labels.at=NA, xscale=1.01, yscale=1.01, return.sets=FALSE){
   if(is.list(set1)){
      if(!missing(set2)){
         stop("set2 or set3 arguments should not be supplied if set1 is a list")
      }
      Sets <- set1
      if(length(Sets)==2){
         Sets$EMPTY <- rep(methods::as(NA, class(Sets[[1]])), 0)
      }
      if(all(labels==paste("Set", 1:3)) & !is.null(names(Sets))){
         # use the list names as the set names by default
         labels <- names(Sets)
      }
      names(Sets) <- LETTERS[1:length(Sets)]
   }else{
      Sets <- list(A=set1, B=set2, C=set3)
   }
   # determine the areas of overlap
   uniqs <- sort(unique(unlist(Sets)))
   sets <- data.frame(row.names=uniqs)
   for(Si in 1:length(Sets))
      sets[, Si] <- factor(uniqs%in%Sets[[Si]], c("FALSE", "TRUE"))
   # scale the areas of the circles to the sizes of the sets
   Ns <- apply(sets=="TRUE", 2, sum)
   r <- sqrt(Ns/pi)
   # calculate the number of things in each region of the diagram
   Areas <- data.frame(table(sets))[-1, ]
   colnames(Areas)[which(colnames(Areas)=="Freq")] <- "Exclusive"
   rownames(Areas) <-
      paste("A", sep="", ifelse(Areas[, 1]=="TRUE", "1", ""), 
      ifelse(Areas[, 2]=="TRUE", "2", ""), ifelse(Areas[, 3]=="TRUE", "3", ""))
   Areas[, c("Overlap", "Union")] <- NA
   for(Ri in rownames(Areas)){
      Ci <- which(Areas[Ri, 1:3]=="TRUE")
      Areas[Ri, "Overlap"] <- 
         sum(Areas$Ex[apply(Areas[, Ci, drop=F]=="TRUE", 1, all)])
      Areas[Ri, "Union"] <- 
         sum(Areas$Ex[apply(Areas[, Ci, drop=F]=="TRUE", 1, any)])
   }
   # determine correct distance between circles 1 and 2
   minD12 <- abs(r[1] - r[2])
   maxD12 <- r[1] + r[2]
   howoff <- Inf
   while(abs(howoff) > 1e-12 ){
      D12 <- mean(c(maxD12, minD12))
      CAB <- acos((r[1]^2 + D12^2 - r[2]^2)/(2*r[1]*D12))
      CAD <- 2*CAB
      CBA <- acos((r[2]^2 + D12^2 - r[1]^2)/(2*r[2]*D12))
      CBD <- 2*(CBA)
      Area <- (CAD*r[1]^2)/2 -
         cos(CAB)*sin(CAB)*r[1]^2 + (CBD*r[2]^2)/2 - cos(CBA)*sin(CBA)*r[2]^2
      howoff <- (Area - Areas["A12", "Overlap"]) / Areas["A1", "Overlap"]
      if(howoff < 0){
         maxD12 <- D12
      }else minD12 <- D12
   }
   if(Ns[3]>0){
      # determine correct distance between circles 1 and 3
      minD13 <- abs(r[1] - r[3])
      maxD13 <- r[1] + r[3]
      howoff <- Inf
      while(abs(howoff) > 1e-13){
         D13 <- mean(c(maxD13, minD13))
         CAB <- acos((r[1]^2 + D13^2 - r[3]^2)/(2*r[1]*D13))
         CAD <- 2*CAB
         CBA <- acos((r[3]^2 + D13^2 - r[1]^2)/(2*r[3]*D13))
         CBD <- 2*CBA
         Area <- (CAD*r[1]^2)/2 -
            cos(CAB)*sin(CAB)*r[1]^2 + (CBD*r[3]^2)/2 - cos(CBA)*sin(CBA)*r[3]^2
         howoff <- (Area - Areas["A13", "Overlap"]) / Areas["A1", "Overlap"]
         if(howoff < 0){
            maxD13 <- D13
         }else minD13 <- D13
      }
      # determine correct distance between circles 2 and 3
      minD23 <- abs(r[2]-r[3])
      maxD23 <- r[2] + r[3]
      howoff <- Inf
      while(abs(howoff) > 1e-13){
         D23 <- mean(c(maxD23, minD23))
         CAB <- acos((r[2]^2 + D23^2 - r[3]^2)/(2*r[2]*D23))
         CAD <- 2*CAB
         CBA <- acos((r[3]^2 + D23^2 - r[2]^2)/(2*r[3]*D23))
         CBD <- 2*CBA
         Area <- (CAD*r[2]^2)/2 - cos(CAB)*sin(CAB)*r[2]^2 + (CBD*r[3]^2)/2 -
            cos(CBA)*sin(CBA)*r[3]^2
         howoff <- (Area - Areas["A23", "Overlap"]) / Areas["A2", "Overlap"]
         if(howoff < 0){
            maxD23 <- D23
         }else minD23 <- D23
      }
      # find center point for set 3 and adjust crossing points
      CAB <- acos(pmax(-1, pmin(1, (D13^2 + D12^2 - D23^2) / (2*D13*D12))))
      x3 <- D13*cos(CAB)
      y3 <- D13*sin(CAB)
      CBA <- acos(pmax(-1, pmin(1, (D23^2 + D12^2 - D13^2)/(2*D23*D12))))
   }else{
      x3 <- 0
      y3 <- 0
      Areas <- Areas[1:3, ]
   }
   #----------------------------------------------------------------------------
   # PLOT THE VENN DIAGRAM HERE
   X <- matrix(c(r[1], r[2], r[3])*rep(seq(-1, 1, length=100), each=3), 3)
   Y <- sqrt(c(r[1], r[2], r[3])^2 - X^2)
   X <- t(cbind(X, X[, ncol(X):1]))
   Y <- t(cbind(Y, -Y[, ncol(Y):1]))
   X[, 2] <- X[, 2] + D12
   X[, 3] <- X[, 3] + x3
   Y[, 3] <- Y[, 3] + y3
   xlims <- range(X)
   ylims <- range(Y)
   lims <- max(c(diff(xlims), diff(ylims)))*c(-1, 1)/2
   # rescale so the plot runs from -1 to +1 along each axis
   X <- (X - mean(xlims))/lims[2]
   Y <- (Y - mean(ylims))/lims[2]
   par(mar=mar)
   matplot(X, Y, lty=1, type="l", lwd=lwd, xlim=sym(xscale), ylim=sym(yscale), 
      axes=F, xlab="", ylab="", col=col, main=main, cex.main=cex.main)
   midpts <- data.frame(x=apply(X, 2, mean), y=apply(Y, 2, mean))
   radii <- apply(X, 2, max) - apply(X, 2, mean)
   midpts$radius <- radii
   # find the middle of each region by throwing darts
   darts <- data.frame(x=runif(100000, -1, 1), y=runif(100000, -1, 1))
   for(Si in 1:3) darts[, paste("V", Si, sep="")] <-
      sqrt((darts$x - midpts$x[Si])^2 + (darts$y - midpts$y[Si])^2) < radii[Si]
   # for each area, find the center of the widest section
   Areas$mid.x <- Areas$mid.y <- NA
   for(Ai in rownames(Areas)){
      hits <- darts[
         selectRows(darts, as.list(Areas[Ai, 1:3]), rownames=FALSE), 1:2]
      if(nrow(hits)==0) next
      hits$ybin <- cut(hits$y, seq(-1, 1, len=21))
      ybins <- suppressWarnings(data.frame(n=untab(table(hits$ybin)), 
         xavg=untab(tapply(hits$x, hits$ybin, mean)), 
         xgap=untab(tapply(hits$x, hits$ybin, function(x) max(diff(sort(x))))), 
         yavg=untab(tapply(hits$y, hits$ybin, mean))))
      ybins <- ybins[which(ybins$n>0 & ybins$xgap<0.02), ]
      ybins <- ybins[which.max(ybins$n), ]
      Areas[Ai, c("mid.x", "mid.y")] <- ybins[, c("xavg", "yavg")]
   }
   # add the number of counts in each region to the plot
   prompt.labels <- is.null(labels.at)
   if(prompt.labels) labels.at <- matrix(NA, 3, 2)
   if(any(is.na(labels.at))) labels.at <- matrix(NA, 3, 2)
   for(Ri in 1:nrow(Areas)){
      if(any(is.na(labels.at))){
         # place labels between exclusive counts and outer edge of each circle
         rndx <- which(unlist(Areas[Ri, 1:ifelse(Ns[3]>0, 3, 2)])=="TRUE")
         if(length(rndx)==1){
            mid.x <- ifelse(is.na(Areas[Ri, "mid.x"]), midpts[rndx, "x"], 
               Areas[Ri, "mid.x"])
            mid.y <- ifelse(is.na(Areas[Ri, "mid.y"]), midpts[rndx, "y"], 
               Areas[Ri, "mid.y"])
            labels.at[rndx, 1] <- mid.x
            labels.at[rndx, 2] <- (mid.y + midpts[rndx, "y"] + 
               ifelse(rndx==3, 1, -1)*sqrt(midpts[rndx, "radius"]^2 - 
               (mid.x - midpts[rndx, "x"])^2))/2
            # check whether there seems to be enough room
            dx <- abs(midpts[rndx, "x"] - labels.at[rndx, 1] + 
               ifelse(rndx==3, 1, -1)*sqrt(midpts[rndx, "radius"]^2 - 
               (mid.y - midpts[rndx, "y"])^2))/diff(par("usr")[1:2])
            dy <- abs(labels.at[rndx, 2]-mid.y)/diff(par("usr")[3:4])
            if(dx<0.1 | dy<0.05){
               # place at same x, but outside the circle
               labels.at[rndx, 2] <- midpts[rndx, "y"] + (ifelse(rndx==3, 1, -1)
                  * (midpts[rndx, "radius"] + 0.05*diff(par("usr")[3:4])))
            }
         }
      }
      if(Areas[Ri, "Exclusive"]==0) next
      text(Areas$mid.x[Ri], Areas$mid.y[Ri], Areas$Exclusive[Ri], 
           adj=c(0.5, 0.5), font=2, cex=cex.text)
   }
   if(!prompt.labels){
      # place set labels using coordinates passed in or automatically determined
      midpts[1:nrow(labels.at), "x.label"] <- labels.at[, 1]
      midpts[1:nrow(labels.at), "y.label"] <- labels.at[, 2]
      for(Li in 1:length(labels)) if(Ns[Li]>0){
         here = list(x=labels.at[Li, 1], y=labels.at[Li, 2])
         text(here$x, here$y, paste0(labels[Li], '\n(n=', Ns[Li], ')'), adj=0.5, 
            font=2, col=col[Li], cex=cex.text)
      }                                   
   }else{
      # prompt user to place set labels
      midpts[, c("x.label", "y.label")] <- NA
      for(Li in 1:length(labels)) if(Ns[Li]>0){
         colorstr <- col[Li]
         utils::winDialog("ok", 
            paste("Click on the plot to place the label for the", colorstr, 
                  "set:\n"))
         here <- locator(1)
         midpts[Li, c("x.label", "y.label")] <- here
         text(here$x, here$y, paste(labels[Li], '\n(', Ns[Li], ')'), adj=0.5, 
              font=2, col=col[Li], cex=cex.text)
      }
   }
   if(Ns[3]==0){
      labels <- labels[1:2]
      midpts <- midpts[1:2, ]
      Areas <- Areas[, -3]
   }
   rownames(midpts) <- labels
   colnames(Areas)[1:length(labels)] <- labels
   rownames(Areas) <- NULL
   Venn <- list(Areas=Areas, Circles=midpts)
   if(return.sets){
      sets <- data.frame(lapply(sets, function(x) as.numeric(x=="TRUE")), 
         row.names=rownames(sets))
      sets <- data.frame(row.names=rownames(sets), 
         overlap=apply(sets, 1, paste0, collapse=""), sets)
      colnames(sets) <- c("overlap", labels)
      Venn$Sets <- sets
   }
   return(Venn)
}
