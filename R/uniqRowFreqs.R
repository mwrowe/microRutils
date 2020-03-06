#' Count Occurrences of Replicate Rows in a Data.Frame
#' 
#' uniqRowFreqs() identifies unique data.frame rows and counts how often each
#' occurs.
#' 
#' \itemize{
#'   \item Missing values are permitted, and missing (NA) will be treated as 
#'     a single unique value when identifying unique combinations of the 
#'     columns of DF. 
#'   \item Often one will only pass particular columns of a larger table, in
#'     order to count the unique combinations of just those columns.
#'   \item If you try to apply this function to a data.frame that already 
#'     contains columns names "UROW.ID" or "UFREQ.ID" (like from an early result
#'     of this function), you will get an error.  To fix this, rename or remove
#'     those columns.
#' }
#' 
#' @param DF 
#'   A data.frame (or object that can be coerced into a data.frame)
#' @param  simplify
#'   Logical; if TRUE, return only UROWS data.frame, without UROW.ID
#'   column and UROW.FREQ column renames as "n.rows".
#' 
#' @return 
#'   if simplify==TRUE, returns a data.frame, where the first column is called
#'   "n.rows" and the remaining columns have the same names as DF.  All
#'   rows are unique, and n.rows contains the counts of occurrences of each row
#'   in DF.
#'   
#'   if simplify==FALSE returns a list with two named elements:
#'   \itemize{
#'     \item \strong{UROWS} A data.frame with the same format described above, 
#'       but with an additional column UROW.ID prepended that holds an 
#'       identifier for each unique row.
#'     \item \strong{UROWIDs} A vector with as many elements as rows in DF,
#'        mapping  the rows in DF into the UROWS data.frame.  This may 
#'        be joined with the original DF using cbind()
#'   }
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
uniqRowFreqs <- 
function(DF, simplify=TRUE){
   # coerce the DF to data.frame if it is not already
   DF <- as.data.frame(DF)
   # save the original column names
   table.names <- names(DF)
   if(any(c("UROW.ID","UROW.FREQ")%in%table.names)){
      stop("Columns UROW.ID and/or UROW.FREQ already exist in DF")
   }
   # replace missing values with flags (can't merge on NA's)
   noNA.table <- DF
   na.flags <- list(logical="ZZ", integer=999998, numeric=1.23456e222,
      character="zzzzzzzzzzzzzzzzz")
   has.NAs <- apply(is.na(noNA.table),2,sum)
   has.NAs <- names(has.NAs)[has.NAs>0]
   factlevs <- list()
   if(length(has.NAs)>0){
      has.NAs <- unlist(lapply(noNA.table[,has.NAs,drop=F],class))
      for(Ci in names(has.NAs)){
         if(has.NAs[[Ci]]=="factor"){
            factlevs[[Ci]] <- levels(noNA.table[,Ci])
            noNA.table[,Ci] <- as.numeric(noNA.table[,Ci])
            has.NAs[Ci] <- "numeric"
         }
         noNA.table[which(is.na(noNA.table[,Ci])),Ci] <- na.flags[[has.NAs[Ci]]]
      }
   }
   # create key columns, where values are replaced by ranks (with NA's last)
   KEYS <- as.data.frame(lapply(noNA.table,rank))
   names(KEYS) <- paste("KEY",sep=".",names(KEYS))
   # merge the keys back into the table
   DF <- data.frame(ORIG.ORDER=1:nrow(DF),noNA.table,KEYS)
   # identify, sort and number the unique rows
   UROWS <- unique(KEYS[Order(KEYS),])
   UROWS <- data.frame(UROW.ID=1:nrow(UROWS),UROWS)
   rownames(UROWS) <- NULL
   # merge these back into the main table
   DF <- merge(DF,UROWS,sort=F)
   DF <- DF[,c("ORIG.ORDER","UROW.ID",table.names)]
   # count the number of times each unique row occurs
   FREQS <- untab(table(DF$UROW.ID))
   # regenerate the table of unique rows using original values; drop KEY columns
   UROWS <- unique(DF[,c("UROW.ID",table.names)])
   if(length(has.NAs)>0){
      for(Ci in names(has.NAs)){
         # put the missing values back
         na.ndxs <- which(UROWS[,Ci]==na.flags[[has.NAs[Ci]]])
         UROWS[na.ndxs,Ci] <- NA
         # put factor levels back
         if(Ci%in%names(factlevs)){
            tmp <- as.char(UROWS[,Ci])
            tmp[-na.ndxs] <- factlevs[[Ci]][UROWS[-na.ndxs,Ci]]
            UROWS[,Ci] <- tmp
         }
      }
   }
   UROWS <- UROWS[order(UROWS$UROW.ID),]
   rownames(UROWS) <- NULL
   UROWS$UROW.FREQ <- FREQS
   # put the UROW.ID and FREQS columns first
   UROWS <- UROWS[,c("UROW.ID","UROW.FREQ",table.names)]
   UROWIDs <- DF[order(DF$ORIG.ORDER),"UROW.ID"]
   if(simplify){
      UNIQ <- rename(UROWS[,-1],"UROW.FREQ","n.rows",sanity=F)
   }else{
      UNIQ <- list(UROWS=UROWS,UROWIDs=UROWIDs)
   }
   return(UNIQ)
}
   
