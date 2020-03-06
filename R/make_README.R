#' Generates README.md from *.rmd package documentation.
#'
#' make_README() is a utility to generate a README.md file from microRutils.R
#' by converting the R markdown to GitHub-friendly markdown.
#'
#' Might be reusable, but meant for internal use (Not exported). Caveat emptor; 
#' hasn't been tested much.  If you really want to try it, use 
#' microRutils:::make_README.R().
#'
#' @param pkg.R_file
#'   character value specifying file path/name to be converted.
#' @param out.file
#'   optional character value specifying file path/name for output *.md file.
#'
#' @return
#'    Invisibly returns a character vector with the output, one line per row.
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
make_README <- function(pkg.R_file, out.file){
   if(missing(pkg.R_file)){
      pkg.R_file <- "microRutils.R"
   }
   if(!file.exists(pkg.R_file)){
      stop("pkg.R_file not found.")
   }
   txt <- readLines(pkg.R_file)
   # get just the markdown commments lines
   txt <- sub("^#' ?", "", grep("^#'", txt, value=T))
   # concatenate into one long string; remove extra spaces
   txt <- gsub(" +", " ", paste0(txt, collapse = "\n"))

   replace_md <- function(txt, open_patt, close_patt, open_nest, open_repl,
                          close_repl, txt_fun=function(x) x){
      if(length(txt)!=1) stop("txt must be length=1 character vector.")
      if(missing(open_nest)){
         open_nest <- gsub("\\}", "\\{", gsub("\\]", "\\[", gsub("\\)", "\\(",
            close_patt)))
      }
      if(missing(close_repl)) close_repl <- open_repl

      open.ndxs <- gregexpr(open_patt, txt)[[1]]
      if(open.ndxs[1]== -1) invisible(txt)
      nest.ndxs <- gregexpr(open_nest, txt)[[1]]
      close.ndxs <- gregexpr(close_patt, txt)[[1]]
      for(open.ndx in rev(open.ndxs)){
         close.ndx <- close.ndxs[which(close.ndxs > open.ndx)]
         if(length(close.ndx)<1) {
            stop("Unclosed expression starting at position ", open.ndx)
         }
         open.end <- open.ndx +
            attr(open.ndxs, "match.length")[which(open.ndxs==open.ndx)]
         while(any(nest.ndxs>open.end & nest.ndxs<close.ndx[1])){
            close.ndx <- close.ndx[-1]
            if(length(close.ndx)<1){
               stop("Unclosed expression starting at position ", open.ndx)
            }
         }
         close.ndx <- close.ndx[1]
         close.end <- close.ndx +
            attr(close.ndxs, "match.length")[which(close.ndxs==close.ndx)]
         prefix <- ifelse(open.ndx==1, "", substr(txt, 1, open.ndx-2))
         inner <- txt_fun(substr(txt, open.end, close.ndx-1))
         suffix <- ifelse(close.end>=nchar(txt), "",
                          substr(txt, close.end, nchar(txt)))
         txt <- paste0(prefix, open_repl, inner, close_repl, suffix)
      }
      txt
   }

   #--------------
   # remove links from named links, like \link[function name]{displayed text}
   txt <- replace_md(txt, open_patt = "\\link\\[", close_patt = "\\]",
      open_repl = "\\link", close_repl = "", txt_fun = function(x) "")
   # find the function names and replace link{...} with __...__ (bold)
   txt <- replace_md(txt, open_patt = "\\link\\{", close_patt = "\\}",
      open_repl = "")  # actually, bold doesn't work inside code.
   # find the replace code{...} with `...` (code)
   txt <- replace_md(txt, open_patt = "\\code\\{", close_patt = "\\}",
      open_repl = "`")
   # remove line breaks between function names and descriptions
   txt <- gsub(":\n ", ": ", txt)
   # remove \itemize altogether; format \item's as bulletted lists
   txt <- replace_md(txt, open_patt = "\\itemize\\{", close_patt = "\\}",
      open_repl="", txt_fun = function(x) gsub(" \\item ", "* ", x, fixed=TRUE))
   # replace @section with H3 headers
   txt <- gsub(" ?@section ", "### ", txt)
   # remove other lines starting with @
   txt <- gsub(" ?@[^\n]+(\n|$)", "\n", txt)
   # remove line breaks within paragraphs of txt
   txt <- gsub(" +", " ", gsub("([0-9a-zA-Z\\.] ?)\n( ?[0-9a-zA-Z\\.])",
      "\\1 \\2", txt))
   # remove extra empty lines
   txt <- gsub("(\n ?){2, }", "\n\n", txt)
   # make the first line H2 header
   txt[1] <- sub("^ ?", "## ", txt)
   # split back into a vector at the line breaks
   txt <- unlist(strsplit(txt, "\n ?"))

   if(!missing(out.file)){
      writeLines(txt, out.file)
   }

   invisible(txt)
}
