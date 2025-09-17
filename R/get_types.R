
#' simplify result of internal parameter processing
#' @param x output of internal .parmtypes
#' @export
packup = function(x) list(parms=dplyr::cross_join(data.frame(fn=x$funcname), x$parms), returns=x$ret[[1]])

#' Parse an R source file with roxygen2 doc comments and extract
#' argument types and return value
#' @param path character(1) path to source file
#' @param chat instance from ellmer
#' @param punct character(1) regular expression of tokens to be removed, defaults to ";"
#' @examples
#' srcf = system.file("demos", "mvcal.R", package="getTypeinfo")
#' tt = get_types(srcf)
#' tt 
#' lkkey = Sys.getenv("OPENAI_API_KEY")
#' if (nchar(lkkey)>0 && requireNamespace("ellmer")) {
#' ch = ellmer::chat_openai()
#' tt2 = get_types(srcf, ch)
#' tt2
#' }
#' @export
get_types = function(path, ch, punct=";") {
 capt = new.env()
 src = source(path, local=capt)
 fns = ls(capt, all=TRUE)
 if (missing(ch)) ans = lapply(seq_len(length(fns)), function(x) .parmtypes(path, ind=x, punct=punct))
 else if (!missing(ch)) ans = lapply(seq_len(length(fns)), function(x) .parmtypes_bot(path, ind=x, ch=ch))
 lapply(ans, packup)
}

# pt = .parmtypes

.parmtypes = function(path,ind,punct) {
  pf = roxygen2::parse_file(path)
  nfun = length(pf)
  stopifnot(ind <= nfun)
  dat = pf[[ind]]
  txt = deparse(dat$call)
  fn = sub(" <-.*| =.*", "", txt[1])
  tags = dat$tag
  tt = lapply(tags, function(x) x$tag)
  ok = which(sapply(tt, function(x) x == "param"))
  rr = lapply(tags[ok], function(x) x$raw)
  lrr = lapply(rr, function(x) strsplit(x, " "))
  droppunct = function(x, punct) gsub(punct, "", x)
  parms = sapply(lrr, function(x) c(parm=x[[1]][1], type=droppunct(x[[1]][2], punct)))
  ok2 = grep("return", tt) #which(sapply(tt, function(x) x == "return"))
  rr = lapply(tags[ok2], function(x) x$raw)
  list(parms=data.frame(t(parms)), ret=rr, funcname=fn)
}

#' use ellmer to infer argument data types from roxygen comments
#' @import btw
.parmtypes_bot = function(path,ind,ch) {
  pf = roxygen2::parse_file(path)
  nfun = length(pf)
  stopifnot(ind <= nfun)
  dat = pf[[ind]]
  txt = deparse(dat$call)
  fn = sub(" <-.*| =.*", "", txt[1])
  tags = dat$tag
  tt = lapply(tags, function(x) x$tag)
  ok = which(sapply(tt, function(x) x == "param"))
  ok2 = grep("return", tt) #which(sapply(tt, function(x) x == "return"))
  rr = lapply(tags[ok2], function(x) x$raw)
  outtmp = ch$chat(btw(tags[ok], "Provide a CSV file with two columns, parameter names and their types, which you will infer from their descriptions, typically just the first word or phrase.  Return only one CSV with your best guesses on data type of parameter.  Do not use any backticks or text formatting."), echo=FALSE)
  outdf = try(read.csv(textConnection(outtmp)))
  list(parms=outdf, ret=rr, funcname=fn)
  }

