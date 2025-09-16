
#' simplify result of internal parameter processing
#' @param x output of internal .parmtypes
#' @export
packup = function(x) list(parms=dplyr::cross_join(data.frame(fn=x$funcname), x$parms), returns=x$ret[[1]])

#' Parse an R source file with roxygen2 doc comments and extract
#' argument types and return value
#' @param path character(1) path to source file
#' @param punct character(1) regular expression of tokens to be removed, defaults to ";"
#' @examples
#' srcf = system.file("demos", "mvcal.R", package="getTypeinfo")
#' tt = get_types(srcf)
#' lapply(tt, packup)
#' @export
get_types = function(path, punct=";") {
 src = source(path, local=TRUE)
 fns = setdiff(ls(sorted=FALSE), c("src", "path", "punct"))
 ans = lapply(seq_len(length(fns)), function(x) .parmtypes(path, ind=x, punct=punct))
 ans
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

