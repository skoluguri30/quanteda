#' @rdname as.xtokens
#' @return external pointer to tokens tokens object
#' @export
as.xtokens <- function(x, ...) {
    docvars(x, '_docname') <- docnames(x)
    attrs <- attributes(x)
    attrs$names <- NULL
    result <- quanteda:::qatd_cpp_xpointer(x)
    attributes(result) <- attrs
    class(result) <- c('xtokens', 'tokens', 'tokenizedTexts')
    return(result)
}

#' print a stokens objects
#' print method for a tokenizedTextsHashed object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print xtokens
#' @noRd
print.xtokens <- function(x, ...) {
    attrs <- attributes(x)
    temp <- qatd_cpp_list(x)
    cat(attrs$class[1], " from ", ndoc(x), " document", 
        if (length(temp) > 1L) "s" else "", ".\n", sep = "")
    types <- c("", attrs$types)
    result <- lapply(temp, function(y) types[y + 1]) # shift index to show padding 
    class(result) <- "listof"
    print(result, ...)
}

#' @method "[" xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' xtoks <- as.xtokens(toks)
#' str(xpt)
#' xpt[c(1,3)]
"[.xtokens" <- function(x, i, ...) {
    attrs <- attributes(x)
    if (is.logical(i))
        i <- which(i)
    result <- quanteda:::qatd_cpp_xpointer_subset(x, i)
    if (is.data.frame(attrs$docvars)) {
        attrs$docvars <- attrs$docvars[i,,drop = FALSE]
    }
    attributes(result) <- attrs
    return(result)
}

#' @method "[[" xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' xpt <- as.xtokens(toks)
#' str(xpt)
#' xpt[[1]]
"[[.xtokens" <- function(x, i, ...) {
    attrs <- attributes(x)
    if (is.logical(i))
        i <- which(i)
    result <- quanteda:::qatd_cpp_xpointer_subset(x, i)
    types <- c("", attrs$types)
    types[qatd_cpp_list(result)[[1]] + 1] # shift index to show padding 
}

#' @noRd
#' @export
docnames.xtokens <- function(x) {
    docvars(x, '_docname')
}


#' @method tokens_select xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(data_corpus_inaugural)
#' xtoks <- as.xtokens(toks)
#' head(xtoks)
#' xtoks2 <- tokens_select(xtoks, stopwords(), padding = TRUE)
#' head(xtoks2)
tokens_select.xtokens <- function(x, pattern, selection = c("keep", "remove"), 
                                 valuetype = c("glob", "regex", "fixed"), window = 0,
                                 case_insensitive = TRUE, padding = FALSE, 
                                 verbose = quanteda_options("verbose")) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    types <- types(x)
    
    ids <- pattern2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    if ("" %in% pattern) ids <- c(ids, list(0)) # append padding index
    
    if (verbose) message_select(selection, length(ids), 0)
    if (any(window < 0)) stop('window sizes cannot be negative')
    if (length(window) > 2) stop("window must be a integer vector of length 1 or 2")
    if (length(window) == 1) window <- rep(window, 2)
    if (selection == 'keep') {
        result <- quanteda:::qatd_cpp_xpointer_select(x, types, ids, 1, padding, window[1], window[2])
    } else {
        result <- quanteda:::qatd_cpp_xpointer_select(x, types, ids, 2, padding, window[1], window[2])
    }
    attributes(result, FALSE) <- attrs
    return(result)
}

#' @rdname tokens_compound
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
#' @examples
#' txts <- c("The new law included a capital gains tax, and an inheritance tax.",
#'              "New York City has raised taxes: an income tax and inheritance taxes.")
#' xtoks <- as.xtokens(tokens(txts, remove_punct = TRUE))
#' 
#' # for lists of sequence elements
#' dict <- list(c("tax"), c("income", "tax"), c("capital", "gains", "tax"), c("inheritance", "tax"))
#' xtoks2 <- tokens_compound(xtoks, dict)
tokens_compound.xtokens <- function(x, pattern,
                                   concatenator = "_", valuetype = c("glob", "regex", "fixed"),
                                   case_insensitive = TRUE, join = TRUE,
                                   verbose = quanteda_options("verbose")) {
    
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    attrs$names <- NULL
    types <- types(x)
    
    seqs_id <- pattern2id(pattern, types, valuetype, case_insensitive, remove_unigram = TRUE)
    if (length(seqs_id) == 0) return(x) # do nothing
    x <- qatd_cpp_xpointer_compound(x, seqs_id, types, concatenator, join)
    attributes(x, FALSE) <- attrs
    attr(x, "concatenator") <- concatenator
    return(x)
}
