#' Plot features as a wordcloud
#'
#' Plot a \link{dfm} object as a wordcloud, where the feature labels are plotted
#' with their sizes proportional to their numerical values in the dfm.  When
#' \code{comparison = TRUE}, it plots comparison word clouds by document.
#' @details The default is to plot the word cloud of all features, summed across
#'   documents.  To produce word cloud plots for specific document or set of
#'   documents, you need to slice out the document(s) from the dfm object.
#'
#'   Comparison wordcloud plots may be plotted by setting \code{comparison =
#'   TRUE}, which plots a separate grouping for \emph{each document} in the dfm.
#'   This means that you will need to slice out just a few documents from the
#'   dfm, or to create a dfm where the "documents" represent a subset or a
#'   grouping of documents by some document variable.
#'
#' @param x a dfm object
#' @param min_size size of the smallest word
#' @param max_size size of the largest word
#' @param max_words maximum number of words to be plotted. least frequent terms
#'   dropped.
#' @param color color of words from least to most frequent
#' @param font font-family of words and labels. Use default font if \code{NULL}.
#' @param adjust ajust sizes of words by a constant. Useful for non-Engish words
#'   for which R failes to obtaine sizes correctly.
#' @param rotation proportion words with 90 degree rotation colors
#' @param spacing add fixed amount of spaces around words
#' @param random_order plot words in random order. If \code{FALSE}, they will be
#'   plotted in decreasing frequency.
#' @param random_color choose colors randomly from the colors. If \code{FALSE},
#'   the color is chosen based on the frequency
#' @param ordered_color if \code{TRUE}, then colors are assigned to words in
#'   order.
#' @param labelcolor color of group labels. Only used when \code{compariosn=TRUE}.
#' @param labelsize size of group labels. Only used when \code{compariosn=TRUE}.
#' @param labeloffset  position of group labels. Only used when
#'   \code{compariosn=TRUE}.
#' @param comparison if \code{TRUE}, plot a wordclound that compares documents
#'   in the same way as \code{\link[wordcloud]{comparison.cloud}}
#' @param ... additional parameters passed to \link{text} (and \link{strheight},
#'   \link{strwidth})
#' @examples
#'   # plot the features (without stopwords) from Obama's two inaugural addresses
#'   mydfm <- dfm(corpus_subset(data_corpus_inaugural, President == "Obama"),
#'                remove = stopwords("english"), remove_punct = TRUE)
#'   mydfm <- dfm_trim(mydfm, min_count = 3)
#'   textplot_wordcloud(mydfm)
#'
#'   # plot in colors with some additional options passed to wordcloud
#'   textplot_wordcloud(mydfm, random_color = TRUE, rotation = 0.25,
#'                      color = sample(colors()[2:128], 5))
#'
#'   # old and new
#'   textplot_wordcloudold(mydfm, random.order = FALSE)
#'   textplot_wordcloud(mydfm)
#'
#'   \dontrun{
#'   # comparison plot of Irish government vs opposition
#'   docvars(data_corpus_irishbudget2010, "govtopp") <-
#'           factor(ifelse(data_corpus_irishbudget2010[, "party"] %in%
#'                  c("FF", "Green"), "Govt", "Opp"))
#'   govtopp_dfm <- dfm(data_corpus_irishbudget2010, groups = "govtopp",
#'                      remove_punct = TRUE)
#'   textplot_wordcloud(dfm_tfidf(govtopp_dfm), comparison = TRUE)
#'   # compare to non-tf-idf version
#'   textplot_wordcloud(govtopp_dfm, comparison = TRUE)
#'   }
#' @export
#' @keywords textplot
#' @import ggplot2
textplot_wordcloud <- function(x, 
                               min_size = 0.5, 
                               max_size = 6,
                               max_words = 1000,
                               color = "skyblue",
                               font = NULL,
                               adjust = 0,
                               rotation = 0.1,
                               spacing = 0.001,
                               random_order = TRUE,
                               random_color = FALSE,
                               ordered_color = FALSE,
                               ...,
                               comparison = FALSE) {
    UseMethod("textplot_wordcloud")
}

#' @export
textplot_wordcloud.default <- function(x, ..., comparison = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textplot_wordcloud"))
}

#' @export
textplot_wordcloud.dfm <- function(x, 
                                   min_size = 0.5, 
                                   max_size = 4,
                                   max_words = 1000,
                                   color = "skyblue",
                                   font = NULL,
                                   adjust = 0,
                                   rotation = 0.1,
                                   spacing = 0.001,
                                   random_order = TRUE,
                                   random_color = FALSE,
                                   ordered_color = FALSE,
                                   labelcolor = 'black',
                                   labelsize = 4,
                                   labeloffset = 0.05,
                                   ...,
                                   comparison = FALSE) {

    x <- as.dfm(x)
    if (comparison) {
        if (ndoc(x) > 8) 
            stop("Too many documents to plot comparison, use 8 or fewer documents.")
        wordcloud_comparison(x, min_size , max_size, max_words,
                             color, font, adjust, rotation, spacing,
                             random_order, random_color, ordered_color,
                             labelcolor, labelsize, labeloffset, ...)
    } else {
        wordcloud(x, min_size, max_size, max_words,
                  color, font, adjust, rotation, spacing,
                  random_order, random_color, ordered_color,
                  labelcolor, labelsize, labeloffset, ...)
    }
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud without dependecies. Code is adopted from 
#' \code{\link[wordcloud]{wordcloud}}.
#' @inheritParams textplot_wordcloud
#' @param scale deprecated argument
#' @param min.freq deprecated argument
#' @param max.words deprecated argument
#' @param random.order deprecated argument
#' @param random.color deprecated argument
#' @param rot.per deprecated argument
#' @param ordered.colors deprecated argument
#' @param use.r.layout deprecated argument
#' @param fixed.asp deprecated argument
#' @keywords internal
wordcloud <- function(x, min_size, max_size, max_words,
                      color, font, adjust, rotation, spacing,
                      random_order, random_color, ordered_color,
                      labelcolor, labelsize, labeloffset,
                      # deprecated arguments
                      colors, scale, min.freq, max.words, random.order, 
                      random.color, rot.per, ordered.colors, use.r.layout, fixed.asp,
                      ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
    if (!missing(colors)) {
        color <- colors
        arg_dep <- c(arg_dep, 'color' = 'colors')
    }
    if (!missing(scale)) {
        max_size <- scale[1]
        min_size <- scale[2]
        arg_dep <- c(arg_dep, 'min_size and max_size' = 'scale')
    }
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max_words' = 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random_order' = 'random.order')
    }
    if (!missing(random.color)) {
        random_color <- random.color
        arg_dep <- c(arg_dep, 'random_color' = 'random.color')
    }
    if (!missing(rot.per)) {
        rotation <- rot.per
        arg_dep <- c(arg_dep, 'rotation' = 'rot.per')
    }
    if (!missing(ordered.colors)) {
        ordered_color <- ordered.colors
        arg_dep <- c(arg_dep, 'ordered_color' = 'ordered.colors')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(fixed.asp)) {
        warning('fixed.asp is no longer used', call. = FALSE)
    }
    if (length(arg_dep))
        warning(paste(arg_dep), " is deprecated; use ", paste(names(arg_dep)), " instead", call. = FALSE)
    
    font <- check_font(font)
    freq <- Matrix::colSums(x)

    words <- data.frame(word = names(freq), freq = unname(freq), stringsAsFactors = FALSE)
    if (random_color) {
        words$col <- sample(color, nrow(words), replace = TRUE)
    } else if (ordered_color) {
        if (length(color) != nrow(words))
            stop("Length of color does not match length of word vector")
        words$col <- color
    } else {
        words$col <- rep(color, each = ceiling(nrow(words)) / length(color),
                         length.out = nrow(words))
    }
    words <- head(words[order(words$freq, decreasing = TRUE),], max_words)
    words <- set_wordsize(words, rotation, spacing, max_size, min_size)
    
    words$x <- NA
    words$y <- NA
    words <- set_wordposition(words)
    
    is_missed <- is.na(words$x)
    if (any(is_missed))
        warning(paste(words$word[is_missed], collapse = ', '), " could not be fit in the plot.", call. = FALSE)
    
    words$mm <- (1 + adjust) * as.numeric(grid::convertUnit(unit(words$size, 'snpc'), 'mm'))
    print(head(words))
    
    x <- y <- w <- h <- label <- NULL
    words <- na.omit(words)
    plot <- ggplot() + 
        geom_text(data = words, aes(x + 0.5 * w, y + 0.5 * h, label = word), color = words$col, family = font,
                  size = words$mm, angle = words$angle, 
                  lineheight = 1, vjust = "center") +
        #geom_point(data = words, aes(x, y)) + # for debug
        #geom_vline(xintercept = c(0, 0.25, 0.75, 1)) + # for debug
        #geom_hline(yintercept = c(0, 0.25, 0.75, 1)) + # for debug
        coord_fixed() + 
        scale_x_continuous(limits = c(0, 1), breaks = NULL) + 
        scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank()
        )
    
    return(plot)
}

#' Internal function for textplot_wordcloud
#'
#' This function impliments wordcloud that compares documents. Code is adopted from
#' \code{\link[wordcloud]{comparison.cloud}}.
#' @inheritParams textplot_wordcloud
#' @param scale deprecated argument
#' @param min.freq deprecated argument
#' @param max.words deprecated argument
#' @param random.order deprecated argument
#' @param random.color  deprecated argument
#' @param rot.per deprecated argument
#' @param ordered.colors deprecated argument
#' @param use.r.layout deprecated argument
#' @param title.size deprecated argument
#' @keywords internal
wordcloud_comparison <- function(x, min_size, max_size, max_words,
                                 color, font, adjust, rotation, spacing,
                                 random_order, random_color, ordered_color,
                                 labelcolor, labelsize, labeloffset,
                                 # deprecated arguments
                                 colors, scale, min.freq, max.words, 
                                 random.order, rot.per, use.r.layout, title.size,
                                 ...) {
    
    if (!missing(min.freq)) {
        warning('min.freq is deprecated; use dfm_trim() before textplot_wordcloud()', call. = FALSE)
        x <- dfm_trim(x, min_count = min.freq)
    }
    arg_dep <- character()
    if (!missing(colors)) {
        color <- colors
        arg_dep <- c(arg_dep, 'color' = 'colors')
    }
    if (!missing(scale)) {
        max_size <- scale[1]
        min_size <- scale[2]
        arg_dep <- c(arg_dep, 'min_size and max_size' = 'scale')
    }
    if (!missing(max.words)) {
        max_words <- max.words
        arg_dep <- c(arg_dep, 'max_words' = 'max.words')
    }
    if (!missing(random.order)) {
        random_order <- random.order
        arg_dep <- c(arg_dep, 'random_order' = 'random.order')
    }
    if (!missing(rot.per)) {
        rotation <- rot.per
        arg_dep <- c(arg_dep, 'rotation' = 'rot.per')
    }
    if (!missing(use.r.layout)) {
        warning('use.r.layout is no longer used', call. = FALSE)
    }
    if (!missing(title.size)) {
        labelsize <- title.size
        arg_dep <- c(arg_dep, 'labelsize' = 'title.size')
    }
    if (length(arg_dep)) {
        warning(paste(arg_dep), " is deprecated; use ", paste(names(arg_dep)), " instead", call. = FALSE)
    }
    
    font <- check_font(font)
    x <- dfm_weight(x, 'propmax')
    if (length(color) < nrow(x))
        color <- RColorBrewer::brewer.pal(max(3, nrow(x)), 'Paired')
    
    words <- data.frame()
    for (h in seq(nrow(x))) {
        freq <- Matrix::colSums(x[h,])
        temp <- data.frame(word = names(freq), freq = unname(freq), stringsAsFactors = FALSE)
        
        temp$col <- color[h]
        temp$group <- h 
        
        temp <- head(temp[order(temp$freq, decreasing = TRUE),], max_words / nrow(x))
        temp <- set_wordsize(temp, rotation, spacing, max_size, min_size)
        words <- rbind(words, temp)
    }
    
    words$x <- NA
    words$y <- NA
    words <- set_wordposition(words)

    is_missed <- is.na(words$x)
    if (any(is_missed))
        warning(paste(words$word[is_missed], collapse = ', '), " could not be fit in the plot.", call. = FALSE)
    
    words$mm <- (1 + adjust) * as.numeric(grid::convertUnit(unit(words$size, 'snpc'), 'mm'))
    #print(head(words))
    
    if (labelsize > 0) {
        labels <- data.frame(word = rownames(x), stringsAsFactors = FALSE)
        labels$freq <- 1 # dumy frequency get sizes
        labels <- set_wordsize(labels, FALSE, 0, labelsize, labelsize)
        for (h in seq(nrow(labels))) {
            theta_limit <- seq(0, 2 * pi, length = nrow(x) + 1)
            theta <- mean(theta_limit[seq(h, h + 1)])
            labels$x[h] <- 0.5 + (0.5 + (labeloffset / 0.5)) * cos(theta)
            labels$y[h] <- 0.5 + (0.5 + (labeloffset / 0.5)) * sin(theta)
        }
        labels$mm <- as.numeric(grid::convertUnit(unit(labels$size, 'snpc'), 'mm'))
        #print(labels)
    }
    
    x <- y <- w <- h <- label <- NULL
    words <- na.omit(words)
    plot <- ggplot() + 
        geom_text(data = words, aes(x + 0.5 * w, y + 0.5 * h, label = word), color = words$col, family = font,
                  size = words$mm, angle = words$angle, 
                  lineheight = 1, vjust = "center") +
        #geom_point(data = words, aes(x, y)) + # for debug
        #geom_vline(xintercept = c(0, 0.25, 0.75, 1)) + # for debug
        #geom_hline(yintercept = c(0, 0.25, 0.75, 1)) + # for debug
        coord_fixed() + 
        scale_x_continuous(limits = if (labelsize > 0) c(-0.1, 1.1) else c(0, 1), breaks = NULL) + 
        scale_y_continuous(limits = if (labelsize > 0) c(-0.1, 1.1) else c(0, 1), breaks = NULL) +
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank()
        )
    
    if (labelsize > 0) {
        labels <- na.omit(labels)
        plot <- plot + 
            geom_text(data = labels, aes(x, y, label = word), color = labelcolor, 
                      size = labels$mm, family = font, lineheight = 1, vjust = "center")
    }
    
    return(plot)
}


set_wordposition <- function(data) {
    
    stopifnot('word' %in% names(data))
    stopifnot('freq' %in% names(data))
    stopifnot('x' %in% names(data))
    stopifnot('y' %in% names(data))
    stopifnot('w' %in% names(data))
    stopifnot('h' %in% names(data))
    
    if (!'group' %in% names(data))
        data$group <- 1L
    
    group <- sort(unique(data$group))
    theta_limit <- seq(0, 2 * pi, length = length(group) + 1)
    theta_step <- 0.1 / length(group)
    r_step <- 0.05
    x1 <- y1 <- 0.5
    
    for (g in group) {
        for (i in seq_len(nrow(data))) {
            if (data$group[i] != g) next
            r <- 0
            theta <- stats::runif(1, 0, 2 * pi)
            is_overlaped <- TRUE
            while (is_overlaped) {
                w <- data$w[i]
                h <- data$h[i] 
                in_region <- theta > theta_limit[g] && theta < theta_limit[g + 1]
                if (in_region && !qatd_cpp_is_overlap(x1 - 0.5 * w, y1 - 0.5 * h, w, h, data) &&
                    x1 - 0.5 * w > 0 && y1 - 0.5 * h > 0 && x1 + 0.5 * w < 1 && y1 + 0.5 * h < 1) {
                    
                    data[i, 'x'] <- x1 - 0.5 * w
                    data[i, 'y'] <- y1 - 0.5 * h
                    
                    is_overlaped <- FALSE
                } else {
                    if (r > sqrt(0.5)) {
                        is_overlaped <- FALSE
                    }

                    theta <- theta + theta_step
                    if (theta > 2 * pi)
                        theta <- theta - 2 * pi # required for comparison cloud
                    r <- r + r_step * theta_step / (2 * pi)
                    x1 <- 0.5 + r * cos(theta)
                    y1 <- 0.5 + r * sin(theta)
                }
            }
        }
    }
    return(data)
}

#' Experimental R++ version (not used for poor performance)
is_wordoverlap <- function(data, x, y, w, h) {
    #cat(x, y, w, h, '\n')
    any(ifelse(data$x < x, data$x + data$w > x, x + w > data$x) &
        ifelse(data$y < y, data$y + data$h > y, y + h > data$y))
}

#' Gauge sizes of words in Normalised Parent Coordinates 
#'
#' @param x a data.frame that contain columns for words (word) and their frequencies (freq)
#' @inheritParams wordclound
#'
#' @return a data.frame with sizes of words in NPC
#' @keywords internal
set_wordsize <- function(data, rotation, spacing, max_size, min_size) {
       
    stopifnot('word' %in% names(data))
    stopifnot('freq' %in% names(data))
    
    data$freq <- data$freq / max(data$freq)
    
    temp <- data.frame()
    for (i in seq_len(nrow(data))) {
        temp <- rbind(temp, data.frame(
            height = as.numeric(grid::convertUnit(grid::unit(1, "strheight", data[i, 'word']), 'npc')),
            width = as.numeric(grid::convertUnit(grid::unit(1, "strwidth", data[i, 'word']), 'npc'))
        ))
    }
    
    result <- cbind(data, temp)
    result$size <- result$height # original size of fonts
    
    tailed <- grepl("g|j|p|q|y", result$word) # contain g, j, p, q, y
    result$height[tailed] <- result$height[tailed] * 1.2
    
    result$scale <- (max_size - min_size) * data$freq + min_size
    result <- result[order(result$scale, decreasing = TRUE),]
    
    # scale up by the frequency
    result$size <- result$size *  result$scale
    result$width <- result$width *  result$scale
    result$height <- result$height *  result$scale
    
    # set height and width for layout
    rotate <- stats::runif(nrow(result)) < rotation
    result$angle <- rotate * 90
    result[rotate, 'h'] <- result$width[rotate] + spacing
    result[rotate, 'w'] <- result$height[rotate] + spacing
    result[!rotate, 'w'] <- result$width[!rotate] + spacing
    result[!rotate, 'h'] <- result$height[!rotate] + spacing
    
    return(result)
}
