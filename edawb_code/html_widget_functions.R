# Source: library htmlwidgets.
createWidgetId <- function(bytes = 10) {
    
    .globals <- new.env(parent = emptyenv())
    
    # Note what the system's random seed is before we start, so we can restore it after
    sysSeed <- .GlobalEnv$.Random.seed
    # Replace system seed with our own seed
    if (!is.null(.globals$idSeed)) {
        .GlobalEnv$.Random.seed <- .globals$idSeed
    }
    on.exit({
        # Change our own seed to match the current seed
        .globals$idSeed <- .GlobalEnv$.Random.seed
        # Restore the system seed--we were never here
        if(!is.null(sysSeed))
            .GlobalEnv$.Random.seed <- sysSeed
        else
            rm(".Random.seed", envir = .GlobalEnv)
    })
    
    paste(
        format(as.hexmode(sample(256, bytes, replace = TRUE)-1), width=2),
        collapse = "")
}

# Source: library htmlwidgets.
prop <- function(x, path) {
    tryCatch({
        for (i in strsplit(path, "$", fixed = TRUE)[[1]]) {
            if (is.null(x))
                return(NULL)
            x <- x[[i]]
        }
        return(x)
    }, error = function(e) {
        return(NULL)
    })
}

# Source: library htmlwidgets.
any_prop <- function(scopes, path) {
    for (scope in scopes) {
        result <- prop(scope, path)
        if (!is.null(result))
            return(result)
    }
    return(NULL)
}

# Source: library htmlwidgets.
resolveSizing <- function(x, sp, standalone, knitrOptions = NULL) {
    
    DEFAULT_WIDTH <- 960
    DEFAULT_HEIGHT <- 500
    DEFAULT_PADDING <- 40
    DEFAULT_WIDTH_VIEWER <- 450
    DEFAULT_HEIGHT_VIEWER <- 350
    DEFAULT_PADDING_VIEWER <- 15
    
    if (isTRUE(standalone)) {
        userSized <- !is.null(x$width) || !is.null(x$height)
        viewerScopes <- list(sp$viewer, sp)
        browserScopes <- list(sp$browser, sp)
        # Precompute the width, height, padding, and fill for each scenario.
        return(list(
            runtime = list(
                viewer = list(
                    width = x$width %||% any_prop(viewerScopes, "defaultWidth") %||% DEFAULT_WIDTH_VIEWER,
                    height = x$height %||% any_prop(viewerScopes, "defaultHeight") %||% DEFAULT_HEIGHT_VIEWER,
                    padding = any_prop(viewerScopes, "padding") %||% DEFAULT_PADDING_VIEWER,
                    fill = !userSized && any_prop(viewerScopes, "fill") %||% TRUE
                ),
                browser = list(
                    width = x$width %||% any_prop(browserScopes, "defaultWidth") %||% DEFAULT_WIDTH,
                    height = x$height %||% any_prop(browserScopes, "defaultHeight") %||% DEFAULT_HEIGHT,
                    padding = any_prop(browserScopes, "padding") %||% DEFAULT_PADDING,
                    fill = !userSized && any_prop(browserScopes, "fill") %||% FALSE
                )
            ),
            width = x$width %||% prop(sp, "defaultWidth") %||% DEFAULT_WIDTH,
            height = x$height %||% prop(sp, "defaultHeight") %||% DEFAULT_HEIGHT
        ))
    } else if (!is.null(knitrOptions)) {
        knitrScopes <- list(sp$knitr, sp)
        isFigure <- any_prop(knitrScopes, "figure")
        # flexdashboard actually adds on another fig.width for intelligent sizing of static 
        # figures in desktop/mobile mode
        # https://github.com/rstudio/flexdashboard/blob/02207b7/R/flex_dashboard.R#L262
        # flexdashboard should really only be doing this for static plots, but we make sure 
        # to just take the first (desktop) sizing to make this "just work" for flexdashboard 
        # (or really anyone else that provides a vector of widths/heights for a widget by 
        # just taking the 1st value)
        figWidth <- if (isFigure) knitrOptions$out.width.px[[1L]] else NULL
        figHeight <- if (isFigure) knitrOptions$out.height.px[[1L]] else NULL
        # Compute the width and height
        return(list(
            width = x$width %||% figWidth %||% any_prop(knitrScopes, "defaultWidth") %||% DEFAULT_WIDTH,
            height = x$height %||% figHeight %||% any_prop(knitrScopes, "defaultHeight") %||% DEFAULT_HEIGHT
        ))
    } else {
        # Some non-knitr, non-print scenario.
        # Just resolve the width/height vs. defaultWidth/defaultHeight
        return(list(
            width = x$width %||% prop(sp, "defaultWidth") %||% DEFAULT_WIDTH,
            height = x$height %||% prop(sp, "defaultHeight") %||% DEFAULT_HEIGHT
        ))
    }
}

widget_html.default <- function (name, package, id, style, class, inline = FALSE, ...) {
    if (inline) {
        tags$span(id = id, style = style, class = class)
    } else {
        tags$div(id = id, style = style, class = class)
    }
}

lookup_func <- function(name, package) {
    tryCatch(
        get(name, asNamespace(package), inherits = FALSE),
        error = function(e) NULL
    )
}

lookup_widget_html_method <- function(name, package) {
    # There are two ways we look for custom widget html container methods:
    #
    # PACKAGE:::widget_html.NAME - This is the newer, preferred lookup. Note that
    # it doesn't actually use S3 dispatch, because we don't have an S3 object to
    # dispatch with (widget_html can be called without a widget instance existing
    # yet).
    #
    # PACKAGE:::NAME_html - This is the original, legacy lookup. This is not
    # preferred because it's not unique enough, i.e. someone could happen to have
    # a function with this name that's not intended for widget_html use. However,
    # we have to keep it for now, for backward compatibility.
    
    fn_name <- paste0("widget_html.", name)
    fn <- lookup_func(fn_name, package)
    if (!is.null(fn)) {
        return(list(fn = fn, name = fn_name, legacy = FALSE))
    }
    
    fn_name <- paste0(name, "_html")
    fn <- lookup_func(fn_name, package)
    if (!is.null(fn)) {
        return(list(fn = fn, name = fn_name, legacy = TRUE))
    }
    
    list(fn = widget_html.default, name = "widget_html.default", legacy = FALSE)
}

widget_html <- function (name, package, id, style, class, inline = FALSE, ...) {
    
    fn_info <- lookup_widget_html_method(name, package)
    
    fn <- fn_info[["fn"]]
    # id, style, and class have been required args for years, but inline is fairly new
    # and undocumented, so unsuprisingly there are widgets out there are don't have an
    # inline arg https://github.com/renkun-ken/formattable/blob/484777/R/render.R#L79-L88
    args <- list(id = id, style = style, class = class, ...)
    if ("inline" %in% names(formals(fn))) {
        args$inline <- inline
    }
    fn_res <- do.call(fn, args)
    if (isTRUE(fn_info[["legacy"]])) {
        # For the PACKAGE:::NAME_html form (only), we worry about false positives;
        # hopefully false positives will return something that doesn't look like a
        # Shiny tag/html and they'll get this warning as a hint
        if (!inherits(fn_res, c("shiny.tag", "shiny.tag.list", "html"))) {
            warning(fn_info[["name"]], " returned an object of class `", class(fn_res)[1],
                    "` instead of a `shiny.tag`."
            )
        }
    }
    
    fn_res
}

createPayload <- function(instance){
    if (!is.null(instance$preRenderHook)){
        instance <- instance$preRenderHook(instance)
        instance$preRenderHook <- NULL
    }
    x <- .subset2(instance, "x")
    list(x = x, evals = JSEvals(x), jsHooks = instance$jsHooks)
}

widget_data <- function(x, id, ...){
    # It's illegal for </script> to appear inside of a script tag, even if it's
    # inside a quoted string. Fortunately we know that in JSON, the only place
    # the '<' character can appear is inside a quoted string, where a Unicode
    # escape has the same effect, without confusing the browser's parser. The
    # repro for the bug this gsub fixes is to have the string "</script>" appear
    # anywhere in the data/metadata of a widget--you will get a syntax error
    # instead of a properly rendered widget.
    #
    # Another issue is that if </body></html> appears inside a quoted string,
    # then when pandoc coverts it with --self-contained, the escaping gets messed
    # up. There may be other patterns that trigger this behavior, so to be safe
    # we can replace all instances of "</" with "\\u003c/".
    payload <- toJSON(createPayload(x))
    payload <- gsub("</", "\\u003c/", payload, fixed = TRUE)
    tags$script(type = "application/json", `data-for` = id, HTML(payload))
}

# Source: toHTML from library htmlwidgets.
to_html <- function(x, standalone = FALSE, knitrOptions = NULL) {
    
    sizeInfo <- resolveSizing(x, x$sizingPolicy, standalone = standalone, knitrOptions = knitrOptions)
    
    if (!is.null(x$elementId))
        id <- x$elementId
    else
        id <- paste("htmlwidget", createWidgetId(), sep="-")
    
    w <- shiny::validateCssUnit(sizeInfo$width)
    h <- shiny::validateCssUnit(sizeInfo$height)
    
    # create a style attribute for the width and height
    style <- paste(
        "width:", w, ";",
        "height:", h, ";",
        sep = "")
    
    x$id <- id
    
    container <- if (isTRUE(standalone)) {
        function(x) {
            div(id="htmlwidget_container", x)
        }
    } else {
        identity
    }
    
    html <- htmltools::tagList(
        container(
            htmltools::tagList(
                x$prepend,
                widget_html(
                    name = class(x)[1],
                    package = attr(x, "package"),
                    id = id,
                    style = style,
                    class = paste(class(x)[1], "html-widget"),
                    width = sizeInfo$width,
                    height = sizeInfo$height
                ),
                x$append
            )
        ),
        widget_data(x, id),
        if (!is.null(sizeInfo$runtime)) {
            tags$script(type="application/htmlwidget-sizing", `data-for` = id,
                        toJSON(sizeInfo$runtime)
            )
        }
    )
    html <- htmltools::attachDependencies(html,
                                          c(widget_dependencies(class(x)[1], attr(x, 'package')),
                                            x$dependencies)
    )
    
    htmltools::browsable(html)
    
}
