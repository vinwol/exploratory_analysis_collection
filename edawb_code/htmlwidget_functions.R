convert_to_HTML <- function(x, standalone = FALSE, knitrOptions = NULL) {
    
    sizeInfo <- resolveSizing(x, x$sizingPolicy, standalone = standalone, knitrOptions = knitrOptions)
    
    if (!is.null(x$elementId))
        id <- x$elementId
    else
        id <- paste("htmlwidget", createWidgetId(), sep="-")
    
    w <- validateCssUnit(sizeInfo$width)
    h <- validateCssUnit(sizeInfo$height)
    
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


resolveSizing <- function(x, sp, standalone, knitrOptions = NULL) {
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


"%||%" <- function(a, b) if (!is.null(a)) a else b

# Copied from shiny 0.14.2
toJSON2 <- function(
    x, ...,  dataframe = "columns", null = "null", na = "null", auto_unbox = TRUE,
    digits = getOption("shiny.json.digits", 16), use_signif = TRUE, force = TRUE,
    POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE, keep_vec_names = TRUE,
    strict_atomic = TRUE
) {
    if (strict_atomic) x <- I(x)
    jsonlite::toJSON(
        x, dataframe = dataframe, null = null, na = na, auto_unbox = auto_unbox,
        digits = digits, use_signif = use_signif, force = force, POSIXt = POSIXt,
        UTC = UTC, rownames = rownames, keep_vec_names = keep_vec_names,
        json_verbatim = TRUE, ...
    )
}

if (requireNamespace('shiny') && packageVersion('shiny') >= '0.12.0') local({
    tryCatch({
        toJSON <- getFromNamespace('toJSON', 'shiny')
        args2 <- formals(toJSON2)
        args1 <- formals(toJSON)
        if (!identical(args1, args2)) {
            warning('Check shiny:::toJSON and make sure htmlwidgets:::toJSON is in sync')
        }
    })
})

toJSON <- function(x) {
    if (!is.list(x) || !('x' %in% names(x))) return(toJSON2(x))
    func <- attr(x$x, 'TOJSON_FUNC', exact = TRUE)
    args <- attr(x$x, 'TOJSON_ARGS', exact = TRUE)
    if (length(args) == 0) args <- getOption('htmlwidgets.TOJSON_ARGS')
    if (!is.function(func)) func <- toJSON2
    res <- if (length(args) == 0) func(x) else do.call(func, c(list(x = x), args))
    # make sure shiny:::toJSON() does not encode it again
    structure(res, class = 'json')
}

#' Get js and css dependencies for  a htmlwidget
#'
#' @param name name of the widget.
#' @param package name of the package, defaults to the widget name.
#' @export
getDependency <- function(name, package = name){
    config = sprintf("htmlwidgets/%s.yaml", name)
    jsfile = sprintf("htmlwidgets/%s.js", name)
    
    # if yaml does not exist then assume no dependencies
    #  in this cases dependencies should be provided through the
    #  dependencies argument of createWidget
    widgetDep <- list()
    if (file.exists(system.file(config, package = package))) {
        config = yaml::yaml.load_file(
            system.file(config, package = package)
        )
        widgetDep <- lapply(config$dependencies, function(l) {
            l$package = package
            do.call(htmlDependency, l)
        })
    }
    
    # if js binding does not exist then assume provided through
    #  some other mechanism such as a specified `htmlDependency` or `script` tag.
    #  Note, this is a very special case.
    bindingDep <- if (file.exists(system.file(jsfile, package = package))) {
        htmlDependency(
            name = paste0(name, "-binding"),
            version = packageVersion(package),
            src = "htmlwidgets",
            package = package,
            script = basename(jsfile),
            all_files = FALSE
        )
    }
    
    c(
        list(htmlDependency(
            name = "htmlwidgets",
            version = packageVersion("htmlwidgets"),
            src = "www",
            package = "htmlwidgets",
            script = "htmlwidgets.js"
        )),
        widgetDep,
        list(bindingDep)
    )
}

`%||%` <- function(x, y){
    if (is.null(x)) y else x
}

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

any_prop <- function(scopes, path) {
    for (scope in scopes) {
        result <- prop(scope, path)
        if (!is.null(result))
            return(result)
    }
    return(NULL)
}

#' Mark character strings as literal JavaScript code
#'
#' This function \code{JS()} marks character vectors with a special class, so
#' that it will be treated as literal JavaScript code when evaluated on the
#' client-side.
#' @param ... character vectors as the JavaScript source code (all arguments
#'   will be pasted into one character string)
#' @author Yihui Xie
#' @export
#' @examples library(htmlwidgets)
#' JS('1 + 1')
#' list(x = JS('function(foo) {return foo;}'), y = 1:10)
#' JS('function(x) {', 'return x + 1;', '}')
JS <- function(...) {
    x <- c(...)
    if (is.null(x)) return()
    if (!is.character(x))
        stop("The arguments for JS() must be a character vector")
    x <- paste(x, collapse = '\n')
    structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}

#' Creates a list of keys whose values need to be evaluated on the client-side
#'
#' It works by transforming \code{list(foo = list(1, list(bar =
#' I('function(){}')), 2))} to \code{list("foo.2.bar")}. Later on the JS side,
#' the \code{window.HTMLWidgets.evaluateStringMember} function is called with
#' the JSON object and the "foo.2.bar" string, which is split to \code{['foo',
#' '2', 'bar']}, and the string at that location is replaced \emph{in-situ} with
#' the results of evaluating it. Note '2' (character) should have been 2
#' (integer) but it does not seem to matter in JS: x[2] is the same as x['2']
#' when all child members of x are unnamed, and ('2' in x) will be true even if
#' x is an array without names. This is a little hackish.
#'
#' This function is intended mostly for internal use. There's generally no need
#' for widget authors or users to call it, as it's called automatically on the
#' widget instance data during rendering. It's exported in case other packages
#' want to add support for \code{\link{JS}} in contexts outside of widget
#' payloads.
#'
#' @param list a list in which the elements that should be evaluated as
#'   JavaScript are to be identified
#' @author Yihui Xie
#' @keywords internal
#' @export
JSEvals <- function(list) {
    # the `%||% list()` part is necessary as of R 3.4.0 (April 2017) -- if `evals`
    # is NULL then `I(evals)` results in a warning in R 3.4.0. This is circumvented
    # if we let `evals` be equal to `list()` in those cases
    evals <- names(which(unlist(shouldEval(list)))) %||% list()
    I(evals)  # need I() to prevent toJSON() from converting it to scalar
}

#' JSON elements that are character with the class JS_EVAL will be evaluated
#'
#' @noRd
#' @keywords internal
shouldEval <- function(options) {
    if (is.list(options)) {
        if ((n <- length(options)) == 0) return(FALSE)
        # use numeric indices as names (remember JS indexes from 0, hence -1 here)
        if (is.null(names(options)))
            names(options) <- seq_len(n) - 1L
        # Escape '\' and '.' by prefixing them with '\'. This allows us to tell the
        # difference between periods as separators and periods that are part of the
        # name itself.
        names(options) <- gsub("([\\.])", "\\\\\\1", names(options))
        nms <- names(options)
        if (length(nms) != n || any(nms == ''))
            stop("'options' must be a fully named list, or have no names (NULL)")
        lapply(options, shouldEval)
    } else {
        is.character(options) && inherits(options, 'JS_EVAL')
    }
}
# JSEvals(list(list(foo.bar=JS("hi"), baz.qux="bye"))) == "0.foo\\.bar"

#' Execute JavaScript code after static render
#'
#' Convenience function for wrapping a JavaScript code string with a
#' \code{<script>} tag and the boilerplate necessary to delay the execution of
#' the code until after the next time htmlwidgets completes rendering any
#' widgets that are in the page. This mechanism is designed for running code to
#' customize widget instances, which can't be done at page load time since the
#' widget instances will not have been created yet.
#'
#' Each call to \code{onStaticRenderComplete} will result in at most one
#' invocation of the given code. In some edge cases in Shiny, it's possible for
#' static rendering to happen more than once (e.g. a \code{renderUI} that
#' contains static HTML widgets). \code{onStaticRenderComplete} calls only
#' schedule execution for the next static render operation.
#'
#' The pure JavaScript equivalent of \code{onStaticRenderComplete} is
#' \code{HTMLWidgets.addPostRenderHandler(callback)}, where \code{callback} is a
#' JavaScript function that takes no arguments.
#'
#' @param jsCode A character vector containing JavaScript code. No R error will
#'   be raised if the code is invalid, not even on JavaScript syntax errors.
#'   However, the web browser will throw errors at runtime.
#' @return An htmltools \code{\link[htmltools]{tags}$script} object.
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(htmltools)
#' library(htmlwidgets)
#'
#' page <- tagList(
#'   leaflet() %>% addTiles(),
#'   onStaticRenderComplete(
#'     "HTMLWidgets.find('.leaflet').setZoom(4);"
#'   )
#' )
#' print(page, browse = TRUE)
#' }
#'
#' @export
onStaticRenderComplete <- function(jsCode) {
    tags$script(
        "HTMLWidgets.addPostRenderHandler(function() {",
        HTML(paste0(jsCode, collapse = "\n")),
        "});"
    )
}

#' Create a widget sizing policy
#'
#' Define the policy by which HTML widgets will be sized in various containers
#' (e.g. Browser, RStudio Viewer, R Markdown, Shiny). Note that typically
#' widgets can accept the default sizing policy (or override only one or two
#' aspects of it) and get satisfactory sizing behavior via the automatic sizing
#' logic built into the htmlwidgets framework (see the notes below for the most
#' typical exceptions to this).
#'
#' @param defaultWidth The default width used to display the widget. This
#'   parameter specifies the default width for viewing in all contexts (browser,
#'   viewer, and knitr) unless it is specifically overridden with e.g.
#'   \code{browser.defaultWidth}.
#' @param viewer.defaultWidth The default width used to display the widget
#'   within the RStudio Viewer.
#' @param browser.defaultWidth The default width used to display the widget
#'   within a standalone web browser.
#' @param knitr.defaultWidth The default width used to display the widget within
#'   documents generated by knitr (e.g. R Markdown).
#' @param defaultHeight The default height used to display the widget. This
#'   parameter specifies the default height for viewing in all contexts
#'   (browser, viewer, and knitr) unless it is specifically overridden with e.g.
#'   \code{browser.defaultHeight}.
#' @param viewer.defaultHeight The default height used to display the widget
#'   within the RStudio Viewer.
#' @param browser.defaultHeight The default height used to display the widget
#'   within a standalone web browser.
#' @param knitr.defaultHeight The default height used to display the widget
#'   within documents generated by knitr (e.g. R Markdown).
#' @param padding Padding around the widget (in pixels). This parameter
#'   specifies the padding for viewing in all contexts (browser and viewer)
#'   unless it is specifically overriden by e.g. \code{browser.padding}.
#' @param browser.padding Padding around the widget when displayed in a
#'   standalone browser (defaults to 40 pixels).
#' @param viewer.padding Padding around the widget when displayed in the RStudio
#'   Viewer (defaults to 15 pixels).
#' @param viewer.fill When displayed in the RStudio Viewer, automatically size
#'   the widget to the viewer dimensions (note that \code{viewer.padding} is
#'   still applied). Default to \code{TRUE}.
#' @param browser.fill When displayed in a standalone web browser, automatically
#'   size the widget to the browser dimensions (note that \code{browser.padding}
#'   is still applied). Defaults to \code{FALSE}.
#' @param browser.external When displaying in a browser, always use an external
#'   browser (via \code{browseURL()}). Defaults to \code{FALSE}, which will
#'   result in the use of an internal browser within RStudio v1.1 and higher.
#' @param viewer.paneHeight Request that the RStudio Viewer be forced to a
#'   specific height when displaying this widget.
#' @param viewer.suppress Never display the widget within the RStudio Viewer
#'   (useful for widgets that require a large amount of space for rendering).
#'   Defaults to \code{FALSE}.
#' @param knitr.figure Apply the default knitr fig.width and fig.height to the
#'   widget when it's rendered within R Markdown documents. Defaults to
#'   \code{TRUE}.
#'
#' @return A widget sizing policy
#'
#' @details
#'
#' The default HTML widget sizing policy treats the widget with the same sizing
#' semantics as an R plot. When printed at the R console the widget is displayed
#' within the RStudio Viewer and sized to fill the Viewer pane (modulo any
#' padding). When rendered inside an R Markdown document the widget is sized
#' based on the default size of figures in the document.
#'
#' You might need to change the default behavior if your widget is extremely
#' large. In this case you might specify \code{viewer.suppress = TRUE} and
#' \code{knitr.figure = FALSE} as well provide for a larger default width and
#' height for knitr.
#'
#' You also might need to change the default behavior if you widget already
#' incorporates padding. In this case you might specify \code{viewer.padding =
#' 0}.
#'
#' For additional details on widget sizing:
#'
#' \code{vignette("develop_sizing", package = "htmlwidgets")}
#'
#'
#' @export
sizingPolicy <- function(
    defaultWidth = NULL, defaultHeight = NULL, padding = NULL,
    viewer.defaultWidth = NULL, viewer.defaultHeight = NULL,
    viewer.padding = NULL, viewer.fill = TRUE, viewer.suppress = FALSE,
    viewer.paneHeight = NULL,
    browser.defaultWidth = NULL, browser.defaultHeight = NULL,
    browser.padding = NULL, browser.fill = FALSE, browser.external = FALSE,
    knitr.defaultWidth = NULL, knitr.defaultHeight = NULL,
    knitr.figure = TRUE) {
    
    list(
        defaultWidth = defaultWidth,
        defaultHeight = defaultHeight,
        padding = padding,
        viewer = list(
            defaultWidth = viewer.defaultWidth,
            defaultHeight = viewer.defaultHeight,
            padding = viewer.padding,
            fill = viewer.fill,
            suppress = viewer.suppress,
            paneHeight = viewer.paneHeight
        ),
        browser = list(
            defaultWidth = browser.defaultWidth,
            defaultHeight = browser.defaultHeight,
            padding = browser.padding,
            fill = browser.fill,
            external = browser.external
        ),
        knitr = list(
            defaultWidth = knitr.defaultWidth,
            defaultHeight = knitr.defaultHeight,
            figure = knitr.figure
        )
    )
}


DEFAULT_WIDTH <- 960
DEFAULT_HEIGHT <- 500
DEFAULT_PADDING <- 40
DEFAULT_WIDTH_VIEWER <- 450
DEFAULT_HEIGHT_VIEWER <- 350
DEFAULT_PADDING_VIEWER <- 15

#' Resolve widget sizing policy
#'
#' Take a widget object and sizing policy, and some other contextual details,
#' and figure out what width/height to use, if possible. Some decisions may need
#' to be deferred until runtime; include any metadata that's needed for that
#' decision in the result as well.
#'
#' @param x The widget object whose size is to be determined. It may have $width
#'   and $height directly on it, which means we should obey those.
#' @param sp The sizing policy to use.
#' @param standalone Logical value indicating whether the widget is being
#'   rendered in a standalone context (where it's the only thing on the page;
#'   this is usually via `print.htmlwidget()`).
#' @param knitrOptions Object representing the knitr options passed to us via
#'   `knit_print`. If we're not doing a `knit_print` right now, then the value
#'   should be `NULL`.
#' @return A list that is guaranteed to have `width` and `height` values, each of
#'   which is either a number or CSS unit string. If `standalone=TRUE` then the
#'   list will also have a `runtime` value that is a list, that contains two
#'   nested lists `viewer` and `browser`. Each of those in turn has `width`,
#'   `height`, `padding` (between 1 and 4 numbers), and `fill` (`TRUE`/`FALSE`).
#' @keywords internal
#' @examples
#' x <- list(
#'   sizingPolicy = list(
#'     defaultWidth = 800,
#'     defaultHeight = 500,
#'     padding = 15,
#'     viewer = list(
#'       fill = TRUE,
#'       padding = 0
#'     ),
#'     browser = list(
#'       fill = FALSE,
#'       defaultWidth = 960,
#'       defaultHeight = 600,
#'       padding = 20
#'     ),
#'     knitr = list(
#'       # Actually knitr$defaultWidth and knitr$defaultHeight
#'       # are ignored if figure = TRUE
#'       defaultWidth = 800,
#'       defaultHeight = 600,
#'       figure = TRUE
#'     )
#'   )
#' )
#'
#' # Sizing for standalone mode
#' str(resolveSizing(x, x$sizingPolicy, TRUE, NULL))
#' # Sizing for knitr
#' str(resolveSizing(x, x$sizingPolicy, FALSE,
#'   list(out.width.px = 150, out.height.px = 100)))
#'
#' # Explicit width/height provided by user--overrides any
#' # default width/height
#' x$width <- 300
#' x$height <- 250
#' str(resolveSizing(x, x$sizingPolicy, FALSE,
#'   list(out.width.px = 150, out.height.px = 100)))
#' @keywords internal
#' @noRd
resolveSizing <- function(x, sp, standalone, knitrOptions = NULL) {
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

setWidgetIdSeed <- function(seed, kind = NULL, normal.kind = NULL) {
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
        .globals$idSeed <- .GlobalEnv$.Random.seed
        if (!is.null(sysSeed))
            .GlobalEnv$.Random.seed <- sysSeed
        else
            rm(".Random.seed", envir = .GlobalEnv)
    })
    set.seed(seed, kind = kind, normal.kind = normal.kind)
}

# create a new unique widget id
createWidgetId <- function(bytes = 10) {
    
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

.globals <- new.env(parent = emptyenv())

#' @export
print.htmlwidget <- function(x, ..., view = interactive()) {
    
    # if we have a viewer then forward viewer pane height (if any)
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
        viewerFunc <- function(url) {
            
            # get the requested pane height (it defaults to NULL)
            paneHeight <- x$sizingPolicy$viewer$paneHeight
            
            # convert maximize to -1 for compatibility with older versions of rstudio
            # (newer versions convert 'maximize' to -1 interally, older versions
            # will simply ignore the height if it's less than zero)
            if (identical(paneHeight, "maximize"))
                paneHeight <- -1
            
            # call the viewer
            viewer(url, height = paneHeight)
        }
    } else {
        viewerFunc <- utils::browseURL
    }
    
    # call html_print with the viewer
    html_print(htmltools::as.tags(x, standalone=TRUE), viewer = if (view) viewerFunc)
    
    # return value
    invisible(x)
}

#' @export
print.suppress_viewer <- function(x, ..., view = interactive()) {
    
    viewer <- if (view) {
        if (isTRUE(x$sizingPolicy$browser$external)) {
            browseURL
        } else if (!is.null(getOption("page_viewer"))) {
            function(url) getOption("page_viewer")(url)
        } else {
            browseURL
        }
    } else {
        NULL
    }
    
    html_print(htmltools::as.tags(x, standalone=TRUE), viewer = viewer)
    invisible(x)
}

#' @method as.tags htmlwidget
#' @export
as.tags.htmlwidget <- function(x, standalone = FALSE) {
    toHTML(x, standalone = standalone)
}

#' Prepend/append extra HTML content to a widget
#'
#' Use these functions to attach extra HTML content (primarily JavaScript and/or
#' CSS styles) to a widget, for rendering in standalone mode (i.e. printing at
#' the R console) or in a knitr document. These functions are NOT supported when
#' running in a Shiny widget rendering function, and will result in a warning if
#' used in that context. Multiple calls are allowed, and later calls do not undo
#' the effects of previous calls.
#'
#' @param x An HTML Widget object
#' @param ... Valid \link[htmltools]{tags}, text, and/or
#'   \code{\link[htmltools]{HTML}}, or lists thereof.
#' @return A modified HTML Widget object.
#'
#' @export
prependContent <- function(x, ...) {
    x$prepend <- c(x$prepend, list(...))
    x
}

#' @rdname prependContent
#' @export
appendContent <- function(x, ...) {
    x$append <- c(x$append, list(...))
    x
}

#' Execute custom JavaScript code after rendering
#'
#' Use this function to supplement the widget's built-in JavaScript rendering
#' logic with additional custom JavaScript code, just for this specific widget
#' object.
#'
#' @param x An HTML Widget object
#' @param jsCode Character vector containing JavaScript code (see Details)
#' @param data An additional argument to pass to the \code{jsCode} function.
#'   This can be any R object that can be serialized to JSON. If you have
#'   multiple objects to pass to the function, use a named list.
#' @return The modified widget object
#'
#' @details The \code{jsCode} parameter must contain valid JavaScript code which
#'   when evaluated returns a function.
#'
#'   The function will be invoked with three arguments: the first is the widget's
#'   main HTML element, and the second is the data to be rendered (the \code{x}
#'   parameter in \code{createWidget}). The third argument is the JavaScript
#'   equivalent of the R object passed into \code{onRender} as the \code{data}
#'   argument; this is an easy way to transfer e.g. data frames without having
#'   to manually do the JSON encoding.
#'
#'   When the function is invoked, the \code{this} keyword will refer to the
#'   widget instance object.
#'
#' @seealso \code{\link{onStaticRenderComplete}}, for writing custom JavaScript
#'   that involves multiple widgets.
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#'
#' # This example uses browser geolocation. RStudio users:
#' # this won't work in the Viewer pane; try popping it
#' # out into your system web browser.
#' leaflet() %>% addTiles() %>%
#'   onRender("
#'     function(el, x) {
#'       // Navigate the map to the user's location
#'       this.locate({setView: true});
#'     }
#'   ")
#'
#'
#' # This example shows how you can make an R data frame available
#' # to your JavaScript code.
#'
#' meh <- "&#x1F610;";
#' yikes <- "&#x1F628;";
#'
#' df <- data.frame(
#'   lng = quakes$long,
#'   lat = quakes$lat,
#'   html = ifelse(quakes$mag < 5.5, meh, yikes),
#'   stringsAsFactors = FALSE
#' )
#'
#' leaflet() %>% addTiles() %>%
#'   fitBounds(min(df$lng), min(df$lat), max(df$lng), max(df$lat)) %>%
#'   onRender("
#'     function(el, x, data) {
#'       for (var i = 0; i < data.lng.length; i++) {
#'         var icon = L.divIcon({className: '', html: data.html[i]});
#'         L.marker([data.lat[i], data.lng[i]], {icon: icon}).addTo(this);
#'       }
#'     }
#'   ", data = df)
#' }
#'
#' @export
onRender <- function(x, jsCode, data = NULL) {
    addHook(x, "render", jsCode, data)
}

addHook <- function(x, hookName, jsCode, data = NULL) {
    if (length(jsCode) == 0)
        return(x)
    
    if (length(jsCode) > 1)
        jsCode <- paste(jsCode, collapse = "\n")
    
    x$jsHooks[[hookName]] <- c(x$jsHooks[[hookName]], list(list(code = jsCode, data = data)))
    x
}


toHTML <- function(x, standalone = FALSE, knitrOptions = NULL) {
    
    sizeInfo <- resolveSizing(x, x$sizingPolicy, standalone = standalone, knitrOptions = knitrOptions)
    
    if (!is.null(x$elementId))
        id <- x$elementId
    else
        id <- paste("htmlwidget", createWidgetId(), sep="-")
    
    w <- validateCssUnit(sizeInfo$width)
    h <- validateCssUnit(sizeInfo$height)
    
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

widget_html.default <- function (name, package, id, style, class, inline = FALSE, ...) {
    if (inline) {
        tags$span(id = id, style = style, class = class)
    } else {
        tags$div(id = id, style = style, class = class)
    }
}

## These functions are to support unit tests #######################

widgetA_html <- function(name, package, id, style, class, inline = FALSE, ...) {
    tags$canvas(id = id, class = class, style = style)
}

widgetB_html <- function(name, package, id, style, class, inline = FALSE, ...) {
    # Return a non-HTML result
    TRUE
}

widgetC_html <- function(name, package, id, style, class, inline = FALSE, ...) {
    tags$strong(id = id, class = class, style = style)
}

widget_html.widgetC <- function(name, package, id, style, class, inline = FALSE, ...) {
    tags$em(id = id, class = class, style = style)
}

widget_html.widgetD <- function(name, package, id, style, class, inline = FALSE, ...) {
    TRUE
}

widgetE_html <- function(name, package, id, style, class, inline = FALSE, ...) {
    tagList(tags$div(id = id, style = style, class = class))
}

widgetF_html <- function(name, package, id, style, class, inline = FALSE, ...) {
    HTML(as.character(tags$div(id = id, style = style, class = class)))
}

## End unit test support functions #################################

widget_dependencies <- function(name, package){
    getDependency(name, package)
}

# Generates a <script type="application/json"> tag with the JSON-encoded data,
# to be picked up by htmlwidgets.js for static rendering.
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

#' Create an HTML Widget
#'
#' Create an HTML widget based on widget YAML and JavaScript contained within
#' the specified package.
#'
#' For additional details on developing widgets, see package vignettes:
#' \code{vignette("develop_intro", package = "htmlwidgets")}.
#'
#' @param name Widget name (should match the base name of the YAML and
#'   JavaScript files used to implement the widget)
#' @param x Widget instance data (underlying data to render and options that
#'   govern how it's rendered). This value will be converted to JSON using
#'   \code{\link[jsonlite]{toJSON}} and made available to the widget's
#'   JavaScript \code{renderValue} function.
#' @param width Fixed width for widget (in css units). The default is
#'   \code{NULL}, which results in intelligent automatic sizing based on the
#'   widget's container.
#' @param height Fixed height for widget (in css units). The default is
#'   \code{NULL}, which results in intelligent automatic sizing based on the
#'   widget's container.
#' @param sizingPolicy Options that govern how the widget is sized in various
#'   containers (e.g. a standalone browser, the RStudio Viewer, a knitr figure,
#'   or a Shiny output binding). These options can be specified by calling the
#'   \code{\link{sizingPolicy}} function.
#' @param package Package where the widget is defined (defaults to the widget
#'   name).
#' @param dependencies Additional widget HTML dependencies (over and above those
#'   defined in the widget YAML). This is useful for dynamic dependencies that
#'   only exist when selected widget options are enabled (e.g. sets of map tiles
#'   or projections).
#' @param elementId Use an explicit element ID for the widget (rather than an
#'   automatically generated one). Useful if you have other JavaScript that
#'   needs to explicitly discover and interact with a specific widget instance.
#' @param preRenderHook A function to be run on the widget, just prior to
#'   rendering. It accepts the entire widget object as input, and should return
#'   a modified widget object.
#'
#' @return An object of class \code{htmlwidget} that will intelligently print
#'   itself into HTML in a variety of contexts including the R console, within R
#'   Markdown documents, and within Shiny output bindings.
#' @export
createWidget <- function(name,
                         x,
                         width = NULL,
                         height = NULL,
                         sizingPolicy = htmlwidgets::sizingPolicy(),
                         package = name,
                         dependencies = NULL,
                         elementId = NULL,
                         preRenderHook = NULL) {
    # Turn single dependency object into list of dependencies, if necessary
    if (inherits(dependencies, "html_dependency"))
        dependencies <- list(dependencies)
    structure(
        list(x = x,
             width = width,
             height = height,
             sizingPolicy = sizingPolicy,
             dependencies = dependencies,
             elementId = elementId,
             preRenderHook = preRenderHook,
             jsHooks = list()),
        class = c(name,
                  if (sizingPolicy$viewer$suppress) "suppress_viewer",
                  "htmlwidget"),
        package = package
    )
}


#' Shiny bindings for HTML widgets
#'
#' Helpers to create output and render functions for using HTML widgets within
#' Shiny applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param name Name of widget to create output binding for
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param package Package containing widget (defaults to \code{name})
#' @param inline use an inline (\code{span()}) or block container (\code{div()})
#' for the output
#' @param outputFunction Shiny output function corresponding to this render
#'   function.
#' @param reportSize Should the widget's container size be reported in the
#'   shiny session's client data?
#' @param reportTheme Should the widget's container styles (e.g., colors and fonts)
#' be reported in the shiny session's client data?
#' @param expr An expression that generates an HTML widget (or a
#'   \href{https://rstudio.github.io/promises/}{promise} of an HTML widget).
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param cacheHint Extra information to use for optional caching using
#'   \code{shiny::bindCache()}.
#'
#' @return An output or render function that enables the use of the widget
#'   within Shiny applications.
#'
#' @details These functions are delegated to from within your widgets own shiny
#'   output and render functions. The delegation is boilerplate and always works
#'   the same for all widgets (see example below).
#'
#' @examples
#' # shiny output binding for a widget named 'foo'
#' fooOutput <- function(outputId, width = "100%", height = "400px") {
#'   htmlwidgets::shinyWidgetOutput(outputId, "foo", width, height)
#' }
#'
#' # shiny render function for a widget named 'foo'
#' renderFoo <- function(expr, env = parent.frame(), quoted = FALSE) {
#'   if (!quoted) { expr <- substitute(expr) } # force quoted
#'   htmlwidgets::shinyRenderWidget(expr, fooOutput, env, quoted = TRUE)
#' }
#' @name htmlwidgets-shiny
#'
#' @export
shinyWidgetOutput <- function(outputId, name, width, height, package = name,
                              inline = FALSE, reportSize = FALSE, reportTheme = FALSE) {
    
    checkShinyVersion()
    
    # Theme reporting requires this shiny feature
    # https://github.com/rstudio/shiny/pull/2740/files
    if (reportTheme &&
        nzchar(system.file(package = "shiny")) &&
        packageVersion("shiny") < "1.4.0.9003") {
        message("`reportTheme = TRUE` requires shiny v.1.4.0.9003 or higher. Consider upgrading shiny.")
    }
    
    # generate html
    html <- htmltools::tagList(
        widget_html(
            name, package, id = outputId,
            class = paste0(
                name, " html-widget html-widget-output",
                if (reportSize) " shiny-report-size",
                if (reportTheme) " shiny-report-theme"
            ),
            style = sprintf("width:%s; height:%s; %s",
                            htmltools::validateCssUnit(width),
                            htmltools::validateCssUnit(height),
                            if (inline) "display: inline-block;" else ""
            ), width = width, height = height
        )
    )
    
    # attach dependencies
    dependencies = widget_dependencies(name, package)
    htmltools::attachDependencies(html, dependencies)
}


#' @rdname htmlwidgets-shiny
#' @export
shinyRenderWidget <- function(expr, outputFunction, env, quoted, cacheHint = "auto")  {
    checkShinyVersion()
    # generate a function for the expression
    shiny::installExprFunction(expr, "func", env, quoted)
    
    renderWidget <- function(instance) {
        if (!is.null(instance$elementId)) {
            warning("Ignoring explicitly provided widget ID \"",
                    instance$elementId, "\"; Shiny doesn't use them"
            )
        }
        
        # We don't support prependContent/appendContent in dynamic Shiny contexts
        # because the Shiny equivalent of onStaticRenderComplete is unclear. If we
        # ever figure that out it would be great to support it. One possibility
        # would be to have a dedicated property for "post-render customization JS",
        # I suppose. In any case, it's less of a big deal for Shiny since there are
        # other mechanisms (that are at least as natural) for putting custom JS in a
        # Shiny app.
        if (!is.null(instance$prepend)) {
            warning("Ignoring prepended content; prependContent can't be used in a ",
                    "Shiny render call")
        }
        if (!is.null(instance$append)) {
            warning("Ignoring appended content; appendContent can't be used in a ",
                    "Shiny render call")
        }
        
        deps <- .subset2(instance, "dependencies")
        deps_payload <- lapply(
            htmltools::resolveDependencies(deps),
            shiny::createWebDependency
        )
        payload <- c(createPayload(instance), list(deps = deps_payload))
        payload <- toJSON(payload)
        attr(payload, "deps") <- deps
        payload
    }
    
    # The cacheHint and cacheReadHook args were added in Shiny 1.6.0.
    if (all(c("cacheHint", "cacheReadHook") %in% names(formals(shiny::createRenderFunction)))) {
        shiny::createRenderFunction(
            func,
            function(instance, session, name, ...) {
                renderWidget(instance)
            },
            outputFunction,
            NULL,
            cacheHint = cacheHint,
            cacheReadHook = function(value) {
                # If we've pulled the value from the cache and we're in a different R
                # process from the one that created it, we'll need to register the
                # dependencies again.
                deps <- attr(value, "deps")
                lapply(
                    htmltools::resolveDependencies(deps),
                    shiny::createWebDependency
                )
                value
            }
        )
    } else {
        shiny::createRenderFunction(
            func,
            function(instance, session, name, ...) {
                renderWidget(instance)
            },
            outputFunction,
            NULL
        )
    }
    
}

# For the magic behind shiny::installExprFunction()
utils::globalVariables("func")

checkShinyVersion <- function(error = TRUE) {
    x <- utils::packageDescription('htmlwidgets', fields = 'Enhances')
    r <- '^.*?shiny \\(>= ([0-9.]+)\\).*$'
    if (is.na(x) || length(grep(r, x)) == 0 || system.file(package = 'shiny') == '')
        return()
    v <- gsub(r, '\\1', x)
    f <- if (error) stop else packageStartupMessage
    if (utils::packageVersion('shiny') < v)
        f("Please upgrade the 'shiny' package to (at least) version ", v)
}

# Helper function to create payload
createPayload <- function(instance){
    if (!is.null(instance$preRenderHook)){
        instance <- instance$preRenderHook(instance)
        instance$preRenderHook <- NULL
    }
    x <- .subset2(instance, "x")
    list(x = x, evals = JSEvals(x), jsHooks = instance$jsHooks)
}

# package globals
.globals <- new.env(parent = emptyenv())
