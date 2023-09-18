## For now, you need a recent enough svn checkout
## and have to compile main.c with -DUSE_BROWSER_HOOK
stopifnot(as.integer(R.version$'svn rev') >= 85116)

bhook <- function(hook, condition, envir) {
    ## cat("Entering Browser Hook. ")
    op <- options(browser.hook = bhook)
    repeat {
        ## cat("Starting Browser REPL\n")
        success <- FALSE
        tryCatch({
            val <- browser("", condition, envir, 0L, ignoreHook = TRUE)
            success <- TRUE
            return(val)
        }, finally = if (! askBrowserExit(success)) next)
    }
}

askBrowserExit <- function(success) {
    if (success)
        TRUE
    else {
        mtitle <- "Enter a selection number:"
        options <- c("Leave the browser with a jump.",
                     "Stay in the browser session.")
        choice <- menu(options, title = mtitle)
        if (choice == 2) FALSE else TRUE
    }
}

options(browser.hook = bhook)
options(browser.error.handler = function(e) NULL)
options(error = NULL)

restartName <- function(r) r[[1]] ## **** need to add this to base

## These top-level handlers should deal with
##
## - warnings
## - traceback
##
## Ideally we would factor out and make available the current C code
## for that

errhandler <- function(e) {
    msg <- conditionMessage(e)
    nf <- sys.nframe()
    call <- if (nf < 3) NULL else sys.call(-3)
    env <- if (nf < 3) .GlobalEnv else sys.frame(-3)
    if (is.null(call))
        cat(sprintf("Error: %s\n", msg))
    else
        cat(sprintf("Error in %s: %s\n", deparse(call, nlines = 1), msg))
    withCallingHandlers({
        browser(condition = e, expr = env)
        invokeRestart(findAbortOrBrowserRestart())
    }, error = errhandler2)
}

errhandler2 <- function(e) {
    msg <- conditionMessage(e)
    cat(sprintf("Error: %s\n", msg))
    r <- findAbortOrBrowserRestart()
    rn <- restartName(r)
    cat(sprintf("Recursive error? Invoking '%s' restart.\n", rn))
    invokeRestart(r)
}

findAbortOrBrowserRestart <- function() {
    goodRestart <- function(x)
        restartName(x) %in% c("abort", "browser")
    Find(goodRestart, computeRestarts())
}

globalCallingHandlers(NULL)
globalCallingHandlers(error = errhandler)
