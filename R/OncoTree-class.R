.api_header <- function(x) x@api_header

#' @importFrom methods new
.OncoTree <- setClass(
    "OncoTree",
    contains = "Service",
    slots = c(api_header = "character")
)

#' @importFrom AnVIL operations
#' @importFrom methods callNextMethod
#' @exportMethod operations
setMethod(
    "operations", "OncoTree",
    function(x, ..., .deprecated = FALSE)
{
    callNextMethod(
        x, .headers = .api_header(x), ..., .deprecated = .deprecated
    )
})
