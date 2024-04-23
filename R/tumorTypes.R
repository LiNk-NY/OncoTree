#' @export
nameLookup <- function(otname) {
    ot <- OncoTree()
    anames <- allNames()
    if (!otname %in% anames$name)
        stop("Name not found in OncoTree")
    anames[anames$name == otname, ]
}

#' @export
tumorTypes <- function() {
    ot <- OncoTree()
    res <- ot$tumorTypesGetUsingGET()
    res <- httr::content(res)
    externalReferences <- dplyr::bind_rows(
        lapply(res, function(x) .flat_ext_refs(x[["externalReferences"]]))
    )
    precursors <- lapply(res, function(x) {
        if (!length(x[["precursors"]]))
            NA_character_
        else
            unlist(x[["precursors"]], recursive = FALSE)
    })
    res <- lapply(
        res,
        function(x)
            x[-which(names(x) %in% c("externalReferences", "precursors"))]
    )
    dplyr::bind_rows(
        lapply(res, function(x) {
            Filter(length, x)
        })
    ) |> dplyr::bind_cols(
        externalReferences, tibble(precursors)
    )
}

.EXT_REF_SENTINEL <- c(UMLS = NA_character_, NCI = NA_character_)

.flat_ext_refs <- function(extref) {
    if (!length(extref))
        .EXT_REF_SENTINEL
    else
        unlist(extref)
}

#' @export
searchTypeQuery <- function(
    type = c("code", "name", "mainType", "level", "nci", "umls", "color"),
    query
) {
    ot <- OncoTree()
    res <- ot$tumorTypesSearchTypeQueryQueryGetUsingGET(
        type = match.arg(type),
        query = query
    )
    res <- httr::content(res)
    externalReferences <- dplyr::bind_rows(
        lapply(res, function(x) .flat_ext_refs(x[["externalReferences"]]))
    )

    res <- lapply(
        res,
        function(x) x[-which(names(x) == "externalReferences")]
    )
    dplyr::bind_rows(
        lapply(res, function(x) {
            Filter(length, x)
        })
    ) |> dplyr::bind_cols(
        externalReferences
    )
}
