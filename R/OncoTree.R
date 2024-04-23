#' @export
OncoTree <- function(
    hostname = "oncotree.mskcc.org",
    protocol = "https",
    api. = "/api-docs",
    token = character()
) {
    if (length(token))
        token <- .handle_token(token)

    apiUrl <- paste0(protocol, "://", hostname, api.)
    .OncoTree(
        Service(
            service = "OncoTree",
            host = hostname,
            config = httr::config(
                ssl_verifypeer = 0L, ssl_verifyhost = 0L, http_version = 0L
            ),
            authenticate = FALSE,
            api_reference_url = apiUrl,
            api_reference_md5sum = "4435cd382d3e7f41c8fad68288e53cc8",
            api_reference_headers = token,
            package = "OncoTree",
            schemes = protocol
        ),
        api_header = token
    )
}
