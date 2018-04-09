#' @title Execute Amazon ML API Request
#' @description This is the workhorse function to execute calls to the Amazon ML API.
#' @param action A character string specifying an API endpoint.
#' @param query An optional named list containing query string parameters and their character values.
#' @param body A list.
#' @param region A character string containing an AWS region. If missing, the default \dQuote{us-east-1} is used.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string containing an AWS temporary Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}.
#' @return If successful, a named list. Otherwise, a data structure of class \dQuote{aws-error} containing any error message(s) from AWS and information about the request attempt.
#' @details This function constructs and signs an Dynamo DB API request and returns the results thereof, or relevant debugging information in the case of error.
#' @author Thomas J. Leeper
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_xml as_list
#' @importFrom aws.signature signature_v4_auth
#' @export
mlHTTP <- 
function(
  action,
  query = list(),
  body = NULL,
  region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
  key = NULL, 
  secret = NULL, 
  session_token = NULL,
  ...
) {
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("https://machinelearning.",region,".amazonaws.com")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "machinelearning",
           verb = "POST",
           action = "/",
           query_args = query,
           canonical_headers = list(host = paste0("machinelearning.",region,".amazonaws.com"),
                                    `x-amz-date` = d_timestamp,
                                    "X-Amz-Target" = paste0("AmazonML_20141212.", action),
                                    "Content-Type" = "application/x-amz-json-1.1"),
           request_body = if (length(body)) jsonlite::toJSON(body, auto_unbox = TRUE) else "",
           key = key,
           secret = secret,
           session_token = session_token)
    headers <- list(`x-amz-date` = d_timestamp,
                    `x-amz-content-sha256` = Sig$BodyHash,
                    `X-Amz-Target` = paste0("AmazonML_20141212.", action),
                    "Content-Type" = "application/x-amz-json-1.1",
                    Authorization = Sig$SignatureHeader)
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    r <- POST(url, H, body = body, encode = "json", ...)
    cont <- content(r, "text", encoding = "UTF-8")
    if (http_error(r)) {
        warn_for_status(r)
        h <- headers(r)
        out <- try(structure(jsonlite::fromJSON(cont), headers = h, class = "aws_error"))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(cont, simplifyDataFrame = FALSE))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
    }
    return(out)
}
