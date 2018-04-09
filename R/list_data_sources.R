#' @title List Data Sources
#' @description Return a list of AWS data sources
#' @param n An integer specifying the maximum number of results to return.
#' @param token A character string specifying a pagination token.
#' @references \urL{https://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeDataSources.html}
#' @export
list_data_sources <-
function(
  n = 100,
  token = NULL,
  ...
) {
    bod <- list()
    stopifnot(n >= 1 & n <= 100)
    bod$Limit <- n
    if (!is.null(token)) {
        bod$NextToken <- token
    }
    out <- mlHTTP(body = bod, action = "DescribeDataSources", ...)
    out$Results
}
