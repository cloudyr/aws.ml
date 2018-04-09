#' @title List Models
#' @description Return a list of AWS ML Models
#' @param n An integer specifying the maximum number of results to return.
#' @param token A character string specifying a pagination token.
#' @references \urL{https://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeMLModels.html}
#' @export
list_models <-
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
    out <- mlHTTP(body = bod, action = "DescribeMLModels", ...)
    out$Results
}
