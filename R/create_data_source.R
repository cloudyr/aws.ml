#' @rdname data_source
#' @title Create Data Source(s)
#' @description Create a data source for an AWS ML Model
#' @param id A character string specifying an identifier for the model
#' @param specification A list.
#' @param name Optionally, a character string specifying a name for the data source
#' @param compute_statistics A logical. This should be TRUE.
#' @param iam_role If \code{specification} is for an RDS or Redshift source, an IAM role ARN.
#' @param \dots
create_data_source <-
function(
  id,
  specification,
  name = NULL,
  compute_statistics = TRUE,
  iam_role = NULL,
  ...
) {
    bod <- list()
    bod$DataSourceId <- id
    if (!is.null(name)) {
        bod$DataSourceName <- name
    }
    bod$DataSpec <- specification
    bod$ComputeStatistics <- compute_statistics
    if (!is.null(iam_role)) {
        bod$RoleARN <- iam_role
    }
    
    out <- mlHTTP(body = bod, target = "AmazonML_20141212.CreateDataSourceFromS3", ...)
    out
}

#' @rdname data_source
#' @export
delete_data_source <-
function(
  id,
  ...
) {
    bod <- list(DataSourceId = id)
    out <- mlHTTP(body = bod, target = "AmazonML_20141212.DeleteDataSource", ...)
    out
}

#' @rdname data_source
#' @importFrom jsonlite toJSON
#' @export
spec_s3 <-
function(
  data,
  schema,
  rearrangement
) {
    out <- list()
    out$DataLocationS3 <- data
    if (is.list(schema)) {
        out$DataSchema <- jsonlite::toJSON(schema)
    } else {
        out$DataSchemaLocationS3 <- schema
    }
    structure(out)
}

#' @rdname data_source
#' @export
spec_rds <-
function(
  data,
  schema,
  rearrangement
) {
    out <- list()
    structure(out)
}

#' @rdname data_source
#' @export
spec_redshift <-
function(
  data,
  schema,
  rearrangement
) {
    out <- list()
    structure(out)
}
