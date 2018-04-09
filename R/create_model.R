#' @rdname model
#' @title Create and Delete ML Models
#' @description Create or Delete an Amazon ML Model
#' @param id A character string specifying an identifier for the model, or an object of class \dQuote{aws_ml_model}.
#' @param data A data source object.
#' @param type A character string specifying the type of model to train.
#' @param parameters A list of parameters to use when training the model.
#' @param recipe A list
#' @param include_recipe A logical specifying whether to include the feature transformation recipe.
#' @param name Optionally, a \dQuote{name} for the model.
#' @param \dots
#' @export
create_model <-
function(
  id,
  data,
  type = c("REGRESSION", "BINARY", "MULTICLASS"),
  parameters = NULL,
  recipe = NULL,
  name = NULL,
  ...
) {
    bod <- list()
    bod$MLModelId <- id
    bod$TrainingDataSourceId <- data
    if (!is.null(name)) {
        bod$MLModelName <- name
    }
    bod$MLModelType <- match.arg(toupper(type))
    if (!is.null(recipe)) {
        bod$Recipe <- recipe
    }
    
    out <- mlHTTP(body = bod, target = "CreateMLModel", ...)
    out
}

#' @rdname model
#' @export
get_model <-
function(
  id,
  include_recipe = FALSE,
  ...
) {
    bod <- list()
    bod$MLModelId <- id
    bod$Verbose <- include_recipe
    out <- mlHTTP(body = bod, target = "GetMLModel", ...)
    out
}

#' @rdname model
#' @export
delete_model <-
function(
  id,
  ...
) {
    bod <- list()
    bod$MLModelId <- id
    out <- mlHTTP(body = bod, target = "DeleteMLModel", ...)
    out
}

