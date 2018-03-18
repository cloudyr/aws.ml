get_modelid <- function(x) {
    UseMethod("get_modelid")
}

get_modelid.default <- function(x) {
    x
}

get_modelid.character <- function(x) {
    x
}

get_modelid.aws_ml_model <- function(x) {
    x$MLModelId
}
