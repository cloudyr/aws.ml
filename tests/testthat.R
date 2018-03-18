library("testthat")
library("aws.ml")

if (Sys.getenv("AWS_ACCESS_KEY_ID") != "") {
    test_check("aws.ml", filter = "authenticated")
}

test_check("aws.ml", filter = "public")
