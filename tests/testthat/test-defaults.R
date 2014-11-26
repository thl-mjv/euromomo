context("Parsing the Defaults file.")

#warning("Current working directory:")
#warning(getwd())
#setwd("../tests/testthat")

#Read the default bogus defaults-example file in the testthat directory.
#parseDefaultsFile(file.path("tests","testthat","defaults-test.txt"))
parseDefaultsFile("defaults-test.txt")
opts <- getOption("euromomo")

# test_that("Missing Important Variables",
#           expect_error(checkOptions(),
#                        "The following variable names are missing: WorkDirectory"))
#opts[["WorkDirectory"]] <- "C:/foobar/foobarbar"
#options(euromomo=opts)

test_that("dStart > dEnd",
          expect_error(checkOptions(),
                       "dStart > dEnd for entries: 2007-W23 : 2007-W03"))

#Modify
opts <- getOption("euromomo")
opts[["except"]][2,2] <- "2008-W03"
options(euromomo=opts)
#checkOptions()

test_that("trend is logical",
          expect_error(checkOptions(),
                      "Attribute \"trend\" of group \"momodefault1\" is not logical"))

opts[["groups"]][["momodefault1"]]["trend"] <- "TRUE"
options(euromomo=opts)

checkOptions()

#Should not make errors.
test_that("trend is logical after fixing problem",
          expect_equal(checkOptions(), TRUE))
          
