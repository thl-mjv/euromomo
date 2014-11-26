context("ISO week WOY functionality")

dates <- c("2014-W12","2012-W11")
test_that("Simple two vector", 
          expect_that(ISOwoy(dates), equals(c(12,11))))

dates <- seq(as.Date("2014-01-01"),as.Date("2014-12-31"),by="1 day")
week <- ISOweek::ISOweek(dates)
test_that("For a range using substr on pos. 7",
        expect_that(ISOwoy(week), equals(as.numeric(substring(week,7)))))

test_that("For year",
          expect_that(ISOyear(week), equals(as.numeric(substr(week,1,4)))))

#ISO doesn't work for 5 digit years. :-()
# dates <- seq(as.Date("20014-01-01"),as.Date("20014-12-31"),by="1 day")
# week <- ISOweek::ISOweek(dates)
# test_that("For a range using substr on pos. 8",
#           expect_that(ISOwoy(week), equals(as.numeric(substring(week,7)))))

