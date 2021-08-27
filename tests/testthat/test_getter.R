context("Getter")

data("economy_texts")
data("economy_dates")

roll_proto = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                        init = 20, K = 5, n = 10)

test_that("bla", {

  getNames(reverse = TRUE)

})
