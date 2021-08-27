context("as.RollingLDA and Getter")

data("economy_texts")
data("economy_dates")

seed = Sys.Date()
set.seed(seed)
roll_proto = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                        init = 20, K = 5, n = 10)

test_that("getter: inverse", {
  expect_identical(
    getDates(roll_proto, "noID", inverse = TRUE),
    getDates(roll_proto, getNames(roll_proto))
  )

  expect_identical(
    unname(getDates(roll_proto, getNames(roll_proto), inverse = TRUE)),
    Date()
  )

  expect_identical(
    unname(getDocs(roll_proto, getNames(roll_proto), inverse = TRUE)),
    list()
  )
})

test_that("as.RollingLDA", {
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, id = getID(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, lda = getLDA(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, docs = getDocs(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, dates = getDates(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, vocab = getVocab(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, chunks = getChunks(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, param = getParam(roll_proto))
  )
  expect_identical(
    roll_proto,
    as.RollingLDA(roll_proto, dates = unname(getDates(roll_proto)))
  )

  expect_error(
    as.RollingLDA(roll_proto, dates = getDates(roll_proto, sample(getNames(roll_proto), 10))),
    "Assertion on 'names' failed"
  )
  expect_error(
    as.RollingLDA(roll_proto, docs = getDocs(roll_proto, sample(getNames(roll_proto), 10))),
    "Assertion on 'names' failed"
  )
  expect_error(
    as.RollingLDA(roll_proto, param = "abc"),
    "input arguments do not create a RollingLDA object"
  )
  expect_error(
    as.RollingLDA(getLDA(roll_proto)),
    "not a RollingLDA object"
  )
})
