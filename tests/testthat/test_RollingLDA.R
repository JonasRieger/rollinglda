context("RollingLDA")

data("economy_texts")
data("economy_dates")

roll_proto = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                        init = 20, K = 5, n = 10)
roll_lda = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                      init = 20, K = 5, type = "lda")

roll_proto2 = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                         init = 100, K = 10, alpha = 1, eta = 1, num.iterations = 20,
                         n = 10, id = "myid", seeds = 1:10)
roll_lda2 = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                       init = 100, K = 10, alpha = 1, eta = 1, num.iterations = 20,
                       id = "myid", type = "lda")

docs = getDocs(roll_proto2, names = getNames(roll_proto2)[1:getChunks(roll_proto2)$n[1]])
vocab = getVocab(roll_proto2)[1:getChunks(roll_proto2)$n.vocab[1]]
dates = getDates(roll_proto2)[1:getChunks(roll_proto2)$n[1]]
proto2 = LDAPrototype(docs, vocab, n = 10, seeds = 1:10,
                      K = 10, alpha = 1, eta = 1, num.iterations = 20)

init_proto = as.RollingLDA(lda = getLDA(proto2), dates = dates, docs = docs, id = "myid")
seed = Sys.time()
set.seed(seed)
init_proto_updated1 = RollingLDA(init_proto,
                                 economy_texts[!names(economy_texts) %in% names(docs)],
                                 economy_dates[!names(economy_dates) %in% names(docs)],
                                 "quarter", "6 month")
set.seed(seed)
init_proto_updated2 = updateRollingLDA(init_proto,
                                       economy_texts[!names(economy_texts) %in% names(docs)],
                                       economy_dates[!names(economy_dates) %in% names(docs)],
                                       "quarter", "6 month")
init_proto_updated3 = updateRollingLDA(init_proto,
                                       economy_texts[!names(economy_texts) %in% names(docs)],
                                       economy_dates[!names(economy_dates) %in% names(docs)],
                                       "quarter", "6 month")
set.seed(Sys.time())

test_that("is.RollingLDA", {
  expect_true(is.RollingLDA(roll_lda))
  expect_true(is.RollingLDA(roll_lda, verbose = TRUE))
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "id")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "lda")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "docs")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "dates")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "vocab")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "chunks")
  expect_message(is.RollingLDA(roll_lda, verbose = TRUE), "param")
  tmp = roll_lda
  ind = sample(getChunks(tmp)$chunk.id, 1)
  tmp$chunks$n[ind+1] = tmp$chunks$n[ind+1] + 1L
  expect_false(is.RollingLDA(tmp))
  expect_false(is.RollingLDA(tmp, verbose = TRUE))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "sum of \"n\" does not match number of texts")
  expect_true(is.RollingLDA(roll_proto))
  expect_true(is.RollingLDA(roll_lda2))
  expect_true(is.RollingLDA(roll_proto2))
  expect_true(is.RollingLDA(init_proto))
  expect_true(is.RollingLDA(init_proto_updated1))
  expect_true(is.RollingLDA(init_proto_updated2))
  expect_true(is.RollingLDA(init_proto_updated3))
})

test_that("is.LDA", {
  expect_true(is.LDA(getLDA(roll_lda)))
  expect_true(is.LDA(getLDA(roll_lda), verbose = TRUE))
  expect_true(is.LDA(getLDA(roll_proto)))
  expect_true(is.LDA(getLDA(roll_lda2)))
  expect_true(is.LDA(getLDA(roll_proto2)))
  expect_true(is.LDA(getLDA(init_proto)))
  expect_true(is.LDA(getLDA(init_proto_updated1)))
  expect_true(is.LDA(getLDA(init_proto_updated2)))
  expect_true(is.LDA(getLDA(init_proto_updated3)))
})

test_that("chunks: match statistics with expactations", {
  expect_equal(getChunks(roll_lda),
               data.table(
                 chunk.id = 0:8L,
                 start.date = as.Date(c("2007-01-01",
                                        "2007-01-21",
                                        "2007-04-20",
                                        "2007-07-20",
                                        "2007-10-20",
                                        "2008-01-21",
                                        "2008-04-23",
                                        "2008-07-21",
                                        "2008-10-20")),
                 end.date = as.Date(c("2007-01-19",
                                      "2007-04-18",
                                      "2007-07-19",
                                      "2007-10-17",
                                      "2008-01-19",
                                      "2008-04-17",
                                      "2008-07-17",
                                      "2008-10-18",
                                      "2008-12-25")),
                 memory = as.Date(c(NA,
                                    "2006-07-20",
                                    "2006-10-20",
                                    "2007-01-20",
                                    "2007-04-20",
                                    "2007-07-20",
                                    "2007-10-20",
                                    "2008-01-20",
                                    "2008-04-20")),
                 n = as.integer(c(21, 84, 92, 85, 65, 67, 65, 60, 32)),
                 n.discarded = as.integer(c(0, 0, 0, 0, 2, 1, 1, 0, 0)),
                 n.memory = as.integer(c(NA, 21, 105, 176, 177, 150, 132, 132, 125)),
                 n.vocab = as.integer(c(55, 547, 841, 1038, 1160, 1261, 1323, 1486, 1500)),
                 key = colnames(getChunks(roll_lda))))
  expect_equal(getChunks(roll_lda)[, 1:2],
               data.table(
                 chunk.id = 0:8L,
                 start.date = as.Date(c("2007-01-01",
                                        "2007-01-21",
                                        "2007-04-20",
                                        "2007-07-20",
                                        "2007-10-20",
                                        "2008-01-21",
                                        "2008-04-23",
                                        "2008-07-21",
                                        "2008-10-20")), key = c("chunk.id", "start.date")))
})

test_that("types: lda, ldaprototype", {
  # roll_proto and roll_lda should only differ in lda (assignments ...) and id
  expect_identical(
    roll_proto[!names(roll_proto) %in% c("id", "lda")],
    roll_lda[!names(roll_lda) %in% c("id", "lda")])

  # roll_proto2 and roll_lda2 should only differ in lda (assignments ...)
  expect_identical(
    roll_proto2[names(roll_proto2) != "lda"],
    roll_lda2[names(roll_lda2) != "lda"])
})

test_that("unnamed dates"){
  seed = Sys.time()
  set.seed(seed)
  model1 = RollingLDA(economy_texts[1:200], unname(economy_dates[1:200]), "quarter", "6 month",
                      init = 20, K = 5, type = "lda")
  set.seed(seed)
  model2 = RollingLDA(economy_texts[1:200], economy_dates[1:200], "quarter", "6 month",
                      init = 20, K = 5, type = "lda")
  expect_identical(model1, model2)

  # keep in mind: dates must not be sorted, but are considered to be in the same
  # order as texts
  set.seed(seed)
  model2 = RollingLDA(economy_texts[1:200], sort(unname(economy_dates[1:200])), "quarter", "6 month",
                      init = 20, K = 5, type = "lda")
  expect_error(expect_identical(model1, model2))
}

test_that("init: date, ineger, character(date)", {
  # TODO

  chunk = unname(sort(economy_dates)[sample(180:220, 1)])
  init = unname(sort(economy_dates)[sample(80:120, 1)])
  mid = unname(sort(economy_dates)[sample(145:155, 1)])

  seed = Sys.time()

  # chunks as character identical to Date
  set.seed(seed)
  model1 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = init, memory = 20, K = 5, type = "lda")
  set.seed(seed)
  model2 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = as.character(init), memory = 20, K = 5, type = "lda")
  expect_identical(model1, model2)


  set.seed(seed)
  model1 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = c(init, mid), memory = 20, K = 5, type = "lda", vocab.abs = 0)
  set.seed(seed)
  model2 = RollingLDA(economy_texts[economy_dates <= mid], economy_dates[economy_dates <= mid],
                      chunks = init, memory = 20, K = 5, type = "lda", vocab.abs = 0)
  model2 = RollingLDA(model2, economy_texts[economy_dates >= mid & economy_dates <= chunk],
                      economy_dates[economy_dates >= mid & economy_dates <= chunk], memory = 20)
  expect_identical(getChunks(model1), getChunks(model2))
  expect_set_equal(getVocab(model1), getVocab(model2))
  expect_set_equal(getDates(model1), getDates(model2))

  economy_texts[1:200]
  economy_dates[1:200]

  # date

  # integer

  # needed if chunks character

  # not used if chunks dates

  # init smaller than minimum of dates
  expect_error(RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                          chunks = min(economy_dates), memory = min(economy_dates) - 1, K = 5, type = "lda"),
               "lowest date")
})

test_that("memory: character, date, integer, character(date)", {
  chunk = unname(sort(economy_dates)[sample(180:220, 1)])
  init = unname(sort(economy_dates)[sample(80:120, 1)])
  seed = Sys.time()

  # month = 1 month
  set.seed(seed)
  model1 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = init, memory = "month", K = 5, type = "lda")
  set.seed(seed)
  model2 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = init, memory = "1 month", K = 5, type = "lda")
  expect_identical(model1, model2)

  # 1month -> error
  expect_error(RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                          chunks = init, memory = "1month", K = 5, type = "lda"))

  # date(s)

  # character

  # integer

  # memory empty -> warning

  # memory.fallback -> warning2
})

test_that("chunks: character, date, character(date)", {
  chunk = unname(sort(economy_dates)[sample(180:220, 1)])
  init = unname(sort(economy_dates)[sample(80:120, 1)])
  seed = Sys.time()

  # chunks as character identical to Date
  set.seed(seed)
  model1 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = init, memory = 20, K = 5, type = "lda")
  set.seed(seed)
  model2 = RollingLDA(economy_texts[economy_dates <= chunk], economy_dates[economy_dates <= chunk],
                      chunks = as.character(init), memory = 20, K = 5, type = "lda")
  expect_identical(model1, model2)

  # date(s)

  # character

  # chunks empty -> warning

})


test_that("staged setting: updateRollingLDA", {
  # init_proto_updated1 and 2 are identical, but 3 is different (different seed)!
  expect_identical(init_proto_updated1, init_proto_updated2)
  expect_error(expect_identical(init_proto_updated1, init_proto_updated3))
  # roll_proto2 and init_proto_updated1 should only differ in lda and
  # first element of n.discarded in chunks table
  expect_identical(
    roll_proto2[!names(roll_proto2) %in% c("lda", "chunks")],
    init_proto_updated1[!names(init_proto_updated1) %in% c("lda", "chunks")])
  # initialized RollingLDA with unknown discarded texts in chunk 0
  expect_equal(
    getChunks(init_proto_updated1)$n.discarded[1],
    NA_integer_)
  # rest of chunks should be equal
  expect_identical(
    getChunks(roll_proto2)[-1,],
    getChunks(init_proto_updated1)[-1,])
})

test_that("Parameter RollingLDA match", {
  expect_identical(getParam(roll_proto),
                   getParam(roll_lda))
  expect_identical(getParam(roll_proto2),
                   getParam(roll_lda))
  expect_identical(getParam(roll_lda2),
                   getParam(roll_lda))
  expect_identical(getParam(init_proto_updated1),
                   getParam(roll_lda))
  expect_identical(rollinglda:::.defaultParam(),
                   getParam(roll_lda))

})

test_that("Parameter LDA match", {
  expect_identical(getParam(getLDA(roll_proto)),
                   getParam(getLDA(roll_lda)))
  expect_equal(getK(getLDA(roll_proto)), 5)
  expect_equal(getAlpha(getLDA(roll_proto)), 1/5)
  expect_equal(getEta(getLDA(roll_proto)), 1/5)
  expect_equal(getNum.iterations(getLDA(roll_proto)), 200)

  expect_equal(getK(getLDA(roll_proto2)), 10)
  expect_equal(getAlpha(getLDA(roll_proto2)), 1)
  expect_equal(getEta(getLDA(roll_proto2)), 1)
  expect_equal(getNum.iterations(getLDA(roll_proto2)), 20)
  expect_identical(getParam(getLDA(roll_proto2)),
                   getParam(getLDA(roll_lda2)))
  expect_identical(getParam(getLDA(roll_proto2)),
                   getParam(getLDA(roll_lda2)))
})

test_that("print.RollingLDA", {
  # prints not equal
  expect_error(expect_output(print(roll_lda), capture.output(print(roll_lda2))))
  # RollingLDA Object
  expect_output(print(roll_lda), "RollingLDA Object")
  expect_output(print(roll_lda2), "RollingLDA Object")

  # LDA print is part of RollingLDA print
  for(i in capture.output(print(getLDA(roll_lda)))[1:5]){
    expect_output(print(roll_lda), i, fixed = TRUE)
  }
  for(i in capture.output(print(getLDA(roll_lda2)))[1:5]){
    expect_output(print(roll_lda2), i, fixed = TRUE)
  }
})
