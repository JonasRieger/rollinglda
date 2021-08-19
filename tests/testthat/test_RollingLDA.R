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

init_proto = as.RollingLDA(lda = getLDA(proto2), dates = dates, docs = docs)
init_proto_updated1 = RollingLDA(init_proto,
                           economy_texts[!names(economy_texts) %in% names(docs)],
                           economy_dates[!names(economy_dates) %in% names(docs)],
                           "quarter", "6 month")

#expect_identical(init_proto_updated1, roll_proto2)

# init_updated1 == roll_proto2

test_that("types: lda, ldaprototype", {
  expect_identical(
    roll_proto[!names(roll_proto) %in% c("id", "lda")],
    roll_lda[!names(roll_lda) %in% c("id", "lda")])
  expect_identical(
    roll_proto2[names(roll_proto2) != "lda"],
    roll_lda2[names(roll_lda2) != "lda"])
  #expect_equal(lda, lda2)
  #expect_equal(lda, lda3)
  #expect_equal(lda4, lda3.manip)
})
