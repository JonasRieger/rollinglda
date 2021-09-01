context("as.RollingLDA and Getter")

data("economy_texts")
data("economy_dates")

roll_lda = RollingLDA(economy_texts, economy_dates, "quarter", "6 month",
                      init = 20, K = 5, type = "lda")

test_that("various messages", {
  a = 12
  class(a) = "RollingLDA"
  expect_false(is.RollingLDA(a))
  expect_message(is.RollingLDA(a, verbose = TRUE), "object is not a list")

  tmp = roll_lda
  names(tmp) = ""
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Must have names")

  tmp = roll_lda
  tmp$id = 2L
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "May only contain the following types: \\{character,LDA,list,Date,character,data.table,list\\},")

  tmp = roll_lda
  names(tmp)[names(tmp) == "id"] = "ID"
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "Must be equal to set \\{'id','lda','docs','dates','vocab','chunks','param'\\}")

  tmp = roll_lda
  tmp$lda = NULL
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "Must be equal to set \\{'id','lda','docs','dates','vocab','chunks','param'\\}")

  # id
  tmp = roll_lda
  tmp$id = c("id1", "id2")
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not a character of length 1")

  # lda
  tmp = roll_lda
  class(tmp$lda) = "not LDA"
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not an \"LDA\" object")

  # docs
  tmp = roll_lda
  tmp$docs = "not list"
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Must be of type 'list', not 'character'")

  tmp = roll_lda
  names(tmp$docs)[2] = names(tmp$docs)[1]
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not same names as \"docs\"")
  # error comes from dates, because getDocs returns all docs, that matches names in dates

  tmp = roll_lda
  tmp$docs[[2]] = "not matrix"
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "May only contain the following types: \\{matrix\\}")

  tmp = roll_lda
  tmp$docs[[2]] = matrix(0, ncol = 1, nrow = 5)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "not all elements have two rows")

  tmp = roll_lda
  tmp$docs[[2]] = matrix(0, ncol = 1, nrow = 2)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "not all values in the second row equal 1")

  # dates
  tmp = roll_lda
  tmp$dates[1] = NA_Date_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Contains missing values")

  tmp = roll_lda
  tmp$dates = as.character(tmp$dates)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Must be of class 'Date'")

  tmp = roll_lda
  names(tmp$dates)[2] = names(tmp$dates)[1]
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Must have unique names")
  # error comes from docs, because getNames return names from dates and matches
  # with names from docs

  tmp = roll_lda
  tmp$docs = append(tmp$docs, list(matrix(c(1,1), nrow = 2)))
  #names(tmp$docs)[length(tmp$docs)] = "ID_X"
  expect_true(is.RollingLDA(tmp))
  # does not return FALSE, because getDocs returns the subsetted docs

  tmp = roll_lda
  tmp$dates = append(tmp$dates, Sys.Date())
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not same names as \"docs\"")

  # vocab
  tmp = roll_lda
  tmp$vocab = c(getVocab(tmp), getVocab(tmp)[1])
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Contains duplicated values")

  tmp = roll_lda
  tmp$vocab = c(getVocab(tmp), NA_character_)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Contains missing values")

  tmp = roll_lda
  tmp$vocab = list(12)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "Must be of type 'character'")

  # chunks

  # param

})
