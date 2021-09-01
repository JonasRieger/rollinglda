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
  tmp = roll_lda
  tmp$chunks = list()
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not a data.table with standard parameters")

  tmp = roll_lda
  tmp$chunks = getChunks(tmp)[, -"chunk.id"]
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "not a data.table with standard parameters")

  # chunk.id
  tmp = roll_lda
  tmp$chunks$chunk.id[2] = tmp$chunks$chunk.id[1]
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "duplicated \"chunk.id\"")

  tmp = roll_lda
  tmp$chunks$chunk.id[1] = 0
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"chunk.id\" is not an integer")

  tmp = roll_lda
  tmp$chunks$chunk.id[1] = NA_integer_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "NA\\(s\\) in \"chunk.id\"")

  # n
  tmp = roll_lda
  tmp$chunks$n[1] = 0
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"n\" is not an integer")

  tmp = roll_lda
  tmp$chunks$n[1] = NA_integer_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "NA\\(s\\) in \"n\"")

  # n.discarded
  tmp = roll_lda
  tmp$chunks$n.discarded[1] = 0
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"n.discarded\" is not an integer")

  # n.memory
  tmp = roll_lda
  tmp$chunks$n.memory[1] = 0
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"n.memory\" is not an integer")

  # n.vocab
  tmp = roll_lda
  tmp$chunks$n.vocab[1] = 0
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"n.vocab\" is not an integer")

  tmp = roll_lda
  tmp$chunks$n.vocab[1] = NA_integer_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "NA\\(s\\) in \"n.vocab\"")

  tmp = roll_lda
  tmp$vocab = c(getVocab(roll_lda), "ABC")
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "max of \"n.vocab\" does not match number of vocabularies")

  tmp = roll_lda
  tmp$chunks$n.vocab = rev(tmp$chunks$n.vocab)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "\"n.vocab\" is not monotonously increasing")

  # start.date
  tmp = roll_lda
  tmp$chunks$start.date = as.character(tmp$chunks$start.date)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"start.date\" is not a Date object")

  tmp = roll_lda
  tmp$chunks$start.date[1] = NA_Date_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "NA\\(s\\) in \"start.date\"")

  tmp = roll_lda
  tmp$chunks$start.date[1] = as.character(Sys.Date())
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE),
                 "minimum of \"start.date\" is larger than minimum of text's dates")

  # end.date
  tmp = roll_lda
  tmp$chunks$end.date = as.character(tmp$chunks$end.date)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"end.date\" is not a Date object")

  tmp = roll_lda
  tmp$chunks$end.date[1] = NA_Date_
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "NA\\(s\\) in \"end.date\"")

  # memory
  tmp = roll_lda
  tmp$chunks$memory = as.character(tmp$chunks$memory)
  expect_false(is.RollingLDA(tmp))
  expect_message(is.RollingLDA(tmp, verbose = TRUE), "\"memory\" is not a Date object")




  # param

})
