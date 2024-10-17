# test that gstm works with quanteda

require(quanteda)

test_that("Test that gstm works on a quanteda dfm", {
  require(quanteda)
  if(utils::compareVersion(as.character(utils::packageVersion("quanteda")), "0.9.9-31") >= 0) {
    gadarian_corpus <- corpus(gadarian, text_field = "open.ended.response")
    gadarian_dfm <- dfm(tokens(gadarian_corpus))
    set.seed(10012) # NYU :-)
    gstm_from_dfm <- gstm(gadarian_dfm,
                        K = 3,
                        prevalence = ~treatment + s(pid_rep),
                        data = docvars(gadarian_corpus),
                        max.em.its=2)
    expect_true(inherits(gstm_from_dfm, "gSTM"))
  } else {
    #basically if the version is old, just skip this test for now.
    expect_identical("gSTM", "gSTM")
  }
})

if(requireNamespace("tm",quietly=TRUE) & utils::packageVersion("tm")>="0.6") {
  test_that("Test that gstm works on a classic gstm object structure", {
    temp <- textProcessor(documents = gadarian$open.ended.response,
                          metadata = gadarian)
    meta <- temp$meta
    vocab <- temp$vocab
    docs <- temp$documents
    out <- prepDocuments(docs, vocab, meta)
    docs <- out$documents
    vocab <- out$vocab
    meta <- out$meta
    set.seed(10012)
    gstm_from_gstmclassic <- 
      gstm(docs, vocab, 3, prevalence = ~treatment + s(pid_rep), data = meta,
          max.em.its = 2)
    expect_true(inherits(gstm_from_gstmclassic, "gSTM"))
  })
}
## could add an additional test to compare the two outputs
## could differ based on tokenizer, although
## tm::stopwords("english") is the same set as quanteda::stopwords("english")