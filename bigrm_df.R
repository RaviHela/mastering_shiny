
bigrm_freq_df <- function(df) {
  df %>%
    # mutate(ID = as.character(round(ID))) %>%
    # mutate(group = if_else(rating >= 4, "High", if_else(rating > 2, "Avg", "Low"))) %>%
    # select(ID, group,  reviewText) %>%
    unnest_tokens(bigram, reviewText, token = "ngrams", n = 2) %>%
    separate(bigram,
             c("word1", "word2"),
             sep = " ",
             remove = FALSE) %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
}


bigrm_freq_tf_idf <- function(df) {
  df %>%
    select(ID, bigram) %>%
    #get tf_idf
    count(ID, bigram) %>%
    bind_tf_idf(bigram, ID, n) %>%
    arrange(desc(tf_idf))
  
}
    
    
    
    
    
  
  
}