wrd_freq_df <- function(df){

df %>%
  # mutate(ID = as.character(round(ID))) %>%
  # mutate(group = if_else(rating >= 4, "High", if_else(rating > 2, "Avg", "Low"))) %>%
  # select(ID, group,  reviewText) %>%
  
  #get word freq
  unnest_tokens(output = word, input = reviewText) %>% ungroup() %>%
  anti_join(stop_words) %>%
  count(ID, word) %>% ungroup() %>%
  group_by(ID) %>%
  mutate(total = sum(n)) %>% ungroup() %>%
  bind_tf_idf(word, ID, n) 
  
  
  # group_by(ID) %>%
  # slice_max(tf_idf, n = 15) %>% ungroup() %>%
  
}
  
  
