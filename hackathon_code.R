######################################
##### Hackathon code - 12/2/2023 #####
######################################
# Authors: Asta M. Rasmussen & Ingrid S. M. Backman

#########################
##### Load packages #####

library(tidyverse)
library(cowplot)
#library(BiocManager)
pacman::p_load(tidyverse, httr, csv, openai, stringi, pandas, tidyr, tibble)

# Set API key
openai_api_key_usr = "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"

#####################
##### Load data #####

## Original input data ##
real_words <- read_table("/your/path/real_words.txt", col_names = F)
token_words <- read_table("/your/path/token_words.txt", col_names = F)

# Generate random sets; Numbers and non-sensical words
word_len <- rep(2:10, 10)
numbers <- unlist(lapply(1:length(word_len), function(x) paste(sample(1:9, word_len[x], replace = T), collapse = "")))
nonsense_words <- unlist(lapply(1:length(word_len), function(x) paste(sample(letters, word_len[x], replace = T), collapse = "")))

# Parameters
model_engine <- c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001")
temperature <- 0

## Generated output data ##
words_df <- read.csv("/your/path/all_words_output.csv", row.names = 1)



###################################################################
##### Function for running language models on large word sets #####

outer <- list() # for if connection is lost, so not all results are lost

rev_word_fun <- function(in_words, in_temperature, in_model_engine){
  
  # setup input data
  word_splt <- unlist(lapply(in_words, function(x) paste(unlist(strsplit(toupper(x), split = "")), collapse = "-"))) # make W-O-R-D-S
  
  prompt_wording <- c(paste("Spell the word", in_words, "backwards", sep = " "), 
                      paste("Spell", in_words, "backwards. Do it step-by-step and show the last result", sep = " "),
                      paste("Spell", word_splt, "backwards", sep = " "),
                      paste("APPLE is ELPPA backwards. Spell", in_words, "backwards", sep = " "),
                      paste("NMUKLS is SLKUMN backwards. Spell", in_words, "backwards", sep = " "),
                      paste("lets play a game. Tell me from right to left letter by letter the word", in_words, sep = ""))
  
  prompt_wording_anno <- rep(1:6, length(in_words))[order(rep(1:6, length(in_words)))] # makes plotting easier later
  words_prompt <- rep(in_words, 6)
  
  # run text models
  idx <- 1
  out <- list()
  for (m in in_model_engine){
    for (i in 1:length(prompt_wording)){
      response <- openai::create_completion( 
        engine = m,
        prompt = prompt_wording[i],
        temperature = in_temperature,
        #max_tokens = 256,
        top_p = 1,
        #frequency_penalty = 0,
        #presence_penalty = 0,
        openai_api_key = openai_api_key_usr)
      
      # generate output
      out[[idx]] <- c(words_prompt[i], nchar(words_prompt[i]), m, prompt_wording[i], 
                      prompt_wording_anno[i], as.character(response$choices$text))
      outer[[idx]] <- out[[idx]]
      
      idx <- idx+1
    }
  }
  
  # make output df
  out <- do.call(rbind, out)
  out <- as.data.frame(out)
  colnames(out) <- c("word", "len", "model", "prompt", "prompt_id", "output")
  return(out)
}

# Run function
out_fun <-  rev_word_fun(nonsense_words, 0, model_engine)   
print(head(out_fun))

## Process output ##

# Only keep letters
out_fun$correct_answer <- toupper(stringi::stri_reverse(out_fun$word))
out_fun$out_letters <- toupper(gsub("\n", "",
                                    gsub(" ", "",
                                         gsub("-", "",
                                              gsub(",", "",
                                                   gsub(")", "", out_fun$output))))))

# Binary test if correct answer is present in output or not:
out_fun$answer_grep <- unlist(lapply(1:length(out_fun$correct_answer), 
                                     function(x) grepl(out_fun$correct_answer[x], out_fun$out_letters[x]))) 

# Summarise results
#out_fun %>% group_by(model) %>% summarise(nr_correct = length(which(answer_grep)))
#out_fun %>% group_by(word) %>% summarise(nr_correct = length(which(answer_grep)))

## Save results in csv file or similar ##
#write.csv(out_fun, "/your/path/real_words_output.csv")
#write.csv(out_fun, "/your/path/token_words_output.csv")
#write.csv(out_fun, "/your/path/number_words_output.csv")
#write.csv(out_fun, "/your/path/nonsense_words_output.csv")


#############################
##### Process output DF #####

# Load output data
#real_words_df <- read.csv("/your/path/real_words_output.csv", row.names = 1)
#token_words_df <- read.csv("/your/path/token_words_output.csv", row.names = 1)
#number_words_df <- read.csv("/your/path/number_words_output.csv", row.names = 1)
#nonsense_words_df <- read.csv("/your/path/nonsense_words_output.csv", row.names = 1)

# make single DF
#real_words_df$set <- "real_words"
#token_words_df$set <- "token_words"
#number_words_df$set <- "number_words"
#nonsense_words_df$set <- "nonsense_words"
#words_df <- rbind(real_words_df, token_words_df, number_words_df, nonsense_words_df)
#drop_idx <- which(words_df$word == "Extravagance") # Extravagance included twice, remove one instance
#drop_idx <- drop_idx[seq(1, length(drop_idx), 2)]
#words_df <- words_df[-drop_idx,]
#words_df$token_size <- NA
#words_df$token_size[which(words_df$set == "token_words")] <- rep(rep(1:4, 10)[order(rep(1:4, 10))], 30)
#write.csv(words_df, "/your/path/all_words_output.csv")


###############################
##### Plot output results #####

words_df$model <- factor(words_df$model, levels = c("text-ada-001", "text-babbage-001", "text-curie-001", "text-davinci-002", "text-davinci-003"))
words_df %>% filter(set == "real_words") %>% group_by(model, len) %>% summarise(nr_correct = length(which(answer_grep))) 
words_df$count_answer <- ifelse(words_df$answer_grep, 1, 0)

# Word length (stacked bar plot) - real words
plot_df <- words_df %>% filter(set == "real_words") %>% group_by(model, len) %>% summarise(count_answer = sum(count_answer),
                                                                                           n = n(),#length(model),
                                                                                           pct_answer = count_answer/n*100)
plot_df$model_id <- rep(1:5, 9)[order(rep(1:5, 9))]
p1 <- ggplot(plot_df, aes(x = len, y = pct_answer, color = model)) + geom_point() + geom_line() +
  theme_bw() + ylab("correct answers (%)") + labs(color = "model") + xlab("word length") + scale_color_brewer(palette="Spectral") + theme(legend.position = "none") + scale_x_continuous(breaks = c(2:10)) + ggtitle("Real words (n = 90)")

# Word length - numbers
plot_df <- words_df %>% filter(set == "number_words") %>% group_by(model, len) %>% summarise(count_answer = sum(count_answer),
                                                                                             n = n(),#length(model),
                                                                                             pct_answer = count_answer/n*100)
plot_df$model_id <- rep(1:5, 9)[order(rep(1:5, 9))]
p2 <- ggplot(plot_df, aes(x = len, y = pct_answer, color = model)) + geom_point() + geom_line() +
  theme_bw() + ylab("correct answers (%)") + labs(color = "model") + xlab("word length") + scale_color_brewer(palette="Spectral") + theme(legend.position = "none") + scale_x_continuous(breaks = c(2:10)) + ggtitle("Number strings (n = 90)")

# Word length - nonsense
plot_df <- words_df %>% filter(set == "nonsense_words") %>% group_by(model, len) %>% summarise(count_answer = sum(count_answer),
                                                                                               n = n(),#length(model),
                                                                                               pct_answer = count_answer/n*100)
plot_df$model_id <- rep(1:5, 9)[order(rep(1:5, 9))]
p3 <- ggplot(plot_df, aes(x = len, y = pct_answer, color = model)) + geom_point() + geom_line() +
  theme_bw() + ylab("correct answers (%)") + labs(color = "model") + xlab("word length") + scale_color_brewer(palette="Spectral") + theme(legend.position = "none") + scale_x_continuous(breaks = c(2:10)) + ggtitle("Nonsense words (n = 90)")

# Token size
plot_df <- words_df %>% filter(set == "token_words") %>% group_by(model, token_size) %>% summarise(count_answer = sum(count_answer),
                                                                                                   n = n(),#length(model),
                                                                                                   pct_answer = count_answer/n*100)
plot_df$model_id <- rep(1:5, 4)[order(rep(1:5, 4))]
p4 <- ggplot(plot_df, aes(x = as.integer(token_size), y = pct_answer, color = model)) + geom_point() + geom_line() +
  theme_bw() + ylab("correct answers (%)") + labs(color = "model") + xlab("token size") + scale_color_brewer(palette="Spectral") + ggtitle("Token words (n = 40)") + scale_x_continuous(breaks = c(1:4))

# Prompts
p5 <- ggplot(words_df, aes(x = prompt_id, y = count_answer, fill = model)) + geom_col() + theme_bw()  + ylab("# correct answers") + xlab("prompt id") + ggtitle("All words (n = 302)") + scale_fill_brewer(palette="Spectral") + scale_x_continuous(breaks = c(1:6)) + theme(legend.position = "none") # %>% filter(set == "real_words")

# Word sets and model complexity
plot_df <- words_df %>% filter(prompt_id == 1) %>% group_by(model, set) %>% summarise(count_answer = sum(count_answer),
                                                                                      pct_answer = count_answer/n()*100)
plot_df$model_id <- rep(1:5, 4)[order(rep(1:5, 4))]
p6 <- ggplot(plot_df, aes(x = model_id, y = pct_answer, color = set)) + geom_point() + geom_line() +
  theme_bw() + scale_x_continuous(breaks = c(1:5), labels = unique(plot_df$model)) + ylab("correct answers (%)") +
  scale_color_discrete(labels = c("nonsense words", "number strings", "real words", "token words")) + labs(color = "Word set") + xlab("Model complexity") + theme(axis.text.x = element_text(angle = -30, vjust = -1)) #theme(axis.title.x = element_blank())

# make plot grid
top <- plot_grid(p1, p2, p3, ncol = 3, labels = c("A", "B", "C"))
middle <- plot_grid(p4, labels = c("D"))
bottom <- plot_grid(p5, p6, ncol = 2, labels = c("E", "F"))

p <- plot_grid(
  top, middle, bottom, 
  nrow = 3
)

# save plot
ggsave( # A4 210 x 297 mm
  paste0("/your/path/analysis_plot.pdf"), # _min5reads
  plot = p, #last_plot(),
  scale = 1,
  width = 210-10,
  height = (350)/2, # (297+3)/2 #-59.4
  units = "mm")
