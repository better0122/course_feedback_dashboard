########################## Course Feedback Dashboard ###########################
rm(list = ls())
getwd()

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(readr)
library(forcats)
library(wordcloud)
library(data.table)
module1 <- read.csv("Module1.csv")
module2 <- read.csv("Module2.csv")
module3 <- read.csv("Module3.csv")
module4 <- read.csv("Module4.csv")
module5 <- read.csv("Module5.csv")
module6 <- read.csv("Module6.csv")
module7 <- read.csv("Module7.csv")
module8 <- read.csv("Module8.csv")
module9 <- read.csv("Module9.csv")
module10 <- read.csv("Module10.csv")
module11 <- read.csv("Module11.csv")
module12 <- read.csv("Module12.csv")

module1 %>% 
  filter(`Q #` == 1, `Answer Match` == "Checked") %>% 
  select(Answer) %>% 
  #Visualization
  count(Answer) %>% 
  ggplot(aes(x = Answer, y = n)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n)) + 
  labs(x = "Number of Answers Count", 
       y = "Answer", 
       title = "Question 1 Answer Distribution")

################################### Module 1 ###################################

################################### Q1 ###################################

##### Filter question 1 and select the text answer #####
module1_q1 <- tibble(module1 %>% 
               filter(Q.. == 1, Answer.Match == "Checked") %>% 
               select(Answer))

##### Q1 Answer summary average score #####
module1_q1_average_score <- module1 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

##### Visualization - Q1 score distribution #####
module1_q1_distribution <- module1_q1 %>% 
  count(Answer) %>% 
    ggplot(aes(x = Answer, y = n)) + 
    geom_col() + 
    coord_flip() + 
    geom_text(aes(label = n)) + 
    labs(x = "Number of Answers Count", 
         y = "Answer", 
         title = "Question 1 Answer Distribution", 
         subtitle = paste("The average score:", 
                          round(module1_q1_average_score,2), 
                          sep = " "))



################################### Q2 ###################################

##### Filter for question 2 and select the text answer #####
module1_q2 <- tibble(module1 %>% 
               filter(Q.. == 2) %>% 
               select(Answer))

##### Q2 Word Frequency Count #####

# Add more stop words
custom_stop_words <- tribble(
  ~word,  ~lexicon,
  "data", "CUSTOM",
  "1",    "CUSTOM",
  "2",    "CUSTOM",
  "10",   "CUSTOM",
  "13",   "CUSTOM"
)

# Combine new stop words to stop_words list
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

# anti_join with stop_word2 
word_module1_q2 <- module1_q2 %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words2)

word_module1_q2 %>%
  count(word, sort = TRUE)

##### Top5 Word Count for Q2 #####
word_count_module1_q2 <- word_module1_q2 %>%
  count(word, sort = TRUE) %>% 
  top_n(5,n) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_col() + 
  geom_text(aes(label = n)) +
  coord_flip() +
  labs(x = "Number of Word Count", y = "Word", title = "Top5 Word Count for Q2")

##### Word Cloud for Q2 #####
pal = brewer.pal(8,"Dark2") # set up color parameter
word_cloud_module1_q2 <- word_module1_q2 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20,
                 random.order = F, random.color = T,
                 color = pal))


################################### Q3 ###################################

##### Filter question 3 and select the text answer #####
module1_q3 <- tibble(module1 %>% 
               filter(Q.. == 3, Answer.Match == "Checked") %>% 
               select(Answer))

##### Q3 Answer summary average score #####
module1_q3_average_score <- module1 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

##### Visualization - Q3 score distribution #####
module1_q3_distribution <- module1_q3 %>% 
  count(Answer) %>% 
  ggplot(aes(x = Answer, y = n)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n)) + 
  labs(x = "Number of Answers Count", y = "Answer", title = "Question 3 Answer Distribution")



################################### Q4 ###################################

##### Filter question 4 and select the text answer #####
module1_q4 <- tibble(module1 %>% 
               filter(Q.. == 4) %>% 
               select(Answer))

##### Q4 Word Frequency Count #####
word_module1_q4 <- module1_q4 %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words2)

word_module1_q4 %>%
  count(word, sort = TRUE)

##### Top5 Word Count for Q4 #####
word_count_module1_q4 <- word_module1_q4 %>%
  count(word, sort = TRUE) %>% 
  top_n(5,n) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_col() + 
  geom_text(aes(label = n)) +
  coord_flip() +
  labs(x = "Number of Word Count", y = "Word", title = "Top5 Word Count for Q4")

##### Word Cloud for Q4 #####
pal = brewer.pal(8,"Dark2") # set up color parameter
j <- word_module1_q4 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20,
                 random.order = F, random.color = T,
                 color = pal))



################################ Whole Semester ################################

##### Word Cloud for Q2 #####

# Combine data into one column
q2_module1 <- module1 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module2 <- module2 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module3 <- module3 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module4 <- module4 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module5 <- module5 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module6 <- module6 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module7 <- module7 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module8 <- module8 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module9 <- module9 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module10 <- module10 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module11 <- module11 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)
q2_module12 <- module12 %>% 
  filter(Q.. == 2) %>% 
  select(Answer)

all_q2_df <- rbind(q2_module1,q2_module2,q2_module3,q2_module4,q2_module5,q2_module6,q2_module7,q2_module8,q2_module9,q2_module10,q2_module11,q2_module12)

all_q2_tibble <- tibble(all_q2_df)

# Word Frequency Count (12 modules)
all_word_q2 <- all_q2_tibble %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words)

all_word_q2 %>%
  count(word, sort = TRUE)

# Word Cloud (12 modules)
pal = brewer.pal(8,"Dark2") # set up color parameter
all_word_q2 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 30,
                 random.order = F, random.color = T,
                 color = pal))

##### Word Cloud for Q4 #####

# Combine data into one column
q4_module1 <- module1 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module2 <- module2 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module3 <- module3 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module4 <- module4 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module5 <- module5 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module6 <- module6 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module7 <- module7 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module8 <- module8 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module9 <- module9 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module10 <- module10 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module11 <- module11 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)
q4_module12 <- module12 %>% 
  filter(Q.. == 4) %>% 
  select(Answer)

all_q4_df <- rbind(q4_module1,q4_module2,q4_module3,q4_module4,q4_module5,q4_module6,q4_module7,q4_module8,q4_module9,q4_module10,q4_module11,q4_module12)

all_q4_tibble <- tibble(all_q4_df)

# Word Frequency Count (12 modules)
all_word_q4 <- all_q4_tibble %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words)

all_word_q4 %>%
  count(word, sort = TRUE)

# Word Cloud (12 modules)
pal = brewer.pal(8,"Dark2") # set up color parameter
all_word_q4 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 30,
                 random.order = F, random.color = T,
                 color = pal))



##### Q1 Average Score #####

module1_q1_average_score <- module1 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module2_q1_average_score <- module2 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module3_q1_average_score <- module3 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module4_q1_average_score <- module4 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module5_q1_average_score <- module5 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module6_q1_average_score <- module6 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module7_q1_average_score <- module7 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module8_q1_average_score <- module8 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module9_q1_average_score <- module9 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module10_q1_average_score <- module10 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module11_q1_average_score <- module11 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

module12_q1_average_score <- module12 %>% 
  filter(Q.. == 1, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "I got it, very clear",4,
                        ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                               ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
  summarise(mean(score))

##### Q3 Average Score #####

module1_q3_average_score <- module1 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module2_q3_average_score <- module2 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module3_q3_average_score <- module3 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module4_q3_average_score <- module4 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module5_q3_average_score <- module5 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module6_q3_average_score <- module6 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module7_q3_average_score <- module7 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module8_q3_average_score <- module8 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module9_q3_average_score <- module9 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module10_q3_average_score <- module10 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module11_q3_average_score <- module11 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

module12_q3_average_score <- module12 %>% 
  filter(Q.. == 3, Answer.Match == "Checked") %>% 
  select(Answer) %>% 
  mutate(score = ifelse(Answer == "4. Very confident",4,
                        ifelse(Answer == "3. Confident", 3, 
                               ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
  summarise(mean(score))

# Create a tibble for average scores

module <- c(1,2,3,4,5,6,7,8,9,10,11,12)

q1_all_avg_score <- rbind(module1_q1_average_score,
                          module2_q1_average_score,
                          module3_q1_average_score,
                          module4_q1_average_score,
                          module5_q1_average_score,
                          module6_q1_average_score,
                          module7_q1_average_score,
                          module8_q1_average_score,
                          module9_q1_average_score,
                          module10_q1_average_score,
                          module11_q1_average_score,
                          module12_q1_average_score)

q3_all_avg_score <- rbind(module1_q3_average_score,
                          module2_q3_average_score,
                          module3_q3_average_score,
                          module4_q3_average_score,
                          module5_q3_average_score,
                          module6_q3_average_score,
                          module7_q3_average_score,
                          module8_q3_average_score,
                          module9_q3_average_score,
                          module10_q3_average_score,
                          module11_q3_average_score,
                          module12_q3_average_score)


# Combine Q1 and Q3 average scores
avg_score_df <- cbind(module,q1_all_avg_score,q3_all_avg_score)

# Set new names
setnames(avg_score_df, new = c("module","Q1","Q3"))

# Change to tibble
avg_score_tibble <- tibble(avg_score_df)

##### Whole Semester Average Score Line Chart #####
avg_score_tibble %>% 
  ggplot(aes(x = module)) + 
  geom_line(aes(y = Q1, colour = "Q1")) +
  geom_point(aes(y = Q1, colour = "Q1")) + 
  geom_line(aes(y = Q3, colour="Q3")) + 
  geom_point(aes(y = Q3, colour="Q3")) + 
  scale_color_manual(name = "Question number", values = c("Q1" = "darkblue", "Q3" = "red")) + 
  scale_x_continuous("module",
                     limits = c(1,12),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous("questions",
                     limits = c(1,4),
                     breaks = c(1,2,3,4)) +
  labs(x = "Modules", y = "Questions", title = "Average Scores for Whole Semester") +
  ggrepel::geom_text_repel(mapping = aes(x = module, y = Q1, label = round(Q1,2))) +
  ggrepel::geom_text_repel(mapping = aes(x = module, y = Q3, label = round(Q3,2)))

