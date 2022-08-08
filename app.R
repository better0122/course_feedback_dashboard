library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(readr)
library(forcats)
library(wordcloud)
library(data.table)
library(rsconnect)
library(DT)
library(shiny)



  ui <- fluidPage(
    titlePanel("Course Feedback Dashboard"),
    wellPanel(p("Please upload your weekly feedback as a csv file."),
              p("When upload one file, it will displays feedback on a dashboard for you."),
              p("When upload more than one files, it will automatically generate the cumulative results.")),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 
                  label = 'Please upload your weekly feedback here.',
                  multiple = TRUE, 
                  buttonLabel = "BROWSE",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",".xlsx"))
      ),
      mainPanel(
        plotOutput("plot1"),#plot 
        plotOutput("plot6"),
        plotOutput("plot2"),
        plotOutput("plot3"),
        plotOutput("plot4"),
        plotOutput("plot5")
      )
    ))
  
  server <- function(input, output, session) {
    
    dataset<- reactive({
      tryCatch({
        req(input$file1)
        if(length(input$file1[, 1])==1){
          df <- read.csv(input$file1$datapath)
          df
        }else{
          all_q2_df <-data.frame()
          
          for(nr in 1:length(input$file1[, 1])){
            module1 <- read.csv(
              file = input$file1[[nr, 'datapath']]
            )
            q2_module1 <- module1 %>% 
              filter(Q.. == 2) %>% 
              select(Answer)
            all_q2_df<-rbind(all_q2_df,q2_module1)
          }
          all_q2_df
        } 
      }, error = function(e) {
        
      })
    })
    
    output$plot1 <- renderPlot({
      tryCatch({
        req(input$file1)
        if(length(input$file1[, 1])==1){
          module1 <-dataset()
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
                 title = "Question 1 Answer Distribution", ### CHANGE THIS TITLE
                 subtitle = paste("The average score:", 
                                  round(module1_q1_average_score,2), 
                                  sep = " "))
          
          module1_q1_distribution
        }else{
          all_q2_df <-dataset()
          all_q2_tibble <- tibble(all_q2_df)
          
          # Q2 Word Frequency Count (12 modules)
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
        }
        
      }, error = function(e) {
        
      })
      
      
      
    })
    
    output$plot2 <- renderPlot({
      
      req(input$file1)
      if(length(input$file1[, 1])==1){
        module1 <-dataset()
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
          labs(x = "Number of Word Count", y = "Word", title = "Top5 Word Count for Q2") ### CHANGE THIS TITLE
        
        ##### Word Cloud for Q2 #####
        pal = brewer.pal(8,"Dark2") # set up color parameter
        word_module1_q2 %>%
          count(word) %>%
          with(wordcloud(word, n, max.words = 20,
                         random.order = F, random.color = T,
                         color = pal))
      }else{
        
        module <- 1:length(input$file1[, 1])
        q1_all_avg_score <-data.frame()
        
        for(nr in 1:length(input$file1[, 1])){
          module1 <- read.csv(
            file = input$file1[[nr, 'datapath']]
          )
          a <- module1 %>% 
            filter(Q.. == 1, Answer.Match == "Checked") %>% 
            select(Answer) %>% 
            mutate(score = ifelse(Answer == "I got it, very clear",4,
                                  ifelse(Answer == "I got it but I am still fuzzy on some items", 3, 
                                         ifelse(Answer == "I am still somewhat confused on many points", 2, 1)))) %>% 
            summarise(mean(score))
          q1_all_avg_score<-rbind(q1_all_avg_score,a)
        }
        
        q3_all_avg_score <-data.frame()
        
        for(nr in 1:length(input$file1[, 1])){
          module1 <- read.csv(
            file = input$file1[[nr, 'datapath']]
          )
          b <- module1 %>% 
            filter(Q.. == 3, Answer.Match == "Checked") %>% 
            select(Answer) %>% 
            mutate(score = ifelse(Answer == "4. Very confident",4,
                                  ifelse(Answer == "3. Confident", 3, 
                                         ifelse(Answer == "2. Somewhat confident", 2, 1)))) %>% 
            summarise(mean(score))
          q3_all_avg_score<-rbind(q3_all_avg_score,b)
        }
        
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
          labs(x = "Modules", y = "Questions", title = "Average Scores for Selected Semesters") +
          ggrepel::geom_text_repel(mapping = aes(x = module, y = Q1, label = round(Q1,2))) +
          ggrepel::geom_text_repel(mapping = aes(x = module, y = Q3, label = round(Q3,2)))
        
        
        
        
      }
      
      
      
    })
    
    output$plot6 <- renderPlot({
      tryCatch({
        req(input$file1)
        if(length(input$file1[, 1])==1){
          module1 <-dataset()
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
          
          word_module1_q2 %>%
            count(word, sort = TRUE) %>% 
            top_n(5,n) %>% 
            ggplot(aes(x = fct_reorder(word, n), y = n)) +
            geom_col() + 
            geom_text(aes(label = n)) +
            coord_flip() +
            labs(x = "Number of Word Count", y = "Word", title = "Top5 Word Count for Q2") ### CHANGE THIS TITLE
          
        }else{
          
          all_q4_df <-data.frame()
          
          for(nr in 1:length(input$file1[, 1])){
            module1 <- read.csv(
              file = input$file1[[nr, 'datapath']]
            )
            q4_module1 <- module1 %>% 
              filter(Q.. == 4) %>% 
              select(Answer)
            all_q4_df<-rbind(all_q4_df,q4_module1)
          }
          all_q4_tibble <- tibble(all_q4_df)
          
          # Q4 Word Frequency Count (12 modules)
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
          
          
          
          
        } 
        
        
        
      }, error = function(e) {
        
      })
      
    })
    
    output$plot3 <- renderPlot({
      tryCatch({
        module1 <-dataset()
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
        
        module1_q3_distribution
      }, error = function(e) {
        
      })
      
    })
    output$plot5 <- renderPlot({
      tryCatch({
        module1 <-dataset()
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
        word_module1_q4 %>%
          count(word) %>%
          with(wordcloud(word, n, max.words = 20,
                         random.order = F, random.color = T,
                         color = pal))
      }, error = function(e) {
        
      })
      
    })
    
    
    output$plot4 <- renderPlot({
      tryCatch({
        module1 <-dataset()
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
        word_module1_q4 %>%
          count(word, sort = TRUE) %>% 
          top_n(5,n) %>% 
          ggplot(aes(x = fct_reorder(word, n), y = n)) +
          geom_col() + 
          geom_text(aes(label = n)) +
          coord_flip() +
          labs(x = "Number of Word Count", y = "Word", title = "Top5 Word Count for Q4")
        
        
      }, error = function(e) {
        
      })
      
    })
    
    
    
    
  }
  
shinyApp(ui, server)
