
#Set up workin directory to easily extract english dataset
setwd("~/GitHub/DS-Capstone/Coursera-SwiftKey/final/en_US")

#Loading libraries to be used on this file
    library(tidyverse)
    library(tidytext)
    library(ggthemes)
    library(wordcloud)

    #Read random lines of the file and save it into another file
        #Blog
            blogs <- read_delim("en_US.blogs.txt",delim = "\n") #read the whole blogs file
            text <- names(blogs) #Saving the name of the column because it corresponds with the first value
            names(blogs) <- "value" #Changing the name of the column to "value"
            blogs <- bind_rows(blogs,as_tibble(text)) #Completing the dataset with its first value
            blogs_rows <- floor(dim(blogs)[1]/2) #Completing the dataset with its first value
            blogs <- sample_n(blogs,blogs_rows, replace = FALSE) #Reducing the dataset to half its length choosing randomly
        #news
            news <- read_delim("en_US.news.txt", delim = "\n") #read the whole news file
            text <- names(news) #Saving the name of the column because it corresponds with the first value
            names(news) <- "value" #Changing the name of the column to "value"
            news <-  bind_rows(news,as_tibble(text)) #Completing the dataset with its first value
            news_rows <- floor(dim(news)[1]/2) #Getting the number of rows
            news <- sample_n(news,news_rows, replace = FALSE) #Reducing the dataset to half its length choosing randomly
        #twitter
            twitter <- read_delim("en_US.twitter.txt", delim = "\n") #read the whole news file
            text <- names(twitter) #Saving the name of the column because it corresponds with the first value
            names(twitter) <- "value" #Changing the name of the column to "value"
            twitter <-  bind_rows(twitter,as_tibble(text)) #Completing the dataset with its first value
            twitter_rows <- floor(dim(twitter)[1]/2) #Getting the number of rows
            twitter <- sample_n(twitter,twitter_rows,replace = FALSE) #Reducing the dataset to half its length choosing randomly
            
        #Remove unnecessary objects
            rm("blogs_rows","news_rows","twitter_rows","text")
    #Now it is the time to tokenize the datasets and join them together
        #Blog
            blog_tk <- unnest_tokens(blogs,word,value) %>% mutate(source = "blogs")
        #news
            news_tk <- unnest_tokens(news,word,value) %>% mutate(source = "news")
        #Twitter
            twitter_tk <- unnest_tokens(twitter,word,value) %>% mutate(source = "twitter")
        #Join
            dataset <- bind_rows(blog_tk,news_tk,twitter_tk) %>%
                        mutate(word = str_extract(word, "[a-z']+")) %>% #eliminate not alphabetic characters
                        filter(!is.na(word),word !="rt") %>% #filter to keep non NA words and eliminate rt as a word
                        anti_join(stop_words) #Eliminate stopwords
                        
    
    ##EXPLORATORY DATA ANALYSIS
        #Overall Word frequencies
            word_freq <- count(dataset,word, sort = TRUE)
            #Visualisation of frequencies
                word_freq %>% 
                    filter(n > 24000) %>%
                    mutate(word = reorder(word, n)) %>%
                    ggplot(aes(word, n)) + 
                    theme_economist()+
                    geom_col(fill = "#336666") +
                    xlab(NULL) +
                    coord_flip()
        #Word frequencies by source
            source_freq <-  count(dataset,word,source, sort = TRUE)
            #visualization of word frequencies by source
                source_freq %>%
                    filter(n > 13000) %>%
                    mutate(word = reorder(word, n)) %>%
                    ggplot(aes(word,n)) + 
                    theme_economist() +
                    geom_col(fill = "#336666") +
                    xlab(NULL) +
                    coord_flip() +
                    facet_wrap(vars(source))

    ##N-GRAMS ANALYSIS
        #2-GRAMS by adding the arguments token = "ngrams" and n = 2 we can tokenize the data in 2-grams
            #Blog
            blog_tk <- unnest_tokens(blogs,bigram,text,token = "ngrams", n = 2) %>% mutate(source = "blogs") %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%#Separate the words from the bigram in word1 and word2 columns
                filter(!word1 %in% stop_words$word) %>%#filter  the cases where either word is a stopword
                filter(!word2 %in% stop_words$word)
            #news
            news_tk <- unnest_tokens(news,bigram,text,token = "ngrams", n = 2) %>% mutate(source = "news") %>%
                separate(bigram, c("word1", "word2"), sep = " ")%>%#Separate the words from the bigram in word1 and word2 columns
                filter(!word1 %in% stop_words$word) %>%#filter  the cases where either word is a stopword
                filter(!word2 %in% stop_words$word)
            #Twitter
            twitter_tk <- unnest_tokens(twitter,bigram,text,token = "ngrams", n = 2) %>% mutate(source = "twitter") %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%#Separate the words from the bigram in word1 and word2 columns
                filter(!word1 %in% stop_words$word) %>%#filter  the cases where either word is a stopword
                filter(!word2 %in% stop_words$word)
            #Bigrams
            dataset_bigram <- bind_rows(blog_tk,news_tk,twitter_tk) %>%
                                filter(!is.na(word1))%>% #filter the cases where either word is NA
                                filter(!is.na(word2))
            #Bigram count
            count_bigram <- dataset_bigram %>% 
                            count(word1,word2, sort = TRUE)
                