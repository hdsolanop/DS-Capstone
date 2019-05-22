
#Set up workin directory to easily extract english dataset
setwd("~/GitHub/DS-Capstone/Coursera-SwiftKey/final/en_US")

#Loading libraries to be used on this file
    library(tidyverse)
    library(tidytext)
    library(ggthemes)
    library(wordcloud)

#Fix encoding of the files
    code = "windows-1252"
    #According the encoding debug list we have this list of bad encoded symbols https://www.i18nqa.com/debug/utf8-debug.html
    weird_stuff = "â|€|œ|€|Â|¾|·|¬|¼|š|ž|¦|¹|’|˜|™|¢|”|Ë|„|¡|º|¸|£|¤|¥|§|©|¨|ª|«|®|¯|±|²|³|´|µ|^[Rr][Tt]$"
    
    #Create a connection to each document in order to sample the lines that will be extractec
    #Blogs
            blog_con <- file("en_US.blogs.txt","r", encoding = code)
        #news
            news_con <- file("en_US.news.txt","r",encoding = code)
        #twitter
            twitter_con <- file("en_US.twitter.txt","r",encoding = code)
        
    #Read random lines of the file and save it into another file
        #Blog
            blogs <- readLines(con = blog_con, n = -1) #read the whole file
            close(blog_con)#Close the connection to the file
            blogs <- sample(blogs,size = floor(length(blogs)/2),replace = FALSE) #Sample each paragraph
            blogs <- gsub("â€˜","'",blogs)#Replace "â€˜" by "'" to have the correct form
            blogs <- gsub(weird_stuff,"",blogs) #Replace the characters of the weird stuff string by the null character due to encoding problems
            blogs <- tibble(line = 1:length(blogs),text = blogs) #convert the blogs vector into a tibble table
        #news
            news <- readLines(con = news_con,n = -1) #read the whole news file
            close(news_con)#Close the connection to the file
            news <- sample(news,size = floor(length(news)/2),replace = FALSE)#Sample each paragraph
            news <- gsub("â€˜","'",news)#Replace "â€˜" by "'" to have the correct form
            news <- gsub(weird_stuff,"",news) #Replace the characters of the weird stuff string by the null character due to encoding problems
            news <- tibble(line = 1:length(news),text = news) #convert the news vector into a tibble table
        #twitter
            twitter <- readLines(con = twitter_con,n = -1) #read the whole twitter file
            close(twitter_con) #Close the connection to the file
            twitter <- sample(twitter, size = floor(length(twitter)/2),replace = FALSE)#Sample each paragraph
            twitter <- gsub("â€˜","'",twitter)#Replace "â€˜" by "'" to have the correct form
            twitter <- gsub(weird_stuff,"",twitter) #Replace the characters of the weird stuff string by the null character due to encoding problems
            twitter <- tibble(line = 1:length(twitter),text = twitter)#convert the twitter vector into a tibble table
    
    #Now it is the time to tokenize the datasets and join them together
        #Blog
            blog_tk <- unnest_tokens(blogs,word,text) %>% mutate(source = "blogs")
        #news
            news_tk <- unnest_tokens(news,word,text) %>% mutate(source = "news")
        #Twitter
            twitter_tk <- unnest_tokens(twitter,word,text) %>% mutate(source = "twitter")
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
                