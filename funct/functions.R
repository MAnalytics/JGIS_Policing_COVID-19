
#----------------------------------------------------
#function to put PC to sleep for an x number of seconds
testit <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

#----------------------------------------------------
#data cleaning functions
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("\"", " ", doc)
  doc <- gsub("\n", " ", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

#----------------------------------------------------
#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

#----------------------------------------------------
#function to perform randomization testing, called from 'OP_Source_Code.Rmd'
net_sentiment_simulation <- function(data, replicas=999){
  
  simulated_esd <- NULL
  
  for(m in 1:replicas){
    p1 <- data %>%
      #dplyr::filter(pandem_keyword=="absent")%>%
      group_by(pandem_keyword)%>%
      mutate(nnrow=length(word))%>%
      mutate(prob1=nnrow/nrow(data))%>% #prob of absent
      group_by(pandem_keyword, sentiment) %>% 
      mutate(pos_neg_count=n())%>%
      mutate(prob2=pos_neg_count/nnrow)##%>%
    
    ab_class_sent <- p1[which(p1$pandem_keyword == "absent"),3]
    
    length_exist_group <- length(which(p1$pandem_keyword == "exist"))
    
    #now collate the unique probabilities of 'absent' class
    p1_prob <- p1 %>%
      dplyr::filter(pandem_keyword == "absent")%>%
      dplyr::distinct(sentiment, .keep_all = TRUE)%>%
      dplyr::select(sentiment, prob2)
    
    
    if(replicas == 1){
      set.seed(nrow(data))
      new_ex_class_sent = sample(p1_prob$sentiment, length_exist_group, replace=TRUE, prob = p1_prob$prob2)
    }
    
    if(replicas > 1){
      
      if(m == 1){
        set.seed(nrow(data))
        new_ex_class_sent = sample(p1_prob$sentiment, length_exist_group, replace=TRUE, prob = p1_prob$prob2)
      }
      
      if(m > 1){
        new_ex_class_sent = sample(p1_prob$sentiment, length_exist_group, replace=TRUE, prob = p1_prob$prob2)
      }
      
    }
    
    new_ex_class_sent
    
    new_sentiment_list <- c(new_ex_class_sent, as.vector(unlist(ab_class_sent))) 

    final_p1 <- data.frame(cbind(p1, sentiment2=new_sentiment_list))
    #expected 
    UK_bing_ESD <- final_p1 %>% 
      dplyr::select(ID, word, sentiment, pandem_keyword, country, sentiment2)%>%
      mutate(sentiment = sentiment2)%>%
      dplyr::select(-c(sentiment2))
    
    UK_bing_ESD <-  UK_bing_ESD %>%
      #filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
      count(sentiment, country) %>%
      group_by(country, sentiment) %>%
      summarise(sentiment_sum = sum(n)) %>%
      ungroup()
    
    UK_bing_ESD_ = UK_bing_ESD %>%
      group_by(country) %>%
      dplyr::mutate(total=sum(sentiment_sum))%>%
      mutate(pct=round((sentiment_sum/total)*100, digits=2))
    
    UK_bing_ESD_ = data.frame(dcast(UK_bing_ESD_, sentiment ~ country))
    
    UK_bing_2_ESD = UK_bing_ESD_ %>% gather(Country, valname, -sentiment) %>%
      spread(sentiment, valname)%>%
      dplyr::rename(negative_ESD = negative)%>%
      dplyr::rename(positive_ESD=positive) %>%
      mutate(net_sentiment_ESD = positive_ESD - negative_ESD)
    
    simulated_esd <- rbind(simulated_esd, UK_bing_2_ESD)
  }
  return(simulated_esd) 
  
}
