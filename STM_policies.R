## STM application to GCoM corpus of local policies 

library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(textmineR)
library(broom)
library(reshape2)
library(stringr)
library(stm)
library(textstem)
library(tm)

setwd("../") 

## import 4th release policies
ax8 = read_excel("actions_2C.xlsx")
ax8$trtext = tolower(paste(ax8$mitigation_action_sector,ax8$mitigation_action_area,ax8$mitigation_action_instrument,ax8$key_action_title_EN,ax8$action_title_EN,ax8$short_description_EN,ax8$short_description_EN, sep=" "))

## Clean actions pillar flags
## actions most probably mitigation
mitigation = c("green energy","public procurement","lighting","solar","photo","voltaic","biomass","energy consumption","energy manage","energy efficien","local energy production","emission mobility", "vehicles","energy poverty","composting","tax credits","insulation")
miti_match = str_c(mitigation, collapse = "|")
has_miti = as.data.frame(str_detect(ax8$trtext, miti_match))
colnames(has_miti) = "miti_filter"
ax9 = cbind(ax8,has_miti)
ax9$adaptation = ifelse(ax9$adaptation_flag==TRUE & ax9$miti_filter==FALSE ,1,0)
ax9$mitigation = ifelse(ax9$mitigation_flag==TRUE | ax9$miti_filter==TRUE,1,0)
ax9$adaptigation = ifelse(ax9$mitigation==1 & ax9$adaptation==1,1,0)
ax9$poverty = ifelse(ax9$energy_poverty_flag==TRUE,1,0)

ax9$length = nchar(ax9$trtext)
freq2 = table(ax9$length)

ax19 = subset(ax9,mitigation==1) 
data0 = ax19[,c("action_plan_id","action_id","trtext")]#,"adaptation_sectors","adaptation_hazards")] 
data0$ID = paste(data0$action_plan_id,data0$action_id,sep=" ")


## Pre-processing
data=data0

## words before cleaning
raw_tokens <- data %>% 
  tidytext::unnest_tokens(word, trtext)
raw_tokens = subset(raw_tokens,!is.na(word) & word!="na")
raw_tokens <- raw_tokens %>% mutate(ind = row_number())
raw_tokens <- raw_tokens %>% group_by(ID) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
raw_tokens [is.na(raw_tokens)] <- ""
drop = c("action_plan_id","action_id","trtext")
raw_tokens = raw_tokens[,!(names(raw_tokens) %in% drop)]
raw_tokens <- tidyr::unite(raw_tokens,text,-ID,sep =" ", na.rm = TRUE) 
raw_tokens$text <- trimws(raw_tokens$text)
raw_tokens$numw = sapply(strsplit(raw_tokens$text, " "), length)
freq = table(raw_tokens$numw)

data$trtext[[21]]
tr_corpus = Corpus(VectorSource(as.vector(data$trtext))) 
tr_corpus = tm_map(tr_corpus, lemmatize_strings)
tr_corpus = tm_map(tr_corpus, PlainTextDocument)
newtext = tr_corpus[['content']]$content
data$text = matrix(newtext,dim(data[1]),1)
colnames(data) = c("action_plan_id","action_id","trtext","ID","text")
data$text[[21]]
data$text <- sub("/", "", data$text)
data$text <- sub("-", "", data$text)
text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)
text_cleaning_tokens$word <- gsub('[[:digit:]]', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)
tokens <- text_cleaning_tokens %>% filter(!(word %in% c("other","na","a","an","at","and","its","or","the","for","from","that","they","all","in","with","de","di","la","el","en","del","les","van","es","amb","els","ada","al","plan","una","canvi",
                                                        "against","about","more","into","through","thorough","towards","around","change","green","situations","con","da","dels","delle","measures","increase","dei","il","een","és",
                                                        "between","where","what","on","which","along","most","under","climate","climate change","municipality","para","focus", 
                                                        "adaptation","adapt","adaptive","adapting","resiliency","resilient","sector","risk","risks","hazard","action","measure","municipal","comunal","city","town",
                                                        "council","urban plan","plans","planning","master","territory","territorial","vulnerability","vulnerabilities","system","projects","programme",
                                                        "strategy","strategic","impact","general","directly","new","implement","implementing","promote","promoting","elaboration","elaborate","improve","improving","draft","drafting",
                                                        "support","supporting","create","creating","streghten","streghtening","develop","developing","optimize","optimizing","reduce","reducing","decrease","expand","expansion","high","low", 
                                                        "set","setting","take","taking","verify","install","installing","provide","elaborate","elaboration","make","making","prepare","preparing","renew","renewal","develop","development","support","supporting",
                                                        "minimize","organization","identify","indentification","update","updating","encourage","encouraging","install","installing",
                                                        "include","introduce","improve","improvement","upgrade","upgrading","provide","extend","promote","promotion","km","error","connection","file","seap","secap","attached","actions","action",
                                                        "application","implementation","efficient","annex","code","active","fer","aa","ab","aj","aq","raise","energy")))

tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(ID) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""

drop = c("action_plan_id","action_id","trtext","freq2","length")#,"adaptation_hazards","adaptation_sectors")
tokens2 = tokens[,!(names(tokens) %in% drop)]

tokens2 <- tidyr::unite(tokens2,text,-ID,sep =" " )
tokens2$text <- trimws(tokens2$text)

## minimum length of words
tokens2$numw = sapply(strsplit(tokens2$text, " "), length)
freq = table(tokens2$numw)
xx = subset(tokens2,numw==0)
data2 = subset(tokens2,numw>2) 

## creation of the DocumentTermMatrix
tokens_x = data2
dtm = CreateDtm(tokens_x$text, 
                doc_names = tokens_x$ID, 
                ngram_window = c(2,4))  

## STM application
plan = as.data.frame(as.matrix(seq(1,dim(tokens_x)[1],by=1)))
for(j in 1:30){
  plan$topic = j
  if(j==1){plans=plan}else{plans=rbind(plans,plan)}
}
plans = plans %>% rename (document=V1)

for(i in 2:16){
  mp = stm(dtm, K = i, seed = 1234)
  ap_acs = tidy(mp, matrix = "gamma")
  colnames(ap_acs) = c("document","topic",gsub(" ", "",(paste(i,"topics",""))))
  plans = left_join(plans,ap_acs,by=c("document","topic"))
  stats <- labelTopics(mp)
  top_terms <- stats$prob
  unique_terms <- stats$frex
  name = paste0("stm_model_",i,"topics_unique_terms.txt")
  write.table(unique_terms, file = name, sep = "\t", row.names = FALSE, col.names = TRUE)
  name2 = paste0("stm_model_",i,"topics_top_terms.txt")
  write.table(top_terms, file = name2, sep = "\t", row.names = FALSE, col.names = TRUE)
}

write.table(plans, file = "stm_30topics_action_membership.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

## STM indices
s2 = searchK(dtm,K=seq(3,15,by=1),N = floor(0.1 * length(tokens_x$text)))
search_results = data.frame(s2$results)
search_results[,1:8] = lapply(search_results[,1:8],as.numeric)
write.table(search_results, file = "searchK_indexes_models_3_15.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
