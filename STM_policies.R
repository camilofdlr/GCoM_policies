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
library(wordcloud)

setwd("../") 

##Part I. Topic extraction

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

for(i in 2:30){
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

## word-clouds per topic
for(i in 1:14){
  path = paste0("20w_topic",i,".jpeg")
  tiff(path, units="cm", width=40, height=12, res=2000)
  cloud(
    mp14,
    topic = i,
    type = "model",
    max.words = 20
  )
  dev.off()
}


## Part II. Selection of best model

## Import the inventories
eAP = read_excel("df_EIners_AP.xlsx")
eMR = read_excel("df_EIners_MR.xlsx")
es = rbind(eAP,eMR)
emissions = setDT(es)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,bei_flag,population_in_the_inventory_year,country_code), 
                      .SDcols=c("emission_measure","energy_measure")]
con = unique(emissions[,c("action_plan_id","country_code")])
con$action_plan_id = as.character(con$action_plan_id)

## commitments
targets = read_excel("df2A_1.xlsx", col_types = c("text",rep("numeric",9),rep("text",8),rep("numeric",8)))
targets$base_year_CoM2030 = ifelse(targets$action_plan_id %in% c(1315),2009, targets$base_year_CoM2030)
targets$base_year_CoM2030 = ifelse(targets$action_plan_id %in% c(912,66),2005, targets$base_year_CoM2030)
targets = subset(targets,!is.na(co2_target_CoM2030) | (!is.na(`co2_target_Mid-term target`)&`target_year_Mid-term target`>=2030))

ax30 = subset(targets,!is.na(co2_target_CoM2030))[c("action_plan_id","co2_target_CoM2030","base_year_CoM2030","reduction_type_CoM2030","population_estimate_in_target_year_CoM2030")]
ax30 = ax30 %>% rename (target=co2_target_CoM2030,inventory_year=base_year_CoM2030,type=reduction_type_CoM2030, population_est=population_estimate_in_target_year_CoM2030)
ax30$target_year = 2030
ax_m = subset(targets,!is.na(`co2_target_Mid-term target`)&`target_year_Mid-term target`>=2030)[c("action_plan_id","co2_target_Mid-term target","base_year_Mid-term target","target_year_Mid-term target","reduction_type_Mid-term target","population_estimate_in_target_year_Mid-term target")]
ax_m = ax_m %>% rename (target=`co2_target_Mid-term target`,inventory_year=`base_year_Mid-term target`,type=`reduction_type_Mid-term target`, population_est=`population_estimate_in_target_year_Mid-term target`,target_year=`target_year_Mid-term target`)
ax_m = subset(ax_m,!(action_plan_id%in%ax30$action_plan_id))
targets2 = rbind(ax30,ax_m)
targets2$target = as.numeric(targets2$target)

## BEIs
## join with beis
emissions$action_plan_id = as.character(emissions$action_plan_id)
axs1 = left_join(targets2,emissions,by=c("action_plan_id","inventory_year"))
axs1 = axs1[with(axs1, order(axs1$action_plan_id,axs1$target_year,axs1$inventory_year,-axs1$bei_flag)), ]
axs2 = setDT(axs1)[, lapply(.SD, first), by=.(action_plan_id,target_year,inventory_year),.SDcols=c("type","population_est","target","population_in_the_inventory_year","emission_measure")]
## Get inventories that don't match the base_year
## Select if bei_flag==TRUE 
xx = emissions[with(emissions, order(emissions$action_plan_id,-emissions$bei_flag)), ]
xx2 = setDT(xx)[, lapply(.SD, first), by=.(action_plan_id),.SDcols=c("bei_flag","inventory_year","population_in_the_inventory_year","energy_measure","emission_measure")]
control = subset(xx2,bei_flag==FALSE) #==0
xx2 = xx2[,-c("bei_flag")]
axs3 = left_join(axs2,xx2,by=c("action_plan_id"))
aab = subset(axs3,is.na(emission_measure.x) & !is.na(emission_measure.y))
axs3$emissions = ifelse(is.na(axs3$emission_measure.x),axs3$emission_measure.y,axs3$emission_measure.x)
axs3$inventory_year = ifelse(is.na(axs3$emission_measure.x),axs3$inventory_year.y,axs3$inventory_year.x)
axs3$population_in_the_inventory_year = ifelse(is.na(axs3$emission_measure.x),axs3$population_in_the_inventory_year.y,axs3$population_in_the_inventory_year.x)

## national trend
nat = read.table("cov_emissions2005_2030.txt", header = TRUE)[c("action_plan_id","emissions.trend")]
nat = subset(nat,!is.na(emissions.trend))

## Predictions
f30 = read.table("cov_pred_50_30.txt", header = TRUE)[c("action_plan_id","emissions")]
f30$action_plan_id = as.character(f30$action_plan_id)
f30 = f30 %>% rename(emission_measure=emissions)
axs4 = subset(axs3,target_year==2030)
data_30a = left_join(axs4,f30,by=c("action_plan_id"))
data_30 = subset(data_30a,!is.na(emission_measure))
data_30$tes_pc = (data_30$emissions/data_30$population_in_the_inventory_year)*(1-data_30$target)
data_30$population_est = as.numeric(data_30$population_est)
data_30$tes = data_30$tes_pc * data_30$population_est
data_30$target_emis = ifelse(data_30$type=="per_capita",data_30$tes,data_30$emissions*(1-data_30$target))

data=data_30

## Prepare the data
data$binary = ifelse(data$emission_measure>data$target_emis,0,1)

data$acc_red = ifelse(data$emission_measure==0,1,1-(data$emission_measure/data$emissions))
data$population = ifelse(data$type=="per_capita",data$population_est,data$population_in_the_inventory_year)
data$size = as.factor(ifelse(data$population>=200000,"Big",ifelse(data$population<200000 & data$population>50000, "Medium", "Small")))
data$ambition = as.factor(ifelse(data$target>mean(data$target),1,0))
data$emissions_pc = data$emissions/data$population_in_the_inventory_year
data_gcom = left_join(data,con,by=c("action_plan_id"))

datas2=data_gcom
datas2$action_plan_id = as.character(datas2$action_plan_id)

orgs = unique(es[,c("organisation_id","organisation_name","action_plan_id")])
orgs$action_plan_id = as.character(orgs$action_plan_id)
datas3 = left_join(datas2,orgs,by=c("action_plan_id"))

drop = c("population_est","tes","inventory_year.x","population_in_the_inventory_year.x","emission_measure.x","inventory_year.y","population_in_the_inventory_year.y","emission_measure.y")
datas3 = data.frame(datas3)
datas3 = datas3[,!(names(datas3)%in%drop)]
datas3 = left_join(datas3,nat,by=c("action_plan_id"))

y = datas3$binary
y2 = datas3$acc_red

## Get topics
tokens = read.table("tokens_lemma_new.txt", header = TRUE)
mitii = read.table("stm_15topics_action_membership.txt", header = TRUE)
tokens$document = seq(1,dim(tokens)[1],by=1)
mitii = left_join(mitii,tokens,by=c("document"))

## model with 14 topics finds confirmation
num_topics = 14

nums = paste0("X",num_topics,"topics")
mg=mitii[,c("ID","topic","text",nums)]
colnames(mg) = c("ID","topic","text","gamma")
group <- as.data.table(mg)
miti2 = setDT(group)[, .SD[which.max(gamma)], by=ID]
miti2$action_plan_id = word(miti2$ID,start=1,sep=fixed(" "))
miti2$action_id = word(miti2$ID,start=-1,sep=fixed(" "))

aps = data.frame(unique(miti2[,c("action_plan_id")]))
## for extracting individual actions
for (i in 1:num_topics){
  aps[,i+1]=0
}
for (i in 1:num_topics){
  aps[,num_topics+i+1]=0
}
features = c("action_plan_id",sprintf("topic%02d",seq(1,num_topics)),sprintf("num_acs_topic%02d",seq(1,num_topics)))
colnames(aps) = features

n = dim(aps)[1]
for(i in 1:n){
  ac1 = subset(miti2,action_plan_id == aps[i,"action_plan_id"])
  acus = unique(ac1[,c("topic")])
  for(j in 1:num_topics){
    rr = dim(subset(ac1,topic==j))[1]
    xn = dim(subset(acus,topic==j))[1]
    if(xn>0){
      topix=j+1
      topix2=j+1+num_topics
      aps[i,topix]=1
      aps[i,topix2]=rr
    }
  }
}

## merge data with topics and model
data8 = left_join(datas3,aps,by=c("action_plan_id"))
data8[is.na(data8)] = 0

## logistic model
model = glm(y ~ emissions.trend  + target +
              topic01 + topic02 + topic03 + topic04  + topic05 + topic06 + topic07 + topic08 + topic09 + topic10 + topic11 + topic12 + topic13 + topic14 ,  
            family=binomial(link='logit'), data=data8)

summary(model)
vif(model)
(R2_dev = 1-(model$deviance/model$null.deviance))

## linear regression on %reduction achieved 
model1 = lm(y2 ~ emissions.trend  + target_emis +
              topic01 + topic02 + topic03 + topic04  + topic05 + topic06 + topic07 + topic08 + topic09 + topic10 + topic11 + topic12 + topic13 + topic14, 
            data=X) 
summary(model1)
summary(model1)$adj.r.squared
vif(model1)
plot(model1)
lmtest::bptest(model1)


## confimation: check causality
df = data8
#df$topix = ifelse(df$topic13==1&df$topic11==1&df$topic08==1&df$topic03==1&df$topic04==1&df$topic10==1,1,0)
df1 = subset(df,topic13 == 0 ) ##minority 
df0 = subset(df,topic13 == 1 )

dd = dim(df1)[1]
ll=0
maxe = max(df0$emissions_pc)-min(df0$emissions_pc)
for(i in 1:dd){
  aux0 = df1[i,c("size","country_code","emissions_pc")]
  aux10 = df0[,c("size","country_code","emissions_pc")]

  l = dim(aux10)[1]
  for(b in 1:l){
    dista1 = sum(abs(aux0[,3]-aux10[b,3])/maxe) ## continuous vars 
    dista2 = sum(ifelse(aux0[,1]==aux10[b,1],0,1))## categorical vars
    dista3 = sum(ifelse(aux0[,2]==aux10[b,2],0,1))## categorical vars
    dista = (dista1/3)+(dista2/3)+(dista3/3)
    
    if(b==1){dists=dista}else{dists=rbind(dists,dista)}
  }
  
  mind = min(dists,na.rm=TRUE)
  mins_aux = which(dists %in% mind)[1] 
  matF = rbind(df1[i,],df0[mins_aux,]) 
  varsF = cbind(df1[i,c("organisation_name","size","country_code","emissions_pc")],df0[mins_aux,c("organisation_name","size","country_code","emissions_pc")])
  df0 = df0[-mins_aux,]
  
  if(i==1){
    data_new = matF
    dist_min = mind
    vars_new = varsF
  }else{
    data_new = rbind(data_new,matF)
    dist_min = rbind(dist_min,mind)
    vars_new = rbind(vars_new,varsF)
  }
  cat("\n",i)
}

modelb = glm(binary ~ emissions.trend  + target_emis + topic13,  family=binomial(link='logit'), data=data_new)
summary(modelb)
(R2_dev = 1-modelb$deviance/modelb$null.deviance)
vif(modelb)
