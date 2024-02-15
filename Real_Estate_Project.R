setwd("C:/Users/91878/Downloads/R files Edvancer/")
getwd()


library(dplyr)


house_train=read.csv("C:\\Users\\91878\\Downloads\\R files Edvancer\\housing_train.csv",stringsAsFactors = F)
house_test=read.csv("C:\\Users\\91878\\Downloads\\R files Edvancer\\housing_test.csv",stringsAsFactors = F)

glimpse(house_test)
glimpse(house_train)

house_test$Price=NA

house_train$data='train' #making column in tain called data
house_test$data='test'  #making new column called data

house_all=rbind(house_train,house_test)


##Start Data preparation one by one

#Suburb - create dummies
#Address - Drop it
#Rooms - alredy in numeric
#Type - create dummies
#Method - create numeric
#SellerG - drop it
#Distance - already in numeric
#Postcode - create dummy
#Bedroom2 - already in integer
#Bathroom - already in integer
#Car - already in integer
#Landsize   - already in integer
#BuildingArea - already in integer
#YearBuilt -replace blank space with mode and create dummies 
# CouncilArea -drop it

# Check Missing value
lapply(house_all,function(x) sum(is.na(x)))

# Drop some var
house_all=house_all %>%
  select(-Address,-CouncilArea,-SellerG)

##Cleaning##

house_all$Bedroom2[is.na(house_all$Bedroom2)] <- mean(house_all$Bedroom2, na.rm = TRUE)

house_all$Bathroom[is.na(house_all$Bathroom)] <- mean(house_all$Bathroom, na.rm = TRUE)

house_all$Car[is.na(house_all$Car)] <- mean(house_all$Car, na.rm = TRUE)

house_all$Landsize[is.na(house_all$Landsize)] <- mean(house_all$Landsize, na.rm = TRUE)

house_all$BuildingArea[is.na(house_all$BuildingArea)] <- mean(house_all$BuildingArea, na.rm = TRUE)

house_all$YearBuilt[is.na(house_all$YearBuilt)] <- mean(house_all$YearBuilt, na.rm = TRUE)

#Lets see if there are any missing values in the data.

lapply(house_all,function(x) sum(is.na(x)))


# Creating Dummy variables
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

house_all=CreateDummies(house_all,"Method",100)
house_all=CreateDummies(house_all,"Type",100)
house_all=CreateDummies(house_all,"Suburb",100)
house_all=CreateDummies(house_all,"Postcode",100)

View(house_all)


###Now we are done with preparing data , lets separate the data 

house_train=house_all %>% filter(data=='train') %>% select(-data)
house_test=house_all %>% filter(data=='test') %>% select(-data,-Price)

# 80:30 ratio

set.seed(2)
s=sample(1:nrow(house_train),0.8*nrow(house_train))
house_train1=house_train[s,]   ##( we have R=6028 and C=86)
house_train2=house_train[-s,]  ##(we have R=1508 and C=86)

# Build a linear model
library(car)
fit=lm(Price~., data= house_train1)
summary(fit)

fit=lm(Price~.-Postcode_3073-Postcode_3165-Postcode_3058-Postcode_3204-Postcode_3072
       -Postcode_3072-Postcode_3182-Postcode_3141-Postcode_3056-Postcode_3146-Postcode_3186
       -Postcode_3101-Postcode_3070-Postcode_3104-Postcode_3122-Postcode_3187-Postcode_3044
       -Postcode_3207-Postcode_3124-Postcode_3103-Postcode_3013-Postcode_3071-Postcode_3039
       -Postcode_3108-Postcode_3184-Postcode_3142-Postcode_3123-Postcode_3033-Postcode_3055
       -Postcode_3107-Postcode_3000-Postcode_3125-Postcode_3143-Postcode_3025-Postcode_3060
       -Postcode_3034-Postcode_3057-Postcode_3205-Postcode_3189-Postcode_3167-Postcode_3051
       -Method_PI-Postcode_3185, data= house_train1)
summary(fit)

sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-Postcode_3020-Postcode_3121-Postcode_3032-Postcode_3040-Postcode_3163-
         Postcode_3084-Postcode_3042-Postcode_3181-Postcode_3081-Postcode_3145-Postcode_3147-
         Postcode_3016-Postcode_3105-Postcode_3078-Postcode_3144-Postcode_3018-Suburb_Richmond
       -Suburb_SouthYarra-Suburb_StKilda-Suburb_Essendon-Suburb_Glenroy-Suburb_Northcote-Suburb_PortMelbourne
       -Suburb_Carnegie-Suburb_Footscray-Suburb_AscotVale-Suburb_Newport-Suburb_Maribyrnong
       -Suburb_Prahran-Suburb_SurreyHills-Suburb_Kensington-Suburb_Sunshine-Suburb_TemplestoweLower
       -Suburb_Burwood-Suburb_WestFootscray-Suburb_Strathmore-Suburb_Bulleen-Suburb_FitzroyNorth-Suburb_Niddrie
       -Suburb_Maidstone-Suburb_AirportWest-Suburb_Rosanna-Suburb_SouthMelbourne-Suburb_Moorabbin
       -Suburb_Malvern-Suburb_HeidelbergHeights-Suburb_Murrumbeena-Suburb_OakleighSouth
       -Suburb_HeidelbergWest-Suburb_Hadfield, data= house_train1)
summary(fit)

fit=lm(Price~.-Method_SP-Method_PI-Suburb_NorthMelbourne-Suburb_Melbourne-Suburb_MooneePonds-Suburb_Thornbury
       -Postcode_3051-Postcode_3167-Postcode_3189-Postcode_3205-Postcode_3057-Postcode_3034-Postcode_3060
       -Postcode_3025-Postcode_3143-Postcode_3000-Postcode_3107-Postcode_3055-Postcode_3033-Postcode_3123
       -Postcode_3142-Postcode_3184-Postcode_3108-Postcode_3068-Postcode_3039-Postcode_3071-Postcode_3013
       -Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104
       -Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3072-Postcode_3204
       -Postcode_3058-Postcode_3165-Postcode_3073, data= house_train1)
summary(fit)

fit=lm(Price~.-Suburb_Hadfield-Suburb_HeidelbergWest-Suburb_OakleighSouth-Suburb_Murrumbeena-Suburb_HeidelbergHeights
       -Suburb_Malvern-Suburb_Moorabbin-Suburb_SouthMelbourne-Suburb_Rosanna-Suburb_Niddrie-Suburb_Maidstone-Suburb_AirportWest
       -Suburb_FitzroyNorth-Suburb_Bulleen-Suburb_Strathmore-Suburb_WestFootscray-Suburb_Williamstown-Suburb_TemplestoweLower
       -Suburb_Prahran-Suburb_SurreyHills-Suburb_Kensington-Suburb_Sunshine-Suburb_Maribyrnong-Suburb_Newport-Suburb_AscotVale
       -Suburb_Footscray-Suburb_Carnegie-Suburb_Northcote-Suburb_Glenroy-Suburb_Essendon-Suburb_Richmond-Postcode_3018-Postcode_3144
       -Postcode_3078-Postcode_3105-Postcode_3125-Postcode_3016-Postcode_3147-Postcode_3145-Postcode_3081-Postcode_3181-Postcode_3042
       -Postcode_3084-Postcode_3141-Postcode_3182-Postcode_3163-Postcode_3040-Postcode_3032-Postcode_3121-Postcode_3020, data= house_train1)
summary(fit)

fit=lm(Price~.-Suburb_Hadfield-Suburb_HeidelbergWest-Postcode_3051-Postcode_3167-Postcode_3018              
       -Postcode_3189-Postcode_3205-Postcode_3144-Postcode_3185-Postcode_3057-Postcode_3078-Postcode_3044                 
       -Postcode_3105-Postcode_3034-Postcode_3060-Postcode_3025-Postcode_3143-Postcode_3125                 
      -Postcode_3000-Postcode_3107-Postcode_3055-Postcode_3033-Postcode_3123-Postcode_3016               
      -Postcode_3142-Postcode_3184-Postcode_3108-Postcode_3039-Postcode_3071-Postcode_3147            
      -Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3145-Postcode_3081-Postcode_3207                                    
      -Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3181-Postcode_3042-Postcode_3070        
      -Postcode_3101-Postcode_3186-Postcode_3084-Postcode_3146-Postcode_3056-Postcode_3141                      
      -Postcode_3182-Postcode_3072-Postcode_3204-Postcode_3058-Postcode_3163-Postcode_3040         
      -Postcode_3032-Postcode_3121-Postcode_3165-Postcode_3073-Postcode_3020-Postcode_3079-Suburb_Ivanhoe-Distance
      ,data= house_train1)            
summary(fit)
# we'll take vif cutoff as 5
sort(vif(fit),decreasing = T)[1:5]

fit=stats::step(fit)
summary(fit)
formula(fit)

fit=lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
         YearBuilt + Method_PI + Method_S + Type_u + Type_h + Suburb_NorthMelbourne + 
         Suburb_OakleighSouth + Suburb_CoburgNorth + Suburb_HeidelbergHeights + 
         Suburb_Malvern + Suburb_Moorabbin + Suburb_SouthMelbourne + 
         Suburb_Ashburton + Suburb_Rosanna + Suburb_Niddrie + Suburb_AirportWest + 
         Suburb_Bulleen + Suburb_Ormond + Suburb_Strathmore + Suburb_SunshineNorth + 
         Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
         Suburb_Armadale + Suburb_Williamstown + Suburb_Melbourne + 
         Suburb_SunshineWest + Suburb_TemplestoweLower + Suburb_BrunswickWest + 
         Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
         Suburb_Sunshine + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
         Suburb_Doncaster + Suburb_AscotVale + Suburb_Hampton + Suburb_Yarraville + 
         Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
         Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_PascoeVale + 
         Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
         Suburb_Coburg + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
         Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
         Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
         Suburb_Reservoir + Postcode_3206 + Postcode_3041 + Postcode_3068 + 
         Postcode_3127 + Postcode_3015 + Postcode_3011 + Postcode_3012 + 
         Postcode_3046, data=house_train1)
summary(fit)

rmse= mean((house_train2$Price-predict(fit,newdata=house_train2))**2) %>%
  sqrt()
rmse   #0.53

fit.final=fit=lm(Price ~ Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
                   YearBuilt + Method_PI + Method_S + Type_u + Type_h + Suburb_NorthMelbourne + 
                   Suburb_OakleighSouth + Suburb_CoburgNorth + Suburb_HeidelbergHeights + 
                   Suburb_Malvern + Suburb_Moorabbin + Suburb_SouthMelbourne + 
                   Suburb_Ashburton + Suburb_Rosanna + Suburb_Niddrie + Suburb_AirportWest + 
                   Suburb_Bulleen + Suburb_Ormond + Suburb_Strathmore + Suburb_SunshineNorth + 
                   Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
                   Suburb_Armadale + Suburb_Williamstown + Suburb_Melbourne + 
                   Suburb_SunshineWest + Suburb_TemplestoweLower + Suburb_BrunswickWest + 
                   Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
                   Suburb_Sunshine + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
                   Suburb_Doncaster + Suburb_AscotVale + Suburb_Hampton + Suburb_Yarraville + 
                   Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
                   Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_PascoeVale + 
                   Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
                   Suburb_Coburg + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                   Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
                   Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
                   Suburb_Reservoir + Postcode_3206 + Postcode_3041 + Postcode_3068 + 
                   Postcode_3127 + Postcode_3015 + Postcode_3011 + Postcode_3012 + 
                   Postcode_3046, data=house_train1)

fit.final=step(fit.final)

summary(fit.final)

#We can use this model to make prediction on the test data
pred.IR=predict(fit.final,newdata=house_test)
write.csv(pred.IR,"mysubmission.csv",row.names = F)


#Random forest 
library(cvTools)
library(gbm)

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))
subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)

myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  params=my_params[i,]
  k=cvTuning(randomForest,Price~.,
             data =house_train,
             tuning =params,
             folds = cvFolds(nrow(house_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  print('DONE')
  
}

myerror
best_params

house.rf.final=randomForest(Price~.,
                            mtry=best_params$mtry,
                            ntree=best_params$ntree,
                            maxnodes=best_params$maxnodes,
                            nodesize=best_params$nodesize,
                            data=house_train)

test.pred=predict(house.rf.final,newdata = house_test)

write.csv(test.pred,"mysubmission.csv",row.names = F)

# Model Decision tree
library(tree)
library(randomForest)

house.tree=randomForest(Price~.,data=house_train1)
val.IR=predict(house.tree,newdata = house_train2)

rmse_val=((val.IR)-(house_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val
#####328376.7

Score =212467/rmse_val

##We can now build the model on entire training data and use that
##to eventually make prediction for test

house.tree.final=tree(Price~.,data=house_train)
test.pred=predict(house.tree,newdata=house_test)
write.csv(test.pred,"Sanskruti_Zodape_P1_part2.csv",row.names = F)



Price_df <- data.frame(Price = test.pred)
house_test$Price = test.pred###adding predicted value in house_test
table(house_test$Price)


#GB Model
library(gbm)
library(lattice)       
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

  
num_trails=10
my_params=subset_paras(param,num_trails)
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  
  params=my_params[i,]
  
  k=cvTuning(gbm,Price~.,
             data =house_train1,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(house_train1), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  
  print('DONE')
  
}

best_params=data.frame(interaction.depth=7,
                       n.trees=500,
                       shrinkage=0.1,
                       n.minobsnode=1)

myerror
best_params
house_train1.gbm.final=gbm(Price~.,data=house_train1,
                        n.trees = best_params$n.trees,
                        n.minobsinnode = best_params$n.minobsnode,
                        shrinkage = best_params$shrinkage,
                        interaction.depth = best_params$interaction.depth,
                        distribution = "gaussian")

test.pred=predict(house_train1.gbm.final,newdata=house_test,n.trees = best_params$n.trees)
write.csv(test.pred,"sanskruti_zodape.csv",row.names = F)

