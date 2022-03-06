# Analysis-of-Sales-Report-of-a-Clothes-Manufacturing-Outlet
A high-end fashion retail store is looking to expand its products. It wants to understand the market and find the current trends in the industry. It has a database of all products with attributes, such as style, material, season, and the sales of the products over a period of two months.

#rm(list=ls()) #clear all previous variables
library(readxl) #to read excel
library(plyr)
library(caTools)
library(e1071)
library(caret)
library(randomForest)

attribute_data <- read_excel("C:/Users/Sahil/Desktop/Attribute DataSet.xlsx")
dress_sale <- read_excel("C:/Users/Sahil/Desktop/Dress Sales.xlsx")

View(attribute_data)
View(dress_sale)

#remove Dress_ID column
attribute_data <- attribute_data[2:14]
dress_sale <- dress_sale[2:24]

View(attribute_data)
View(dress_sale)

#check the unique values for each column
lapply(attribute_data[1:13],unique)

#values checking
#style

attribute_data$Style[attribute_data$Style=='sexy']='Sexy'

#price
attribute_data$Price[attribute_data$Price=='low']='Low'
attribute_data$Price[attribute_data$Price=='high']='High'

#Size
attribute_data$Size[attribute_data$Size=='s']='S'
attribute_data$Size[attribute_data$Size=='small']='S'

#Season
attribute_data$Season[attribute_data$Season=='spring']='Spring'
attribute_data$Season[attribute_data$Season=='summer']='Summer'
attribute_data$Season[attribute_data$Season=='Automn']= 'Autumn'
attribute_data$Season[attribute_data$Season=='winter']='Winter'

#Neckline
attribute_data$NeckLine[attribute_data$NeckLine=='sweetheart']='Sweetheart'

#SleeveLength
attribute_data$SleeveLength[attribute_data$SleeveLength=='sleevless'] = 'sleeveless'
attribute_data$SleeveLength[attribute_data$SleeveLength=='sleeevless'] = 'sleeveless'
attribute_data$SleeveLength[attribute_data$SleeveLength=='sleveless'] = 'sleeveless'
attribute_data$SleeveLength[attribute_data$SleeveLength== 'threequater'] = 'threequarter'
attribute_data$SleeveLength[attribute_data$SleeveLength=='thressqatar'] = 'threequarter'
attribute_data$SleeveLength[attribute_data$SleeveLength=='urndowncollor'] = 'turndowncollar'

#FabricType
attribute_data$FabricType[attribute_data$FabricType=='shiffon']='chiffon'
attribute_data$FabricType[attribute_data$FabricType=='sattin']='satin'
attribute_data$FabricType[attribute_data$FabricType=='wollen']='woolen'
attribute_data$FabricType[attribute_data$FabricType=='flannael']='flannel'
attribute_data$FabricType[attribute_data$FabricType=='knitting']='knitted'

#Decoration
attribute_data$Decoration[attribute_data$Decoration=='embroidary']='embroidery'
attribute_data$Decoration[attribute_data$Decoration=='sequined']='sequins'
attribute_data$Decoration[attribute_data$Decoration=='ruched']='ruche'
attribute_data$Decoration[attribute_data$Decoration=='none']='null'

#PatternType
attribute_data$`Pattern Type`[attribute_data$`Pattern Type`=='none']='null'
attribute_data$`Pattern Type`[attribute_data$`Pattern Type`=='leapord']='leopard'

#factoring
attribute_data$Style=factor(attribute_data$Style,
                            levels = c('Sexy','Casual','vintage','Brief','cute','bohemian','Novelty','Flare','party','work','OL','fashion'),
                            labels = c(0,1,2,3,4,5,6,7,8,9,10,11))
                            
                         
                           
attribute_data$Price=factor(attribute_data$Price,
                            levels=c('Low','High','Average','Medium','very-high'),
                            labels = c(0,1,2,3,4))                           

attribute_data$Size=factor(attribute_data$Size,
                           levels=c('M','L','XL','free','S'),
                           labels=c(0,1,2,3,4))

attribute_data$Season=factor(attribute_data$Season,
                           levels = c('Summer', 'Autumn', 'Spring', 'Winter'),
                           labels = c(0,1,2,3))

attribute_data$NeckLine=factor(attribute_data$NeckLine,
                            levels = c('o-neck', 'v-neck', 'boat-neck', 'peterpan-collor', 'ruffled', 'turndowncollor', 'slash-neck', 'mandarin-collor', 'open', 'sqare-collor', 'Sweetheart', 'Scoop', 'halter', 'backless', 'bowneck', 'NULL'),
                            labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

attribute_data$SleeveLength=factor(attribute_data$SleeveLength,
                                levels = c('sleeveless', 'Petal', 'full', 'butterfly', 'short', 'threequarter', 'halfsleeve', 'cap-sleeves', 'turndowncollor', 'capsleeves', 'half', 'turndowncollar', 'NULL'),
                                labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

attribute_data$waiseline=factor(attribute_data$waiseline,
                                levels = c('empire','natural','null','princess','dropped'),
                                labels = c(0,1,2,3,4))

attribute_data$Material=factor(attribute_data$Material,
                               levels =c('null','microfiber','polyster','silk','chiffonfabric','cotton','nylon','other','milksilk','linen','rayon','lycra','mix','acrylic','spandex','lace','modal','cashmere','viscos','knitting','sill','wool','model','shiffon'),
                               labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

attribute_data$FabricType=factor(attribute_data$FabricType,
                                 levels = c('chiffon','null','broadcloth','jersey','other','batik','satin','flannel','worsted','woolen','poplin','dobby','knitted','tulle','organza','lace','Corduroy','terry'),
                                 labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))

attribute_data$Decoration=factor(attribute_data$Decoration,
                                 levels = c('ruffles','null','embroidery','bow','lace','beading','sashes','hollowout','pockets','sequins','applique','button','Tiered','rivet','feathers','flowers','pearls','pleat','crystal','ruche','draped','tassel','plain','cascading'),
                                 labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

attribute_data$`Pattern Type`=factor(attribute_data$`Pattern Type`,
                                     levels = c('animal','print','dot','solid','null','patchwork','striped','geometric','plaid','leopard','floral','character','splice'),
                                     labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

attribute_data$Recommendation=sapply(attribute_data$Recommendation,factor)

#counts of missing values in attribute_dataset

colSums(is.na(attribute_data))

#create the function

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#fill missing value with mode

attribute_data$Price[is.na(attribute_data$Price)==TRUE] <- getmode(attribute_data$Price)

attribute_data$Season[is.na(attribute_data$Season)==TRUE] <- getmode(attribute_data$Season)

attribute_data$NeckLine[is.na(attribute_data$NeckLine)==TRUE] <- getmode(attribute_data$NeckLine)

attribute_data$SleeveLength[is.na(attribute_data$NeckLine)==TRUE] <- getmode(attribute_data$SleeveLength)

attribute_data$waiseline[is.na(attribute_data$waiseline)==TRUE] <- getmode(attribute_data$waiseline)

attribute_data$Material[is.na(attribute_data$Material)==TRUE] <- getmode(attribute_data$Material)

attribute_data$FabricType[is.na(attribute_data$FabricType)==TRUE] <- getmode(attribute_data$FabricType)

attribute_data$Decoration[is.na(attribute_data$Decoration)==TRUE] <- getmode(attribute_data$Decoration)

attribute_data$`Pattern Type`[is.na(attribute_data$`Pattern Type`)==TRUE] <- getmode(attribute_data$`Pattern Type`)

attribute_data_data <- data.frame(attribute_data)
str(attribute_data_data)

#Update columns name in dress_sale dataset

dress_sale = rename(dress_sale,c('41314'='2/9/2013'))
dress_sale = rename(dress_sale,c('41373'='4/9/2013'))
dress_sale = rename(dress_sale,c('41434'='6/9/2013'))
dress_sale = rename(dress_sale,c('41495'='8/9/2013'))
dress_sale = rename(dress_sale,c('41556'='10/9/2013'))
dress_sale = rename(dress_sale,c('41617'='12/9/2013'))
dress_sale = rename(dress_sale,c('41315'='2/10/2013'))
dress_sale = rename(dress_sale,c('41374'='4/10/2013'))
dress_sale = rename(dress_sale,c('41435'='6/10/2013'))
dress_sale = rename(dress_sale,c('40400'='8/10/2013'))
dress_sale = rename(dress_sale,c('41557'='10/10/2013'))
dress_sale = rename(dress_sale,c('41618'='12/10/2013'))

#Convert all variable types to numeric

dress_sale <- as.data.frame(apply(dress_sale,2,as.numeric))

#mean row

dress_sale=as.matrix(dress_sale)
k <- which(is.na(dress_sale),arr.ind = TRUE)
dress_sale[k] <- rowMeans(dress_sale,na.rm = TRUE)[k[,1]]
dress_sale=as.data.frame(dress_sale)

#sum of all values on row on total sales

dress_sale$total_sales=rowSums(dress_sale)
head(dress_sale)

#merge data

merge_data <- data.frame(attribute_data,dress_sale)
head(merge_data)

str(merge_data)

#splitting dataset 
set.seed(100)

spl=sample.split(merge_data$Recommendation,SplitRatio = 0.7)
train=subset(merge_data,spl==TRUE)
test=subset(merge_data,spl==FALSE)

print(dim(train));print(dim(test))

#naive bayes model
naive_model=naiveBayes(Recommendation~.,data=train) #build model

confusionMatrix(train$Recommendation,predict(naive_model,train),positive = '1') #create a confusion matrix
print('-----------------')

naive_predict=predict(naive_model,test) #predict test set
table(naive_predict,test$Recommendation) #create table

#Support vector machine
svm_model=svm(Recommendation~.,train) #build model
confusionMatrix(train$Recommendation,predict(svm_model),positive = '1') #create a confusion matrix

svm_predict=predict(svm_model,test) #predict test set
table(svm_predict,test$Recommendation) #create table

#Random forest
randomForest_model=randomForest(x=train,y=train$Recommendation,ntree=800) #build model
confusionMatrix(train$Recommendation,predict(randomForest_model),positive = '1') #create confusion matrix

randomForest_predict=predict(randomForest_model,test) #predict test set
table(randomForest_predict,test$Recommendation) #create table

#regression (total sales and (style+season+material+price))

regression_sales=lm(formula = total_sales~ Style+Season+Material+Price,data=train) #build model
summary(regression_sales) #print model summary

plot(regression_sales,pch=16,col="blue") #plot the results
<Return>

 

abline(regression_sales) #add regression line

#regression (total sales and rating)
regression_Rating=lm(formula=total_sales~Rating,data = train) #build model
summary(regression_Rating) #print model summary

plot(regression_Rating,pch=16,col="red") #plot the results
abline(regression_Rating) #add regression line
<Return>
  

 #evaluation
original=test$total_sales
pred= predict(regression_Rating,test)
predicted=pred
d=original-predicted
mse=mean((d)^2)
mae=mean(abs(d))
rmse=sqrt(mse)
R2=1-(sum((d)^2)/sum((original-mean(original))^2))
cat("MAE:",mae,"\n","MSE:",mse,"\n","RMSE:",rmse,"\n","R-squared:",R2)
