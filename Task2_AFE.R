
#### ---- Libraries ----
library("readr")
library("ggplot2")
library("reshape2") #For the correlation matrix as a heatmap
library("ggcorrplot") #For the correlation matrix as a heatmap
library("corrplot")  
library("GGally") #It is like an extension of ggplot2
library("caret") #To dummify variables
library("dplyr") #Pipelines
library("ggpubr") #For one way anove test
library("tidyverse")  
library("corrr") #Extra work with correlation matrix
library("reshape") #Reorder things, like the correlation matrix
library("caret") #For the models (Random Forest)
library("party") #For ctree function (Decision Tree)
library("Hmisc") #To reorder the correlation matrix (Pairwise)
library("plotly") #Nice Violin Plots!
library("readxl")
library("rpart.plot") #For nice plotting of Decision Tree 
library("partykit") #Playing with decision tree plottings
library("scales") #To add percentage of the piechart

#### ---- Functions ----
  #For plotting the confusion matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Acer', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Sony', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Acer', cex=1.2, srt=90)
  text(140, 335, 'Sony', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

#### ---- Load Data ----
CompleteData<-read.csv("CompleteResponses.csv")
colnames(CompleteData)<-c("Salary", "Age","EducationLevel", "Car", "ZipCode", "Credit", "Brand")
IncompleteData<-read.csv("SurveyIncomplete.csv")
colnames(IncompleteData)<-c("Salary", "Age","EducationLevel", "Car", "ZipCode", "Credit", "Brand")

#Numerical to Factor Conversion

CompleteData$Brand <- factor(CompleteData$Brand,
                       levels = c(0,1),
                       labels = c("Acer","Sony"))


CompleteData$ZipCode <- factor(CompleteData$ZipCode,
                         levels = c(0:8),
                         labels = c("New England","Mid-Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))

CompleteData$Car <- factor(CompleteData$Car,
                     levels = c(1:20),
                     labels = c("BMW","Buick","Cadillac","Chevrolet", "Chrysler","Dodge","Ford","Honda","Hyundai","Jeep","Kia","Lincoln","Mazda","Mercedes Benz","Mitsubishi","Nissan","Ram","Subaru","Toyota","None of the above"))

CompleteData$EducationLevel <- ordered(CompleteData$EducationLevel,
                         levels = c(0:4), 
                         labels = c("Less than High School Degree","High School Degree","Some College","4-Year College Degree", "Master, Doctoral or Professional Degree"))

IncompleteData$Brand <- factor(IncompleteData$Brand,
                             levels = c(0,1),
                             labels = c("NA","NA"))


IncompleteData$ZipCode <- factor(IncompleteData$ZipCode,
                               levels = c(0:8),
                               labels = c("New England","Mid-Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))

IncompleteData$Car <- factor(IncompleteData$Car,
                           levels = c(1:20),
                           labels = c("BMW","Buick","Cadillac","Chevrolet", "Chrysler","Dodge","Ford","Honda","Hyundai","Jeep","Kia","Lincoln","Mazda","Mercedes Benz","Mitsubishi","Nissan","Ram","Subaru","Toyota","None of the above"))

IncompleteData$EducationLevel <- ordered(IncompleteData$EducationLevel,
                                       levels = c(0:4), 
                                       labels = c("Less than High School Degree","High School Degree","Some College","4-Year College Degree", "Master, Doctoral or Professional Degree"))

#### ---- Data Exploration ----
  
  #Pie chart for Brand Preference

   piechart<-as.data.frame(table(CompleteData$Brand))
   piechart$percentage <- round(100 * piechart$Freq / sum(piechart$Freq)) 
   
   ggplot(data = piechart, aes(x = 0, y = percentage, fill = Var1)) + 
     geom_bar(stat = "identity") +
     geom_text(aes(label = percent(percentage/100)), position = position_stack(vjust = 0.5)) +
     scale_x_continuous(expand = c(0,0)) +
     labs(fill = 'Type', title = 'Brand Preference of the Complete Data') +
     coord_polar(theta = "y") +
     theme_void()
    
  #More Data Exploration
    ggplot(CompleteData, aes(x=Brand, fill=Brand)) + geom_bar() + theme_bw()
      ggtitle("Brand Preference Distribution") + ylab("Counts")+ theme(plot.title = element_text(hjust = 0.5))
    
  #Barchart with educational level added 
    ggplot(CompleteData, aes(x = EducationLevel, fill = Brand))+ ylab("Counts")+
      theme_bw() + geom_bar() + ggtitle("Preferred Brand per Educational Level")+
      theme(axis.text.x = element_text(angle=60, hjust=1)) + theme(plot.title = element_text(hjust = 0.5))
    
    #Barchart with Car
    ggplot(CompleteData, aes(x = Car, fill = Brand)) +
      theme_bw() + geom_bar() + ggtitle("Preferred Brand per Car")+
      theme(axis.text.x = element_text(angle=60, hjust=1))  
    
    #Barchart with ZipCode
    ggplot(CompleteData, aes(x = ZipCode, fill = Brand)) +
      theme_bw() + geom_bar() + ggtitle("Preferred Brand per ZipCode")+
      theme(axis.text.x = element_text(angle=60, hjust=1)) 
    
    # Histogram of salary and brand
    ggplot(CompleteData, aes(x=Salary, fill=Brand)) + geom_histogram(color="black", bins=30) + theme_classic()
    ggtitle("Brand Preference Salary Distribution") + ylab("Counts")+ theme(plot.title = element_text(hjust = 0.5))
    
    # Histogram of age and brand
    ggplot(CompleteData, aes(x=Age, fill=Brand)) + geom_histogram(color="black", bins=30) + theme_classic()
    ggtitle("Brand Preference Age Distribution") + ylab("Counts")+ theme(plot.title = element_text(hjust = 0.5))
    
    # Histogram of credit and brand
    ggplot(CompleteData, aes(x=Credit, fill=Brand)) + geom_histogram(color="black", bins=30) + theme_classic()
    ggtitle("Brand Preference Age Distribution") + ylab("Counts")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Density Plot Brand and Salary
    ggplot(CompleteData, aes(x = Salary,  fill = Brand)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Brand Preference Salary Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Density Plot Brand and Age
    ggplot(CompleteData, aes(x = Age,  fill = Brand)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Brand Preference Age Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Density Plot Brand and Credit
    ggplot(CompleteData, aes(x = Credit,  fill = Brand)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Brand Preference Credit Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    hist(CompleteData$Car)
    
    #Density Plot Salary
    ggplot(CompleteData, aes(x = Salary)) + theme_bw() + 
      geom_density(alpha = 0.8, color="darkblue", fill="lightblue") + ggtitle("Salary Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Density Plot Age
    ggplot(CompleteData, aes(x = Age)) + theme_bw() + 
      geom_density(alpha = 0.8,color="darkblue", fill="lightblue") + ggtitle("Age Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Density Plot  Credit
    ggplot(CompleteData, aes(x = Credit)) + theme_bw() + 
      geom_density(alpha = 0.8,color="darkblue", fill="lightblue") + ggtitle("Credit Distribution") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    hist(CompleteData$Car)
    
    #Scatteplot of Age and Salary coloured by Brand
    ggplot(CompleteData, aes(x = Age,  y = Salary, color = Brand)) +
      theme_bw() + geom_point(position = "jitter") +
      ggtitle("Age and Salary by Brand Preference") + ylab("Salary")+ xlab("Age")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Scatteplot of Age and Credit coloured by Brand
    ggplot(CompleteData, aes(x = Age,  y = Credit, color = Brand)) +
      theme_bw() + geom_point(position = "jitter") +
      ggtitle("Age and Credit by Brand Preference") + ylab("Credit")+ xlab("Age")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Distribution of EducationLevel, ZipCode and Car by Age and Salary
    ggplot(CompleteData, aes(x = Age,  fill = EducationLevel)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Education Level distribution by Age") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(CompleteData, aes(x = Age, fill=Car)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Car distribution by Age") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(CompleteData, aes(x = Age,  fill = ZipCode)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("ZipCode distribution by Age") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(CompleteData, aes(x = Salary,  fill = EducationLevel)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Education Level distribution by Salary") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(CompleteData, aes(x = Salary, fill=Car)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("Car distribution by Salary") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(CompleteData, aes(x = Salary,  fill = ZipCode)) + theme_bw() + 
      geom_density(alpha = 0.8) + ggtitle("ZipCode distribution by Salary") + ylab("Density")+ theme(plot.title = element_text(hjust = 0.5))
    
    #Columns Distribution per categories: Car, Region and Education Level
    Car_Freq<- CompleteData %>%group_by(Car, Brand) %>% summarise(Freq=n())
    ggplot() + geom_col(data = Car_Freq, aes(x = Car, y = Freq, fill = Brand), position = "fill")+
    ggtitle("Brand Distribution by Car") + ylab("Frequency")+ theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=60, hjust=1)) 
    
    EducationLevel_Freq<- CompleteData %>%group_by(EducationLevel, Brand) %>% summarise(Freq=n())
    ggplot() + geom_col(data = EducationLevel_Freq, aes(x = EducationLevel, y = Freq, fill = Brand), position = "fill")+
      ggtitle("Brand Distribution by Education Level") + ylab("Frequency")+ theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle=60, hjust=1)) 
    
    ZipCode_Freq<- CompleteData %>%group_by(ZipCode, Brand) %>% summarise(Freq=n())
    ggplot() + geom_col(data = ZipCode_Freq, aes(x = ZipCode, y = Freq, fill = Brand), position = "fill")+
      ggtitle("Brand Distribution by Zip Code") + ylab("Frequency")+ theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle=60, hjust=1)) 
    
#### ---- Preprocessing & Feature Engineering ----
    #No observed outliers
    boxplot(CompleteData$Salary)
    boxplot(CompleteData$Age)
#### ---- Correlation Analysis ----
  #### ---- Chi Square Analysis -----
    chi_EducationLevel_Brand <- chisq.test(CompleteData$EducationLevel,CompleteData$Brand)
    chi_EducationLevel_Brand
    
    chi_Car_Brand <- chisq.test(CompleteData$Car,CompleteData$Brand)
    chi_Car_Brand
    
    chi_ZipCode_Brand <- chisq.test(CompleteData$ZipCode,CompleteData$Brand)
    chi_ZipCode_Brand
    
    #Bining Age variable to make a Chi-Square Test
      #Vector for breakpoints
      b <- c(-Inf, 40, 60, Inf)
      #Vector for naming the breakpoints
      names <- c("20-40_Young", "40-60_Medium", "60-80_Old")
      #Cut the variable using the breakpoints
      CompleteData$AgeCategory <- cut(CompleteData$Age, breaks = b, labels = names)
      #Chi-Square Test
      chi_Age_Brand <- chisq.test(CompleteData$AgeCategory,CompleteData$Brand)
      chi_Age_Brand
    
  #### ---- One Way Anova Analysis ----
    
    #Anova for Brand and Age
    CompleteData %>% group_by(Brand) %>% summarise(count = n(), mean = mean(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE))
    ggboxplot(CompleteData, x = "Brand", y = "Age", color = "Brand",ylab = "Age", xlab = "Brand")
    anova_Age_Brand <- aov(Age ~ Brand, data = CompleteData)
    summary(anova_Age_Brand)
    TukeyHSD(anova_Age_Brand)
    plot(TukeyHSD(anova_Age_Brand))
    
    #Anova for Brand and Salary
    CompleteData %>% group_by(Brand) %>% summarise(count = n(), mean = mean(Salary, na.rm = TRUE), sd = sd(Salary, na.rm = TRUE))
    ggboxplot(CompleteData, x = "Brand", y = "Salary", color = "Brand",ylab = "Salary", xlab = "Brand")
    anova_Salary_Brand <- aov(Salary ~ Brand, data = CompleteData)
    summary(anova_Salary_Brand)
    TukeyHSD(anova_Salary_Brand)
    plot(TukeyHSD(anova_Salary_Brand))
    
    #Anova for Brand and Credit
    CompleteData %>% group_by(Brand) %>% summarise(count = n(), mean = mean(Credit, na.rm = TRUE), sd = sd(Credit, na.rm = TRUE))
    ggboxplot(CompleteData, x = "Brand", y = "Credit", color = "Brand",ylab = "Credit", xlab = "Brand")
    anova_Credit_Brand <- aov(Credit ~ Brand, data = CompleteData)
    summary(anova_Credit_Brand)
    TukeyHSD(anova_Credit_Brand)
    plot(TukeyHSD(anova_Credit_Brand))
    
    #Correlation between Age and Salary
    cor(CompleteData$Age, CompleteData$Salary)
    
  #### ---- Decission Tree ----
    
    my_tree <- train(Brand ~ ., data = CompleteData, method = "rpart")
    my_tree
    rpart.plot(my_tree$finalModel)
    
#### ---- Modelling ----
    #### ---- Create Data Partition & Fit Control for Cross Validation ----
    set.seed(123)
    training_indices <- createDataPartition(CompleteData$Brand, p=.70, list = FALSE)
    TrainingSet <- CompleteData[training_indices,]
    TestingSet <- CompleteData[-training_indices,]
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
    
    #### ---- Random Forest & C5.0 & kNN & GBM---- It took me 36min to compute
    start_time <- Sys.time()
   
    a <- c("Brand ~ Salary", "Brand ~ Salary + Age","Brand ~ Salary + Age + ZipCode")
    b <- c("knn","gbm", "rf","C5.0")
    compare_models <- c()
    predictions<-c()
    
    for ( i in a) {
      for (j in b) {
        Fit <- train(formula(i), data = TrainingSet, method = j, trControl = fitControl, tuneLength = 5, preProcess = c("center", "scale"), na.action = na.omit )
        pred <- predict(Fit, TestingSet)
        res <- postResample(pred, TestingSet$Brand)
        compare_models <- cbind(compare_models,res)
        predictions<-cbind(predictions,pred)
        }
    }
    
    names_columns <- c()
    
    for (i in a) {
      for(j in b) {
        names_columns <- append(names_columns,paste(i,j))
      }
    }
    
    colnames(compare_models) <- names_columns
    colnames(predictions) <- names_columns
    
    run_time <- Sys.time() - start_time
    run_time
    
    #Melting the Data
    
    compare_models_melt <- melt(compare_models, varnames = c("metric", "model"))
    compare_models_melt <- as.data.frame(compare_var_mod_melt)
    compare_models_melt
    
    #Plotting de Metrics
    ggplot(compare_models_melt, aes(x=model, y=value))+
      geom_col(fill="darkblue", width=0.6)+ facet_grid(metric~., scales="free")+
      ggtitle("Metrics Comparison for each tested model")+ ylab("") + xlab("")+ theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle=60, hjust=1))
    
    #Plotting in Descending order
    
    ggplot(compare_models_melt, aes(x=reorder(model, -value), y=value))+
      geom_col(fill="darkblue", width=0.6)+ facet_grid(metric~., scales="free")+
      ggtitle("Metrics Comparison for each tested model")+ ylab("") + xlab("")+ theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text.x = element_text(angle=60, hjust=1))
    
    
    #### ---- Runing the best model:GBM with Salary and Age ---- 
    
    gbm_model <- train(Brand~Age+Salary, data = TrainingSet, method = "gbm", trControl = fitControl, tuneLength = 5, preProcess = c("center", "scale"), na.action = na.omit )
    gbm_model
    PredictionBrand_gbm <- predict(gbm_model, TestingSet)
    postResample(PredictionBrand_gbm, TestingSet$Brand)
    
    ConfusionMatrix_gbm<-confusionMatrix(TestingSet$Brand,PredictionBrand_gbm)
    ConfusionMatrix_gbm$table
    draw_confusion_matrix(ConfusionMatrix_gbm)

#### ---- Prediction in the new Dataset ----    
    IncompleteData$PredictedBrand <- predict(gbm_model,IncompleteData)
    
    #Visualization
    ggplot(IncompleteData, aes(x = Age,  y = Salary, color = PredictedBrand)) +
      theme_bw() + geom_point(position = "jitter") +
      ggtitle("Age and Salary by Predicted Brand Preference") + ylab("Salary")+ xlab("Age")+ theme(plot.title = element_text(hjust = 0.5))
    
    
    #PieChart with the total amount of the predicted
    piechart_predicted<-as.data.frame(table(IncompleteData$PredictedBrand))
    piechart_predicted$percentage<-round(100*piechart_predicted$Freq/sum(piechart_predicted$Freq))
    
    ggplot(data = piechart_predicted, aes(x = 0, y = percentage, fill = Var1)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = percent(percentage/100)), position = position_stack(vjust = 0.5)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(fill = 'Brand', title = 'Brand Preference of the new Data Set') +
      coord_polar(theta = "y") +
      theme_void()+theme(plot.title = element_text(hjust = 0.5))
    
    
#### ---- Error Analysis ----
    #Make variables integers for substracting data
    PredictionBrand_gbm
    integers_PredictionBrand_gbm <- as.integer(PredictionBrand_gbm)-1
    integers_BrandActual <- as.integer(TestingSet$Brand) - 1
    
    #Generate error variables: 0 will be no error, 1 will be error
    gbm_error <- as.factor(abs(integers_PredictionBrand_gbm - integers_BrandActual))
    gbm_error

    #generate testdatasets with error 
    TestingSet<- cbind(TestingSet, gbm_error)
    
    #Plotting errors
    ggplot(TestingSet, aes(x = Age,  y = Salary, color = gbm_error)) +
      theme_bw() +
      scale_color_discrete(name = "Predictions", labels = c("Correctly Predicted","Wrongly Predicted"))+
      facet_wrap(~ Brand) + 
      geom_point() +
      labs(y = "Salary", 
           x = "Age",
           title = "Errors in predicted values per Brand (GBM)")
    #Error percentage
    piechart_errors<-as.data.frame(table(TestingSet$gbm_error))
    piechart_errors$percentage<-round(100*piechart_errors$Freq/sum(piechart_errors$Freq))
    
    ggplot(data = piechart_errors, aes(x = 0, y = percentage, fill = Var1)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = percent(percentage/100)), position = position_stack(vjust = 0.5)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(fill = 'Brand', title = 'Proportion of Corretly/Wrongly predicted Brand') +
      scale_fill_discrete(name = "Prediction", labels = c("Properly Predicted", "Wrongly Predicted"))+ 
      coord_polar(theta = "y") +
      theme_void()+theme(plot.title = element_text(hjust = 0.5))
   
    
#### ---- Extra Stuff for Error Analysis ----
     
    ##Error Analysis for Knn and C5.0(with salary+age)
    
    #knn
    knn_model <- train(Brand~Age+Salary, data = TrainingSet, method = "knn", trControl = fitControl, tuneLength = 5, preProcess = c("center", "scale"), na.action = na.omit )
    knn_model
    PredictionBrand_knn <- predict(knn_model, TestingSet)
    postResample(PredictionBrand_knn, TestingSet$Brand)
    
    PredictionBrand_knn
    integers_PredictionBrand_knn <- as.integer(PredictionBrand_knn)-1
    
    knn_error <- as.factor(abs(integers_PredictionBrand_gbm - integers_BrandActual))
    knn_error
    
    TestingSet<- cbind(TestingSet, knn_error)
    
    ggplot(TestingSet, aes(x = Age,  y = Salary, color = knn_error)) +
      theme_bw() +
      scale_color_discrete(name = "Predictions", labels = c("Correctly Predicted","Wrongly Predicted"))+
      facet_wrap(~ Brand) + 
      geom_point() +
      labs(y = "Salary", 
           x = "Age",
           title = "Errors in predicted values per Brand (kNN)")
    
    #Error percentage
    piechart_errors_knn<-as.data.frame(table(TestingSet$knn_error))
    piechart_errors_knn$percentage<-round(100*piechart_errors_knn$Freq/sum(piechart_errors_knn$Freq))
    
    ggplot(data = piechart_errors_knn, aes(x = 0, y = percentage, fill = Var1)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = percent(percentage/100)), position = position_stack(vjust = 0.5)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(fill = 'Brand', title = 'Proportion of Corretly/Wrongly predicted Brand') +
      scale_fill_discrete(name = "Prediction", labels = c("Properly Predicted", "Wrongly Predicted"))+ 
      coord_polar(theta = "y") +
      theme_void()+theme(plot.title = element_text(hjust = 0.5))
    
    #C5.0
    C50_model <- train(Brand~Age+Salary, data = TrainingSet, method = "C5.0", trControl = fitControl, tuneLength = 5, preProcess = c("center", "scale"), na.action = na.omit )
    C50_model
    PredictionBrand_C50 <- predict(C50_model, TestingSet)
    postResample(PredictionBrand_C50, TestingSet$Brand)
    
    PredictionBrand_C50
    integers_PredictionBrand_C50 <- as.integer(PredictionBrand_C50)-1
    
    C50_error <- as.factor(abs(integers_PredictionBrand_C50 - integers_BrandActual))
    C50_error
    
    
    TestingSet<- cbind(TestingSet, C50_error)
    
    ggplot(TestingSet, aes(x = Age,  y = Salary, color = C50_error)) +
      theme_bw() +
      scale_color_discrete(name = "Predictions", labels = c("Correctly Predicted","Wrongly Predicted"))+
      facet_wrap(~ Brand) + 
      geom_point() +
      labs(y = "Salary", 
           x = "Age",
           title = "Errors in predicted values per Brand (C5.0)")
    
    #Error percentage
    piechart_errors_C50<-as.data.frame(table(TestingSet$C50_error))
    piechart_errors_C50$percentage<-round(100*piechart_errors_C50$Freq/sum(piechart_errors_C50$Freq))
    
    ggplot(data = piechart_errors_C50, aes(x = 0, y = percentage, fill = Var1)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = percent(percentage/100)), position = position_stack(vjust = 0.5)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(fill = 'Brand', title = 'Proportion of Corretly/Wrongly predicted Brand') +
      scale_fill_discrete(name = "Prediction", labels = c("Properly Predicted", "Wrongly Predicted"))+ 
      coord_polar(theta = "y") +
      theme_void()+theme(plot.title = element_text(hjust = 0.5))
    
    
    ## Error Analysis using if and for function
    Prediction_if<-cbind(TestingSet,PredictionBrand_gbm)
   
    #Creating the column for the TRUES and FAlSES
    Prediction_if$True <- 1
    names(Prediction_if)
    #Loops
    for(i in 1:nrow(Prediction_if)) {
      if (Prediction_if[i,7] == Prediction_if[i,12]) {Prediction_if[i,13]<-"TRUE"} 
      else {Prediction_if[i,13]<-"FALSE"}
     }    
    