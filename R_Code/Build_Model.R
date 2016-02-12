
objControl <- trainControl(method        =  "cv",
                           number        =  3, 
                           returnResamp  =  "none")

objModel <- train(x            =  trainDF[, predictorsNames], 
                  y            =  trainDF[, outcomeName], 
                  method       =  "glmnet",  
                  metric       =  "RMSE", 
                  trControl    =  objControl)

predictions <- predict(objModel, testDF[, predictorsNames])

auc <- roc(testDF[,outcomeName], predictions)
print(auc$auc)
plot(auc)

imp    <-  varImp(objModel, scale = F)
impdf  <-  imp$importance
Vars   <-  rownames(impdf)
Vals   <-  impdf$Overall
impdf  <-  data.frame(Vars, Vals)
impdf  <-  data.frame(impdf[impdf$Vals != 0, ])
impdf  <-  impdf[with(impdf, order(-Vals)), ]












