# Andrew Trujillo and Vanessa Palzes
# Loan Default Prediction Code
# Submitted 03/2014

##ReplaceNA with Median
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
ss=data.frame(apply(df,2,f))

##randomForest
X<-ss[,1:758]
Y<-ss[,759]
Y<-as.factor(as.numeric(Y>0))
fit<-randomForest(X,Y,ntree=300,replace=TRUE,sampsize(c(2000,2000)))

##Select only Defaulted Rows
DefaultOnly<-ss[which(ss$loss>0),]

##Get lambda for Ridge
ridgeXval <- function (mydata,targetCol,nxval) {
  colnames(mydata)[targetCol]<- "target"
  colnames(mydata)
  
  Err <- matrix(nrow = 31, ncol = 3)
  betas <- matrix(nrow = 31, ncol = 8)
  I <- seq(1:nrow(mydata))
  
  for(iLambda in seq(from = 0, to = 30)){
    #
    exp <- (+3 -4*(iLambda/20))
    xlambda <- 10^exp
    
    testErr <- 0.0
    trainErr <- 0.0
    
    for(ixval in seq(from = 1, to = 10)){
      Iout <- which(I%%10 == (ixval - 1))
      Xin <- mydata[-Iout,]
      Xout <- mydata[Iout,]
      Yin <- mydata[-Iout,targetCol]
      Yout <- mydata[Iout,targetCol]
      mod <- linearRidge(target ~.,data=as.data.frame(Xin),lambda=xlambda, scaling="none")
      
      Yh <- predict(mod,Xin)
      dY <- Yin - Yh
      trainErr <- trainErr + sqrt(sum(dY*dY))/(nrow(as.matrix(Yin)))  
      
      Yh <- predict(mod,Xout)
      dY <- Yout - Yh
      
      testErr <- testErr + sqrt(sum(dY*dY))/(nrow(as.matrix(Yout)))
    }
    Err[(iLambda+1),1] = trainErr/nxval
    Err[(iLambda+1),2] = testErr/nxval
    Err[(iLambda+1),3] = xlambda
    betas[iLambda+1,1:8] <- mod$coef[1:8,1] # str(mod) results in variable coef
    mod$coef[,1]
  }
  answer <- list(title = "ridgeXval", Err = Err, betas = betas) 
  return(answer)
}

## Ridge 
#Training set
mod<-linearRidge(V230 ~.,dataDefault, lambda=25.11886, scaling="scale")
yhat<-predict(mod,dataDefault[,1:229])
dY <- Yact - yhat
sqrt(sum(dY*dY))/(length(Yact))

#test
yhatTest<-predict(mod, x_testScalePredicted)
#Prediction formatting and out
NewVec<-rep(0,nrow(RForestPredictScale))
for (x in seq(from=1, to=length(NewVec)))
{NewVec[vec[x]]<-yhatTest[x]}
NewVecFinal<-cbind(test[,1],NewVec)
write.csv(FinalVec, file="doublescale229Final.csv",row.names=FALSE)
