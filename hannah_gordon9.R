#Exercise 9
#Hannah Gordon

#set working directory
setwd("/Users/Public/Documents/Biocomputing2020_Tutorial11")

#Write a function that takes a directory name as an argument called dir plus any 
#other arguments requried to accomplish the specified task.
#The function should read data from each file in the specified directory and 
#calculate the coefficient of variation (standard deviation divided by the mean) 
#for a user specified column. These values should be returned as a vector.
#To calculate a reliable coefficient of variation we would like to have 50 
#observations, but we also don't want to force the user to use our high standard 
#for the data. 
#Make your function, by default, report an error if any file has less than 50
#observations, but allow the user to override this behavior and only receive 
#a warning if 50 observations are not present in a file.
#Also consider what the function should do if a file doesn't have the correct 
#number of columns or the provided data includes NA's.

#Specify the directory
#User should add pathway to directory in "."
userDirectory<- list.files(path=".")

#Use of function (user must input column)
covarFunction(dir=userDirectory,column=inputcolumn)

#Create the function
covarFunction<-function(dir,column){
  
  #Create a vector for the coefficients of variation
  definedCovars<-c()
  
  #Use a for loop to loop through each file in the directory
  for (i in dir){
    #read.csv will create a data-frame from each file and put it into "allFiles"
    allFiles=read.csv(file=i,header=FALSE,sep=",")
    
    #Create variables to calculate std Dev and the mean of each file
    #User should specify the column in "inputcolumn"
    calcStdDev<-sd(allFiles[,inputcolumn])
    calcMean<-mean(allFiles[,inputcolumn])
    
    #Calculate coefficient of variation
    calcCovars= calcStdDev/calcMean
    
    #Create if/else statement based on the number of observations
    #If they have less than 50, give them the option to override
    if(nrow(allFiles<50)){
      override=readline(prompt="You have fewer than 50 observations. Would you like to override and continue anyways (Yes/No): ")
      if(override=="Yes"){
        #Add calculated coefficients of var to vector
        definedCovars<-c(definedCovars,calcCovars)
      }
      else{
        next
        
      }
    }
    #Going back to the first if/else statement, if there are already more than 50 observations, no input is needed
    else{
      definedCovars<-c(definedCovars,calcCovars)
    }
  }
  if(is.na(x)=TRUE) {
    override2= readline(prompt = "File is missing values. Would you like to continue (Yes/No): ")
    if(override2 == "Yes"){
      definedCovars<-c(definedCovars,calcCovars)
    }
    else{
      next
      
    }
    print("Here is the vector containing the coefficients of variation")
    return(definedCovars)
  }
}
  