#Tommy Carroll
#Problem Set 2

####Problem 1####
BenfordsLaw <- function(data.matrix, M=TRUE, D=TRUE){    
  data.matrix <- as.matrix(data.matrix) #convert input vector/dataframe to matrix
  for(j in 1:ncol(data.matrix)){
    data.matrix[,j] <- as.character(as.numeric(data.matrix[,j])) #transform every column of matrix to character
  } #Note I use the as.numeric() function first to convert any character vectors to numerics, thereby dropping
  #any leading 0's that may be in the data. This ensures the first number I get will not be a leading 0, which
  #would not be a *significant* digit
  output <- vector('list', length=ncol(data.matrix)) #create empty list for the results
  
  #Next take every column of the data matrix and calculate the relevant statistics and store the results
  #into the elements of the output list created above
  for(i in 1:ncol(data.matrix)){
    charactervector <- data.matrix[,i] #selects i'th column
    n <- length(charactervector) #calculates n as sample size, necessary to use the modified formula posted on facebook
    FirstSignificantDigit <- substr(charactervector, 1, 1) #selects only 1st significant digit in each observation
    proportions <- numeric(9) 
    numbers <- numeric(9)
    for(k in 1:9){ 
      proportions[k] <- sum(FirstSignificantDigit==k)/length(FirstSignificantDigit) #calculate the X_i from Benford's equation
      numbers[k] <- proportions[k] - log(1 + 1/k, base=10) #calculate the vector of numbers used to calculate the 2 statistics
    }  
    m.statistic <- sqrt(n)*max(numbers) #calculate Leemis' statistic
    d.statistic <- sqrt(n)*sqrt(sum((numbers)^2)) #calculate Cho-Gains statistic
    
    #Next set of for loops gives different instructions on what to store in the output depending on whether M or D (or both)
    #was set to TRUE
    if(M==TRUE & D==FALSE){ 
        output[[i]] <- list(sigDigitFreq=proportions, Leemis=m.statistic)
    }
    if(D==TRUE & M==FALSE){
        output[[i]] <- list(sigDigitFreq=proportions, ChoGains=d.statistic)
    }
    if(M==TRUE & D==TRUE){
        output[[i]] <- list(sigDigitFreq=proportions, Leemis=m.statistic, ChoGains=d.statistic)
    }
    if(M==FALSE & D==FALSE){ #If M and D are both false, return a message telling user to set at least one of them to true
        output[[i]] <- paste("At least one of the arguments -- M or D -- must be set to TRUE")
    }  
  }
  return(output) 
}

####Problem 2####
print.benfords <- function(data.matrix, M=TRUE, D=TRUE){
  stored.output <- BenfordsLaw(data=data.matrix, M=M, D=D) #First run the function from problem 1. 
                  #Store the output from this function in a list called stored.output. Each element
                  #of the list corresponds to the output for one of the columns of the matrix input. 
  if(M == TRUE){
    m.statistics <- numeric(length(stored.output))    #Create 2 empty numeric vector whose lengths are 
    m.significances <- numeric(length(stored.output)) #equal to the number of columns of the matrix input
                                                      
    for(i in 1:length(stored.output)){
      m.statistics[i] <- sprintf("%.3f", round(stored.output[[i]]$Leemis, 5)) 
              #plug into m.statistics the Leemis' values. The sprintf() function keeps trailing 0s
      #Next few if() statements basically add asterisks (*) to denote the significance of the Leemis'
        #statistics based on their size
    }
    for(i in 1:length(stored.output)){
      if(m.statistics[i] < 0.851){
        m.significances[i] <- m.statistics[i]
      }
      if(m.statistics[i] >= 0.851 & m.statistics[i] < 0.967){
        m.significances[i] <- paste(m.statistics[i], "*", sep="")
      }
      if(m.statistics[i] >= 0.967 & m.statistics[i] < 1.212){
        m.significances[i] <- paste(m.statistics[i], "**", sep="")
      }
      if(m.statistics[i] >= 1.212){
        m.significances[i] <- paste(m.statistics[i], "***", sep="")
      } 
    }  
  }
  
  #This next if statement does the same as the last, only for the ChoGains' statistics. 
  if(D == TRUE){
    d.statistics <- numeric(length(stored.output))
    d.significances <- numeric(length(stored.output))
    for(i in 1:length(stored.output)){
      d.statistics[i] <- sprintf("%.3f", round(stored.output[[i]]$ChoGains, 5))
    }
    for(i in 1:length(stored.output)){
      if(d.statistics[i] < 1.212){
        d.significances[i] <- d.statistics[i]
      }
      if(d.statistics[i] >= 1.212 & d.statistics[i] < 1.330){
        d.significances[i] <- paste(d.statistics[i], "*", sep="")
      }
      if(d.statistics[i] >= 1.330 & d.statistics[i] < 1.569){
        d.significances[i] <- paste(d.statistics[i], "**", sep="")
      }
      if(d.statistics[i] >= 1.569){
        d.significances[i] <- paste(d.statistics[i], "***", sep="")
      } 
    }  
  }  
  
  #Finally print the results in a table that gets output into the R console. 
  cat(cat("\nLeemis' M      ", m.significances, 
      "\nCho-Gains' D   ", d.significances, sep="       "), 
      "\n\nSignif. Codes: '***' 0.001, '**' 0.05, '*' 0.1, '' 1")
}

####Problem 3####
#Arguments:
#   dataset1 should be a vector, matrix, or dataframe object. In this example it is the dataset 
#     that satisfies Benford's Law. 
#   dataset2 should be a vector, matrix, or dataframe object. In this example it is the dataset 
#     that does not satsify Benford's Law.
#   true.values1 is a list of length 3. The first element is itself a list whose elements are vectors; each vector contains 
#     the digit distribution for each column of dataset1. The second element of true.values1 is a vector
#     containing the true Leemis statistics that would be calculated if the function is working
#     properly. The elements of this vector should go in the same order as the columns of dataset1. 
#     The second element of the list should be a vector containing the true ChoGains statistics. 
#     Again the elements of the vector should go in the same order as the columns of dataset1. 
#   true.values2 is the same as true.values1, except for dataset2

unit.test <- function(dataset1, dataset2, true.values1, true.values2){
# Calculate the statistics and distributions using my functions  
  stored.output1 <- BenfordsLaw(data=dataset1, M=TRUE, D=TRUE) 
  stored.output2 <- BenfordsLaw(data=dataset2, M=TRUE, D=TRUE)
# Turn dataframes or vectors into matrices
  dataset1 <- as.matrix(dataset1)
  dataset2 <- as.matrix(dataset2)
# Create empty objects to store info into later
  Leemis1 <- numeric(length(stored.output1))
  ChoGains1 <- numeric(length(stored.output1))
  DigitDistribution1 <- vector("list", length=length(stored.output1))
  Leemis2 <- numeric(length(stored.output2))
  ChoGains2 <- numeric(length(stored.output2))
  DigitDistribution2 <- vector("list", length=length(stored.output2))
# Calculate Leemis, ChoGains, and Digit Distribution for dataset1  
  for(i in 1:ncol(dataset1)){
    Leemis1[i] <- stored.output1[[i]]$Leemis
    ChoGains1[i] <- stored.output1[[i]]$ChoGains 
    DigitDistribution1[[i]] <- stored.output1[[i]]$sigDigitFreq
  }
# Calculate Leemis, ChoGains, and Digit Distribution for dataset2
  for(i in 1:ncol(dataset2)){
    Leemis2[i] <- stored.output2[[i]]$Leemis
    ChoGains2[i] <- stored.output2[[i]]$ChoGains
    DigitDistribution2[[i]] <- stored.output2[[i]]$sigDigitFreq
  }

# Next chunk creates 3 objects: (1) test1.distribution; (2) test1.Leemis; and (3) test1.ChoGains. 
# Each of these is a single logical (T/F) testing whether the 2 statistics and/or digit distribution 
# are equal to their true values
# First test whether the digit distributions are correct
  digit.logical1 <- logical(length=length(stored.output1))
  for(i in 1:length(digit.logical1)){
    if(all(DigitDistribution1[[i]]==true.values1[[1]][[i]])){ 
      digit.logical1[i] <- TRUE
    }
  }
  if(all(digit.logical1)){
    test1.distribution <- TRUE
  } else(test1.distribution <- FALSE)
#Next test if the Leemis and ChoGains values are correct   
  if(all(Leemis1==true.values1[[2]])){ #IF every Leemis statistic for dataset1 matches the true Leemis value...
    test1.Leemis <- TRUE
  } else(test1.Leemis <- FALSE)
  if(all(ChoGains1==true.values1[[3]])){ #IF every ChoGains statistic for dataset1 matches the true ChoGains value...
    test1.ChoGains <- TRUE
  } else(test1.ChoGains <- FALSE)
# NOW test to make sure all three logicals are TRUE
  if(test1.Leemis==TRUE & test1.ChoGains==TRUE & test1.distribution==TRUE){
    test1 <- TRUE
  } else(test1 <- FALSE)

# Next, same ideas as above except for dataset2
  digit.logical2 <- logical(length=length(stored.output2))
  for(i in 1:length(digit.logical2)){
     if(all(DigitDistribution2[[i]]==true.values2[[1]][[i]])){ 
       digit.logical2[i] <- TRUE
      }
  }
  if(all(digit.logical2)){
     test2.distribution <- TRUE
  } else(test2.distribution <- FALSE)
  if(all(Leemis2==true.values2[[2]])){ #IF every Leemis statistic for dataset2 matches the true Leemis value...
    test2.Leemis <- TRUE
  } else(test2.Leemis <- FALSE)
  if(all(ChoGains2==true.values2[[3]])){ #IF every ChoGains statistic for dataset2 matches the true ChoGains value...
    test2.ChoGains <- TRUE
  } else(test2.ChoGains <- FALSE)
  if(test2.Leemis==TRUE & test2.ChoGains==TRUE & test2.distribution==TRUE){
    test2 <- TRUE
  } else(test2 <- FALSE)

# Generate warning messages which tell user which statistic is being calculated incorrectly
  if(test1.distribution==FALSE){
    warning("Benford Distribution for Dataset1 is Incorrect")
  }
  if(test2.distribution==FALSE){
    warning("Benford Distribution for Dataset2 is Incorrect")
  }
  if(test1.Leemis==FALSE | test1.ChoGains==FALSE){
    warning("Leemis and ChoGains statistics for Dataset1 are Incorrect")
  }
  if(test2.Leemis==FALSE | test2.ChoGains==FALSE){
    warning("Leemis and ChoGains statistics for Dataset2 are Incorrect")
  }

#Finally create final output of TRUE/FALSE
  final.output <- list(test1, test2)
  return(final.output)
}



# NEXT I test my function using two self-created datasets. The first dataset is called testing1. It
# is a single vecctor, 20 elements long. I calculated by hand the digit distribution and stored it as 
# an object valled dis1. I then calculated (using R) the Leemis and ChoGains statistics. Their values 
# are recorded below. Finally I plug these into a list object to plug in as one of the arguments to 
# my unit.test function. 
testing1 <- c(1, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 9)
dis1 <- c(0.05, 0.05, 0.15, 0.15, 0.2, 0.15, 0.15, 0.05, 0.05) #Gives distribution for 1st test
Leemis.test1 <- sqrt(20)*max(dis1 - log(1 + (1/(1:9)), base=10)) #gives Leemis for 1st test; value is 0.5403179
ChoGains.test1 <- sqrt(20)*sqrt(sum((dis1 - log(1 + (1/(1:9)), base=10))^2)) #gives ChoGains for 1st test; value is 1.498943
true.values.list1 <- list(list(dis1), Leemis.test1, ChoGains.test1)

# Same thing below, except this is for dataset2 (which I call testing2). 
testing2 <- c(6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9)
dis2 <- c(0, 0, 0, 0, 0, 0.25, 0.3, 0.25, 0.2) #gives distribution for 2nd test
Leemis.test2 <- sqrt(20)*max(dis2 - log(1 + (1/(1:9)), base=10)) #gives value 1.082293
ChoGains.test2 <- sqrt(20)*sqrt(sum((dis2 - log(1 + (1/(1:9)), base=10))^2)) #gives value 2.483165
true.values.list2 <- list(list(dis2), Leemis.test2, ChoGains.test2)

#Finally I run my unit.test function. As we can see, it returns all TRUE values because my function is 
# working correctly. 
unit.test(dataset1=testing1, dataset2=testing2, true.values1=true.values.list1, true.values2=true.values.list2)

true.values.list2.1 <- list(list(c(1)), Leemis.test1, ChoGains.test1)
unit.test(dataset1=testing1, dataset2=testing2, true.values1=true.values.list1, true.values2=true.values.list2.1)
