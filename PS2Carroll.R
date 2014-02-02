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
    FirstSignificantDigit <- substr(charactervector, 1, 1) #selects only 1st significant digit in each observation
    proportions <- numeric(9) 
    numbers <- numeric(9)
    for(k in 1:9){ 
      proportions[k] <- sum(FirstSignificantDigit==k)/length(FirstSignificantDigit) #calculate the X_i from Benford's equation
      numbers[k] <- proportions[k] - log(1 + 1/k, base=10) #calculate the vector of numbers used to calculate the 2 statistics
    }  
    m.statistic <- max(numbers) #calculate Leemis' statistic
    d.statistic <- sqrt(sum((numbers)^2)) #calculate Cho-Gains statistic
    
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
  stored.output <- BenfordsLaw(data=data.matrix, M=M, D=D)
  if(M == TRUE){
    m.statistics <- numeric(length(stored.output))
    m.significances <- numeric(length(stored.output))
    for(i in 1:length(stored.output)){
      m.statistics[i] <- sprintf("%.3f", round(stored.output[[i]]$Leemis, 5))
      if(m.statistics[i] < 0.851){
        m.significances[i] <- m.statistics[i]
      }
      if(m.statistics[i] >= 0.851 & m.statistics[i] < 0.967){
        m.significances[i] <- cat(m.statistics[i], "*", sep="")
      }
      if(m.statistics[i] >= 0.967 & m.statistics[i] < 1.212){
        m.significances[i] <- cat(m.statistics[i], "**", sep="")
      }
      if(m.statistics[i] >= 1.212){
        m.significances[i] <- cat(m.statistics[i], "***", sep="")
      } 
    }  
  }
  
  if(D == TRUE){
    d.statistics <- numeric(length(stored.output))
    d.significances <- numeric(length(stored.output))
    for(i in 1:length(stored.output)){
      d.statistics[i] <- sprintf("%.3f", round(stored.output[[i]]$ChoGains, 5))
      if(d.statistics[i] < 1.212){
        d.significances[i] <- d.statistics[i]
      }
      if(d.statistics[i] >= 1.212 & d.statistics[i] < 1.330){
        d.significances[i] <- cat(d.statistics[i], "*", sep="")
      }
      if(d.statistics[i] >= 1.330 & d.statistics[i] < 1.569){
        d.significances[i] <- cat(d.statistics[i], "**", sep="")
      }
      if(d.statistics[i] >= 1.569){
        d.significances[i] <- cat(d.statistics[i], "***", sep="")
      } 
    }  
  }  
  
  cat(cat("\nLeemis' M      ", m.significances, 
      "\nCho-Gains' D   ", d.significances, sep="       "), 
      "\n\nSignif. Codes: '***' 0.001, '**' 0.05, '*' 0.1, '' 1")
}

test1 <- rnorm(50000, 50000, 25000)
test2 <- rnorm(50000, 50000, 25000)
test3 <- rnorm(50000, 50000, 25000)
test4 <- rnorm(50000, 50000, 25000)
test5 <- rnorm(50000, 50000, 25000)
Test <- cbind(test1, test2, test3, test4, test5)
print.benfords(Test)

####Problem 3####

