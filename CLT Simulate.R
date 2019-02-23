# Melinda Cundiff
# STA 580

# used the following source to help solve this problem:
# https://www.statmethods.net/advgraphs/layout.html

# Problem 4
# For this question you are going to write a function to help a student visualize 
# the central limit theorem for a single mean.

# a.) Your function should do the following:

distributionFun = function(shape, sample.size, n.samples){
# Takes the following as arguments:
# Returns an error if any of the arguments are the incorrect format.

  # shape. Where shape can take the value bell, right, or left. The 
  # default for shape should be bell.
  
  # not entered correctly
  if(shape != "bell" && shape != "left" && shape != "right" && !missing(shape)){
    stop("Not a valid shape")
  }
  # default
  if(missing(shape)){
    shape = "bell"
  }

  
  # sample.size. Where sample.size is an integer from 1 to 500. The 
  # default for sample.size should be 100.
  
  # not entered correctly
  if(sample.size <= 1 && sample.size >= 500 && !missing(sample.size)){
    stop("Not a valid sample size")
  }
  # default
  if(missing(sample.size)){
    sample.size = 100
  }
 
  # n.samples. Where n.samples is an integer from 1 to 100000. The 
  # default for n.samples should be 10000.

  # not entered correctly
  if(n.samples <= 1 && n.samples >= 100000 && !missing(n.samples)){
    stop("Not a valid number of samples")
  }
  # default
  if(missing(n.samples)){
    n.samples = 10000
  }
# Simulates population data based on shape.
# Selects n.samples of size sample.size from the population data.
# Calculates the sample mean of each sample.

# Outputs two side by side histograms: one of the population data and one 
# of the simulated sample means.
  # Each plot should include a title which indicates the type of distribution
  # The x-axis should be labeled simulated values
  # The y-axis should be labeled frequency
    # For left-skewed population data color the bars light grey, 
    # for bell shaped population data color the bars grey and 
    # for right-skewed population data color the bars dark grey.

  # If the user chooses left skewed simulate 100000 random draws from a 
  # Beta(100,1) distribution. Multiply each value by 10 million
  
  if(shape == "left"){
    leftDist = 10000000*rbeta(100000, 100, 1)
    leftMeans = vector(length = n.samples)
    leftMeans[1:n.samples] = 0
    for(i in 1:n.samples){
      leftSamples = sample(leftDist, sample.size)
      leftMeans[i] = mean(leftSamples)
    }
    par(mfrow = c(1,2))
    left.dist.hist = hist(leftDist, xlab = "Simulated Values", 
                          ylab = "Frequency", col = "light grey")
    left.means.hist = hist(leftMeans, xlab = "Simulated Values", 
                           ylab = "Frequency", col = "light grey")
    return(list(left.dist.hist, left.means.hist))
  }

  # If the user chooses right skewed simulate 100000 random draws from a 
  # Beta(1,100) distribution. Multiply each value by 10 million.
  
  if(shape == "right"){
    rightDist = 10000000*rbeta(100000, 1, 100)
    rightMeans = vector(length = n.samples)
    rightMeans[1:n.samples] = 0
    for(i in 1:n.samples){
      rightSamples = sample(rightDist, sample.size)
      rightMeans[i] = mean(rightSamples)
    }  
    par(mfrow = c(1,2))
    right.dist.hist = hist(rightDist, xlab = "Simulated Values", 
                           ylab = "Frequency", col = "grey")
    right.means.hist = hist(rightMeans, xlab = "Simulated Values", 
                            ylab = "Frequency", col = "grey")
    return(list(right.dist.hist, right.means.hist))
  }  
  
  
  # If the user chooses bell simulate 100000 random draws from a 
  # Beta(100,100) distribution. Multiply each value by 10 million.

  if(shape == "bell"){
    bellDist = 10000000*rbeta(100000, 100, 100)
    bellMeans = vector(length = n.samples)
    bellMeans[1:n.samples] = 0
    for(i in 1:n.samples){
      bellSamples = sample(bellDist, sample.size)
      bellMeans[i] = mean(bellSamples)
    }
    par(mfrow = c(1,2))
    bell.dist.hist = hist(bellDist, xlab = "Simulated Values", 
                          ylab = "Frequency", col = "dark grey")
    bell.means.hist = hist(bellMeans, xlab = "Simulated Values", 
                           ylab = "Frequency", col = "dark grey")
    return(list(bell.dist.hist, bell.means.hist))
  }
    
}

# b.) Use your function to generate a single plot which contains six histograms. 
# For each call of the function use n.samples =10000 and sample.size =10.

leftHist = distributionFun(shape = "left", sample.size = 10, n.samples = 10000)
bellHist = distributionFun(shape = "bell", sample.size = 10, n.samples = 10000)
rightHist = distributionFun(shape = "right", sample.size = 10, n.samples = 10000)

par(mfrow = c(3,2))
plot(leftHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(leftHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(bellHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(bellHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(rightHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")
plot(rightHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")


# c.) Create another plot where n.samples =10000 and sample.size =30.

leftHist = distributionFun(shape = "left", sample.size = 30, n.samples = 10000)
bellHist = distributionFun(shape = "bell", sample.size = 30, n.samples = 10000)
rightHist = distributionFun(shape = "right", sample.size = 30, n.samples = 10000)

par(mfrow = c(3,2))
plot(leftHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(leftHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(bellHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(bellHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(rightHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")
plot(rightHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")


# d.) Create another plot where n.samples =10000 and sample.size =100.

leftHist = distributionFun(shape = "left", sample.size = 100, n.samples = 10000)
bellHist = distributionFun(shape = "bell", sample.size = 100, n.samples = 10000)
rightHist = distributionFun(shape = "right", sample.size = 100, n.samples = 10000)

par(mfrow = c(3,2))
plot(leftHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(leftHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "light grey")
plot(bellHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(bellHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "dark grey")
plot(rightHist[[1]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")
plot(rightHist[[2]], xlab = "Simulated Values", 
     ylab = "Frequency", col = "grey")


# e.) Write a paragraph summarizing what the comparison of the plots 
# illustrates about the central limit theorem for the sample mean.

# The plots of the means for the left and right distributions, 
# created by using a sample size of 10, are very skewed. 
# The plots created by using a sample size of 30 are slightly skewed, 
# but it is a massive improvement. The plots created by using a 
# sample size of 100 are generally normally, but they may also 
# have a slight skew. Overall, these plots illustrate how 
# using a minimum sample size of 30 is required for 
# non-normal (bell-shaped) data in order to perform tests that 
# require normal data. While larger sample sizes may be useful for 
# improving normality, they are not necessary.


