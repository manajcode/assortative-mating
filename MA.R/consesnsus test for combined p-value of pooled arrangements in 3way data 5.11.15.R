#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@  stouffer.test 5.9.15  @@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#do the stouffer test in place of pooled binomial analyses of arrangements from
# 3way amte choice. combines p-values, compensates for unknown variables, makes
# nomrally uncomparable or unmergable data mergable and a p-vale averaged across
# both data sets where the null hypothesis being compared is the same.

#step 1: record the p-values of each arrangement, then create lists
#p.cmy: FC =0.09, MC = 0.056
#p.starch: FS = 0.225, FC = 0.485

p.cmy <- as.vector(c(0.09, 0.056))
p.starch <- as.vector(c(0.225, 0.485))


# @@@ Stouffer tests for CMY @@@
# step 2: modify the function for stouffer's consensus test from online codes
# and bill's paper:
zi.k.storage <- rep(0,2)
k = 2

stouffer.test <- function(p.cmy){ # p.cmy is a vector of p-values
  for (i in 1:k){
    zofi <- qnorm(1-p.cmy)
    zi.k.storage[i] <- zofi
    }
  Zi<- sum(zi.k.storage)
  Z <- Zi/sqrt(1/2)
  p.val <- (1-pnorm(Z))
  output <- c(Z = Z, p.value = p.val)
  return(output)
}

stouffer.test(p.cmy)


#output of test:
#stouffer.test(p)
#Z      p.value 
#3.7922279049 0.0000746509 
#Warning messages:
#  1: In zi.k.storage[i] <- zofi :
#  number of items to replace is not a multiple of replacement length
#2: In zi.k.storage[i] <- zofi :
#  number of items to replace is not a multiple of replacement length


#step 3: redo the analysis to include weighted averages and exclude the for loop-
# ie just run the code as seen on wikipedia
St.test <- function(p.cmy, w) { # p is a vector of p-values
  if (missing(w)) {
    w <- rep(1, length(p.cmy))/length(p.cmy)
  } else {
    if (length(w) != length(p.cmy)) 
    stop("Length of p and w must equal!")
  }  
  
  Zi <- qnorm(1-p.cmy) 
  Z  <- sum(w*Zi)/sqrt(sum(w^2))
  p.val <- 1-pnorm(Z)
  output <- (c(Z = Z, p.value = p.val)) 
  return(output)
}
St.test(p.cmy)

# output:
#Z    p.value 
#2.07183884 0.01914024

#Step 4: go back to your code in step 2, remove for loop, and see if it matches
# default code
thirdstffer.test <- function(p.cmy){ # p.cmy is a vector of p-values
  Xi<- qnorm(1-p.cmy)
  X <- (sum(Xi))/sqrt(2)
  p.val <- (1-pnorm(X))
  output <- c(X = X, p.value = p.val)
  return(output)
}
thirdstffer.test(p.cmy)

# output:
#X- Z-value    p.value 
#2.07183884 0.01914024

# same as code from text- w doesn't matter. use this code.

# @@@ Stouffer tests for starch @@@
# step 1: uaw previous code from thirdstffer, change out pvalue.
starchstffer.test <- function(p.starch){ # p.cmy is a vector of p-values
  Xi<- qnorm(1-p.starch)
  X <- (sum(Xi))/sqrt(2)
  p.val <- (1-pnorm(X))
  output <- c(X = X, p.value = p.val)
  return(output)
}
starchstffer.test(p.starch)

#output:
#X-Z value   p.value 
#0.5607522 0.2874833 



#~~~~~~~~UPDATE ON 5.25.15~~~~~~~~~~~~~
# i LEFT OUT A DATA POINT IN MS AND AM NOW GOING BACK TO INCLUDE IT IN DATA.
#THIS CHANGES THE P-VALUES OF THE STARCH TO:

p.starch <- as.vector(c(0.225,0.536))

starchstffer.test <- function(p.starch){ # p.cmy is a vector of p-values
  Xi<- qnorm(1-p.starch)
  X <- (sum(Xi))/sqrt(2)
  p.val <- (1-pnorm(X))
  output <- c(X = X, p.value = p.val)
  return(output)
}
starchstffer.test(p.starch)