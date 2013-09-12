# generate a testbed of n k-armed bandits
# A bandit is a vector of length k, whose entry is the mean reward a
# a ~ N(0,1)

# A testbed is a matrix, each row of which is an k-armed bandit

testbed.generate <- function(n, k){
    tbed <- matrix(rnorm(n*k) , nrow=n, ncol=k, 
                   dimnames=list(NULL,paste("Arm",1:k,sep=""))
                  )
}


tbed <- testbed.generate(2000, 10)

write.csv(tbed,"testbed.csv",row.names=FALSE)
