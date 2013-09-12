# Generate a testbed of n k-armed bandits
# A bandit is a vector of length k, each of whose entries 
# is the mean reward r for the corresponding arm
# r ~ N(0,1)

# A testbed is a matrix, each row of which is an k-armed bandit

testbed.generate <- function(n, k){
    tbed <- matrix(rnorm(n*k) , nrow=n, ncol=k, 
                   dimnames=list(NULL,paste("Arm",1:k,sep=""))
                  )
}

# Create a testbed of 2000 10-armed bandits, as in S&B's book
tbed <- testbed.generate(2000, 10)

write.csv(tbed,"testbed.csv",row.names=FALSE)
