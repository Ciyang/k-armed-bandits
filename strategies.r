# We maintain the belief-goal-action view.

# The goal can be either directly from the environment,
# or implicitly assumed by the decision rule.

# A strategy actually consists of (at least) two parts:
# the decision rule, which takes in a belief and outputs an action,
# and a belief revision rule, which takes in the current belief
# and the new information, and outputs a new belief

# In this sense, the action-value methods all share 
# the same belief revision rule.

action.value <- function(oldvals, action, reward, nth){
    #Take as input the vector of old expected values, the current action
    #and reward, and the times this action has been chosen

    oldvals[action] <- oldvals[action] + (reward - oldvals[action])/nth
    return(oldvals)
}


#Greedy rule always chooses the (first) current optimal action
greedy <- function(vals){
    return( which.max(vals) )
}

#Epsilon greedy rule has probability epsilon to randomly choose a 
#non-optimal action

eps.greedy <- function(vals,eps){
    optimal = which.max(vals)
    if (runif(1) >= eps){
        return(optimal)
    }
    return( sample( (1:length(vals))[-optimal] ,1) )
}

#Softmax





