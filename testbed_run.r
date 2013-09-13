source("strategies.r")

# play one round

play1round<-function(vals, strat.dec, 
                     strat.rev = action.value, t.max = 1000, 
                     init.vals = rep(0,length(vals)), ...){
    # play 1 round for t.max pulls, the mean value vector vals, using the 
    # decision and revision strategies, with initial value estimations and
    # optional arguments for strat.dec  

    actions.times <- rep(0,length(vals))
    actions <- rep(NA, t.max)
    rewards <- rep(NA, t.max)
 
    action <- strat.dec(init.vals, ...)
    reward <- rnorm(1, mean=vals[action], sd=1)
    actions.times[action] <- 1
    actions[1] <- action
    rewards[1] <- reward
    vals.expected <- strat.rev(init.vals, action, reward, 1)

    for (t in 2:t.max){
        action <- strat.dec(vals.expected, ...)
        reward <- rnorm(1, mean=vals[action], sd=1)
        actions.times[action] <- actions.times[action] + 1
        actions[t] <- action
        rewards[t] <- reward
        vals.expected <- strat.rev(vals.expected, action, reward,
                                   actions.times[action]  
                                  )
    }

    optimal <- (vals[actions]==max(vals))
    optimal.percents <- cumsum(optimal) / seq_along(optimal)
    avgrewards <- cumsum(rewards) / seq_along(rewards)

    #return(c(actions,rewards))
    return( c(optimal.percents,avgrewards) )
}


testbed.run <- function(tbed, strat.dec, 
                     strat.rev = action.value, t.max = 1000, 
                     init.vals=rep(0,NCOL(tbed)), ...){

    outcome <- apply(testbed,1,
                     function(v)play1round(v, strat.dec, 
                     strat.rev, t.max, 
                     init.vals, ...))

    avg.optpercent <- colMeans(t(outcome)[,1:t.max])
    avg.cumrewards <- colMeans(t(outcome)[,(t.max+1):(2*t.max)])

    return( data.frame(OptimalPercent=avg.optpercent,
                       CummulativeRewards=avg.cumrewards)
          )
}

testbed <- read.csv("testbed.csv")


