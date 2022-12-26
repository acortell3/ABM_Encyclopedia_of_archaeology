

################################################################################
################################################################################

## Code for the Toy Model in Agent-Based Modelling in Archaeology, in 
## Encyclopedia of Archaeology

## By Alfredo Cortell-Nicolau

## Use and modify at your own risk

################################################################################
################################################################################

## START CODE

## Set decision rules

set.seed(12345)

Pm <- 0.2 ## Probability that they meet
Pc <- 0.5 ## Probability that there is conflict
Pk <- 0.5 ## Probability that one individual kills an individual from the opposite population
PAB <- 0.4 ## Probability that A becomes B
PBA <- 0.1 ## Probability that B becomes A

sims <- 1000 ## number of simluations

simulations <- list() ## Store the simulations here

## Start model
for (j in 1:sims){ ## Run 1000 simulations
  
  ## Set initial conditions
  A <- 100 ## Initial population for Population A
  B <- 100 ## Initial population for Population B
  t <- 300 ## Total time allowed (1000 years)
  
  ## Create a data frame to store results
  results <- data.frame("PopA" = rep(0,t),
                        "PopB" = rep(0,t),
                        "time" = rep(0,t))
  
  #i <- 1
  
  for (i in 1:t){
  #while ((A != 0) | (i > t)){ ## The loop will continue as long as one population is not 0
    
    ## Store results
    results$PopA[i] <- A
    results$PopB[i] <- B
    results$time[i] <- i
    
    ## Subset potential population to meet
    subA <- sum(rbinom(1,A,Pm))
    subB <- sum(rbinom(1,B,Pm))
    
    ## Do they meet?
    Meet <- rbinom(1,1,Pm)
    
    if (A == 0 | B == 0){ ## If there is 0 of one population, population cannot meet
      Meet <- 0
    }
    
    ## If they meet
    if (Meet == 1){
      
      ## Is there conflict
      Conflict <- rbinom(1,1,Pc)
      
      ## If there is conflict
      if (Conflict == 1){
        killedA <- sum(rbinom(1,subB,Pk)) ## killed population A
        killedB <- sum(rbinom(1,subA,Pk)) ## killed population B
        
        ## Remaining population
        A <- A - killedA
        B <- B - killedB
        
      } 
      ## If there is not conflict
      else {
        AB <- sum(rbinom(1,subA,PAB)) ## Population A switching to B
        BA <- sum(rbinom(1,subB,PBA)) ## Population B swithing to A
        
        ## Remaining population
        A <- A + BA - AB
        B <- B + AB - BA
        
      }
    }
    
    if (A <= 0){
      A <- 0
    }
    
    if (B <= 0){
      B <- 0
    }
    
    ## Advance index in while
    #i <- i + 1
    
  }
  
  simulations[[j]] <- results
  
}

png("Culture_adopt.png", width = 1500, height = 1500, pointsize = 25)
plot(simulations[[1]]$PopA, type ="l", col = "pink", ylim = c(0,170), 
     ylab = "Population", xlab = "Year", main = "Culture adoption")
legend("topleft", legend = c("Pop A", "Pop B"), lty = c(1,1), col = c("pink","aquamarine3"))
lines(simulations[[1]]$PopB, col = "aquamarine3")

for (i in 2:sims){
  lines(simulations[[i]]$PopA, col = "pink")
  lines(simulations[[i]]$PopB, col = "aquamarine3")
}
dev.off()

## Quick results
## When does population A disappear
disappearA <- rep(NA,sims)
for (i in 1:sims){
  part <- simulations[[i]]
  disappearA[i] <- nrow(part[part$PopA != 0,])
}
summary(disappearA)
sd(disappearA)

## How much population is left from population B at 200 years
popleftB <- rep(NA,sims)
for (i in 1:sims){
  part <- simulations[[i]]
  popleftB[i] <- part$PopB[t]
}

summary(popleftB)
sd(popleftB)

