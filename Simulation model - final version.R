####Simulation model for lifetime reproductive success of different assessment rules during dyadic contests#####

####Creating helping functions####

####Function to determine whether each rival should flee during a contest if it adopts mutual assessment. The greater the difference in fighting skill, the greater the chance that the individual with the smaller value of fighting skill will flee####
flee.stay = function(invest1, invest2, prob.intercept.flee, prob.slope.flee, data, previous_flee = NULL) {
  flee1 = rep(NA, nrow(data))
  flee2 = rep(NA, nrow(data))
  
  # If this is the first loop, initialize previous_flee
  if (is.null(previous_flee)) {
    previous_flee = list(
      flee1 = rep(NA, nrow(data)),
      flee2 = rep(NA, nrow(data))
    )
  }
  
  for (i in 1:nrow(data)) {
    if (is.na(invest1[i]) || is.na(invest2[i])) {
      flee1[i] = NA
      flee2[i] = NA
    } else if (invest1[i] <= 0 || invest2[i] <= 0) {
      flee1[i] = NA
      flee2[i] = NA
    } else {
      dif.hab = data$fight.skill1[i] - data$fight.skill2[i]
      
      # For individual 1 (only if strategy is 'mut')
      if (!is.na(data$contest.strat1[i]) && data$contest.strat1[i] == "mut") {
        if (!is.na(previous_flee$flee1[i]) && previous_flee$flee1[i] == "flee") {
          # If already fled in previous loop, keep fleeing
          flee1[i] = "flee"
        } else {
          # Otherwise, calculate new decision
          prob.stay1 = inv.logit(prob.intercept.flee + dif.hab * prob.slope.flee)
          flee1[i] = ifelse(rbinom(1, 1, prob.stay1), "stay", "flee")
        }
      }
      
      # For individual 2 (only if strategy is 'mut')
      if (!is.na(data$contest.strat2[i]) && data$contest.strat2[i] == "mut") {
        if (!is.na(previous_flee$flee2[i]) && previous_flee$flee2[i] == "flee") {
          # If already fled in previous loop, keep fleeing
          flee2[i] = "flee"
        } else {
          # Otherwise, calculate new decision
          prob.stay2 = inv.logit(prob.intercept.flee - dif.hab * prob.slope.flee)
          flee2[i] = ifelse(rbinom(1, 1, prob.stay2), "stay", "flee")
        }
      }
    }
  }
  
  return(list(flee1 = flee1, flee2 = flee2))
}


##############################Main simulation model#############################
library(ggplot2)
library(dplyr)
library(readr)
library(boot)
library(faux)
library(patchwork)
library(grid)

########External loop - determine how many times each scenario will be run######

results.total=c()#vector to save the mean reproductive success of each strategy across all simulations

for (k in 1:30) {
#Creating individuals that will establish in the territories. These individuals have four traits: energy (total energy that can be spent before dying), contest.invest (the maximum amount of energy that an individual invest per contest. It is a proportion of its total amount of energy), injury.cap (the amount of damage that an individual will make in a rival if it hits it during a contest), fight.skill (the ability of the individual to hit is opponent or flee from an attack).
  
terr.number=100 #number of individuals occupying a territory
percent=0.1 #proportion of total energy that each individual will spent per contest
perc.fighters=0.2 #proportion of individuals without a territory 
prob.modif.injury=0.1 #value that affect the slope of the logistic relationship that associate probability of hitting a rival in relation to the difference in fighting skill between rivals. Smaller values provide a less steeper relationship
prob.slope.flee=0.1 #value that affect the slope of the logistic relationship that associate probability that an individual adopting mutual assessment will flee from the contest in relation to the difference in fighting skill between rivals. Smaller values provide a less steeper relationship.
prob.intercept.flee=1 #value that affect the threshold for the probability of fleeing. Greater values reduces that probability of fleeing for a given difference in fighting skill
prob.mat1=0.8 #Probability that an individual in a territory will mate
prob.mat2=0.2 #Probability that an individual outside a territory will mate
contest.cost=1 #Basal contest cost. Individuals that remain in the contest have this value subtracted from the contest.invest at each contest iteration.
cor.energy=0.8#correlation between total energy and fighting skill of individuals.
cor.injury=0.8#correlation between total energy and injury capacity of individuals
mean.injury=0.6#values 0.65, 1.6 and 2.6 generated mean injury capacities representing 10%, 30% and 50% of average contest investment.


#Individual traits from the group tha will start with the possession of a reproductive resource
energy1=rnorm(terr.number, 100, 10)
contest.invest1=percent*energy1
injury.cap1=rnorm_pre(energy1, mu=mean.injury,sd=0.1, r=cor.injury)
injury.cap1=injury.cap1+min(injury.cap1)+0.01 #removing negative values
fight.skill1=rnorm_pre(energy1, mu=100,sd=10, r=cor.energy)
contest.strat.ori1=c(rep('self', 0.2*length(energy1)), rep('mut', 0.2*length(energy1)),rep('self-mut', 0.2*length(energy1)),rep('mut-self', 0.2*length(energy1)),rep('rand', 0.2*length(energy1)))#determine the strategy of each individual
contest.strat1=c(rep('self', 0.2*length(energy1)), rep('mut', 0.2*length(energy1)),rep('self', 0.2*length(energy1)),rep('mut', 0.2*length(energy1)),rep('self', 0.1*length(energy1)),rep('mut', 0.1*length(energy1)))#indicates the decision rule adopted during the contest.
rival.id1=c(1:length(energy1))
#plot(fight.skill1~energy1) #for checking purposes
#plot(injury.cap1~energy1) #for checking purposes

#Grouping all traits in a data frame
data=as.data.frame(list(energy1, contest.invest1, injury.cap1, fight.skill1, contest.strat.ori1, contest.strat1, rival.id1))
colnames(data)=c('energy1', 'contest.invest1', 'injury.cap1', 'fight.skill1', 'contest.strat.ori1', 'contest.strat1', 'rival.id1')

#Individual traits of a second group of contestants with the same traits that will fight against the first group. To determine that not all males in the first group will fight, some rows will be composed of NA values
energy2=rnorm(perc.fighters*length(energy1), 100, 10)
contest.invest2=percent*energy2
injury.cap2=rnorm_pre(energy2, mu=mean.injury,sd=0.1, r=cor.injury)
injury.cap2=injury.cap2+min(injury.cap2)+0.01#removing negative and zero values
fight.skill2=rnorm_pre(energy2, mu=100,sd=10, r=cor.energy)
contest.strat.ori2=c(rep('self', 0.2*length(energy2)), rep('mut', 0.2*length(energy2)),rep('self-mut', 0.2*length(energy2)),rep('mut-self', 0.2*length(energy2)),rep('rand', 0.2*length(energy2)))
contest.strat2=c(rep('self', 0.2*length(energy2)), rep('mut', 0.2*length(energy2)),rep('self', 0.2*length(energy2)),rep('mut', 0.2*length(energy2)),rep('self', 0.1*length(energy2)),rep('mut', 0.1*length(energy2)))
rival.id2=c((length(energy1)+1):(length(energy1)+length(energy2)))

#Adding NAs to the second group
energy2=c(energy2, rep(NA, length(energy1)-length(energy2)))
contest.invest2=c(contest.invest2, rep(NA, length(contest.invest1)-length(contest.invest2)))
injury.cap2=c(injury.cap2, rep(NA, length(injury.cap1)-length(injury.cap2)))
fight.skill2=c(fight.skill2, rep(NA, length(fight.skill1)-length(fight.skill2)))
contest.strat.ori2=c(contest.strat.ori2, rep(NA, length(contest.strat.ori1)-length(contest.strat.ori2)))
contest.strat2=c(contest.strat2, rep(NA, length(contest.strat1)-length(contest.strat2)))
rival.id2=c(rival.id2, rep(NA, length(rival.id1)-length(rival.id2)))

#Grouping traits from contestants in another data frame
rivals=as.data.frame(list(energy2, contest.invest2, injury.cap2, fight.skill2, contest.strat.ori2, contest.strat2, rival.id2))
colnames(rivals)=c('energy2', 'contest.invest2', 'injury.cap2', 'fight.skill2','contest.strat.ori2', 'contest.strat2', 'rival.id2')

#Randomizing individuals and NAs across rows
rivals=rivals[sample(nrow(rivals)),]

#Preparing to merge the data frames with individuals from groups 1 and 2
data$id=c(1:nrow(data))#created to merge data frames
rivals$id=c(1:nrow(rivals))#created to merge data frames

#final data frame with pairs of fighting individuals
data=merge(data, rivals, by = "id", all = TRUE)
summary(data)
data.original=data


#######Initial conditions for the middle loop that determine rival allocations and energy reduction after contests#####

stoping_condition_pop = F #condition to determine when this loop should stop (i.e. when all contestants completely deplete their total energy).
results.mean=c()#vector to save the mean reproductive success of each strategy after each contest
stop_pop=rep('no', nrow(data)) #condition to determine when the middle loop should stop (i.e. when there is a winner and a loser in each fighting pair)
total.energy1=data$energy1#vector with the total energy of individuals with the possession of the reproductive resource
total.energy2=data$energy2#vector with the total energy of contestants
result.energy1=c()#vector to save the remaining energy of individuals with the possession of the reproductive resource after a contest
result.energy2=c()#vector to save the remaining energy of contestants after a contest
result_external_loop1=c()#vector to save the results of each contest iteration of individuals with the possession of a reproductive resource over all contests
result_external_loop2=c()#vector to save the results of each contest iteration of contestants over all contests
result_owner.id=c()#vector to save the IDs of individuals in the possession of reproductive resources at each iteration of each contest 
result_contestant.id=c()#vector to save the IDs of contestants at each iteration of each contest 
result_contest.iteration.count=c()#vector to save the ID of each iteration of each contest
contest.count=0#necessary to correctly identify contest ID
result.contest.id=c()#vector to save the contest ID for reference of the contest energy lost during each iteration of each contest
result.contest.id2=c()#vector to save the contest ID for reference of the total energy lost after each contest
result_owner.id2=c()#vector to save the IDs of individuals in the possession of the reproductive resource for the calculations of energy lost after each contest
result_contestant.id2=c()#vector to save the IDs of contestants for the calculations of energy lost after each contest

####Middle loop - population level#####
while (!stoping_condition_pop) {
  
#######Initial conditions for the contest loop#####
  
invest1 = data$contest.invest1#vector with the contest.investment of individuals with the possession of the reproductive resource
invest2 = data$contest.invest2#vector with the contest.investment of contestants
result1 = numeric(nrow(data))#vector to save the remaining contest.invest of individuals with the possession of a reproductive resource after each contest
result2 = numeric(nrow(data))#vector to save the remaining contest.invest of contestants after each contest
stoping_condition_fight = F #condition to determine when each contest should stop
stop = rep('no', nrow(data))
fight.result1=c()#vector to group the decrease in contest.invest of individuals with the possession of a reproductive resource after each contest iteration. Provides information of the last contest
fight.result2=c()#vector to group the decrease in contest.invest of contestants after each contest iteration. Provides information of the last contest
owner.id=c()#vector to save the IDs of individuals in the possession of the reproductive resource in each contest
contestant.id=c()#vector to save the IDs of contestants in each contest
previous_flee = NULL
contest.iteration=c(rep(0, nrow(data)))#vector to identify the iteration number inside each contest
contest.ids=c()#vector to save the number of each contest during the last external loop
contest.count=contest.count+1#necessary for a correct identification of the contest number during the last external loop
contest.iteration.count=c()#vector to save the iteration number inside each contest


######Internal loop - simulation of each contest. Each iteration determine whether mutual assessors will stay or flee and for situation in which both rivals remain in the contest, calculate the loss in contest.investment######

while (!stoping_condition_fight) {
  res.flee.stay = flee.stay(invest1, invest2, prob.intercept.flee, prob.slope.flee, data, previous_flee)#determine for mutual assessor whether each one will stay or flee from the contest
   previous_flee = res.flee.stay
   contest.iteration=contest.iteration+1
  
#Simulating one step of a fight
  for (j in 1:nrow(data)) {
     # Task 1 (Keeping invest values for individuals that did not fight or individuals with zero invest)
    if (is.na(invest1[j]) | is.na(invest2[j]) | invest1[j] == 0 | invest2[j] == 0) {
      result1[j] = invest1[j]
      result2[j] = invest2[j]
      stop[j]='yes'} #if there is no rival or the contestant depleted its contest investment, the contest stops
      
    if (is.na(data$contest.strat1[j]) |is.na(data$contest.strat2[j])) {
      next  # skip to the next pair of rivals in case there is a NA for strategy
      }
      
      # Task 2 (simulating contests between self and mutual assessors)
    else if (data$contest.strat1[j] == "self" && data$contest.strat2[j] == "mut") {
        # Condition 1: keeping original invest values if the individual has no rival
      if (is.na(res.flee.stay$flee2[j])) {
        next
      } else if (res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]  # Individual 1 (self) keeps its current contest investment
          result2[j] = invest2[j]  # Individual 2 (mut) flees and keeps its current contest investment
          stop[j]='yes' #if an individual flee, the contest stops
        } 
        # Condition 2: if the mutual assessor stays
      else if (res.flee.stay$flee2[j] == "stay" & invest1[j]>0 & invest2[j]>0) {
        dif.hab1 = data$fight.skill1[j] - data$fight.skill2[j]#calculate the difference in fighting skill between rival 1 and 2
        dif.hab2 = data$fight.skill2[j] - data$fight.skill1[j]#calculate the difference in fighting skill between rival 2 and 1
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)#calculate a probability that individual 1 strikes individual 2
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)#calculate a probability that individual 2 strikes individual 1
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)#use the corresponding probability to determine if the strike actually occurred  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2)#use the corresponding probability to determine if the strike actually occurred 
        result2[j] = ifelse(prob.hit1 == 1, 
                             invest2[j] - contest.cost- data$injury.cap1[j], 
                             invest2[j] - contest.cost)#if individual 1 makes a successfully strike, individual 2 loses basal contest cost plus injury capacity of individual 1. Otherwise, loses only basal contest cost
        result1[j] = ifelse(prob.hit2 == 1, 
                             invest1[j] - contest.cost- data$injury.cap2[j], 
                             invest1[j] - contest.cost)#if individual 2 makes a successfully strike, individual 1 loses basal contest cost plus injury capacity of individual 2. Otherwise, loses only basal contest cost
        }
      }
      
      # Task 3 (simulating contests between mutual and self assessors)
    if (data$contest.strat1[j] == "mut" && data$contest.strat2[j] == "self") {
        # Condition 1 (Keeping invest values for individuals that did not fight)
      if (is.na(res.flee.stay$flee1[j])) {
        next
      } else if (res.flee.stay$flee1[j]=="flee") {
          result1[j] = invest1[j]  # Individual 2 (mut) flees and keeps its current fight investment
          result2[j] = invest2[j]  # Individual 1 (self) keeps its current fight investment
          stop[j]='yes' #if an individual flees, the contest stop
        } 
        # Condition 3: if the mutual assessor stays
      else if (res.flee.stay$flee1[j] == "stay" & invest1[j]>0 & invest2[j]>0) {
        dif.hab1 = data$fight.skill1[j] - data$fight.skill2[j]
        dif.hab2 = data$fight.skill2[j] - data$fight.skill1[j]
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
        result2[j] = ifelse(prob.hit1 == 1, 
                            invest2[j] - contest.cost- data$injury.cap1[j], 
                            invest2[j] - contest.cost)
        result1[j] = ifelse(prob.hit2 == 1, 
                            invest1[j] - contest.cost- data$injury.cap2[j], 
                            invest1[j] - contest.cost)
        }
      }
      
      # Task 4 (simulating contests between self assessors)
    else if (data$contest.strat1[j] == "self" && data$contest.strat2[j] == "self" & invest1[j]>0 & invest2[j]>0) {
      dif.hab1 = data$fight.skill1[j] - data$fight.skill2[j]
      dif.hab2 = data$fight.skill2[j] - data$fight.skill1[j]
      prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
      prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
      prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
      prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
      result2[j] = ifelse(prob.hit1 == 1, 
                          invest2[j] - contest.cost- data$injury.cap1[j], 
                          invest2[j] - contest.cost)
      result1[j] = ifelse(prob.hit2 == 1, 
                          invest1[j] - contest.cost- data$injury.cap2[j], 
                          invest1[j] - contest.cost)
      } 
      
      # Task 5 (simulating contests between mutual assessors)
    else if (data$contest.strat1[j] == "mut" && data$contest.strat2[j] == "mut") {
      
        # Condition 1: Keeping original invest values if there is no rival
      if (is.na(res.flee.stay$flee1[j]) | is.na(res.flee.stay$flee2[j])) {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes' #if there is no contestant, the contest do not occur
        } 
        # Condition 2: Keeping invest values if both rivals flees
      else if (res.flee.stay$flee1[j] == "flee" && res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes' #if both rivals flees, the contest stop
        } 
        # Condition 3: individual 1 stays and individual 2 flees. Both keeps their current fight investment
      else if (res.flee.stay$flee1[j] == "stay" && res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes' #if a rival flees, the contest stop
        } 
        # Condition 4: same as condition 3 but with individual 1 fleeing and individual 2 staying
      else if (res.flee.stay$flee1[j] == "flee" && res.flee.stay$flee2[j] == "stay") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes' #if a rival flees, the contest stop
        } 
        # Condition 5: same as self assessment if both stays
      else if (res.flee.stay$flee1[j] == "stay" && res.flee.stay$flee2[j] == "stay" & invest1[j]>0 & invest2[j]>0) {
        dif.hab1 = data$fight.skill1[j] - data$fight.skill2[j]
        dif.hab2 = data$fight.skill2[j] - data$fight.skill1[j]
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
        result2[j] = ifelse(prob.hit1 == 1, 
                            invest2[j] - contest.cost- data$injury.cap1[j], 
                            invest2[j] - contest.cost)
        result1[j] = ifelse(prob.hit2 == 1, 
                            invest1[j] - contest.cost- data$injury.cap2[j], 
                            invest1[j] - contest.cost)
        }
      }
    }
    

# Transforming negative contest investment values into zero
    result1 = pmax(result1, 0)
    result2 = pmax(result2, 0)
    
    
    contest.id=(rep(contest.count, length(result1)))
    
#Saving results of each contest iteration during the loop (for checking purposes)     
  fight.result1=append(fight.result1, result1)
  fight.result2=append(fight.result2, result2)
  owner.id=append(owner.id, data$rival.id1)
  contestant.id=append(contestant.id, data$rival.id2)
  contest.iteration.count=append(contest.iteration.count, contest.iteration)
  contest.ids=append(contest.ids, contest.id)
  
# Updating invest1 e invest2 for the next loop within the same contest
  invest1 = result1
  invest2 = result2
  
# Stopping condition
  stoping_condition_fight = all(stop == 'yes') # yes values are attributed for each pair whenever there is no opponent to contest the resource in the possession of rival 1 or one individual in the pair lost/flee the contest
  }


#Saving the results of the last contest loop with all within-contest iterations  (to check everything is correct with reductions in contest.invest)   
result_external_loop1=append(result_external_loop1, fight.result1)
result_external_loop2=append(result_external_loop2, fight.result2)
result_owner.id=append(result_owner.id, owner.id)
result_contestant.id=append(result_contestant.id, contestant.id)
result_contest.iteration.count=append(result_contest.iteration.count, contest.iteration.count)
result.contest.id=append(result.contest.id, contest.ids)


result_middle_loop=data.frame(result.contest.id=result.contest.id, result_contest.iteration.count=result_contest.iteration.count, result_owner.id=result_owner.id, result_contestant.id=result_contestant.id, result_external_loop1=result_external_loop1, result_external_loop2=result_external_loop2)

write_excel_csv2(result_middle_loop, 'result_middle_loop.csv')


#Calculating the amount of energy remaining from contest.invest after the contest
energyloss1=data$contest.invest1-result1
energyloss2=data$contest.invest2-result2

#Calculating the amount of total energy consumed after the contest
total.energy.loss1=total.energy1-energyloss1
total.energy.loss2=total.energy2-energyloss2

#Updating result1 and 2 in the data frame
data$result1=result1
data$result2=result2


#Calculating energy loss for each individual and keeping the energy of rival 1 if there is no rival2 or rival2 reaches energy = 0
for(i in 1: nrow(data)) {
  if (data$energy2[i]==0 | is.na(data$energy2[i])) {
    data$energy1[i]=data$energy1[i]
    data$energy2[i]=data$energy2[i] }
    else if (data$energy2[i]>0) {
     data$energy1[i]=data$energy1[i]-energyloss1[i]
     data$energy2[i]=data$energy2[i]-energyloss2[i]
   }
}

#Removing negative values - important when individuals have small amounts of energy
data$energy1=pmax(data$energy1, 0)
data$energy2=pmax(data$energy2, 0)


##Saving the results of the remaining total energy in the last external (to check whether reductions in total energy is correct)
result.energy1=append(result.energy1, data$energy1)
result.energy2=append(result.energy2, data$energy2)
result_owner.id2=append(result_owner.id2, data$rival.id1)
result_contestant.id2=append(result_contestant.id2, data$rival.id2)
result.contest.id2=append(result.contest.id2, contest.id)


results.energy=data.frame(result.contest.id2=result.contest.id2,result_owner.id2=result_owner.id2, result_contestant.id2=result_contestant.id2, result.energy1=result.energy1, result.energy2=result.energy2)

write_excel_csv2(results.energy, 'results.energy.csv')


#Changing individuals positions when ind1 lose the contest. Otherwise they remain with the same resource
for(i in 1:nrow(data)) {
  if (!is.na(data$result1[i]) && data$result1[i] == 0) {
    temp_row = data[i, 2:8]
    data[i, 2:8] = data[i, 9:15]
    data[i, 9:15] = temp_row
  }
}


for (i in 1:nrow(data)){
stop_pop[i]=ifelse(data$energy2[i]<=0 | is.na(data$energy2[i]), 'yes', 'no') #whenever an individual depletes all its energy, its condition change to yes
}

#determining reproductive success
data$prob.mating1=rbinom(nrow(data), 1, prob.mat1)#determine whether each individual with the possession of a reproductive resource after the contest was able to copulate
data$prob.mating2=rbinom(nrow(data), 1, prob.mat2)#determine whether each contestant was able to copulate after the contest

data$rep.success1=rpois(nrow(data),10)*data$prob.mating1#calculate the number of offspring for individuals with the reproductive resource that were successful in mating
data$rep.success2=rpois(nrow(data),10)*data$prob.mating2#calculate the number of offspring for contestants that were successful in mating


#Function to calculate the mean reproductive success for each original strategy

calculate_strategy_mean <- function(strategy, data, total_initial_counts) {
  # Reproductive success of individuals with the possession of a reproductive resource separated by each strategy
  rs1 = data$rep.success1[data$contest.strat.ori1 == strategy]
  # Reproductive success of contestants separated by each strategy
  rs2 = data$rep.success2[data$contest.strat.ori2 == strategy]
  
  # Suming the reproductive success of all individual separated by strategies
  total_rs = sum(rs1, na.rm = TRUE) + sum(rs2, na.rm = TRUE)
  
  # Initial number of individuals with each strategy
  initial_n = total_initial_counts[[strategy]]
  
  # Mean reproductive success of individuals per strategy
  if(initial_n > 0) {
    return(total_rs / initial_n)
  } else {
    return(NA)
  }
}

# Number of individuals per strategy (i.e. at the start of the middle loop)
initial_counts = c(
  'self' = sum(data.original$contest.strat.ori1 == 'self', na.rm = TRUE) + 
    sum(data.original$contest.strat.ori2 == 'self', na.rm = TRUE),
  'mut' = sum(data.original$contest.strat.ori1 == 'mut', na.rm = TRUE) + 
    sum(data.original$contest.strat.ori2 == 'mut', na.rm = TRUE),
  'self-mut' = sum(data.original$contest.strat.ori1 == 'self-mut', na.rm = TRUE) +     sum(data.original$contest.strat.ori2 == 'self-mut', na.rm = TRUE),
  'mut-self' = sum(data.original$contest.strat.ori1 == 'mut-self', na.rm = TRUE) +     sum(data.original$contest.strat.ori2 == 'mut-self', na.rm = TRUE),
  'rand' = sum(data.original$contest.strat.ori1 == 'rand', na.rm = TRUE) + 
    sum(data.original$contest.strat.ori2 == 'rand', na.rm = TRUE)
)

# applying the function to calculate the mean reproductive success of individual per strategy:
mean.self <- calculate_strategy_mean('self', data, initial_counts)
mean.mut <- calculate_strategy_mean('mut', data, initial_counts)
mean.self.mut <- calculate_strategy_mean('self-mut', data, initial_counts)
mean.mut.self <- calculate_strategy_mean('mut-self', data, initial_counts)
mean.rand <- calculate_strategy_mean('rand', data, initial_counts)

#grouping the mean reproductive success of individuals per strategy after each contest
mean.sim <- c(mean.self, mean.mut, mean.self.mut, mean.mut.self, mean.rand)

#determining new strategies for mut-self, self-mut and rand

for (i in 1:nrow(data)){
if(is.na(data$contest.strat1[i])) next
if (data$contest.strat.ori1[i]=='self-mut' & data$contest.strat1[i]=='self' & rbinom(1,1,0.5)==1){
data$contest.strat1[i]='mut'
} else if (data$contest.strat.ori1[i]=='mut-self' & data$contest.strat1[i]=='mut' & rbinom(1,1,0.5)==1){
  data$contest.strat1[i]='self'
} else if (data$contest.strat.ori1[i]=='rand' & data$contest.strat1[i]=='self' & rbinom(1,1,0.5)==1){
  data$contest.strat1[i]='mut'
} else if (data$contest.strat.ori1[i]=='rand' & data$contest.strat1[i]=='mut' & rbinom(1,1,0.5)==1){
  data$contest.strat1[i]='self'
  }
}

#Randomly distribution of losers among reproductive resources (i.e. lines) to begin a new cycle of contests
data[, 9:15] = data[sample(nrow(data)), 9:15]

#adding the mean reproductive success of individuals in each strategy after each contest in a single vector
results.mean=append(results.mean,mean.sim)

stoping_condition_pop = all(stop_pop=='yes')#stop the middle loop when all contestans die (i.e. depletes all their energy)

#print(stoping_condition_pop) 
}

results.total=append(results.total,results.mean)
strategies=rep(c('self', 'mut', 'self-mut', 'mut-self', 'rand'), length(results.total)/5)
}

####Final figure####

dados.fig = data.frame(
  strategies = strategies,       
  results.total = results.total    
)

# 2. Caculating summary statistics
dados_summary = dados.fig %>%
  group_by(strategies) %>%
  summarise(
    mean = mean(results.total, na.rm = TRUE),
    n = sum(!is.na(results.total)),
    sd = sd(results.total, na.rm = TRUE),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se
  ) %>%
  filter(n > 0)  

ordem_estrategias = c("self", "mut", "self-mut", "mut-self", "rand")
rotulos_estrategias = c("S", 
                         "M", 
                         "S\u2192M", 
                         "M\u2192S", 
                         "R")

# Assuring that the data is in the correct order
dados_summary$strategy = factor(dados_summary$strategies, 
                                 levels = ordem_estrategias,
                                 labels = rotulos_estrategias)

# Figure
ggplot(dados_summary, aes(x = strategy, y = mean)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    color = "blue",
    linewidth = 0.8
  ) +
  labs(
    x = "Assessment Strategy",
    y = "Mean Reproductive Success"
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    axis.title = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 35)  
  )