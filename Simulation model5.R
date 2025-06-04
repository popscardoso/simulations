####Simulation model for lifetime reproductive success of different assessment rules during dyadic contests#####

####Creating helping functions####

####Function to determine whether each rival should flee during a contest if it adopts mutual assessment####
####increasing prob.intercept.flee, reduce prob of flee for s given dif in fight.hab. and reducing and prob.slope.flee reduce the probability change for intermediate values of dif.hab
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
      dif.hab = data$fight.hab1[i] - data$fight.hab2[i]
      
      # For individual 1 (only if strategy is 'mut')
      if (!is.na(data$fight.strat1[i]) && data$fight.strat1[i] == "mut") {
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
      if (!is.na(data$fight.strat2[i]) && data$fight.strat2[i] == "mut") {
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


results.total=c()
for (k in 1:10) {
#Creating individuals that will establish in the territories. These individuals have four traits: energy (total energy that can be spent before dying), fight.invest (the maximum amount of energy that an individual invest per fight. It is a proportion of its total amount of energy), injury.cap (the amount of damage that an individual will make in a rival if it hits it during a fight), fight.hab (the hability of the individual to hit is opponent or flee from an attack)
terr.number=100 #number of individuals occupying a territory
percent=0.1 #proportion of total energy that each individual will spent per contest
perc.fighters=0.2 #proportion of individuals without a territory 
prob.modif.injury=0.1 #value that affect the slope of the logistic relationship that associate probability of hitting a rival in relation to the difference in fighting ability between rivals. Smaller values provide a less steeper relationship
prob.slope.flee=0.1 #value that affect the slope of the logistic relationship that associate probability that an individual adopting mutual assessment will flee from the contest in relation to the difference in fighting ability between rivals. Smaller values provide a less steeper relationship
prob.intercept.flee=1 #value that affect the threshold for the probability of fleeing. Greater values reduces that probability of fleeing for a given difference in fighting ability
prob.mat1=0.8 #Probability that an individual in a territory will mate
prob.mat2=0.2 #Probability that an individual outside a territory will mate
fighting.cost=1 #Basal cost of fight. Individuals that keep fighting have this value subtracted from the fight invest at each fight time step
cor.energy=0.8#correlation between energy and fighting hability of individuals
#injury.cap1=runif(terr.number, min=injury.min*percent, max=injury.max*percent) - original commando for injury.cap
cor.injury=0.8#correlation between energy and injury capacity of individuals
mean.injury=1.6#values 0.65, 1.6 and 2.6 generated mean injury capacities represeting 10%, 30% and 50% of contest investment



energy1=rnorm(terr.number, 100, 10)
fight.invest1=percent*energy1 #posso colocar a porcentagem como uma média com var tb
injury.cap1=rnorm_pre(energy1, mu=mean.injury,sd=0.1, r=cor.injury)
injury.cap1=injury.cap1+min(injury.cap1)+0.01 #removing negative values
fight.hab1=rnorm_pre(energy1, mu=100,sd=10, r=cor.energy)
fight.strat.ori1=c(rep('self', 0.2*length(energy1)), rep('mut', 0.2*length(energy1)),rep('self-mut', 0.2*length(energy1)),rep('mut-self', 0.2*length(energy1)),rep('rand', 0.2*length(energy1)))
fight.strat1=c(rep('self', 0.2*length(energy1)), rep('mut', 0.2*length(energy1)),rep('self', 0.2*length(energy1)),rep('mut', 0.2*length(energy1)),rep('self', 0.1*length(energy1)),rep('mut', 0.1*length(energy1)))
#plot(fight.hab1~energy1) (#for checking purposes)
#plot(injury.cap1~energy1) #for checking purposes
#mean(injury.cap1)

data=as.data.frame(list(energy1, fight.invest1, injury.cap1, fight.hab1, fight.strat.ori1, fight.strat1))
colnames(data)=c('energy1', 'fight.invest1', 'injury.cap1', 'fight.hab1', 'fight.strat.ori1', 'fight.strat1')

#setting a second group of rivals with the same traits that will fight against the first group. To determine that not all males in the first group will fight, some rows will be composed of NA values
energy2=rnorm(perc.fighters*length(energy1), 100, 10)
fight.invest2=percent*energy2#posso colocar a porcentagem como uma média com var tb
injury.cap2=rnorm_pre(energy2, mu=mean.injury,sd=0.1, r=cor.injury)
injury.cap2=injury.cap2+min(injury.cap2)+0.01#removing negative and zero values
fight.hab2=rnorm_pre(energy2, mu=100,sd=10, r=cor.energy)
fight.strat.ori2=c(rep('self', 0.2*length(energy2)), rep('mut', 0.2*length(energy2)),rep('self-mut', 0.2*length(energy2)),rep('mut-self', 0.2*length(energy2)),rep('rand', 0.2*length(energy2)))
fight.strat2=c(rep('self', 0.2*length(energy2)), rep('mut', 0.2*length(energy2)),rep('self', 0.2*length(energy2)),rep('mut', 0.2*length(energy2)),rep('self', 0.1*length(energy2)),rep('mut', 0.1*length(energy2)))
#plot(fight.hab2~energy2) (#for checing purposes)

energy2=c(energy2, rep(NA, length(energy1)-length(energy2)))
fight.invest2=c(fight.invest2, rep(NA, length(fight.invest1)-length(fight.invest2)))
injury.cap2=c(injury.cap2, rep(NA, length(injury.cap1)-length(injury.cap2)))
fight.hab2=c(fight.hab2, rep(NA, length(fight.hab1)-length(fight.hab2)))
fight.strat.ori2=c(fight.strat.ori2, rep(NA, length(fight.strat.ori1)-length(fight.strat.ori2)))
fight.strat2=c(fight.strat2, rep(NA, length(fight.strat1)-length(fight.strat2)))


rivals=as.data.frame(list(energy2, fight.invest2, injury.cap2, fight.hab2, fight.strat.ori2, fight.strat2))
colnames(rivals)=c('energy2', 'fight.invest2', 'injury.cap2', 'fight.hab2','fight.strat.ori2', 'fight.strat2')

#grouping individuals in a data frame do randomize who will be paired
rivals=rivals[sample(nrow(rivals)),]

data$id=c(1:nrow(data))#created to merge data frames
rivals$id=c(1:nrow(rivals))#created to merge data frames

#final data frame with pairs of fighting individuals
data=merge(data, rivals, by = "id", all = TRUE)
summary(data)
data.original=data

stoping_condition_pop = F
results.mean=c()
stop_pop=rep('no', nrow(data))

#########

total.energy1=data$energy1
total.energy2=data$energy2
result.energy1=c()
result.energy2=c()
result_external_loop1=c()
result_external_loop2=c()


while (!stoping_condition_pop) {
  
###### Loop for contests######
invest1 = data$fight.invest1
invest2 = data$fight.invest2
result1 = numeric(nrow(data))
result2 = numeric(nrow(data))
loops_until_flee = rep(NA, nrow(data))
loop_count = 0
stoping_condition_fight = F
loop_results_list = list()
stop = rep('no', nrow(data))
fight.result1=c()
fight.result2=c()
fight.energy.loss1=c()
fight.energy.loss2=c()
previous_flee = NULL

while (!stoping_condition_fight) {#simulates one fight
   loop_count = loop_count + 1
   res.flee.stay = flee.stay(invest1, invest2, prob.intercept.flee, prob.slope.flee, data, previous_flee)
   previous_flee = res.flee.stay
  
#Simulating one step of a fight
  for (j in 1:nrow(data)) {
     # Task 1 (Keeping invest values for individuals that did not fight or individuals with zero invest)
    if (is.na(invest1[j]) | is.na(invest2[j]) | invest1[j] == 0 | invest2[j] == 0) {
      result1[j] = invest1[j]
      result2[j] = invest2[j]
      stop[j]='yes'}
      
    if (is.na(data$fight.strat1[j]) |is.na(data$fight.strat2[j])) {
      next  # Go to the next pair of rivals in case there is a NA
      }
      
      # Task 2 (simulating contests between self and mutual assessors)
    else if (data$fight.strat1[j] == "self" && data$fight.strat2[j] == "mut") {
        # Condition 1: keeping original invest values if the individual has no rival
      if (is.na(res.flee.stay$flee2[j])) {
        next
      } else if (res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]  # Individual 1 (self) keeps its current fight investment
          result2[j] = invest2[j]  # Individual 2 (mut) flees and keeps its current fight investment
          stop[j]='yes'
        } 
        # Condition 2: if the mutual assessor stay
      else if (res.flee.stay$flee2[j] == "stay") {
        dif.hab1 = data$fight.hab1[j] - data$fight.hab2[j]
        dif.hab2 = data$fight.hab2[j] - data$fight.hab1[j]
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
        result2[j] = ifelse(prob.hit1 == 1, 
                             invest2[j] - fighting.cost- data$injury.cap1[j], 
                             invest2[j] - fighting.cost)
        result1[j] = ifelse(prob.hit2 == 1, 
                             invest1[j] - fighting.cost- data$injury.cap2[j], 
                             invest1[j] - fighting.cost)
        }
      }
      
      # Task 3 (simulating contests between mutual and self assessors)
    if (data$fight.strat1[j] == "mut" && data$fight.strat2[j] == "self") {
        # Condition 1 (Keeping invest values for individuals that did not fight)
      if (is.na(res.flee.stay$flee1[j])) {
        next
      } else if (res.flee.stay$flee1[j]=="flee") {
          result1[j] = invest1[j]  # Individual 2 (mut) flees and keeps its current fight investment
          result2[j] = invest2[j]  # Individual 1 (self) keeps its current fight investment
          stop[j]='yes'
        } 
        # Condition 3: if the mutual assessor stays
      else if (res.flee.stay$flee1[j] == "stay") {
        dif.hab1 = data$fight.hab1[j] - data$fight.hab2[j]
        dif.hab2 = data$fight.hab2[j] - data$fight.hab1[j]
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
        result2[j] = ifelse(prob.hit1 == 1, 
                            invest2[j] - fighting.cost- data$injury.cap1[j], 
                            invest2[j] - fighting.cost)
        result1[j] = ifelse(prob.hit2 == 1, 
                            invest1[j] - fighting.cost- data$injury.cap2[j], 
                            invest1[j] - fighting.cost)
        }
      }
      
      # Task 4 (simulating contests between self assessors)
    else if (data$fight.strat1[j] == "self" && data$fight.strat2[j] == "self") {
      dif.hab1 = data$fight.hab1[j] - data$fight.hab2[j]
      dif.hab2 = data$fight.hab2[j] - data$fight.hab1[j]
      prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
      prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
      prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
      prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
      result2[j] = ifelse(prob.hit1 == 1, 
                          invest2[j] - fighting.cost- data$injury.cap1[j], 
                          invest2[j] - fighting.cost)
      result1[j] = ifelse(prob.hit2 == 1, 
                          invest1[j] - fighting.cost- data$injury.cap2[j], 
                          invest1[j] - fighting.cost)
      } 
      
      # Task 5 (simulating contests between mutual assessors)
    else if (data$fight.strat1[j] == "mut" && data$fight.strat2[j] == "mut") {
      
        # Condition 1: Keeping original invest values if there is no rival
      if (is.na(res.flee.stay$flee1[j]) | is.na(res.flee.stay$flee2[j])) {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes'
        } 
        # Condition 2: Keeping invest values if both rivals flees
      else if (res.flee.stay$flee1[j] == "flee" && res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes'
        } 
        # Condition 3: individual 1 stays and individual 2 flees. Both keeps their current fight investment
      else if (res.flee.stay$flee1[j] == "stay" && res.flee.stay$flee2[j] == "flee") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes'
        } 
        # Condition 4: same as condition 3 but with individual 1 fleeing and indivudal 2 staying
      else if (res.flee.stay$flee1[j] == "flee" && res.flee.stay$flee2[j] == "stay") {
          result1[j] = invest1[j]
          result2[j] = invest2[j]
          stop[j]='yes'
        } 
        # Condition 5: same as self assessment if both stays
      else if (res.flee.stay$flee1[j] == "stay" && res.flee.stay$flee2[j] == "stay") {
        dif.hab1 = data$fight.hab1[j] - data$fight.hab2[j]
        dif.hab2 = data$fight.hab2[j] - data$fight.hab1[j]
        prob.hit.cont1 = inv.logit(runif(1,-0.3,0.3)+dif.hab1 * prob.modif.injury)
        prob.hit.cont2 = inv.logit(runif(1,-0.3,0.3)+dif.hab2 * prob.modif.injury)
        prob.hit1 = rbinom(1, 1, prob.hit.cont1)  
        prob.hit2 = rbinom(1, 1, prob.hit.cont2) 
        result2[j] = ifelse(prob.hit1 == 1, 
                            invest2[j] - fighting.cost- data$injury.cap1[j], 
                            invest2[j] - fighting.cost)
        result1[j] = ifelse(prob.hit2 == 1, 
                            invest1[j] - fighting.cost- data$injury.cap2[j], 
                            invest1[j] - fighting.cost)
        }
      }
      
      # Updating loop count
      loops_until_flee = loop.count.func(loops_until_flee, res.flee.stay, j, loop_count)
    }
    

# Transforming negative invest values into zero
    result1 = pmax(result1, 0)
    result2 = pmax(result2, 0)
    
#Saving results of each fighting loop (for checking purposes)    
    fight.result1=append(fight.result1, result1)
    fight.result2=append(fight.result2, result2)
    
#Calculating the amount of energy lost in each loop
    loss1=invest1-result1
    loss2=invest2-result2
    
#Saving the amount of energy lost during each loop
    fight.energy.loss1=append(fight.energy.loss1, loss1)
    fight.energy.loss2=append(fight.energy.loss2, loss2)
    

# Updating invest1 e invest2 for the next loop
  invest1 = result1
  invest2 = result2
  
  
# Stopping condition
  stoping_condition_fight = all(stop == 'yes')
  }


#Saving the results of each fighting loop
result_external_loop1=append(result_external_loop1, fight.result1)
result_external_loop2=append(result_external_loop2, fight.result2)
id=rep(c(1:50), length(result_external_loop1)/nrow(data))

result_external_loop=data.frame(id=id, result_external_loop1=result_external_loop1, result_external_loop2=result_external_loop2)

write_excel_csv2(result_external_loop, 'result_external_loop.csv')


#Calculating the amount of energy remaining from fight.invest afetr the fight
energyloss1=data$fight.invest1-result1
energyloss2=data$fight.invest2-result2

#Calculating the amount of total energy consumed. after que fight
perdatotal1=total.energy1-energyloss1
perdatotal2=total.energy2-energyloss2

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

#Removing negative values
data$energy1=pmax(data$energy1, 0)
data$energy2=pmax(data$energy2, 0)


#Saving the results of the remaining energy in each external loop (for checkng purposes)
result.energy1=append(result.energy1, data$energy1)
result.energy2=append(result.energy2, data$energy2)
id.energy=rep(c(1:50), length(result.energy1)/nrow(data))

results.energy=data.frame(id=id.energy,result.energy1=result.energy1, result.energy2=result.energy2, energyloss1=perdatotal1, energyloss2=perdatotal2)
write_excel_csv2(results.energy, 'results.energy.csv')


#result.teste2=data.frame(energyloss1=energyloss1, energyloss2=energyloss2, perdatotal1=perdatotal1, perdatotal2=perdatotal2, total.energy1=total.energy1, total.energy2=total.energy2)
#write_excel_csv2(result.teste2, 'result.teste2.csv')


#Changing individuals positions when ind1 lose the contest
for(i in 1:nrow(data)) {
  if (!is.na(data$result1[i]) && data$result1[i] == 0) {
    temp_row = data[i, 2:7]
    data[i, 2:7] = data[i, 8:13]
    data[i, 8:13] = temp_row
  }
}


for (i in 1:nrow(data)){
stop_pop[i]=ifelse(data$energy2[i]<=0 | is.na(data$energy2[i]), 'yes', 'no')
}

#determining reproductive success
data$prob.mating1=rbinom(nrow(data), 1, prob.mat1)
data$prob.mating2=rbinom(nrow(data), 1, prob.mat2)

data$rep.success1=rpois(nrow(data),10)*data$prob.mating1
data$rep.success2=rpois(nrow(data),10)*data$prob.mating2


#saving mean reproductive success for each original strategy

calculate_strategy_mean <- function(strategy, data, total_initial_counts) {
  # Sucesso reprodutivo dos indivíduos 1 com essa estratégia
  rs1 = data$rep.success1[data$fight.strat.ori1 == strategy]
  # Sucesso reprodutivo dos indivíduos 2 com essa estratégia
  rs2 = data$rep.success2[data$fight.strat.ori2 == strategy]
  
  # Soma todos os valores válidos
  total_rs = sum(rs1, na.rm = TRUE) + sum(rs2, na.rm = TRUE)
  
  # Obtém o número inicial de indivíduos para esta estratégia
  initial_n = total_initial_counts[[strategy]]
  
  # Calcula a média normalizada
  if(initial_n > 0) {
    return(total_rs / initial_n)
  } else {
    return(NA)
  }
}

# Primeiro calcula o número inicial de indivíduos por estratégia (ANTES do loop principal)
initial_counts = c(
  'self' = sum(data.original$fight.strat.ori1 == 'self', na.rm = TRUE) + 
    sum(data.original$fight.strat.ori2 == 'self', na.rm = TRUE),
  'mut' = sum(data.original$fight.strat.ori1 == 'mut', na.rm = TRUE) + 
    sum(data.original$fight.strat.ori2 == 'mut', na.rm = TRUE),
  'self-mut' = sum(data.original$fight.strat.ori1 == 'self-mut', na.rm = TRUE) + 
    sum(data.original$fight.strat.ori2 == 'self-mut', na.rm = TRUE),
  'mut-self' = sum(data.original$fight.strat.ori1 == 'mut-self', na.rm = TRUE) + 
    sum(data.original$fight.strat.ori2 == 'mut-self', na.rm = TRUE),
  'rand' = sum(data.original$fight.strat.ori1 == 'rand', na.rm = TRUE) + 
    sum(data.original$fight.strat.ori2 == 'rand', na.rm = TRUE)
)

# Depois no seu loop principal, substitua o cálculo das médias por:
mean.self <- calculate_strategy_mean('self', data, initial_counts)
mean.mut <- calculate_strategy_mean('mut', data, initial_counts)
mean.self.mut <- calculate_strategy_mean('self-mut', data, initial_counts)
mean.mut.self <- calculate_strategy_mean('mut-self', data, initial_counts)
mean.rand <- calculate_strategy_mean('rand', data, initial_counts)

mean.sim <- c(mean.self, mean.mut, mean.self.mut, mean.mut.self, mean.rand)

#determining new strategies for mut-self, self-mut and rand

for (i in 1:nrow(data)){
if(is.na(data$fight.strat1[i])) next
if (data$fight.strat.ori1[i]=='self-mut' & data$fight.strat1[i]=='self' & rbinom(1,1,0.5)==1){
data$fight.strat1[i]='mut'
} else if (data$fight.strat.ori1[i]=='mut-self' & data$fight.strat1[i]=='mut' & rbinom(1,1,0.5)==1){
  data$fight.strat1[i]='self'
} else if (data$fight.strat.ori1[i]=='rand' & data$fight.strat1[i]=='self' & rbinom(1,1,0.5)==1){
  data$fight.strat1[i]='mut'
} else if (data$fight.strat.ori1[i]=='rand' & data$fight.strat1[i]=='mut' & rbinom(1,1,0.5)==1){
  data$fight.strat1[i]='self'
  }
}

#reposicionamento dos perdedores entre os territórios
data[, 8:13] = data[sample(nrow(data)), 8:13]

results.mean=append(results.mean,mean.sim)
stoping_condition_pop = all(stop_pop=='yes')#quando os bichos mudarem de posição, tem que inbserir uma regra que se um yes previo foi ocupado por um bicho com energy2 maior que zero, ele tem que voltar pra no.

#print(stoping_condition_pop) 
}

results.total=append(results.total,results.mean)


strategies=rep(c('self', 'mut', 'self-mut', 'mut-self', 'rand'), length(results.total)/5)
}

####Final figure####

dados.fig = data.frame(
  strategies = strategies,       # Seu vetor de estratégias
  results.total = results.total    # Seu vetor de valores numéricos
)

# 2. Calcular estatísticas resumidas
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
  filter(n > 0)  # Remove estratégias sem dados

ordem_estrategias = c("self", "mut", "self-mut", "mut-self", "rand")
rotulos_estrategias = c("S", 
                         "M", 
                         "S\u2192M", 
                         "M\u2192S", 
                         "R")

# Garantir que os dados estejam na ordem correta
dados_summary$strategy = factor(dados_summary$strategies, 
                                 levels = ordem_estrategias,
                                 labels = rotulos_estrategias)

# Criar o gráfico
b=ggplot(dados_summary, aes(x = strategy, y = mean)) +
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
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  # Quebra de linha
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    axis.title = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 35)  # Ajuste para rótulos longos
  )

#Saving all figures
fig.final = 
  (a | b | c) /  
  (d | e | f) /  
  (g | h | i)    


fig.final + 
  plot_annotation(tag_levels = 'a') +  # Adiciona letras (a, b, c, ...)
  plot_layout(guides = "collect")       # Unifica legendas, se houver


ggsave("fig.final.png", fig.final, width = 12, height = 10, dpi = 300)