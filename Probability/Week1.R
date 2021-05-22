#install.packages('tidyverse')
#install.packages('gtools')
library(gtools)
library(tidyverse)

#=======================================

#        WEEK - 1 ; SECTION - 2        #

#=======================================

# Game of Cards
suits <- c("D", "C", "H", "S")
numbers <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K")

# Making a Deck
deck <- expand.grid(suits = suits,numbers = numbers)
deck <- paste(deck$numbers , deck$suits)

# Possibilty of getting a King
kings <- paste("K",suits)   #win condition

p_king <- round(prop.table(table(deck %in% kings))[["TRUE"]], digits = 4)
p_king #Probability of drawing a King from a Deck



##PERMUTATIONS

#load library
library(gtools)

n <- length(deck) 
r <- 2

#randomly picking n cards from a deck WITH REPLACEMENT
hands_r <- permutations(n, r, v = deck, repeats.allowed = T)  # n^r
nrow(hands_r)

#randomly picking n cards from a deck WITHOUT REPLACEMENT
hands <- permutations(n, r, v = deck) # nCr
allHands <- nrow(hands)
card1 <- hands[,1] #selects first column (first card of all hands)
card2 <- hands[,2] #selects second column (second card of all hands)

#probability of first card King
p_card1 = sum(card1 %in% kings)/allHands

# second card ALSO being a king 
p_card1_card2 = sum(card1 %in% kings & card2 %in% kings)/allHands # NOTE: single '&' for AND

#Fraction of First hand king with Second card King
#OR
#Probability of Second Card King given First Card King
pConditional_card2 = p_card1_card2/p_card1  # P(A|B) = P(A⋂B)/P(B)


##Probability that a class of 50 has atleast two overlapping birthdays

# Function for Monte Carlo Simulation
    #NOTE: Assignment of def. val with '='
mcBirthdayProb <- function(n,B = 100000){
  mean(replicate(B, {
    bDays <- sample(1:365,n, replace = TRUE) #selecting 50 bdays w. replacement
    any(duplicated(bDays))
  }))
}

# Function for exact Probability
    #NOTE: Do not use Combinations function as it is computationall taxing
exBirthdayProb <- function(n){
  1 - prod(seq(365,365-n+1)/365)
}

p_birthday_MC <- mcBirthdayProb(50)
p_birthday <- exBirthdayProb(50)

#USING SAPPLY function for element wise iteration.
x <- 1:60
prob_mc <- sapply(x, mcBirthdayProb)
plot(x,prob_mc) #Plotting a x vs Prob(x) graph

prob <- sapply(x, exBirthdayProb)
lines(x, prob, col="red") # Plotting a line graph

#Stability of Monte Carlo Simulations
    #Vary Batch sizes and plot result for a fixed experiment

mcProb <- function(B, n=20){  #20 students
  mean(replicate(B, {
    bDays <- sample(1:365,n, replace = TRUE) #selecting 50 bdays w. replacement
    any(duplicated(bDays))
  }))
}

B <- 10^seq(0,5, len=100) # 10^(vector 100 entries range: [0,5])
mcStability <- sapply(B,mcProb)
plot(log10(B),mcStability, type="l") # In this case, MC stabilises after 10^3


#=======================================

#        WEEK - 1 ; SECTION - 3        #

#=======================================


# Monty Hall by Monte Carlo

B <- 10000
door <- as.character(1:3) #doors defined

results <- mean(replicate(B,{
  prize <- sample(c("Car","Goat","Goat")) #Prize Distributed
  prizeDoor <- door[prize == "Car"]
  
  # Picking first door
  choice1 = sample(door,1)
  
  #Showing a different door with goat
  goatDoor <- sample(door[!(door %in% c(prizeDoor, choice1))],1)
  
  #Switch Door ( Win probability : 0.67 )
  finalDoorChoice = door[!(door %in% c(choice1,goatDoor))]
  
  # Door Not Switched ( Win probability : 0.33 )
  #finalDoorChoice <- choice1
  
  (prizeDoor == finalDoorChoice)  
}))


#=======================================

#       WEEK - 1  ; AssIGNMENT         #

#=======================================

seed.set(1)
B <- 10000

# Question - 1
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

#Probability that all 3 winners are Jamaican
mean(
  replicate(B,{
    winners <- sample(runners,3)
    sum(winners %in% "Jamaica") == 3
  })
)


# Question - 2

entree <- seq(1,12,1)
sapply(entree,function(n){paste(n,(45*n > 365))})

side <- seq(2,12,1)
sapply(side,function(n){paste(n,(18*nrow(combinations(n,2, v=1:n)) > 365))})


# Question - 3,4

all_cases = sum(esoph["ncases"])
all_controls = sum(esoph["ncontrols"])
p_cases = all_cases/(all_cases+all_controls) # P(Case)
p_controls = all_controls/(all_cases+all_controls) # P(Controls)

max_alcgp = esoph[(esoph["alcgp"] == "120+"),]
ncases_max_alcgp = sum(max_alcgp["ncases"])
ncontrols_max_alcgp = sum(max_alcgp["ncontrols"])
p_cases_max_alcgp = ncases_max_alcgp/(ncases_max_alcgp+ncontrols_max_alcgp)


min_alcgp = esoph[(esoph["alcgp"] == "0-39g/day"),]
ncases_min_alcgp = sum(min_alcgp["ncases"])
ncontrols_min_alcgp = sum(min_alcgp["ncontrols"])
p_cases_min_alcgp = ncases_min_alcgp/(ncases_min_alcgp+ncontrols_min_alcgp)


min_tobgp <- esoph[(esoph["tobgp"] == "0-9g/day"),]
#number of cases Smokes more than 9g
ncases_min_tobgp = sum(min_tobgp["ncases"])
ncases_tob_MoreThan10g = all_cases - ncases_min_tobgp

#P(Smokes more than 9g & Case)
p_cases_tob_moreThan10g = ncases_tob_MoreThan10g/(all_cases+all_controls)
#P(Smokes more than 9g/ Case) = P(Smokes more than 9g & Case)/P(Cases)
p_cases_conditional = p_cases_tob_moreThan10g/p_cases

#number of controls Smokes more than 9g
ncontrols_min_tobgp = sum(min_tobgp["ncontrols"])
ncontrols_tob_MoreThan10g = all_controls - ncontrols_min_tobgp

#P(Smokes more than 9g & controls)
p_controls_tob_moreThan10g = ncontrols_tob_MoreThan10g/(all_cases+all_controls)
#P(Smokes more than 9g/ controls) = P(Smokes more than 9g & Case)/P(controls)
p_controls_conditional = p_controls_tob_moreThan10g/p_controls


# Question - 5,6

#CASES
ncases_max_alcgp/all_cases


max_tobgp = esoph[(esoph["tobgp"] == "30+"),]
ncases_max_tobgp = sum(max_tobgp["ncases"])
ncases_max_tobgp/all_cases

max_tobgp_alcgp = esoph[(esoph["tobgp"] == "30+" & esoph["alcgp"] == "120+"),]
ncases_max_tobgp_alcgp = sum(max_tobgp_alcgp["ncases"])
ncases_max_tobgp_alcgp/all_cases


max_tobgp_OR_alcgp = esoph[(esoph["tobgp"] == "30+" | esoph["alcgp"] == "120+"),]
ncases_max_tobgp_OR_alcgp = sum(max_tobgp_OR_alcgp["ncases"])
ncases_max_tobgp_OR_alcgp/all_cases

#CONTROLS
ncontrols_max_alcgp/all_controls

(ncases_max_alcgp/all_cases)/(ncontrols_max_alcgp/all_controls)

ncontrols_max_tobgp = sum(max_tobgp["ncontrols"])
ncontrols_max_tobgp/all_controls

ncontrols_max_tobgp_alcgp = sum(max_tobgp_alcgp["ncontrols"])
ncontrols_max_tobgp_alcgp/all_controls

ncontrols_max_tobgp_OR_alcgp = sum(max_tobgp_OR_alcgp["ncontrols"])
ncontrols_max_tobgp_OR_alcgp/all_controls

(ncases_max_tobgp_OR_alcgp/all_cases)/(ncontrols_max_tobgp_OR_alcgp/all_controls)