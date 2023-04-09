n_stay <- 0    #counts the number of times there is staying
n_switch <- 0  #counts how many times switched
n_random <- 0  # counts how many times the random choice won 


for ( i in 1:100) {
  
  #sets up the vectors etc.
  door <- c(1,2,3) 
  # sets up a vector for the random choice 
  random <- c(1, 2)
  #randomly select a door for the car
  cardoor <- sample(door,1) 
  #randomly make a choice for the contestant
  choice <- sample(door,1) 
  #vector corresponding to goat doors
  goatdoors <- setdiff(door, cardoor) 
  #identify the option for revealing
  reveal_options <- setdiff(goatdoors, choice)
  # if the choice is the same as the door the car is behind
  if (choice == cardoor) { 
    #pick one of the doors to reveal if there are two goats to choose from
    reveal <- sample(reveal_options,1)}  else {
    #else reveal the single element
    reveal <- reveal_options 
  }
  #identifies the two remaining doors
  remaining_doors <-setdiff(door, reveal)
  #new variable recording final door choice if contestant switches
  newchoice <- setdiff(remaining_doors, choice) 
  #make the decision
  decision <- sample(random, 1)
  #if the choice is equal to car door
  if (choice == cardoor) {
    # add one to the stay variable
    n_stay <- n_stay + 1
    # print answer
    print('Stay: You got a car')
    # checks if the random choice is staying and adds to the wins form random
    if (decision == 1) {
      print('random choice won!')
      n_random <- n_random + 1
    }
  } else {
    print('Stay: You got a goat')
  }
  #if the newChoice is equal to car door
  if (newchoice == cardoor) {
    #add one to the switch variable
    n_switch <- n_switch + 1
    print('Switch: You got a car')
    # checks if the random choice is switching and adds to the wins form random
    if (decision == 2) {
      print('random choice won!')
      n_random <- n_random + 1
    }
  } else {
    print('Switch: You got a goat')
  }
}
#print the probability of staying
print(n_stay/100)
#print the probability of switching
print(n_switch/100)
#print probability of random choice winning
print(n_random/100)

