#MSC2011H- ASSIGNMENT 3
#IMRAN RHEMTULLA
#JUNE 24, 2022

###### SETUP FOR GAME #####

# Set the working directory to manage files
setwd("C:/Users/Ronny/iCloudDrive/Mbiotech/MSCH2011H/MSC2011H-assignments")

# Read word list into the program
word_dictionary <- read.table("hangman_words.txt", header = FALSE, sep = "")

# Choose a word from the list randomly
word_from_dict <- sample(word_dictionary$V1, 1, replace = TRUE)
word <- as.list(strsplit(word_from_dict,"")[[1]])

# Find the length of the word and set the number of tries for removal if an incorrect answer is provided as a flag vlaue

tries <- 6 # Traditional hangman allows for 8 incorrect guesses. I want to make it a bit harder

# Setup the visual
visual <- character()
for (i in 1:sum(nchar(word))) {
  visual <- c(visual, "_")
}

# Lists for tracking guesses
wrong <- c("")
right <- c("")
cat(wrong)

# Counters for flag values and print messages at the end
correct_guesses <- 0
total_tries = 0

# Letters to be included in correct input

correct_inputs = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
                   "m", "n", "p","q","q", "r", "s", "t", "u", "v", "w", "x", 
                   "w", "z")

##### GAME #####

# Print intro message

print(paste("Hello! Welcome to Hangman. Currently, the word is", sum(nchar(word)), "letters long."), quote = FALSE)
print("", quote = FALSE)
print("RULES:", quote =  FALSE)
print(paste("You have", tries, "incorrect guesses."), quote = FALSE)
print("You can guess one letter at a time or attempt to guess the whole word at the same time.", quote = FALSE)
print("", quote = FALSE)

# Prompt the user to input a letter or the whole world, repeat until user guesses correctly or runs out of tries

while (correct_guesses != length(word) && tries > 0) {
  # Updates to game and input call
  print("Correct guesses:")
  cat(visual, sep = " ")
  print("", quote = FALSE)
  
  print("Incorrect guesses:", quote = FALSE)
  cat(wrong, sep = " ")
  print("", quote = FALSE)
  
  print(paste("You have", tries, "incorrect guesses remaining."), quote = FALSE)
  print("", quote = FALSE)
  
  guess <- readline("Please guess a letter or the entire word (if you are feeling lucky): ")
  guess <- tolower(guess) # Converts guess to lowercase
  
  # Check to see if the user guess was the correct input (a single letter or an input that is the length of the word)
  if (!(guess %in% correct_inputs) && !(sum(nchar(guess) == 1 || sum(nchar(guess) == length(word))))) {
    print("Sorry, that was not a valid guess. Please try again.", quote = FALSE)
    
    # Check to see if the user guess has already been used
  } else if (guess %in% right || guess %in% wrong){
    print("You've already attempted to guess this letter. Please try again.", quote = FALSE)
    
    # End the game if the user inputs the whole word correctly, increment total tries
  } else if (guess == word_from_dict){
    correct_guesses <- length(word)
    total_tries <- total_tries + 1
    
    # Add a point to the correct guess counter if the user guesses correctly, increment total tries
  } else if (guess %in% word && sum(nchar(guess) == 1)) {
    print("That was a correct guess! Nice job!", quote = FALSE)
    correct_guesses <- correct_guesses + length(which(word == guess))
    positions <- gregexpr(guess, word_from_dict) # find location of where the guess is in the word
    
    total_tries <- total_tries + 1
    
    #Update the visual
    for (i in 1:length(positions[[1]])){
      visual[positions[[1]][i]] <- guess
    } 
    
    #Update the list of right answers to prevent repeats of inputs
    right <- append(right, guess) 
    
    
    # Remove a point from total tries if the user incorrectly guesses, increment total tries
  } else {
    print("That guess was incorrect.", quote = FALSE)
    tries <- tries - 1
    total_tries <- total_tries + 1
    
    #Update the list of incorrect entries for both words and letters
    wrong <- append(wrong, guess) 
    
  }
}

# PRint end of game messages depending on if the person won or lost
if (correct_guesses == length(word)) {
  print(paste0("Congratulations! You won :). It only took you ", total_tries, " tries to guess that the word was ", word_from_dict, "."), quote = FALSE)
} else {
  print(paste0("Sorry! You didn't correctly guess the word :(. The correct word was ", word_from_dict, "."), quote = FALSE)
}

