# MSC2011H - ASSIGNMENTS
# IMRAN RHEMTULLA
# JUNE 15, 2022

# Prompt the user to input a number
user_number <- readline(prompt = "Please enter a positive 3 digit number: ")

# Convert user input from character to numeric
user_number <- as.numeric(user_number)

# Check to see if user input is valid (integer, 3 digits, non-negative)

  #If not valid, exit the program
  if (is.na(user_number) || length((user_number) != 3 || user_number < 0)) {
    print("Your input was invalid! Exiting program.")
  } else {
  
  # Else store the digits into variables, storage 
    digit_1 <- user_number[1]
    digit_2 <- user_number[2]
    digit_3 <- user_number[3]
    
  # Calculate the sum of cubes of the digits
    cube_sum <- (digit_1^3 + digit_2^3 + digit_3^3)
    
  # Check to see if the number is narcisstic and the print appropriate message
    if (cube_sum == user_number) {
      print(paste(user_number, "is a narcissistic number."))
    } else {
      print(paste(user_number, "is not a narcissistic number."))
      
    } # if-else for narcissistic number check
  } # if-else for valid entry

  
