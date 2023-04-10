# NOTE: The entirity of this description of this script is the output of a query to OpenAI's ChatGPT
#
#
# Liar's Dice is a popular dice game that involves players making guesses about the number of dice that are showing a certain face value. Each player rolls a set of dice and hides the result from the other players. Then, players take turns making guesses about how many dice are showing a certain value, with the option to challenge the previous guess if they believe it to be incorrect. If a player is challenged and the guess was incorrect, they lose a die. The last player with dice remaining is the winner.

#To simulate this game in R, we can define a function that takes as inputs the number of players, the number of dice per player, and the maximum face value on the dice. We can then simulate the dice rolls and implement the logic for making guesses and challenging previous guesses.

# Function to simulate Liar's Dice game
liars_dice <- function(num_players, num_dice_per_player, max_face_value) {
  
  # Initialize player dice rolls
  player_dice <- matrix(sample(1:max_face_value, num_players*num_dice_per_player, replace = TRUE), nrow = num_players)
  
  # Initialize game state
  active_players <- 1:num_players
  num_dice_remaining <- rep(num_dice_per_player, num_players)
  prev_guess <- c(0, 0)
  
  # Helper function to check if a guess is valid
  is_valid_guess <- function(guess) {
    sum(player_dice[active_players, ] == guess[1]) >= guess[2]
  }
  
  # Main game loop
  while (length(active_players) > 1) {
    
    # Get current player's guess
    curr_player <- active_players[1]
    curr_guess <- as.numeric(readline(sprintf("Player %d's turn. Enter your guess (value count): ", curr_player)))
    
    # Check if guess is valid
    if (!is_valid_guess(curr_guess)) {
      cat("Invalid guess. Try again.\n")
      next
    }
    
    # Update game state
    prev_guess <- curr_guess
    active_players <- active_players[-1]
    
    # Check if any players want to challenge previous guess
    while (length(active_players) > 1) {
      challenge_player <- active_players[1]
      challenge <- as.numeric(readline(sprintf("Player %d, do you want to challenge the previous guess? (0/1): ", challenge_player)))
      if (challenge == 1) {
        if (!is_valid_guess(prev_guess)) {
          cat(sprintf("Player %d wins the challenge. Player %d loses a die.\n", challenge_player, curr_player))
          num_dice_remaining[curr_player] <- num_dice_remaining[curr_player] - 1
          if (num_dice_remaining[curr_player] == 0) {
            cat(sprintf("Player %d is out of the game.\n", curr_player))
            active_players <- active_players[-1]
          }
        } else {
          cat(sprintf("Player %d loses the challenge. Player %d loses a die.\n", challenge_player, challenge_player))
          num_dice_remaining[challenge_player] <- num_dice_remaining[challenge_player] - 1
          if (num_dice_remaining[challenge_player] == 0) {
            cat(sprintf("Player %d is out of the game.\n", challenge_player))
            active_players <- active_players[-1]
          }
        }
        break
      } else {
        active_players <- c(active_players[-1], challenge_player)
      }
    }
  }
  
  # Determine winner
  winner <- active_players[1]
  cat(sprintf("Player %d wins the game!\n", winner))
  
}
