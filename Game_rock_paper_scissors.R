# Game rock paper scissors

rps <- function() {
  # print game rules
  cat("Game Starts!\n")
  
  hands <- c("Rock", "Paper", "Scissors", "Quit")
  game_play <- TRUE
  result <- list(win=0, tie=0, loss=0)
  
  while (game_play) {
    # Player
    select <- as.numeric(readline("\nChoose your hand: [1] Rock [2] Paper [3] Scissors [4] Quit"))
    user_hand <- hands[select]
    
    # Bot
    comp_hand <- sample(hands[1:3], 1)
    
    if (is.na(select) || select == '') {
      game_play <- TRUE
      cat("\nPress Try Again!")
    } else if (select >= 5) {
      game_play <- TRUE 
      cat("\nPress Try Again!")
    } else if (user_hand == "Quit") {
      game_play <- FALSE # break from loop
      cat("\nYou chose:", user_hand)
    } else {
      # result
      cat("\nYou chose:", user_hand, "\nComputer chose:", comp_hand)
      
      if (user_hand == comp_hand) {
        result$tie <- result$tie + 1
        cat("\nIt's a tie!")
      } else if (
        (user_hand == "Rock" & comp_hand == "Scissors")  | 
        (user_hand == "Scissors" & comp_hand == "Paper") | 
        (user_hand == "Paper" & comp_hand == "Rock")
      ) {
        result$win <- result$win + 1
        cat("\nYou won!")
      } else {
        result$loss <- result$loss + 1
        cat("\nYou lost!")
      }
    }
  }
  cat("\n++++ Game results [win, tie, loss]:", result[[1]], result[[2]], result[[3]])
  cat("\n++++ Thank you for playing this game with us.")
}
