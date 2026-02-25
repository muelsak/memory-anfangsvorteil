library(tidyverse)

# Define possible combinations of strategies
strategies <- list(
  # all players use normal offensive strategy
  rep("offensive", 4),
  # only first player uses defensive strategy
  c("defensive", rep("offensive", 3)),
  # only first player uses offensive strategy
  c("offensive", rep("defensive", 3)),
  # all players use defensive strategy
  rep("defensive", 4)
)

# Define simulation scenarios
expand.grid(
  n.pairs = 20:32,
  n.players = 2:4,
  strategy = 1:4,
  mem.prop = 1
) ->
  df.sim

# Define sample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

# Initiate results data frame
df.res <- df.sim
df.res[c(paste0("player", 1:max(df.sim$n.players), "_solo.wins"), paste0("player", 1:max(df.sim$n.players), "_draws"))] <- NA

# Simulation ####
# Number of simulations per scenario
N.sim <- 10^4
# Whether to print moves; helpful for debugging
print.turns <- F
# Start time for clocking
st <- Sys.time()
# Set seed for reproducibility
set.seed(6321)

# Go through scenarios
for(row in 1:nrow(df.sim)){
  # Specify parameters
  n.pairs <- df.sim$n.pairs[row]
  n.players <- df.sim$n.players[row]
  mem.prop <- df.sim$mem.prop[row]
  strategy <- df.sim$strategy[row]
  
  # Initiate list of solo wins
  wins <- lapply(
    1:n.players,
    function(x) c()
  )
  # Initiate list of draws
  draws <- lapply(
    1:n.players,
    function(x) c()
  )
  # Initiate vector of pair values
  all.cards <- rep(1:n.pairs, each = 2)
  
  # Simulate N.sim games
  for(i in 1:N.sim){
    # Initiate list of gained pairs of the players
    gained.pairs <- lapply(
      1:n.players,
      function(x) c()
    )
    # Initiate list of memorized cards of the players
    memorized.cards <- lapply(
      1:n.players,
      function(x) c()
    )
    # Initiate vector of indices of remaining cards on the table
    remaining.cards.index <- 1:length(all.cards)
    
    # Initiate start player
    player <- 1
    # Initiate step number; helpful for debugging
    step <- 0
    
    # Start game and keep playing until no more cards on the table
    while(length(remaining.cards.index) > 0){
      # Player can make moves until turn is set to FALSE
      turn <- T
      # Reduce memorized card indices to the card indices on the table
      lapply(
        memorized.cards,
        function(x) x[x %in% remaining.cards.index]
      ) ->
        memorized.cards
      # Count number of card values in memorized cards
      freq.memorized <- table(all.cards[memorized.cards[[player]]])
      # Detected memorized pairs
      memorized.pairs <- as.numeric(names(freq.memorized)[freq.memorized == 2])
      # Collect memorized pairs
      if(length(memorized.pairs) > 0){
        # Add memorized pair values to gained.pairs of player
        gained.pairs[[player]] <- c(gained.pairs[[player]], memorized.pairs)
        # Update remaining cards on the table
        remaining.cards.index <- remaining.cards.index[!(all.cards[remaining.cards.index] %in% memorized.pairs)]
        # Print move
        if(print.turns) cat("\nPlayer ", player, " collects pair ", memorized.pairs)
      }
      # If no cards anymore on the table, finish turn
      if(length(remaining.cards.index) == 0) turn <- F
      
      # Start actual turn (after collecting memorized pairs)
      while(turn){
        if(print.turns) cat("\nStep ", step)
        step <- step+1
        # Reduce memorized cards to cards on the table
        lapply(
          memorized.cards,
          function(x) x[x %in% remaining.cards.index]
        ) ->
          memorized.cards
        
        # Pull new random card out of non-memorized cards
        if(print.turns) cat("\nPlayer ", player, " pulls new card")
        card.1 <- resample(x = remaining.cards.index[!(remaining.cards.index %in% memorized.cards[[player]])], size = 1)
        
        # If pulled card value is in memorized cards, collect pair
        if(all.cards[card.1] %in% all.cards[memorized.cards[[player]]]){
          if(print.turns) cat("\nPlayer ", player, " collects pair ", all.cards[card.1])
          gained.pairs[[player]] <- c(gained.pairs[[player]], all.cards[card.1])
          remaining.cards.index <- remaining.cards.index[!(all.cards[remaining.cards.index] %in% all.cards[card.1])]
          if(length(remaining.cards.index) == 0) turn <- F
        } else {
          # If pulled card value is unknown, the second card depends on the strategy.
          # If the strategy is offensive or no more memorized cards remain, pull new random non-memorized card
          if(strategies[[strategy]][player] == "offensive" | length(memorized.cards[[player]]) == 0){
            if(print.turns) cat("\nPlayer ", player, " pulls new second card")
            # Pull second non-memorized card
            card.2 <- resample(x = remaining.cards.index[!(remaining.cards.index %in% c(memorized.cards[[player]], card.1))], size = 1)
            
            # If second card value luckily matches first card value, collect pair
            if(all.cards[card.1] == all.cards[card.2]){
              if(print.turns) cat("\nPlayer ", player, " collects luckily pair ", all.cards[card.1])
              gained.pairs[[player]] <- c(gained.pairs[[player]], all.cards[card.1])
              remaining.cards.index <- remaining.cards.index[!(all.cards[remaining.cards.index] %in% all.cards[card.1])]
              if(length(remaining.cards.index) == 0) turn <- F
            } else {
              # If first and second cards do not match, end turn and update memorized cards
              if(print.turns) cat("\nPlayer ", player, " does not collect")
              lapply(
                memorized.cards,
                function(x) c(x, card.1, card.2)
              ) ->
                memorized.cards
              turn <- F
            }
          }
          # If the strategy is defensive, and there are memorized cards left, drop the second card and end turn
          if(turn & strategies[[strategy]][player] == "defensive" & length(memorized.cards[[player]]) > 0){
            if(print.turns) cat("\nPlayer ", player, " drops second card")
            lapply(
              memorized.cards,
              function(x) c(x, card.1)
            ) ->
              memorized.cards
            turn <- F
          }
        }
      }
      # Next player
      player <- (player %% n.players) + 1
    }
    # Calculate the number of gained pairs per player
    stacks <- sapply(
      gained.pairs,
      length 
    )
    # Append solo win to vector wins
    wins <- lapply(
      1:n.players,
      function(j) c(wins[[j]], all(stacks[j] > stacks[-j]))
    )
    # Append draws to vector draws
    draws <- lapply(
      1:n.players,
      function(j) c(draws[[j]], stacks[j] == max(stacks) & any(stacks[j] == stacks[-j]))
    )
  }
  # Save proportion of wins and draws in result data frame
  df.res[row, c(paste0("player", 1:n.players, "_solo.wins"), paste0("player", 1:n.players, "_draws"))] <- c(
    sapply(wins, mean),
    sapply(draws, mean)
  )
}
