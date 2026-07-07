# Slot Machine simulator
# Based on "Hands-On Programming with R" (Garrett Grolemund)
# Project 3: https://rstudio-education.github.io/hopr/project-3-slot-machine.html

# The sequence of the game is:
#   1. Generate and print symbols
#   2. Score symbols
#      - If three of a kind          -> lookup prize
#      - If any three bars           -> assign $5
#      - If one or more cherries     -> assign $2 or $5
#      - Count diamonds
#      - If one or more diamonds     -> double prize once per diamond
#      - Diamonds are also "wild" and can substitute for any symbol


# 1. Generate symbols for a single play

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

# 2. Score a set of three symbols

score <- function(symbols) {
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only non-diamonds matter
  # for "three of a kind" and "all bars"
  
  slots <- symbols[symbols != "DD"]
  same  <- length(unique(slots)) == 1
  bars  <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  
  if (diamonds == 3) {
    prize <- 100
  } else if (length(slots) == 0) {
    # all three symbols were diamonds is handled above;
    # this branch guards against an empty `slots` vector
    prize <- 0
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries, so long as there is at least one real cherry
    prize <- c(0, 2, 5)[min(cherries + diamonds, 2) + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}

# 3. Play once: generate symbols, score them, attach as attribute

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

# 4. Display

slot_display <- function(prize) {
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a character string
  # \n is special escape sequence for a new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  
  # display character string in console without quotes
  cat(string, "\n")
}

print.slots <- function(x, ...) {
  slot_display(x)
}

# 5. Enumerate every possible combination and its prize

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prize <- NA_real_
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

# expected value of the machine (for reference / sanity check)
combos$prob <- prob[combos$Var1] * prob[combos$Var2] * prob[combos$Var3]
expected_value <- sum(combos$prob * combos$prize)

# 6. Simulate playing until broke

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + unclass(play())
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}

# 7. Vectorized versions for simulating many plays at once

get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
                prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[, 1], w2 = symb_mat[, 2],
             w3 = symb_mat[, 3], prize = score_many(symb_mat),
             stringsAsFactors = FALSE)
}

# symbols should be a matrix with a column for each slot machine window
score_many <- function(symbols) {
  
  # 1. Assign base prize based on cherries and diamonds
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  # Wild diamonds count as cherries
  prize <- c(0, 2, 5)[pmin(cherries + diamonds, 2) + 1]
  
  # But not if there are zero real cherries
  prize[cherries == 0] <- 0
  
  # 2. Change prize for combinations that contain three of a kind
  same <- symbols[, 1] == symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40,
               "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  # 3. Change prize for combinations that contain all bars
  bars <- symbols == "B" | symbols == "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  # 4. Handle wilds
  
  ## Combos with two diamonds
  two_wilds <- diamonds == 2
  
  ### Identify the nonwild symbol
  one <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] &
    symbols[, 2] != symbols[, 3]
  
  ### Treat as three of a kind
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  ## combos with one wild
  one_wild <- diamonds == 1
  
  ### Treat as all bars (if appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  ### Treat as three of a kind (if appropriate)
  one   <- one_wild & symbols[, 1] == symbols[, 2]
  two   <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one]   <- payoffs[symbols[one, 1]]
  prize[two]   <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # 5. Double prize for every diamond in combo
  unname(prize * 2^diamonds)
}
