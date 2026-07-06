# slot_machine_in_R

A simple slot-machine simulator written in R, based on Project 3 of
[Hands-On Programming with R](https://rstudio-education.github.io/hopr/project-3-slot-machine.html) by Garrett Grolemund.

# What it does:
The machine has three windows, each of which lands on one of 7 symbols:

    DD  7  BBB  BB  B  C  0

Symbols are drawn with these probabilities:

    DD   3%
    7    3%
    BBB  6%
    BB   10%
    B    25%
    C    1%
    0    52%

# Scoring rules:
  - Three matching diamonds (DD DD DD)        -> $100 (special case)
  - Three of a kind (non-diamond)             -> looked up in a payout table
  - Any combination of bars (B/BB/BBB), all three positions -> $5
  - One or more cherries (C)                  -> $2 (one cherry) or $5 (two+)
  - Diamonds (DD) are wild: they substitute for any other symbol
    when checking "three of a kind", "all bars", or "cherries", AND
  - Every diamond in the combination doubles the final prize.

# How to run:
Copy code to R or RStudio:

       play()              # play once, prints symbols + prize
       play_many(10)       # simulate 10 plays at once, returns a data.frame
       plays_till_broke(100)  # how many plays until you run out of $100
