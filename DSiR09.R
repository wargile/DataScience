
deck = rep(c(1:10, 10, 10, 10), 4)

shuffle_decks = function(n) sample(rep(deck, n))

handValue = function(cards) {
  value = sum(cards)
  
       # Check for an Ace and change value if it doesn't bust
  if (any(cards == 1) && value <= 11) 
    value = value + 10
  
       # Check bust (set to 0); check black jack (set to 21.5)
  if(value > 21)  
    0 
  else if (value == 21 && length(cards) == 2)  
    21.5 # Blackjack
  else 
    value
}

winnings = function(dealer, players) {
  if (dealer > 21) {
       # Dealer has blackjack, ties players with blackjack
    -1 * (players <= 21)
  } else if (dealer == 0) {
       # Dealer busts - all non-busted players win
    1.5 * (players > 21) +
      1 * (players <= 21 & players > 0) +
     -1 * (players == 0) 
  } else {
       # Dealer 21 or below, all player values > dealer win
    1.5 * (players > 21) +  
      1 * (players <= 21 & players > dealer) +
     -1 * (players <= 21 & players < dealer) 
  }
}
winnings = function(dealer, players){
  (players > dealer & players > 21) * 1.5 + # blackjack
  (players > dealer & players <= 21) * 1 +  # win
  (players < dealer | players == 0) * -1    # lose
}

test_cards = list( c(10, 1), c(10, 5, 6), c(10, 1, 1), 
                   c(7, 6, 1, 5), c(3, 6, 1, 1), 
                   c(2, 3, 4, 10), c(5, 1, 9, 1, 1),
                   c(5, 10, 7), c(10, 9, 1, 1, 1)) 

test_cards_val = c(21.5, 21, 12, 19, 21, 19, 17, 0, 0)

identical(test_cards_val, sapply(test_cards, handValue))


test_vals = c(0, 16, 19, 20, 21, 21.5)

testWinnings =
  matrix(c( -1,  1,  1,  1,  1, 1.5,
            -1,  0,  1,  1,  1, 1.5,
            -1, -1,  0,  1,  1, 1.5,
            -1, -1, -1,  0,  1, 1.5,
            -1, -1, -1, -1,  0, 1.5,
            -1, -1, -1, -1, -1, 0), 
         nrow = length(test_vals), byrow = TRUE)
dimnames(testWinnings) = list(dealer = test_vals, 
                              player = test_vals)

testWinnings

check = testWinnings
check[] = NA
 
for(i in seq_along(test_vals)) {
  for(j in seq_along(test_vals)) {
    check[i, j] = winnings(test_vals[i], test_vals[j])
  }
}

identical(check, testWinnings)

shoe = function(m = 1) sample(deck, m, replace = TRUE)

new_hand = function(shoe, cards = shoe(2), bet = 1) {
  list(bet = bet, shoe = shoe, cards = cards)
}

new_hand = function(shoe, cards = shoe(2), bet = 1, ...) {
  structure(list(bet = bet, shoe = shoe, cards = cards), 
            class = "hand")
}

print.hand = function(x, ...) {
  cat("Blackjack hand: ", paste(x$cards, collapse = "-"), 
      " (", handValue(x$cards), ").  Bet: ", x$bet, 
      "\n", sep = "")
}

myCards = new_hand(shoe, bet = 7)

myCards

hit = function(hand) {
  hand$cards = c(hand$cards, hand$shoe(1))
  hand
}

hit(myCards)

stand = function(hand) hand

dd =  function(hand) {
  hand$bet = hand$bet * 2
  hand = hit(hand)
  stand(hand)
}

dd(myCards)

splitPair = function(hand) {
  list(
    new_hand(hand$shoe, 
             cards = c(hand$cards[1], hand$shoe(1)),
             bet = hand$bet),
    new_hand(hand$shoe, 
             cards = c(hand$cards[2], hand$shoe(1)),
             bet = hand$bet)
    )
}

splitHand = splitPair(myCards)
splitHand

set.seed(1014)

dealer = new_hand(shoe)
player = new_hand(shoe)

dealer$cards[1]

player

player = hit(player)
player
 
dealer

dealer = hit(dealer)
dealer

winnings(handValue(dealer$cards), handValue(player$cards))

strategy_simple = function(mine, dealerFaceUp) {
  if (handValue(dealerFaceUp) > 6 && handValue(mine) < 17) 
     "H" 
  else 
     "S"
}

strategy_simple = function(mine, dealerFaceUp) {
  if (handValue(mine) == 0) return("S")
  if (handValue(dealerFaceUp) > 6 && handValue(mine) < 17) 
     "H" 
  else 
     "S"
}

dealer_cards = function(shoe) {
  cards = shoe(2)
  while(handValue(cards) < 17 && handValue(cards) > 0) {
    cards = c(cards, shoe(1))
  }
  cards
}

play_hand = function(shoe, strategy, 
                      hand = new_hand(shoe), 
                      dealer = dealer_cards(shoe)) {
  
  face_up_card = dealer[1]
  
  action = strategy(hand$cards, face_up_card)
  while(action != "S" && handValue(hand$cards) != 0) {
    if (action == "H") {
      hand = hit(hand)
      action = strategy(hand$cards, face_card)
    } else if (action == "D") {
      hand = dd(hand)
      action = "S"
    } else {
      stop("Unknown action: should be one of S, H, D, SP")
    }
  }  

  winnings(handValue(dealer), handValue(hand$cards)) * hand$bet
}

set.seed(1014)
play_hand(shoe, strategy_simple)

play_hand = function(shoe, strategy, 
                     hand = new_hand(shoe), 
                     dealer = dealer_cards(shoe), 
                     verbose = FALSE) {
  
  if (verbose) {
    cat("New hand \n")
    cat("  Dealer: ", paste(dealer, collapse = "-"), 
        " (", handValue(dealer), ")\n", sep = "")
    cat("  Player: ", paste(hand$cards, collapse = "-"),
        ": ", sep = "")
    
  }
  face_card = dealer[1]
  
  action = strategy(hand$cards, face_card)
  while(action != "S" && handValue(hand$cards) != 0) {
    if (verbose) cat(action)
    if (action == "H") {
      hand = hit(hand)
      action = strategy(hand$cards, face_card)
    } else if (action == "D") {
      hand = dd(hand)
      action = "S"
    } else {
      stop("Unknown action: should be one of S, H, D, SP")
    }
  }  
  if (verbose) {
    cat(action, " -> ", paste(hand$cards, collapse = "-"), 
        " (", handValue(hand$cards), ")", sep = "", "\n")
  }
  
  winnings(handValue(dealer), handValue(hand$cards)) * hand$bet 
}

set.seed(1014)
play_hand(shoe, strategy_simple, verbose = TRUE)

play_hand = function(shoe, strategy, 
                     hand = new_hand(shoe), 
                     dealer = dealer_cards(shoe), 
                     verbose = FALSE) {
  
  if (verbose) {
    cat("New hand \n")
    cat("  Dealer: ", paste(dealer, collapse = "-"), 
        " (", handValue(dealer), ")\n", sep = "")
    cat("  Player: ", paste(hand$cards, collapse = "-"), ": ", sep = "")
    
  }
  face_card = dealer[1]
  
  action = strategy(hand$cards, face_card)
  while(action != "S" && handValue(hand$cards) != 0) {
    if (verbose) cat(action)
    if (action == "H") {
      hand = hit(hand)
      action = strategy(hand$cards, face_card)
    } else if (action == "D") {
      hand = dd(hand)
      action = "S"
    } else if (action == "SP") {
      hands = splitPair(hand)
      if (verbose) cat("\nSPLIT\n")
      
      one = play_hand(shoe, strategy, hands[[1]], dealer,
                      verbose = verbose)
      two = play_hand(shoe, strategy, hands[[2]], dealer, 
                      verbose = verbose)
      
      return(one + two)
    } else {
      stop("Unknown action: should be one of S, H, D, SP")
    }
  }  
  if (verbose) {
    cat(action, " -> ", paste(hand$cards, collapse = "-"), 
        " (", handValue(hand$cards), ")", sep = "", "\n")
  }
  
  winnings(handValue(dealer), handValue(hand$cards)) * hand$bet
}

set.seed(1014)
play_hand(shoe, strategy_simple, verbose = TRUE)

lookuptable = read.csv("Data/strategy.csv", header = TRUE, 
                   stringsAsFactors = FALSE, check.names = FALSE)
head(lookuptable)

strategy_optimal = function(player_hand, dealerFaceUp,
                            optimal = lookuptable) {
       # Stand if 21 or already busted
  player_value = handValue(player_hand)
  if (player_value == 0) return("S")
  if (player_value >= 21) return("S")

  dealer_value = handValue(dealerFaceUp)
  loc_ace = player_hand == 1

  if (length(player_hand) == 2 && 
      player_hand[1] == player_hand[2]) {
    type = "pair"
    if (player_hand[1] == 1) player_value = 2
  } else if (sum(loc_ace) > 0 && 
               (player_value - sum(loc_ace)) > 
                            handValue(player_hand[!loc_ace])) {
    type = "soft"
  } else {
    type = "hard"
  }
  
  out = optimal[optimal$type == type & 
                optimal$value == player_value,
                as.character(dealer_value)]    
  if (length(out) == 0) browser()
  if (out == "Dh") 
    if (length(player_hand) > 2) out = "H" else out = "D"
  if (out == "Ds" ) 
    if (length(player_hand) > 2) out = "S"  else out = "D"
  out
}

stopifnot(strategy_optimal(c(4, 1, 6), 3) == "S", 
          strategy_optimal(c(3, 2, 6), 3) == "H",
          strategy_optimal(c(5, 6), 3) == "D",
          strategy_optimal(c(3, 2, 6), 1) == "H",
          strategy_optimal(c(5, 6), 1) == "H",
          strategy_optimal(c(6, 1, 5, 1), 7) == "H",
          strategy_optimal(c(6, 1, 5, 1), 6) == "H",
          strategy_optimal(c(1, 1), 7) == "SP",
          strategy_optimal(c(9, 9), 7) == "S",
          strategy_optimal(c(6, 6), 7) == "H",
          strategy_optimal(c(3, 3), 7) == "SP",
          strategy_optimal(c(7, 1), 5) == "D",
          strategy_optimal(c(7, 1), 7) == "S",
          strategy_optimal(c(2, 1), 9) == "H",
          strategy_optimal(c(2, 1), 5) == "D")

set.seed(101451)
replicate(10, play_hand(shoe = shoe, 
                        strategy = strategy_optimal))

replicate(3, play_hand(shoe = shoe, strategy = strategy_optimal, 
                       verbose = TRUE))

set.seed(10114)
win_optimal = replicate(1000, play_hand(shoe = shoe,
                                    strategy = strategy_optimal))
set.seed(10114)
win_simple = replicate(1000, play_hand(shoe = shoe,
                                   strategy = strategy_simple))

pdf("DensityWinningsStrategies.pdf", width = 8, height = 6)
title = "Comparison of optimal and simple strategies\n for 1,000 bets of $1"
oldPar = par(mar = c(4.1, 4.1, 0.5, 0.5))

plot(density(win_optimal, bw = 0.25), col = "green", lwd = 2,
     xlab = "Winnings", xlim = c(-3, 3), 
     ylim = c(0, 0.9), main = "")
lines(density(win_simple, bw = 0.25), col = "purple", lwd = 2)
legend("topright", col = c("green", "purple"), 
       legend = c("Optimal", "Simple"), bty = "n", lty = 1)

par(oldPar)
dev.off()

mean(win_optimal)

mean(win_simple)

payoff = function(n, strategy, shoe) {
  results = replicate(n, play_hand(shoe = shoe, 
                                    strategy = strategy))
  c(avgGain = mean(results), sdGain = sd(results), 
    medGain = median(results))
}

pdf("WinningPolygons.pdf",  width = 8, height = 6)

win_simple50 = replicate(1000,  
                         payoff(50, strategy_simple, shoe))
win_optimal50 = replicate(1000, 
                          payoff(50, strategy_optimal, shoe))

df = data.frame(
  value = c(win_simple50[ "avgGain", ], 
            win_optimal50[ "avgGain", ]), 
  strategy = rep(c("simple", "optimal"), each = 1000))

library(ggplot2)
qplot(value, data = df, geom = "freqpoly", colour = strategy, 
      binwidth = 0.05)

dev.off()

y = list()
y$a = 1
f = function(x) {
  x$a = 2
}
y$a

f(y)
y$a

f = function(x) {
  x$a = 2
}
y$a


A = setRefClass("A", fields = c(a = "numeric"))
y = A$new()
y$a = 1
f(y)
y$a

Counter = setRefClass("Counter", 
  fields = list("count" = "numeric"), 
  methods = list(
    initialize = function() {
      count <<- 1
    },
    increment = function() {
      count <<- count + 1
    }
))


ctr1 = Counter$new()
ctr2 = Counter$new()

ctr1$count

ctr2$count

ctr1$increment()
ctr1$count

ctr2$count

ctr2$increment()
ctr2$increment()
ctr2$increment()
ctr2$count

Shoe = setRefClass("Shoe", 
  fields = list(
    decks = "numeric",  # number of decks
    cards = "numeric",  # vector of cards
    pos = "numeric",    # current position in shoe
    debug = "logical"   # display informative messages?
  ), 
  methods = list(
    shuffle = function() {
      if (debug) message("Shuffling the shoe")
      cards <<- shuffle_decks(decks)
      pos <<- 0
    },
  
    draw_n = function(n) {
      if (decks_left() <= 1) shuffle()
    
      drawn = cards[pos + seq_len(n)]
      pos <<- pos + n
      drawn
    },

    decks_left = function() floor(decks - pos / 52),

    played = function() cards[seq_len(pos)]
  )
)

new_shoe = function(decks = 6, debug = FALSE) {
  shoe = Shoe$new(decks = decks, debug = debug)
  shoe$shuffle()
  shoe
}

my_shoe = new_shoe(decks = 3, debug = TRUE)

my_shoe$draw_n(12)

my_shoe$pos

my_shoe$decks_left()

my_shoe$draw_n(37)
 
my_shoe$pos

my_shoe$draw_n(54)

my_shoe = new_shoe(decks = 6, debug = TRUE)
replicate(3, play_hand(shoe = my_shoe$draw_n, 
                       strategy = strategy_optimal,
                       verbose = TRUE))

my_shoe$pos = 0
payoff(50, strategy_optimal, my_shoe$draw_n)

my_shoe$pos

hi_low = function(cards) {
  decks = ceiling(length(cards) / 52)
  sum(c(-1, 1, 1, 1, 1, 1, 0, 0, 0, -1)[cards]) / decks
}

count_table = read.csv("Data/counts.csv", 
                       stringsAsFactors = FALSE, 
                       check.names = FALSE)

counts = base::split(count_table[, -1], count_table$strategy)
counts = lapply(counts, as.integer)

any_count = function(strategy, cards) {
  decks = ceiling(length(cards) / 52)
  sum(counts[[strategy]][cards]) / decks
}

bet = function(count) pmax(floor(count), 1)

plot(bet, from = -5, to = 10)

pdf("StepBetPlot.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 0.5, 0.5))
plot(bet, from = -5, to = 10)
par(oldPar)
dev.off()

count_payoff = function(shoe, n = 100) {
  gain = numeric(n)
  count = numeric(n)
  
  gain[1] = play_hand(shoe$draw_n, strategy_optimal)
  count[1] = 0
  
  for (i in 2:n) {
    count[i] = hi_low(shoe$played())
    gain[i] = play_hand(shoe$draw_n, strategy_optimal) 
  }
  c(sum(gain), sum(gain * bet(count)))
}

set.seed(155100)
my_shoe = new_shoe()
payoffs = replicate(1000, count_payoff(my_shoe, 50))

apply(payoffs, 1, mean)

apply(payoffs, 1, sd)

diffs = payoffs[2,] - payoffs[1,]
mean(diffs)

sd(diffs)

df = data.frame(
  value = c(payoffs[1, ], payoffs[2, ]), 
  bet = rep(c("Fixed", "Hi-Low"), each = 50))

df2 = data.frame(
  value = c(payoffs[2, ] - payoffs[1, ]))

library(ggplot2)

pdf("BJ_CompareCounting.pdf", width = 8, height = 4)

qplot(value, data = df2, geom = "freqpoly", binwidth = 2.5,
      xlab = "Difference in Gain for 50 Bets (Hi-Low - Fixed)")

#qplot(value, data = df, geom = "freqpoly", colour = bet, binwidth = 0.05,
#      xlab = "Average Gain (50 Bets)")
dev.off()
#ggsave("compare-count.pdf", width = 8, height = 4)
