# Daemon Poker

[![Build Status](https://secure.travis-ci.org/daemonpoker/dp.cli.png?branch=master)](http://travis-ci.org/daemonpoker/dp.cli)

Play Daemon Poker on the command line!

## Rules of the game

First of all, the cards are different from the usual cards. They have only one
suit, the Sun, denoted by a circled dot, and they have 16 ranks, which are,
from the lowest to the highest:

* 1 to 10, where 1 is distinct from the Ace,
* The Bishop (B), a new rank inserted between the 10 and the Jack,
* The Jack (J),
* The Queen (Q),
* The King (K),
* The Ace (A), which is distinct from the 1,
* The Daemon (D), a new rank, which is the highest.

The deck is composed of an infinite number of cards. This allows for an
infinite number of players.  The Daemon Poker hands are also different. They
are composed of five cards, but as there are an infinite number of cards in
the deck, it is possible to have 5 same cards. Also, as the cards have only
one suit, flush and straight flush are not possible. Also, straight is not a
legal hand for Daemon Poker, and is treated as High-card.  The total number of
hands is 1,048,576.

## Betting Rules

The betting rules are also very different to be able to handle a huge number
of players. In traditional poker, each player acts in turn. When there are
hundreds of player, this not possible any longer, so each player must act
simultaneously.
In Daemon Poker, there are two different rounds of betting, separated by a
draw. First, each player are dealt 5 cards from the infinite deck after having
paid an ante. A first betting round begins. Each player can bet the amount of
their choice, or fold. Then, after a specific time (between 15-30 seconds),
the median of all the bets is given to each player. If the bet of the player
is more than this median, then he continues, else he must call or fold.
Then, the draw phase begins. Each player can discard the cards of their
choice, and they are dealt with new ones.
A second betting round occurs. In this round, each player can fold or choose
an amount to bet. This amount is either 1, 2, 4, 8, 16, 32, 64, 128, 512 or
1,024. Each player is notified of the initial pot (collected from the first
round) that is allocated for each bet amount. Then, the player with the best
hand for a specified amount wins the pot allocated for this amount.
