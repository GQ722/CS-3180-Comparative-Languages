# Solution to Small Project 01 in CS 3180 - Fall 2015
# By Erik M. Buck

# Read each line in linuxwords into a list
allWords = [line.strip() for line in open("linuxwords.txt", 'r')]

# Produce list of words all lower case
allWordsLowerCase = [x.lower() for x in allWords]

# Produce list of six letter words all lower case
allSixLetterWordsWithout_aeio = [word for word in allWordsLowerCase if
                     (6 == len(word)) and
                     ('a' not in word) and
                     ('e' not in word) and
                     ('i' not in word) and
                     ('o' not in word)]

# [3 points] You Racket program must output all six letter words that
# do not contain the letters, a, e, i, o. In other words, the only
# vowels are u or y. The case of the letter should not matter. U is
# interchangeable with u. The words must all be on the same line
# delimited by commas with no quotation marks.
print ("Part 1: List of six letter words in linuxwords that don't contain a e i o.\n")
print (', '.join(word for word in allSixLetterWordsWithout_aeio))

# [2 points] Your Racket program must output a count of total number
# of letter u found in the entire file regardless of word length. The
# case of the letter should not matter. U is interchangeable with u.
# 'reduce' is the same thing as Racket foldl.
print ("\n\nPart 2: number of letter u in linuxwords\n")
from functools import reduce
print (reduce(lambda x, y: x+y, map(lambda word: word.count('u'), allWordsLowerCase)))
