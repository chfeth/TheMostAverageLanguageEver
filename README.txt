Team Members:
Christopher Feth ONID fethc@oregonstate.edu
Trevor Jones ONID jonetrev@oregonstate.edu
Katelynn Thorn ONID thornka@oregonstate.edu
Natalie Coppa ONID coppan@oregonstate.edu

The name of this language is TheMostAverageLanguageEver, implemented with a stack-based paradigm. Features include stack manipulations, while loops, naming conventions (for local variable declarations and functions) and basic math operations. 

Instructions:
Run in GHCi, load Project.hs. 

To run goodExample 1, execute "prog goodExample1 empty []" in GHCi. Note that the result should be Just [S "Non-Leap Year"], because the coded year is 1900. To test a different year that may be a leap year, change "1900" in the code to the desired year.

To run goodExample2, execute "prog goodExample2 empty []" in GHCi. It will check an isbn number for validity using the check digit and its algorithm.
You can change the isbn it is checking in the code, and some samples are there in comments. The top of the stack will be either true or false based on the validity.
You can also comment out the line pushing the isbn to the stack in code, and run it like "prog goodExample2 empty [I 9783161484100]" to pass it an isbn to run
More info on isbn can be found here: https://isbn-information.com/check-digit-for-the-13-digit-isbn.html

To run badExample 1 & badExample2, execute "prog badExample1 empty []" and "prog badExample2 empty []" respectively in GHCi. The expected output for both is "Nothing" where badExample1 causes a type error and badExample2 causes an underflow error. 