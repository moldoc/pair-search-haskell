# pair-search-haskell
Coursework for Advanced Functional Programming 2018 at the University of Tampere.

The program returns frequent ordered pairs in text file lines with a given gap. 
The program requires a file called "input.txt" in the same directory as coursework.erl.

The input.txt has to be in format:

```
4
5
data1.txt
data2.txt
data3.txt
```

Where the first line has the gap constraint (there are at most that many characters between the character pair), 
second line the number of most frequent pairs you want to find,
and the line after that the names of the files you want to find the frequent ordered pairs from.

## Running the program from the command line

```
ghci
:load coursework.hs
main
```
