# brackets

## Current features:
- Read a text file containing team names and generate evenly spaced groups.
    - The group amount is always a power of two.
        - The group amount is minimized by using the max teams per group config parameter.
        - I.e. if the max team amount is four and there are eight teams, 
            two groups of four will be made. Similarly, if the max team 
            amount is three and there are eight teams, four groups of two will be made.  
    - The groups are random seeded in the order they occur in the file.
        - I.e. if there's four groups, the first four teams in the file are first seeds,
            the second four teams are second seeds, etc.
        - The first seeds are all in different groups, etc. 
- Generate a schedule for the group games.
    - Groups of three teams and less will play a double round robin.
    - Groups of four teams and more play a single round robin.
    - This works relatively well with groups of three, four and five, 
        but groups of two and six might not work so well because the
        difference in the amount of games between the groups grows too large.
    - The schedule is balanced based on the amount of teams in a group.
        - For groups of five and larger, the schedule is created so that two
            games can be played simultaneously. This should cut the difference
            in total duration of the group games when there's both groups of five and four.
        - For groups of four and less, the schedule is created so that no team plays three 
            times in a row.
        - The cutoff amount for the different schedule logic will be a configurable parameter
            in the future.

## Usage

- The first parameter for the program is the "max teams per group" parameter
- The second parameter is a path to the teams file (plain text file containing one team per line).

- Once the program has been run once, it will generate the necessary save files, so next time you run the program, it's enough to run it without any parameters. 
    - Running it without parameters will output the current information in the save files.

## Example output

Group output

```
Group 1
==================================================================
|    | Team                    | Games | Points | Average | Best |
==================================================================
|  1 | AiM                     |     3 |      6 |   -2.00 |    1 |
|  2 | Suppohauet              |     3 |      4 |    1.00 |    3 |
|  3 | DDR:n Naisvoimistelijat |     3 |      2 |   -5.00 |   -2 |
|  4 | Skepparklubben          |     3 |      0 |   -6.00 |   -4 |
==================================================================

         Skepparklubben    -8  -   -2  AiM                    
         Skepparklubben    -6  -    2  Suppohauet             
DDR:n Naisvoimistelijat   -10  -   -5  AiM                    
DDR:n Naisvoimistelijat    -2  -    3  Suppohauet             
                    AiM     1  -   -2  Suppohauet             
         Skepparklubben    -4  -   -3  DDR:n Naisvoimistelijat

```

Playoffs output

```
Semifinals
==========

             Suppohauet        -       MOT                    
                    AiM        -       Disko_Lisko            

Final
=====

                    TBA        -       TBA            
```