# How to insert tex code math equation into R help file with some hacks

## the Rd file doesn't support tex code 
https://stackoverflow.com/questions/14041601/documenting-equations-with-deqn-and-roxygen

## so here is the steps to hack:

#### Step 1:  generate the Rd file through roxygen2::roxygenise()
#### Step 2:  go to https://www.codecogs.com/eqnedit.php to type in the equation with tex code
#### Step 3:  copy the html (Edit) source code from the buttom of the page
#### Step 4:  replace tag a with tag center

## this hack depends on the codecogs.com's tex 2 html engine