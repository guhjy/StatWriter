# StatWriter
R package to help writing reproducible research

# Introduction

statwriter can be used when working with Rmarkdown to speed up writing papers and statistical reports that can be reproduced easely by others.
It gives a series of function that extract common statistics and output then in a nice way. An experimental features writes
also results in natural language adapting them to the results.

# Example
(see the help for more)

First command is `r ruminate()`, which print out the description of a variable or a table in a sensible way depending on the class of the variable.
## rmarkdown

```r
library(statwriter)

# a numeric variable 
x<-rnorm(100,0,1)
# factors
a<-factor(rep(1:2,15))
levels(a)<-c("Yellow","Green")


b<-factor(rep(1:2,35))
levels(b)<-c("male","female")


```

The data included the variable x ( \`r ruminate(x)\` ). Stimuli were \`r ruminate(a)\` , participants' were \`r ruminate(b,plural=T)\`

(lots more to add here)
 

## Compiled pdf
The data included the variable x (M=0.05, SD=1.05). Stimuli were 15 yellow, 15 green , participants' were 35  males, 35  females ...
