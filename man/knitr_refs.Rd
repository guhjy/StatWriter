\name{knitr_refs}
\alias{knitr_refs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reference for Tables and Figures in .Rmd files. 
}
\description{
Allow to refer to tables and figures in .Rmd files compiled with \code{\link[rmarkdown]{rmarkdown-package}} for LaTex (pdf).
}
\usage{
knitr_refs()
}
%- maybe also 'usage' for other objects documented here.

\details{ 

Simply call the function without arguments in a r code chunk at the begining of the file. The function will take care of the problem that rmarkdown does not allow to make references to Table and Figures number. The only constrain is that the figure should have a fig.cap= option filled with some text and a label= option set. The reference in the text ( \\ref\{\} ) should point to the value of the label= set in the r code chunk.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a string
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marcello Gallucci
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# ```{r, results='hide',echo=FALSE,error=FALSE, warning=FALSE,message=FALSE }
#
# library(knitr)
# library(statwriter) 
# knit_refs()
# ```
#
# There is a nice graph in Fig. \ref{fig:one}
# 
# ```{r label=fig:one, ,results='tex',  fig.cap="A nice picture"}
# 
# plot(1:3)
# ```


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ knitr }% __ONLY ONE__ keyword per line
