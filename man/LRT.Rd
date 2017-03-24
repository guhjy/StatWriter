\name{LRT}
\alias{LRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the log-likelihood ratio test (LRT) for writing
}
\description{
Extract the log-likelihood ratio test (LRT) from an \code{\link[stats]{anova}} table and writes it in APA style
}
\usage{
LRT(amod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amod}{
a object of class \code{\link[stats]{anova}}, comparing two models
}

}
\details{

Returns the LRT value with degrees of freedom and p-value. 

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

dat<-as.data.frame(cbind(y=rbinom(100,1,.5),x=rnorm(100,0,1),id=rep(1:20)))
mod0<-glmer(y~(1|id),dat,family=binomial())
mod1<-glmer(y~(1|id)+x,dat,family=binomial())
res<-anova(mod0,mod1,test="LRT")
LRT(res) 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ F-test }% __ONLY ONE__ keyword per line
