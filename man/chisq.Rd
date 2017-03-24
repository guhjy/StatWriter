\name{chisq}
\alias{chisq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract chi-square for writing from LRT and other techniques
}
\description{
Extract chi-square and  writes it in APA style
}
\usage{
chisq(amod,pos=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amod}{
a object of class \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, \code{\link[stats]{anova}}, or "htest" from \code{\link[stats]{chisq.test}} 
}
  \item{pos}{
the row of the object amod referring to the position of the  test to be extracted. Default pos=2 so it is the first effect in objects referring to variables effects, or the comparison of two model for anova like objects. 
}

}
\details{
Returns the chi-square value with degrees of freedom and p-value. It extracts the values in different ways depending on the object passed as input. When the model is of class \code{\link[stats]{lm}},  \code{\link[stats]{glm}}, or \code{\link[lme4]{glmer}}, \code{anova(x,test="Chisq")} is computed and the chisq, df, and p-value are extracted. When the object is of the class  \code{\link[stats]{anova}} the object is tested for the presence of "deviance", which means that the anova has been ran with the \code{test="Chisq"} option. Then the values are extracted. If the object is of class "htest" the chisq is extracted from "statistic" field of the object. If "pos" is specified, the values in row=pos of the object are extracted, otherwise pos=2 is used, so it is the first effect in objects referring to variables effects, or the comparison of two model for anova like objects. . 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a string with Chisq, df, p.value and valid N is returned
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
res<-anova(mod1,mod0)
chisq(res) 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ round }% __ONLY ONE__ keyword per line
