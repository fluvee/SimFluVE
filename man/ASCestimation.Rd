\name{ASCestimation}
\alias{ASCestimation}
\title{Estimate VE from Active Surveillance Cohort Study (ASC)}
\usage{
ASCestimation(N, SENS, SPEC, outcomes)
}
\arguments{
\item{N}{number of sample size}

\item{SENS}{sensitivity}

\item{SPEC}{specificity}

\item{outcomes}{generated outcome file}
}
\details{
1.	User Setup: Set up cohort size, sensitivity and specificity.
2.	Estimate VE
}
\value{
Mean and std of estimated VE
}
