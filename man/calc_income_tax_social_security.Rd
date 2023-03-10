% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{calc_income_tax_social_security}
\alias{calc_income_tax_social_security}
\title{Calculate social security tax}
\usage{
calc_income_tax_social_security(income, income_max, tax_rate)
}
\arguments{
\item{income}{The amount of income to calculate the tax for}

\item{income_max}{The maximum amount of income that can be taxed}

\item{tax_rate}{The tax rate (as a percentage from 0 - 1)}
}
\value{
The calculated tax
}
\description{
Note that social security tax is applicable to salary income only -- not on 401k distributions
}
