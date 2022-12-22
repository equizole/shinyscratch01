#' Given an income amount, standard deduction, tax brackets, and bracket minimums, calculates the
#' tax (dollar amount), tax bracket (percent), and effective tax rate (percent)
#'
#' @param income The amount of income to calculate the tax for
#' @param standard_deduction The standard deduction for the tax type. Specify 0 if none.
#' @param tax_brackets A vector of the tax bracket percentages (specified as numbers from 0 - 100)
#' @param tax_brackets_mins A vector of the lower bound dollar amounts associated with each tax bracket
#'
#' @return
#' A list of 3 numbers giving the tax (dollar amount), tax bracket (percent), and effective tax rate (percent)
#'
#' @export
#'
#' @examples
calc_income_tax <- function(
        income,
        standard_deduction,
        tax_brackets,
        tax_brackets_mins
) {

    # Calculate taxable income
    if (income <= standard_deduction) {
        taxable_income <- 0
    } else {
        taxable_income <- income - standard_deduction
    }

    # If no taxable income
    if (taxable_income <= 0) {
        results <- list(
            "tax" = 0,
            "tax_bracket" = 0,
            "effective_tax_rate" = 0
        )

        return(results)
    }

    # Calculate the tax
    num_tax_brackets <- length(tax_brackets)
    tax_bracket_index <- 1
    total_tax <- 0
    remaining_taxable_income <- taxable_income

    while (remaining_taxable_income > tax_brackets_mins[tax_bracket_index]) {

        taxable_amount <- remaining_taxable_income - tax_brackets_mins[tax_bracket_index]
        tax_rate <- tax_brackets[tax_bracket_index] / 100

        if (tax_bracket_index < num_tax_brackets) {
            taxable_amount <- min(
                taxable_amount,
                tax_brackets_mins[tax_bracket_index + 1] - tax_brackets_mins[tax_bracket_index]
            )
            tax_bracket_index <- tax_bracket_index + 1
        } else {
            remaining_taxable_income <- 0
        }

        total_tax = total_tax + round(taxable_amount * tax_rate, 2)
    }

    # Calculate the effective tax rate
    effective_tax_rate <- round(total_tax / income, 4)

    # Return the results
    results <- list(
        "tax" = total_tax,
        "tax_bracket" = tax_rate,
        "effective_tax_rate" = effective_tax_rate
    )
}


#' Calculate social security tax
#'
#' Note that social security tax is applicable to salary income only -- not on 401k distributions
#'
#' @param income The amount of income to calculate the tax for
#' @param income_max The maximum amount of income that can be taxed
#' @param tax_rate The tax rate (as a percentage from 0 - 1)
#'
#' @return The calculated tax
#' @export
#'
#' @examples
calc_income_tax_social_security <- function(income, income_max, tax_rate) {
    social_security_tax <- round(min(income_max, income) * tax_rate, 2)
}


#' Compile the tax calculations into a tabular arrangement of line items for presentation
#'
#' @param income_salary The amount of income coming from a salary
#' @param income_401k The amount of income coming from a 401k distribution
#' @param tax_config A dictionary with the tax brackets and rates
#'
#' @return A DataFrame for presenting the tax calculation line items
#' @export
#'
#' @examples
calc_tax_table <- function(income_salary, income_401k, tax_config) {

    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculations
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    income_gross <- income_salary + income_401k

    # Federal income tax
    federal_income_taxes <- calc_income_tax(
        income = income_gross,
        standard_deduction = tax_config$federal$standard_deduction,
        tax_brackets = tax_config$federal$tax_brackets,
        tax_brackets_mins = tax_config$federal$tax_brackets_mins
    )

    # State income tax
    state_income_taxes <- calc_income_tax(
        income = income_gross,
        standard_deduction = tax_config$state$standard_deduction,
        tax_brackets = tax_config$state$tax_brackets,
        tax_brackets_mins = tax_config$state$tax_brackets_mins
    )

    # Total income tax
    income_gross_adjusted_federal <- max(income_gross - tax_config$federal$standard_deduction, 0)
    income_gross_adjusted_state <- max(income_gross - tax_config$state$standard_deduction, 0)
    total_income_tax <- federal_income_taxes$tax + state_income_taxes$tax
    effective_tax_rate_income <- round(total_income_tax / income_gross, 4)

    # Social security tax
    social_security_tax <- calc_income_tax_social_security(
        income = income_salary,
        income_max = tax_config$social_security$income_max,
        tax_rate = tax_config$social_security$tax_rate
    )

    # Medicare tax
    medicare_taxes <- calc_income_tax(
        income = income_salary,
        standard_deduction = tax_config$medicare$standard_deduction,
        tax_brackets = tax_config$medicare$tax_brackets,
        tax_brackets_mins = tax_config$medicare$tax_brackets_mins
    )

    # Total FICA
    total_fica_tax <- social_security_tax + medicare_taxes$tax

    # Total tax
    total_tax <- total_income_tax + total_fica_tax
    effective_tax_rate_total <- round(total_tax / income_gross, 4)
    income_net <- income_gross - total_tax

    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Line items
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Initialize columns
    line_items <- character()
    monthly_amounts <- double()
    yearly_amounts <- double()
    tax_brackets <- double()
    effective_tax_rates <- double()

    # Gross income
    line_items <- append(line_items, "Gross Income")
    monthly_amounts <- append(monthly_amounts, round(income_gross / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(income_gross, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Adjusted gross income -- Federal
    line_items <- append(line_items, "Adjusted Gross Income -- Federal")
    monthly_amounts <- append(monthly_amounts, round(income_gross_adjusted_federal / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(income_gross_adjusted_federal, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Adjusted gross income -- State
    line_items <- append(line_items, "Adjusted Gross Income -- State")
    monthly_amounts <- append(monthly_amounts, round(income_gross_adjusted_state / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(income_gross_adjusted_state, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Total taxes
    line_items <- append(line_items, "Total Tax")
    monthly_amounts <- append(monthly_amounts, round(total_tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, total_tax)
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, effective_tax_rate_total)

    # Net income
    line_items <- append(line_items, "Net Income")
    monthly_amounts <- append(monthly_amounts, round(income_net / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(income_net, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Blank row
    line_items <- append(line_items, NA_character_)
    monthly_amounts <- append(monthly_amounts, NA_real_)
    yearly_amounts <- append(yearly_amounts, NA_real_)
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Federal income taxes
    line_items <- append(line_items, "Federal Income Tax")
    monthly_amounts <- append(monthly_amounts, round(federal_income_taxes$tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(federal_income_taxes$tax, 2))
    tax_brackets <- append(tax_brackets, federal_income_taxes$tax_bracket)
    effective_tax_rates <- append(effective_tax_rates, federal_income_taxes$effective_tax_rate)

    # State income taxes
    line_items <- append(line_items, "State Income Tax")
    monthly_amounts <- append(monthly_amounts, round(state_income_taxes$tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(state_income_taxes$tax, 2))
    tax_brackets <- append(tax_brackets, state_income_taxes$tax_bracket)
    effective_tax_rates <- append(effective_tax_rates, state_income_taxes$effective_tax_rate)

    # Total income taxes
    line_items <- append(line_items, "Total Income Tax")
    monthly_amounts <- append(monthly_amounts, round(total_income_tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, total_income_tax)
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, effective_tax_rate_income)

    # Blank row
    line_items <- append(line_items, NA_character_)
    monthly_amounts <- append(monthly_amounts, NA_real_)
    yearly_amounts <- append(yearly_amounts, NA_real_)
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Social Security taxes (salary income only)
    line_items <- append(line_items, "Social Security Tax")
    monthly_amounts <- append(monthly_amounts, round(social_security_tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(social_security_tax, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Medicare taxes (salary income only)
    line_items <- append(line_items, "Medicare Tax")
    monthly_amounts <- append(monthly_amounts, round(medicare_taxes$tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(medicare_taxes$tax, 2))
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Total FICA taxes
    line_items <- append(line_items, "Total FICA Tax")
    monthly_amounts <- append(monthly_amounts, round(total_fica_tax / 12, 2))
    yearly_amounts <- append(yearly_amounts, total_fica_tax)
    tax_brackets <- append(tax_brackets, NA_real_)
    effective_tax_rates <- append(effective_tax_rates, NA_real_)

    # Create the DataFrame
    tax_table <- tibble::tibble(
        line_item = line_items,
        monthly_amount = monthly_amounts,
        yearly_amount = yearly_amounts,
        tax_bracket = tax_brackets,
        effective_tax_rate = effective_tax_rates
    )
}

# temp_01 <- calc_tax_table(50000, 50000, config::get("tax_config"))

