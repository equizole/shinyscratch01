# Given an income amount, standard deduction, tax brackets, and bracket minimums, calculates the
# tax (dollar amount), tax bracket (percent), and effective tax rate (percent)
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


calc_income_tax_federal <- function(income) {

    results <- calc_income_tax(
        income,
        standard_deduction = 25900,
        tax_brackets = c(10, 12, 22, 24, 32, 35, 37),
        tax_brackets_mins = c(0, 19900, 81050, 172750, 329850, 418850, 628300)
    )
}

calc_income_tax_state <- function(income) {

    results <- calc_income_tax(
        income,
        standard_deduction = 8520,
        tax_brackets = c(0, 3, 4, 5, 6, 7),
        tax_brackets_mins = c(0, 3070, 6150, 9230, 12310, 15400)
    )
}

# Only on salary income (not on 401k distributions)
calc_income_tax_medicare <- function(income) {

    results <- calc_income_tax(
        income,
        standard_deduction = 0,
        tax_brackets = c(1.45, 0.9),
        tax_brackets_mins = c(0, 250000)
    )
}


# Only on salary income (not on 401k distributions)
calc_income_tax_social_security <- function(income) {

    social_security_max <- 147000
    social_security_tax <- round(min(social_security_max, income) * 0.062, 2)
}


calc_tax_table <- function(income_salary, income_401k) {

    # Initialize columns
    line_items <- character()
    monthly_amounts <- double()
    yearly_amounts <- double()
    tax_brackets <- double()
    effective_tax_rates <- double()

    # Calculations
    income_gross <- income_salary + income_401k

    federal_income_taxes <- calc_income_tax_federal(income_gross)
    state_income_taxes <- calc_income_tax_state(income_gross)
    total_income_tax <- federal_income_taxes$tax + state_income_taxes$tax
    effective_tax_rate_income <- round(total_income_tax / income_gross, 4)

    social_security_tax <- calc_income_tax_social_security(income_salary)
    medicare_taxes <- calc_income_tax_medicare(income_salary)
    total_fica_tax <- social_security_tax + medicare_taxes$tax

    total_tax <- total_income_tax + total_fica_tax
    effective_tax_rate_total <- round(total_tax / income_gross, 4)
    income_net <- income_gross - total_tax

    # Gross income
    line_items <- append(line_items, "Gross Income")
    monthly_amounts <- append(monthly_amounts, round(income_gross / 12, 2))
    yearly_amounts <- append(yearly_amounts, round(income_gross, 2))
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

temp_01 <- calc_tax_table(50000, 50000)
