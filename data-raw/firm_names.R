## code to prepare `firm_names` dataset goes here

candidates <- stock["stock_name"]

usethis::use_data(aprilnames, overwrite = TRUE) # tibble w/ cols name long_name stock_name
usethis::use_data(candidates, overwrite = TRUE) # tibble w/ col stock_name
