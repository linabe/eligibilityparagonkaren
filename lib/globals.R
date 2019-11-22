# Add any project specific configuration here

select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete
matches <- tidyselect::matches

z05 <- qnorm(1 - 0.025)