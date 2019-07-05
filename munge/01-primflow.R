
# Primary criteria --------------------------------------------------------

flow <- c("Number of posts in KaRen", nrow(karen))

pdata <- karen %>%
  filter(!is.na(FSI_PREDAT) & FSI_PREDAT != "")

flow <- rbind(flow, c("Follow-up visit at 4 weeks", nrow(pdata)))

pdata <- pdata %>%
  filter(VG10 >= 45 | is.na(VG10))

flow <- rbind(flow, c("Posts with EF >= 40% or missing EF", nrow(pdata)))

colnames(flow) <- c("Criteria", "N")
