## Merge PD diagnosis data with enrollment data and baseline
str(PDFEAT)
str(RANDOM)

table(PDFEAT$event_id, useNA = "always")
table(RANDOM$event_id, useNA = "always")

PDFEATSUB <- subset(PDFEAT, event_id %in% c("SC", "BL"))

Temp <-  merge(
    PDFEATSUB[c("patno", "pddxdt")],
    RANDOM[c("patno", "birthdt", "enrolldt")],
    by = "patno"
)


## Convert character mm/YYYY to dates using the 15th as the day
fix_date <- function(x){
    date_split <- strsplit(x, split = "/")
    n <- length(date_split)
    dmy <- rep(NA, n)
    class(dmy) <- "Date"
    for (i in 1:n){
        dmy_char <- paste(date_split[[i]][1], "15", date_split[[i]][2], sep = "/")
        dmy_date <- as.Date(dmy_char, "%m/%d/%Y")
        dmy[i] <- dmy_date
    }
    return(dmy)
}

Temp$pddxdt_new <- fix_date(Temp$pddxdt)
Temp$birthdt_new <- fix_date(Temp$birthdt)
Temp$enrolldt_new <- fix_date(Temp$enrolldt)


## Compute age at baseline and time from PD diagnosis to enrollment (both in years)
Enroll <- within(Temp, {
    age_at_enroll <- as.numeric(floor((enrolldt_new - birthdt_new)/365.25))
    time_dx_enroll <- as.numeric((enrolldt_new - pddxdt_new)/365.25)
})
Enroll <- Enroll[c("patno", "age_at_enroll", "time_dx_enroll")]

str(Enroll)
summary(Enroll)

## Save dataset
save(Enroll, file = "Data/Enroll.RData")
