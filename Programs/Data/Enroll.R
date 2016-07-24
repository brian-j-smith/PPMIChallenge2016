## Merge PD diagnosis data with enrollment data and baseline
str(PDFEAT)
str(RANDOM)
str(VITAL)

table(PDFEAT$event_id, useNA = "always")
table(RANDOM$event_id, useNA = "always")

Temp <-  join_all(
    list(
        subset(PDFEAT, event_id %in% c("SC", "BL"), select = c(patno, pddxdt)),
        subset(RANDOM, select = c(patno, birthdt)),
        subset(VITAL, event_id == "BL", select = c(patno, infodt))
    ),
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
Temp$infodt_new <- fix_date(Temp$infodt)


## Compute age at baseline and time from PD diagnosis to enrollment (both in years)
Enroll <- within(Temp, {
    age_at_bl <- as.numeric(floor((infodt_new - birthdt_new)/365.25))
    time_dx_bl <- as.numeric((infodt_new - pddxdt_new)/365.25)
})
Enroll <- Enroll[c("patno", "age_at_bl", "time_dx_bl")]

str(Enroll)
summary(Enroll)

## Save dataset
save(Enroll, file = "Data/Enroll.RData")
