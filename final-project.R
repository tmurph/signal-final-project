library(readr)
library(dplyr)


## The rate data includes many fields we don't need for this analysis.
## Select just the rates by plan, county, age, and issuer.
df <- read_csv("data/rate-data.csv") %>%
    select(c(IndividualRate,                                 
             PlanId,                        
             RatingAreaId,                                   
             Age,                                      
             IssuerId))

## The Age information comes to us with some non-numeric strings.
## This gives us an equivalent numeric field, AgeNum, for later plotting.
df$AgeNum <- df$Age %>%
    sapply(FUN=function(.x) {
        switch(.x,
               "0-20"="20",
               "65 and over"="65",
               .x)   
    }) %>%
    as.numeric

## Issuer information comes to us as a federally-assigned ID number.
## This gives us the issuer names and mnemonics to work with instead.
df$IssuerName <- df$IssuerId %>%
    as.character %>%
    sapply(FUN=function(.id) {
        switch(.id,
               "16842"="BCBSFL",
               "68398"="United",
               "30252"="FB.HMO",
               "21663"="Celtic",
               "56503"="FHCP",
               "27357"="Health.First",
               "35783"="Humana",
               "57451"="Coventry",
               "54172"="Molina",
               "18628"="Aetna")
    })

## Most variables should be treated as factors.
for (var in c("PlanId", "RatingAreaId", "IssuerId", "IssuerName", "Age")) {
    df[[var]] <- factor(df[[var]])
}

library(lfe)

## In the language of the lfe package, the "|" symbol separates
## regressors (on the left) from fixed effect groupings (on the right).

## For this model, log(Rate) is modeled entirely as a sum of fixed effects,
## with no non-trivial regression terms.
model.formula <- log(IndividualRate) ~ 1 | PlanId + RatingAreaId + Age

## Loop with lapply / combine with rbind.
effects.df <- levels(df$IssuerName) %>% 
    lapply(function(name) {
        df %>%
            filter(IssuerName == name) %>%
            felm(formula=model.formula) %>%
            getfe() %>%
            select(fe, idx, effect) %>%
            mutate(issuer=name) %>%
            return()
    }) %>%
    do.call(what=rbind)

## To build a prediction for each issuer's rating observation, we need to sum
## the modeled effects and exponentiate the result.

## The effects.df comes in "tidy" or "long" form, but we need to transform it
## to a "messy" or "wide" form to calculate the exponentiated sum.

## Split effects.df on the fixed effect groups and join the results together.
wide.effects.df <- levels(effects.df$fe) %>%
    lapply(FUN=function(effect.group) {
        filter(effects.df, fe == effect.group)
    }) %>%
    Reduce(f=function(x, y){merge(x, y, by="issuer")})

## Horizontally aggregate the indices and effects.
agg.indices <- wide.effects.df %>%
    select(issuer, starts_with("idx")) %>%
    c(sep=".") %>%
    do.call(what=paste)
agg.effects <- wide.effects.df %>%
    select(starts_with("effect")) %>%
    rowSums() %>%
    exp()
agg.df <- data.frame(index=agg.indices,
                     ModeledRate=agg.effects,
                     stringsAsFactors=F)

## Create an equivalent index in the original data frame.
df2 <- df
df2$index <- df2 %>%
    select(IssuerName, Age, PlanId, RatingAreaId) %>%
    c(sep=".") %>%
    do.call(what=paste)

## Join the two together.
data.with.predict <- left_join(df2, agg.df, by="index") %>%
    select(-c(index))

library(reshape2)

## The csv referenced in this code contains the federally specified mapping of
## rating area numbers to county names.  The mapping's available online.
florida.county.factors <- effects.df %>%
    filter(fe == "RatingAreaId") %>%
    dcast(idx ~ issuer, value.var="effect") %>%
    merge(y=read_csv("data/govt-rating-areas.csv"),
          by.x="idx", by.y="Rating Area ID", all=T)

## All counties are (by construction) spelled canonically in this data.
## Just convert to lowercase.
florida.county.factors$County <- florida.county.factors$County %>%
    tolower()

## All files were downloaded from the Florida Department of Health's
## Environmental Public Health Tracking website:
## http://www.floridatracking.com/HealthTrackFL/default.aspx

florida.tracking.files <- c("data/fl-track-birth-low-weight.csv",
                            "data/fl-track-birth-preterm.csv",
                            "data/fl-track-birth-very-low-weight.csv",
                            "data/fl-track-birth-very-preterm.csv",
                            "data/fl-track-cancer-bladder.csv",
                            "data/fl-track-cancer-brain.csv",
                            "data/fl-track-cancer-breast49.csv",
                            "data/fl-track-cancer-breast50.csv",
                            "data/fl-track-cancer-kidney.csv",
                            "data/fl-track-cancer-leukemia.csv",
                            "data/fl-track-cancer-liver.csv",
                            "data/fl-track-cancer-lung.csv",
                            "data/fl-track-cancer-lymphoma.csv",
                            "data/fl-track-cancer-melanoma.csv",
                            "data/fl-track-cancer-mesothelioma.csv",
                            "data/fl-track-cancer-pancreas.csv",
                            "data/fl-track-cancer-thyroid.csv",
                            "data/fl-track-ecoli.csv",
                            "data/fl-track-heart-er.csv",
                            "data/fl-track-heart-hosp.csv",
                            "data/fl-track-obesity.csv",
                            "data/fl-track-poverty5.csv",
                            "data/fl-track-poverty65.csv",
                            "data/fl-track-salmonella.csv",
                            "data/fl-track-self-report.csv",
                            "data/fl-track-smoke2.csv")

## Give the data frames meaningful names.
florida.tracking.data <- Map(florida.tracking.files,
                             f=function(file) {
                                 return(read_csv(file, na=c("", "NA", "*")))
                             })
names(florida.tracking.data) <- florida.tracking.files %>%
    gsub(pattern="^data/fl-track-", replacement="") %>%
    gsub(pattern=".csv$", replacement="") %>%
    gsub(pattern="-", replacement=".")

## Merge all the frames together, by county.
florida.tracking.summ <- names(florida.tracking.data) %>%
    lapply(FUN=function(nam) {
        dat <- florida.tracking.data[[nam]]
        names(dat) <- names(dat) %>%
            sub(pattern="([[:digit:]]+)",
                replacement=paste0(nam, ".", "\\1"))
        return(dat)
    }) %>%
    Reduce(f=function(x, y){return(merge(x, y, by="County"))}) %>%
    filter(County != "Florida")

## By coincidence, all counties are spelled canonically in this data.
## Just convert to lowercase.
florida.tracking.summ$County <- florida.tracking.summ$County %>%
    tolower()

## All files were downloaded from the Florida Department of Health's
## FloridaCHARTS website:
## http://www.floridacharts.com/flquery/population/populationrpt.aspx

florida.demo.files <- c("data/fl-demo-population.csv",
                        "data/fl-demo-pop-age.csv",
                        "data/fl-demo-pop-sex.csv",
                        "data/fl-demo-pop-race.csv",
                        "data/fl-demo-pop-ethnic.csv")

## Give the data frames meaningful names.
florida.demo.data <- Map(florida.demo.files,
                         f=function(file) {
                             return(read_csv(file, na=c("", "NA", "*")))
                         })
names(florida.demo.data) <- florida.demo.files %>%
    gsub(pattern="^data/fl-demo-", replacement="") %>%
    gsub(pattern=".csv$", replacement="") %>%
    gsub(pattern="-", replacement=".")

## Merge all the frames together by county.
florida.demo.summ <- names(florida.demo.data) %>%
    lapply(FUN=function(nam) {
        dat <- florida.demo.data[[nam]]
        names(dat) <- names(dat) %>%
            sub(pattern="^", replacement="pop.2014.") %>%
            sub(pattern="pop.2014.County", replacement="County")
        return(dat)
    }) %>%
    Reduce(f=function(x, y){return(merge(x, y, by="County"))})

## Some counties are spelled multiple ways "in the wild".
## Switch to canonical names.
florida.demo.summ$County <- florida.demo.summ$County %>%
    tolower() %>%
    lapply(FUN=function(cnty){
        return(switch(cnty,
                      "saint johns"="st. johns",
                      "saint lucie"="st. lucie",
                      cnty))
    }) %>%
    as.character()

library(caret)


## Properly normalized data has had the following transformations applied:
## - BoxCox, which requires a shift to make all values strictly positive
## - Imputation of NA entries (we're imputing to the mean)
## - Rescaling to mean 0 and sd 1
normalized.data <- list(florida.county.factors,
                        florida.tracking.summ,
                        florida.demo.summ) %>%
    Reduce(f=function(x, y){merge(x, y, by="County")}) %>%
    select(BCBSFL, United, matches("[[:digit:]]{4}")) %>%
    lapply(FUN=function(col) {
        tmp <- col + min(0, col, na.rm=TRUE) + sd(col, na.rm=TRUE) / 10
        ret <- BoxCoxTrans(tmp, na.rm=TRUE) %>% predict(tmp)
        ret[is.na(ret)] <- mean(ret, na.rm=TRUE)
        return(ret)
    }) %>%
    data.frame() %>%
    scale() %>%
    as.data.frame()

library(glmnet)

X.United <- model.matrix(United ~ . - BCBSFL, normalized.data)
Y.United <- model.frame(United ~ . - BCBSFL, normalized.data) %>% model.response()
coef.United <- cv.glmnet(X.United, Y.United, nfolds=nrow(normalized.data)) %>%
    coef(s="lambda.1se")

X.BCBSFL <- model.matrix(BCBSFL ~ . - United, normalized.data)
Y.BCBSFL <- model.frame(BCBSFL ~ . - United, normalized.data) %>% model.response()
coef.BCBSFL <- cv.glmnet(X.BCBSFL, Y.BCBSFL, nfolds=nrow(normalized.data)) %>%
    coef(s="lambda.1se")
