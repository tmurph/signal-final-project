side.length <- 2L
bcbsfl.with.predict <- filter(data.with.predict, IssuerName == "BCBSFL")

some.areas <- bcbsfl.with.predict$RatingAreaId %>%
    unique() %>%
    sample(side.length)
some.plans <- bcbsfl.with.predict$PlanId %>%
    unique() %>%
    sample(side.length)

df.to.graph <- bcbsfl.with.predict %>%
    filter(RatingAreaId %in% some.areas, PlanId %in% some.plans)

while ((length(unique(df.to.graph$RatingAreaId)) != side.length) ||
       (length(unique(df.to.graph$PlanId)) != side.length)) {

    ## yay, copy-paste 
    some.areas <- bcbsfl.with.predict$RatingAreaId %>%
        unique() %>%
        sample(side.length)
    some.plans <- bcbsfl.with.predict$PlanId %>%
        unique() %>%
        sample(side.length)

    df.to.graph <- bcbsfl.with.predict %>%
        filter(RatingAreaId %in% some.areas, PlanId %in% some.plans)

}


library(ggplot2)
library(reshape2)
library(wesanderson)


df.to.graph <- melt(df.to.graph, measure.vars=c("IndividualRate", "ModeledRate"))
graph.colors <- wes_palette("Royal1", 2)

ggplot(df.to.graph, aes(x=AgeNum, y=value, color=variable, fill=variable)) +
    facet_grid(RatingAreaId ~ PlanId) +
    geom_bar(stat="identity", data=filter(df.to.graph, variable == "IndividualRate"),
             width=0.5) +
    geom_point(data=filter(df.to.graph, variable == "ModeledRate"),
               size=1) +
    labs(title="Sample Individual Rates for BCBSFL Plans and Counties",
         x="Member Age", y="Individual Rate") +
    scale_color_manual(values=graph.colors,
                       labels=c("Published Rates", "Modeled Rates"),
                       name=NULL) +
    scale_fill_manual(values=c(graph.colors[1], "white"),
                      labels=c("Published Rates", "Modeled Rates"),
                      name=NULL)

ggplot(florida.county.factors, aes(x=scale(BCBSFL), y=scale(United))) +
    geom_point() + geom_smooth(method="lm") +
    ggtitle("Linear Regression of County Factors, United on BCBSFL")

df.to.graph <- florida.tracking.summ %>%
    select(County, matches("cancer.*2007")) %>%
    melt(id.vars="County")

union.to.graph <- filter(df.to.graph, County == "union")

ggplot(df.to.graph, aes(x=variable, y=value)) +
    geom_bar(stat="identity") +
    geom_bar(data=union.to.graph, stat="identity", fill="red") +
    facet_wrap(~ County) +
    theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
    labs(title="Age-Adjusted Cancer Incidence, 2007",
         x="Various Cancers",
         y="Incidence / 100,000")

df.to.graph <- florida.county.factors

union.to.graph <- filter(df.to.graph, County == "union")

ggplot(df.to.graph, aes(x=County, y=United)) +
    geom_point() +
    geom_point(data=union.to.graph, color="red", size=3) +
    geom_text(data=union.to.graph, label="Union", color="Red", hjust=1.3) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
    labs(title="Rate Relativities by County, United",
         y="Area Rating Factors",
         x=NULL)

library(maps)


map.data.df <- map_data("county", "florida") %>%
    select(x=long, y=lat, id=subregion)

map.data.df$id <- map.data.df$id %>%
    tolower() %>%
    lapply(FUN=function(cnty){
        return(switch(cnty,
                      "de soto"="desoto",
                      "st johns"="st. johns",
                      "st lucie"="st. lucie",
                      cnty))
    }) %>%
    as.character()

ggplot(florida.county.factors, aes(map_id=County)) +
    geom_map(aes(fill=BCBSFL), map=map.data.df) +
    expand_limits(map.data.df) +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          panel.background=element_blank(),
          legend.title=element_blank()) +
    scale_fill_gradientn(colors=wes_palette("Zissou", 100, "continuous")) +
    labs(x=NULL, y=NULL, title="Rate Relativities by County (BCBSFL)")
