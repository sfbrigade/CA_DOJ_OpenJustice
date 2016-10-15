
## Prep the workspace
library("AzureML")
ws <- workspace()

## load the data: Get all 'arrests' file names.
## Then, download those files from our Azure ML workspace and subset to 2005+,
## because our population data (next) only begins in 2005.
file_names <- grep("^ca_doj_arrests.*csv", ws$datasets$Name, value = T)
dat_arrests <- do.call(rbind, download.datasets(ws, file_names[!grepl("200[0-4]", file_names)]))

## Remove row names
row.names(dat_arrests) <- NULL

## Now, get county population data by race and gender.
dat_pop <- download.datasets(ws, "ca_county_population_by_race_gender_age_2005-2014_02-05-2016.csv")

## Preview the arrests data
dim(dat_arrests)
names(dat_arrests)
head(dat_arrests, 3)

## Load necessary libraries
library(dplyr)
library(ggplot2)
library(grid)
library(stats)

## Subset arrests to only juveniles.
dat_juv <- dat_arrests[dat_arrests$age_group %in% "juvenile",]

## Group by county, then by year, then by race/ethnicity and give me the counts.
cty_ethnic <- summarise(group_by(dat_juv, county, arrest_year, race_or_ethnicity), total = n())

## Now remove those records supressed due to privacy concern.
cty_ethnic <- cty_ethnic[!(cty_ethnic$race_or_ethnicity %in% "suppressed_due_to_privacy_concern"),]

#### !! Some counties are reporting only "NA"s in their arrest totals per ethnic group. :-\
## Let's remove those from our analysis...for now.
cty_ethnic <- cty_ethnic[!is.na(cty_ethnic$total),]

## Confirm via preview
dim(cty_ethnic)
head(cty_ethnic)
tail(cty_ethnic)

## Panel bar charts: ethnic breakdown of arrests, by county.
## Note: this is sheerly by count (not rate).
plot_ethnic <- ggplot(cty_ethnic[cty_ethnic$arrest_year %in% "2014",], aes(x = race_or_ethnicity, y = total, fill = race_or_ethnicity)) + 
                geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                      axis.title.y=element_blank()) +
                ggtitle("Ethnic Breakdown of Arrest FREQ by County\r
2014 Only (test year)")

## Print plot
suppressWarnings(
    print(plot_ethnic)
)

## Stacked bar chart: ethnic breakdown of arrests, stacked between counties.
plot_ethnic2 <- ggplot(cty_ethnic[cty_ethnic$arrest_year %in% "2014",], aes(x = race_or_ethnicity, y = total, fill = county)) + 
                geom_bar(stat = "identity") + coord_flip() + 
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      strip.text=element_text(size = 8), axis.title.x=element_blank(), axis.title.y=element_blank(),
                      legend.text=element_text(size= 6), legend.key.height=unit(.4, "cm")) +
                ggtitle("Cumulative Ethnic Breakdown of Arrest Freq by County\r
2014 Only (test year)")

## Print plot
suppressWarnings(
    print(plot_ethnic2)
)

## Now, let's preview the population data
dim(dat_pop)
names(dat_pop)
head(dat_pop)

## Looks like it's already aggregated along a number of dimensions. 
## Let's subset only the juveniles.
dat_pop_jv <- dat_pop[dat_pop$age_group %in% "Juvenile",]

## Ok, now, let's look at arrests of both genders and ignore the 'all combined' county value.
dat_pop_jv <- dat_pop_jv[dat_pop_jv$gender %in% "All Combined" & !(dat_pop_jv$county %in% "All Combined"),]

## Let's also remove the race 'all combined.'
dat_pop_jv <- dat_pop_jv[!(dat_pop_jv$race %in% "All Combined"),]

## Confirm we did this right by previewing the head and tail.
head(dat_pop_jv)
tail(dat_pop_jv)

## Show the race / ethnicity categories of each dataset (arrests vs. population)
unique(dat_juv$race_or_ethnicity)
unique(dat_pop_jv$race)

## Join the pop and arrests datasets.
## Start by relabeling the 'race' variable in the pop table. Also, until we've bound all years together, 
names(dat_pop_jv)[3] <- "race_or_ethnicity"
names(cty_ethnic)[2] <- "year"
dat_joined <- right_join(cty_ethnic, dat_pop_jv, by = c("county","year","race_or_ethnicity"))

## Let's sub out those counties that aren't represented in the arrests file.
dat_joined <- dat_joined[!(dat_joined$county %in% "Alpine" | 
                           dat_joined$county %in% "Amador" |
                           dat_joined$county %in% "Yuba"),]

## Preview to confirm. 
head(dat_joined)
tail(dat_joined)

### Plot Arrest Rates by County for a Single Year (2014)...
## Let's remove post-join arrest total NAs from our analysis...for now.
dat_joined <- dat_joined[!is.na(dat_joined$total),]

## Actually add a column just for arrest rate by ethnic population per county.
dat_joined$eth_arrest_rate <- round((dat_joined$total)/(dat_joined$population), 5)

## Now, let's panel plot arrest rates by county.
plot_ethnic_norm <- ggplot(dat_joined[!(dat_joined$race_or_ethnicity %in% "Native American") & dat_joined$year %in% "2014",], 
                        aes(x = race_or_ethnicity, y = eth_arrest_rate, fill = race_or_ethnicity), na.rm=T) + 
                        geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                        theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                        legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                        axis.title.y=element_blank()) +
                        ggtitle("Ethnic Breakdown of Arrest Rates by County\r
-2014 Only-")

## Print plot
suppressWarnings(print(plot_ethnic_norm))

#### Looping approach (Please don't hate me, RNG! I'll vectorize asap :)) ####

## Create empty dataframe
dat_stats <- dat_joined[0,]
dat_stats$rate_prob <- numeric(0)
dat_stats$z_score <- numeric(0)

## Nested loop (computing stats per race/ethnic group, per year)
for(i in unique(dat_joined$year)){
    
    ## Subset to iterative year
    dat_year <- dat_joined[dat_joined$year %in% i,]
    
    for(j in unique(dat_year$race_or_ethnicity)){
        
        ## Subset to iterative race/ethnicity
        dat_race <- dat_year[dat_year$race_or_ethnicity %in% j,]
        
        ## Compute the probability of the observed arrest rate
        dat_race$percentile <- round(pnorm(dat_race$eth_arrest_rate, mean(dat_race$eth_arrest_rate, na.rm = T), 
                                    sd(dat_race$eth_arrest_rate, na.rm = T), lower.tail = T, log.p = F), 5)
        
        ## Compute the Z-score of the observed arrest rates
        dat_race$z_score <- qnorm(dat_race$percentile, lower.tail = T, log.p = F)
        
        ## Bind to burgeoning dataframe
        dat_stats <- rbind(dat_stats, dat_race)
    }
}

## Now, preview those who have evidently been outliers in enforcement upon every ethnic group per every year.
paste("Number of outlying instances over this time-span:", nrow(dat_stats[dat_stats$z_score >= 3,]))
head(dat_stats[dat_stats$z_score >= 3,], 20)
tail(dat_stats[dat_stats$z_score >= 3,], 20)

## Draw up a frequency table of the instances per outlying county from above.
table(dat_stats[dat_stats$z_score >= 3, "county"])

## Let's isolate those SF cases to see if there's a pattern there.
dat_stats[dat_stats$z_score >= 3 & dat_stats$county %in% "San Francisco",]

## Isolate Kings cases to see if there's a pattern.
dat_stats[dat_stats$z_score >= 3 & dat_stats$county %in% "Kings",]

## Isolate Kings cases to see if there's a pattern at just 2 SDs from the mean.
dat_stats[dat_stats$z_score >= 2& dat_stats$county %in% "Kings",]

## Isolate Marin cases to see if there's a pattern.
dat_stats[dat_stats$z_score >= 3 & dat_stats$county %in% "Marin",]

dat_stats[dat_stats$z_score <= -3,]

dat_stats[dat_stats$z_score <= -2,]

## Subset to Hispanics, 2014 (entire dataset is already subsetted to juveniles)
dat_stats_test <- dat_stats[dat_stats$year %in% "2014" & dat_stats$race_or_ethnicity %in% "Hispanic",]

## Test for difference between observed distribution and normal distribution (Shapiro-Wilk normality test). 
## If difference's p is < .05, then the observed distribution is not sufficiently normal.
shapiro.test(dat_stats_test$eth_arrest_rate)

## Plot the density w/outliers
suppressWarnings(
    plot(density(dat_stats_test$eth_arrest_rate), main = "Arrest Rate Density for Hispanics in 2014\r
(test ethnicity and year)")
)

## Plot vs. purely normal distribution
suppressWarnings(
    qqnorm(dat_stats_test$eth_arrest_rate, main = "Arrest Rate Observations for Hispanics in 2014\r
vs. Theoretical Norm Estimates")
)
qqline(dat_stats_test$eth_arrest_rate)

## Subset to "Other," 2014 (entire dataset is already subsetted to juveniles)
dat_stats_test <- dat_stats[dat_stats$year %in% "2014" & dat_stats$race_or_ethnicity %in% "Other",]

## Test for difference between observed distribution and normal distribution (Shapiro-Wilk normality test). 
## If difference's p is < .05, then the observed distribution is not sufficiently normal.
shapiro.test(dat_stats_test$eth_arrest_rate)

## Plot the density w/outliers
suppressWarnings(
    plot(density(dat_stats_test$eth_arrest_rate), 
     main = "Arrest Rate Density for OTHER in 2014\r
(test ethnicity and year)")
)
    
## Plot vs. purely normal distribution
suppressWarnings(
    qqnorm(dat_stats_test$eth_arrest_rate, main = "Arrest Rate Observations for OTHER in 2014\r
vs. Theoretical Norm Estimates")
)
qqline(dat_stats_test$eth_arrest_rate)

## Upload full stats DF to Azure ML and save to local csv.
upload.dataset(dat_stats, ws, name = "juv-ethnic-arrests_stats_2005-2014")
write.csv(dat_stats, "juv-ethnic-arrests_stats_2005-2014.csv", row.names = F)

## Upload outliers DF to Azure ML and save to local csv.
upload.dataset(dat_stats[dat_stats$z_score > 2,], ws, name = "juv-ethnic-arrests_outliers_2005-2014")
write.csv(dat_stats, "juv-ethnic-arrests_outliers_2005-2014.csv", row.names = F)

## ^Please disregard the Azure ML upload status messages the system produces...

dat_stats[dat_stats$county %in% "Los Angeles" & dat_stats$race_or_ethnicity %in% "Other",]

dat_stats[dat_stats$county %in% "San Francisco" & dat_stats$race_or_ethnicity %in% "Other",]

dat_pop[dat_pop$county %in% "San Francisco" & (dat_pop$race %in% "Other" | dat_pop$race %in% "All Combined") &
        dat_pop$gender %in% "All Combined" & dat_pop$age_group %in% "Juvenile",]

head(dat_joined[dat_joined$county %in% "San Francisco",], 20)

head(dat_joined[dat_joined$county %in% "Los Angeles",])

write.csv
