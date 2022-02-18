library(rvest)
library(readr)
suppressWarnings(library(dplyr))
suppressWarnings(library(janitor))
library(ggplot2)
library(mytools)

# 1. Load the species table using the code in the Setup block below
webpage <- read_html("https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm")
tbls <- html_nodes(webpage, "table") %>% html_table(fill = TRUE)
species <- tbls[[1]] %>% clean_names() %>%select(alpha_code, common_name) %>% mutate(alpha_code=tolower(alpha_code))

# 2. Read the following two files into your environment
pred <- read_csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9ffec04c-7e2d-41dd-9e88-b6c2e8c4375e")
nests <- read_csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A982bd2fc-4edf-4da7-96ef-0d11b853102d")

# 3. Write a function that will translate species codes into common names
spcode_to_common_names <- function(predator_df, species_df) {
  combine_df <- left_join(predator_df, species_df, by=c("species"="alpha_code"))
}

spcode_to_common_names <- function(predator_df, species_df, left_name="species",right_name="alpha_code") {
  combine_df <- left_join(predator_df, species_df, by=c(left_name=right_name))
}

# 4. Calculate total number of predators by year and species, and plot the result
pred <- spcode_to_common_names(pred,species)
predator_totals <- pred %>% group_by(year, common_name) %>% summarise(predator_count=sum(count, na.rm=TRUE))  %>% filter(!is.na(common_name))
ggplot(data=predator_totals, mapping=aes(x=year, y=predator_count, color=common_name)) +  geom_line() + geom_point()
ggplot(data=predator_totals, mapping=aes(x=year, y=predator_count, color=common_name)) +  geom_line() + geom_point() + custom_theme(base=11)

# 5. Calculate total number of eggs predated by year and species.
nests <- spcode_to_common_names(nests,species)
egg_predation_totals <- nests %>% group_by(year, common_name) %>% summarise(egg_predation_count=sum(number_eggs_predated, na.rm=T))  %>% filter(!is.na(common_name))

# 6. Calculate total number of predators by year, join to summarized egg predation table, and plot the result
annual_predators <- predator_totals %>% group_by(year) %>% summarize(annual_predator_count=sum(predator_count, na.rm=TRUE)) 
combo_df <- left_join(annual_predators, egg_predation_totals, by=c("year"))
ggplot(data=combo_df, mapping=aes(x=egg_predation_count, y=annual_predator_count)) + geom_point() + facet_wrap(~common_name) + theme_bw()
