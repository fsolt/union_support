library(tidyverse)
library(stringr)
library(haven)
library(jsonlite)
library(mi)

acs_s1901 <- read_csv("data/ACS_11_5YR_S1901/ACS_11_5YR_S1901.csv",
                      skip = 1,
                      col_types = cols(.default = col_double(),
                                       `Id` = col_character(),
                                       `Geography` = col_character())) %>%
  select(Id, Id2, Geography, starts_with("Households")) %>%
  filter(!is.na(Id2)) %>%
  transmute(zipcode = Id2,
            below25k = `Households; Estimate; Less than $10,000` +
              `Households; Estimate; $10,000 to $14,999` +
              `Households; Estimate; $15,000 to $24,999`,
            below25k_se = sqrt(`Households; Margin of Error; Less than $10,000`^2 +
                                 `Households; Margin of Error; $10,000 to $14,999`^2 +
                                 `Households; Margin of Error; $15,000 to $24,999`^2),
            above100k = `Households; Estimate; $100,000 to $149,999` +
              `Households; Estimate; $150,000 to $199,999` +
              `Households; Estimate; $200,000 or more`,
            above100k_se = sqrt(`Households; Margin of Error; $100,000 to $149,999`^2 +
                                  `Households; Margin of Error; $150,000 to $199,999`^2 +
                                  `Households; Margin of Error; $200,000 or more`^2))


cces07 <- read_dta("data/cces/cces2007/cces07_output.dta") %>% # haven says .sav has 'invalid byte sequence', so first converted to .dta with StatTransfer
  mutate(zipcode = inputzip) %>%
  left_join(acs_s1901, by = "zipcode") %>%
  mutate(union_influence2 = (cc06_v2071 == 1),
         union_influence3 = 4 - cc06_v2071)

# latest version of cces_06_common.dta lacks zip codes (noted in codebook as question for Polimetrix), but includes
# caseid as v1000.  cces_common_cumulative_4.dta has caseids and zips for pre- and post-election surveys,
# but no union_influence question (which was only asked in 2006 and 2007). union_influence was asked as part
# of pre-election Profile Survey in Aug 2006, so we want zip_pre from cces_common_cumulative.dta file.
# Postscript: It turns out that zip_post is all missing for 2006 in cces_common_cumulative.dta anyway.

cces_combo <- read_dta("data/cces/cces_cumulative_0612/cces_common_cumulative_4.dta") %>%
  filter(year == 2006) %>%
  transmute(v1000 = caseid,
            zipcode = as.numeric(zip_pre))

cces06 <- read_dta("data/cces/cces2006/cces_2006_common.dta") %>%
  left_join(cces_combo, by = "v1000") %>%
  left_join(acs_s1901, by = "zipcode") %>%
  transmute(zipcode = zipcode,
            state_abb = v1002,
            union_influence2 = as.numeric(v2071 == 1),
            union_influence3 = as.numeric(4 - v2071),
            age = as.numeric(2006 - v2020),
            male = as.numeric(2 - v2004),
            black = as.numeric(v2005 == 2),
            hispanic = as.numeric(v2005 == 3),
            asian = as.numeric(v2005 == 4),
            other = as.numeric(between (v2005, 5,8)),
            hs = as.numeric(v2018 == 2 ),
            somecollege = as.numeric(v2018 == 3),
            yr2college = as.numeric(v2018 == 4),
            yr4college = as.numeric(v2018 == 5),
            postgrad = as.numeric(v2018 == 6),
            edu = as.numeric(v2018),
            income = as.numeric(v2032),
            unemployed = as.numeric(between(v2030, 3,4)),
            parttime = as.numeric(v2030 == 2),
            presentunion = as.numeric(v2082 == 1),
            pastunion = as.numeric(v2082 == 2),
            south = as.numeric(v1006 == 3),
            partyid = as.numeric(v3005),
            con_ideology = as.numeric(v3007),
            religiosity = as.numeric(v2026+v2027+v2029),
            church_attend = if_else(v2026 == 5, NA_integer_, as.integer(5-v2026)),
            prayerfreq = as.numeric(v2027),
            godimport = as.numeric(v2029))

blackpct <- read_csv("data/ACS_11_5YR_%black/blackpct.csv") %>%
  transmute(zipcode = as.numeric(str_replace(`GEO.display-label`, "^.*(\\d{5})", "\\1")),
            blackpct_zip = as.numeric(HD01_VD03/HD01_VD01),
            totalpop = as.numeric(HD01_VD01))

#table scraped using http://apps.resourcegovernance.org/pdf-table-extractor/
#These data were obtained from the Bureau of Labor Statistics (BLS) at https://www.bls.gov/news.release/archives/union2_01252007.pdf
bls <- read_csv("data/scraped-data-union-state.csv") %>% 
  separate(col = V4, into = c("total_members", "percent_members", "total_rep", "percent_rep"), sep = "\\s+", convert = TRUE) %>% 
  transmute(state = str_replace_all(V1, "\\.*", "") %>% str_trim(),
            percent_members = as.numeric(percent_members)) %>% 
  filter(!is.na(state))

##The BLS does not provide estimates of union membership at the county or zip code level.

area_zip <- "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2013_Gazetteer/2013_Gaz_zcta_national.zip"
download.file(area_zip, "data/acs_zip_area.zip")

acs_zip_area <- read_tsv(unz("data/acs_zip_area.zip", "2013_Gaz_zcta_national.txt")) %>% 
  mutate(zipcode = as.numeric(stringr::str_replace(GEOID, "^.*(\\d{5})", "\\1")))

fips_cnty <- read_csv("https://raw.githubusercontent.com/raypereda/fips-county-codes/master/lib/national.txt",
                      col_types="ccccc")
names(fips_cnty) <- tolower(gsub(" ", "_", names(fips_cnty)))
fips_cnty$fips <- as.numeric(do.call(paste0, c(fips_cnty[, c(2,3)])))
fips_cnty$county <- tolower(gsub(" County| Parish", "", fips_cnty$county_name))
fips_cnty$county <- gsub(" ", "", fips_cnty$county)

bush04 <- read_tsv("http://bactra.org/election/vote-counts-with-NE-aggregated")
bush04$perc_bush04 <- with(bush04, Bush/(Bush+Kerry+Nader))
names(bush04) <- tolower(names(bush04))
bush04$county <- tolower(gsub(" County| Parish", "", bush04$county))
bush04$county <- gsub("saint", "st.", bush04$county)
bush04$county <- gsub(" ", "", bush04$county)
bush04$county[(bush04$state=="LA"|bush04$state=="MS") & bush04$county=="jeffdavis"] <- "jeffersondavis"
bush04$county[(bush04$state=="ME") & bush04$county=="linc"] <- "lincoln"
bush04$county[(bush04$state=="ME") & bush04$county=="andr"] <- "androscoggin"
bush04$county[(bush04$state=="ME") & bush04$county=="pen-s"] <- "penobscot"
bush04$county[(bush04$state=="ME") & bush04$county=="som-s"] <- "somerset"
bush04$county[(bush04$state=="ME") & bush04$county=="oxf-s"] <- "oxford"
bush04$county[(bush04$state=="MA") & bush04$county=="hamd"] <- "hamden"
bush04$county[(bush04$state=="MA") & bush04$county=="esse"] <- "essex"
bush04$county[(bush04$state=="MA") & bush04$county=="hams"] <- "hampshire"
bush04$county[(bush04$state=="NH") & bush04$county=="graf"] <- "grafton"
bush04$county[(bush04$state=="NY") & bush04$county=="manhattan"] <- "newyork"
bush04$county[(bush04$state=="NY") & bush04$county=="statenisland"] <- "richmond"
bush04$county[(bush04$state=="NY") & bush04$county=="brooklyn"] <- "kings"
bush04$county[(bush04$state=="VT") & bush04$county=="fran"] <- "franklin"
bush04$county[(bush04$state=="VT") & bush04$county=="wins"] <- "windsor"
bush04$county[(bush04$state=="VT") & bush04$county=="addi"] <- "addison"
bush04$county[(bush04$state=="VT") & bush04$county=="gris"] <- "grandisle"
bush04$county[(bush04$state=="VT") & bush04$county=="oran"] <- "orange"
bush04$county[(bush04$state=="VA") & bush04$county=="manassas"] <- "manassascity"
bush04$county[(bush04$state=="VA") & bush04$county=="norton"] <- "nortoncity"
bush04_cnty <- left_join(bush04, fips_cnty)

missing <- bush04_cnty[is.na(bush04_cnty$fips), 1:8] # election results still without fips due to county name inconsistencies
bush04_cnty <- bush04_cnty[!is.na(bush04_cnty$fips), ] # keep only results that already have fips
remaining <- anti_join(fips_cnty, bush04) %>% arrange(state) # fips without election results

missing$county0 <- missing$county # move county names to a tempvar
missing$county <- NA

states <- unique(missing$state)
states <- states[states != "AK"] # nothing to be done with Alaska election results--no breakdown in data
for(i in 1:length(states)) {
  t.rem <- remaining$county[remaining$state==states[i]] # fips without election results, one state at a time
  missing$county[missing$state==states[i]] <- lapply(missing$county0[missing$state==states[i]], function (ii) agrep(ii, t.rem, value=T, max.distance=.2)) # find matches to county name by state
}
missing$county <- unlist(lapply(missing$county, function(ii) ii[1])) # use closest match to county name
missing <- left_join(missing, fips_cnty) # now merge; some results still without fips in Maine, otherwise good
missing$county0 <- NULL # drop tempvar

bush04_cnty %<>% rbind(missing) %>% select(fips, perc_bush04)


zip_fips <- jsonlite::fromJSON("https://raw.githubusercontent.com/bgruber/zip2fips/master/zip2fips.json", flatten = TRUE) %>% 
  enframe() %>% 
  transmute(zipcode = as.numeric(name),
          fips = as.numeric(value))

median_income <- read_csv("data/median_household_income.csv") %>%
  transmute(zipcode = as.numeric(str_replace(`GEO.display-label`, "^.*(\\d{5})", "\\1")),
            median_income = as.numeric(HC02_EST_VC02)) %>%
  filter(!is.na(zipcode))

cces_merged <- cces06 %>% 
  left_join(acs_s1901, by = "zipcode") %>% 
  left_join(blackpct, by = "zipcode") %>% 
  left_join(tibble(postal = state.abb, state = state.name), by = c("state_abb" = "postal")) %>% 
  left_join(bls, by = "state") %>%
  left_join(acs_zip_area, by = "zipcode") %>% 
  left_join(zip_fips, by = "zipcode") %>% 
  left_join(bush04_cnty, by = "fips") %>%
  left_join(median_income, by = "zipcode") %>%
  mutate(pop_density=totalpop/ALAND_SQMI) %>% 
  select(zipcode, state_abb,
         union_influence2, union_influence3, 
         below25k, above100k, 
         median_income, unemployed, blackpct_zip, perc_bush04, pop_density, percent_members,
         edu, income, age, male, black, hispanic, asian, other, parttime, unemployed,
         presentunion, pastunion, partyid, con_ideology, church_attend, south) 

#rachel fix this
vars_list <- c("zipcode", "state_abb",
               "union_influence2", "union_influence3", 
               "below25k", "above100k", 
               "median_income", "unemployed", "blackpct_zip", "perc_bush04", "pop_density", "percent_members",
               "edu", "income", "age", "male", "black", "hispanic", "asian", "other", "parttime", "unemployed",
               "presentunion", "pastunion", "partyid", "con_ideology", "church_attend", "south")
vars_proper <- c("Zipcode", "State", 
                 "Union Influence 1", "Union Influence 2", 
                 "Below 25k", "Above 100k", 
                 "Median Income", "Unemployment Rate", "% Black", "% Republican Vote", "Population Density", "% Unionized Workers", 
                 "Education", "Income", "Age", "Male", "Black", "Hispanic", "Asian", "Other", "Employed Part-Time", "Unemployed",
                 "Current Union Member", "Past Union Member", "Party ID", "Ideology", "Religiosity", "South")

hhn_mi <- function(df, seed=324) {
  # multiply impute missing data
  mdf <- missing_data.frame(as.data.frame(df))
  mdf <- change(mdf, y = c("fips", "state_abb"), what = "type", to = "irrelevant")
  mdf <- change(mdf, y = c("income", "edu", "con_ideology"), what = "type", to = "ordered-categorical")
  mdf_mi <- mi(mdf, seed=seed) 
  
  # switch to mitools format (no support for glmer in mi::pool)
  mdf_mi_list <- complete(mdf_mi, m=10) 
  mdf_mi_list <- lapply(mdf_mi_list, function(df) 
    sapply(df, function(v) 
      if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>% data.frame) # get rid of %&*(&^ factors
  imputationList(mdf_mi_list)
}

# this right here, we'll have to come back to
format_mi_results <- function(m) {
  # format results of analysis of multiply imputed dataset
  m_fe <- MIextract(m, fun=fixef) # see https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
  m_vars <- MIextract(m, fun=vcov)
  m_vars2 <- list()
  m_vars2 <- lapply(m_vars, as.matrix)
  m_res <- MIcombine(m_fe, m_vars2)
  b <- m_res$coefficients
  se <- diag(m_res$variance^.5)
  df <- data.frame(term = names(b),
                   estimate = b,
                   std.error = se,
                   model = deparse(substitute(m)), 
                   stringsAsFactors = FALSE)
  df %>% filter(term!="(Intercept)")
}

cces_merged_mi <- hhn_mi(cces_merged)

save(cces_merged_mi, "~/data/cces_merged_mi.rda")
