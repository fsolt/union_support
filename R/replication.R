library(haven)
library(jsonlite)
library(mi)
library(mitools)
library(lme4)
library(acs)
library(janitor)
library(interplot)
library(dotwhisker)
library(tidyverse)

acs_s1901 <- read_csv("data/ACS_11_5YR_S1901/ACS_11_5YR_S1901.csv",
                      skip = 1,
                      col_types = cols(.default = col_double(),
                                       `Id` = col_character(),
                                       `Geography` = col_character())) %>%
  select(Id, Id2, Geography, starts_with("Households")) %>%
  filter(!is.na(Id2)) %>%
  transmute(zipcode = as.numeric(Id2),
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



# latest version of cces_06_common.dta lacks zip codes (noted in codebook as question for Polimetrix), but includes
# caseid as v1000.  cces_common_cumulative_4.dta has caseids and zips for pre- and post-election surveys,
# but no union_influence question (which was only asked in 2006 and 2007). union_influence was asked as part
# of pre-election Profile Survey in Aug 2006, so we want zip_pre from cces_common_cumulative.dta file.
# Postscript: It turns out that zip_post is all missing for 2006 in cces_common_cumulative.dta anyway.

cces07 <- read_dta("data/cces/cces2007/cces07_output.dta", encoding = "latin1") %>% # haven says .sav has 'invalid byte sequence', so first converted to .dta with StatTransfer
    transmute(zipcode = as.numeric(inputzip),
              state_abb = as.character(cc06_v1002),
              union_influence2 = as.numeric(cc06_v2071 == 1),
              union_influence3 = as.numeric(4 - cc06_v2071),
              educ = as.numeric(cc06_v2018),
              income = as.numeric(cc06_v2032),
              age = as.numeric(2007 - cc06_v2020),
              male = as.numeric(2 - cc06_v2004),
              black = as.numeric(race == 2),
              hispanic = as.numeric(race == 3),
              asian = as.numeric(race == 4),
              other = as.numeric(between(as.numeric(race), 5, 8)),
              parttime = as.numeric(cc06_v2030 == 2),
              unemployed = as.numeric(between(as.numeric(cc06_v2030), 3, 4)),
              presentunion = as.numeric(cc06_v2082 == 1),
              pastunion = as.numeric(cc06_v2082 == 2),
              rep_partyid = as.numeric(cc06_v3005),
              con_ideology = as.numeric(ideo5),
              church_attend = if_else(churatd == 5, NA_integer_, as.integer(5-churatd)),
              south = as.numeric(cc06_v1006  == 3))


# African-American population pecentage by zip code from ACS
acs_black_pop <- acs::acs.fetch(endyear = 2011, 
                                geography = acs::geo.make(zip.code = "*"),
                                variable = c("C02003_004"),
                                key = "")

acs_total_pop <- acs::acs.fetch(endyear = 2011, 
                                geography = acs::geo.make(zip.code = "*"),
                                table.number = "B01003",
                                key = "")

blackpct <- acs_black_pop@geography %>% 
  transmute(zipcode = as.numeric(zipcodetabulationarea),
            pop_black = as.numeric(acs_black_pop@estimate),
            pop_total = as.numeric(acs_total_pop@estimate),
            blackpct_zip = if_else(pop_total > 0, pop_black/pop_total, 0))

# These data were obtained from the Bureau of Labor Statistics (BLS) at https://www.bls.gov/news.release/archives/union2_01252007.pdf
# Table scraped using http://apps.resourcegovernance.org/pdf-table-extractor/ and saved to data/
# Data are at state level; the BLS does not provide estimates of union membership at the county or zip code level.
bls <- read_csv("data/union2_01252007.csv", col_types = "ccccccc") %>% 
  separate(col = V4, into = c("total_members", "percent_members", "total_rep", "percent_rep"), sep = "\\s+", convert = TRUE) %>% 
  transmute(state_name = str_replace_all(V1, "\\.*", "") %>% str_trim(),
            union_st = as.numeric(percent_members)) %>% 
  filter(!is.na(state_name)) %>% 
  left_join(tibble(state_abb = state.abb, state_name = state.name), by = "state_name") %>% 
  mutate(state_abb = if_else(state_name=="District of Columbia", "DC", state_abb))

# Area of zip code (to calculate population density)
area_zip <- "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2013_Gazetteer/2013_Gaz_zcta_national.zip"
download.file(area_zip, "data/acs_zip_area.zip")

acs_zip_area <- read_tsv(unz("data/acs_zip_area.zip", "2013_Gaz_zcta_national.txt")) %>% 
  transmute(zipcode = as.numeric(stringr::str_replace(GEOID, "^.*(\\d{5})", "\\1")),
            area = ALAND_SQMI)


# Bush share of 2004 vote by county
fips_cnty <- read_csv("https://raw.githubusercontent.com/raypereda/fips-county-codes/master/lib/national.txt",
                      col_types="ccccc") %>% 
  janitor::clean_names() %>% 
  transmute(state = state,
            fips = as.numeric(paste0(state_ansi, county_ansi)),
            county = str_replace_all(county_name, "(County)|(Parish)| ", "") %>% tolower()) %>% 
  bind_rows(tribble(~state, ~fips, ~county,  # add back discontinued fips
                    "AK", 2280, "wrangellpetersburgcensusarea",
                    "AK", 2201, "princeofwalesouterketchikancensusarea"))

bush04 <- read_tsv("http://bactra.org/election/vote-counts-with-NE-aggregated",
                   col_types = "cciiiii") %>% 
  mutate(County = if_else(State == "ME",    # fix Maine counties that are disaggregated
                          str_extract(County, "^[^-]*"),
                          County)) %>% 
  group_by(State, County) %>% 
  summarise(bush = sum(Bush),
            kerry = sum(Kerry),
            nader = sum(Nader)) %>% 
  ungroup() %>% 
  transmute(bush04_county = bush/(bush+kerry+nader),
            state = State,
            county = str_replace(County %>% tolower(), " county| parish", "") %>% 
              str_replace("saint", "st.") %>% 
              str_replace_all(" ", ""))

bush04_cnty <- left_join(bush04, fips_cnty, by = c("state", "county"))

missing <- bush04_cnty %>%  # election results still without fips due to county name inconsistencies
  filter(is.na(fips)) %>% 
  rename(orig_county = county) %>% 
  left_join(read_csv("data/county_crosswalk.csv", col_types = "ccc"), by = c("state", "orig_county")) %>% 
  bind_rows(anti_join(fips_cnty, bush04, by = c("state", "county")) %>% # use state-level results for Alaska
              filter(state=="AK") %>%
              left_join(bush04 %>% select(bush04_county, state), by = "state")) %>% 
  filter(!is.na(county)) %>% 
  select(bush04_county, state, county) %>% 
  left_join(fips_cnty, by = c("state", "county"))

bush04_cnty <- bush04_cnty %>% 
  bind_rows(missing) %>%
  select(fips, state, county, bush04_county) %>% 
  filter(!is.na(fips))

# zip to county fips crosswalk from Dept of Housing and Urban Development
# <https://www.huduser.gov/portal/datasets/usps_crosswalk.html>
zip_fips <- readxl::read_excel("data/ZIP_COUNTY_032010.xlsx") %>% 
  group_by(ZIP) %>% 
  filter(TOT_RATIO == max(TOT_RATIO)) %>%
  ungroup() %>% 
  transmute(zipcode = as.numeric(ZIP),
            fips = as.numeric(COUNTY)) %>% 
    bind_rows(tribble(
        ~zipcode, ~fips,
        5486, 50013,
        6520, 9009,
        27619, 37183,
        28680, 37023,
        29151, 45085,
        33970, 12071,
        36616, 1097,
        51502, 19155,
        54602, 55063,
        70064, 22051,
        72630, 5009,
        77342, 48471,
        81157, 8007,
        82602, 56025,
        85038, 4013,
        85218, 4021,
        85220, 4021,
        85222, 4021,
        85242, 4013,
        86339, 4005,
        88311, 35035,
        89803, 32007,
        92030, 6073,
        92190, 6073,
        92275, 6025,
        93921, 6053,
        95853, 6067,
        96788, 15009,
        98111, 53033,
        99687, 2170
    ))

# Median income and unemployment rate by zip code from ACS (no API, so downloaded from American FactFinder)
median_income <- read_csv("data/ACS_11_5YR_S1903.csv", 
                          col_types = "ccciiii") %>%
  transmute(zipcode = as.numeric(GEO.id2),
            median_income_zip = HC02_EST_VC02)

unemployment_rate <- read_csv("data/ACS_11_5YR_S2301.csv", 
                              col_types = "cccdd") %>%
  transmute(zipcode = as.numeric(GEO.id2),
            unemployment_rate_zip = HC04_EST_VC01) 

# Merge contextual variables
cces07_merged <- cces07 %>% 
  filter(!is.na(zipcode)) %>% 
  left_join(acs_s1901, by = "zipcode") %>% 
  left_join(blackpct, by = "zipcode") %>% 
  left_join(bls, by = "state_abb") %>%
  left_join(acs_zip_area, by = "zipcode") %>% 
  left_join(zip_fips, by = "zipcode") %>% 
  left_join(bush04_cnty, by = "fips") %>%
  left_join(median_income, by = "zipcode") %>%
  left_join(unemployment_rate, by = "zipcode") %>%
  mutate(pop_density_zip = pop_total/area,
         state_alph = as.numeric(as.factor(state_abb))) %>% # by postcode, not name
  select(zipcode, state_alph,
         union_influence2, union_influence3, 
         below25k, above100k, 
         median_income_zip, unemployment_rate_zip, blackpct_zip, pop_density_zip,
         fips, bush04_county,
         union_st,
         educ, income, age, male, black, hispanic, asian, other, parttime, unemployed,
         presentunion, pastunion, rep_partyid, con_ideology, church_attend, south) %>% 
  filter(!is.na(union_influence2) & !is.na(blackpct_zip) & !is.na(bush04_county)) # exclude individuals with no DV response

vars_list <- c("zipcode", "state_abb",
               "union_influence2", "union_influence3", 
               "below25k", "above100k", 
               "median_income", "unemployed", "blackpct_zip", "perc_bush04", "pop_density", "percent_members",
               "edu", "income", "age", "male", "black", "hispanic", "asian", "other", "parttime", "unemployed",
               "presentunion", "pastunion", "partyid", "con_ideology", "church_attend", "south")
vars_proper <- c("Zipcode", "State", 
                 "Union Influence", "Union Influence", 
                 "Below 25k", "Above 100k", 
                 "Median Income", "Unemployment Rate", "% Black", "% Republican Vote", "Population Density", "% Unionized Workers", 
                 "Education", "Income", "Age", "Male", "Black", "Hispanic", "Asian", "Other", "Employed Part-Time", "Unemployed",
                 "Current Union Member", "Past Union Member", "Party ID", "Ideology", "Religiosity", "South")

multiply_impute <- function(df, seed=324) {
  # multiply impute missing data
  mdf <- missing_data.frame(as.data.frame(df))
  mdf <- change(mdf, y = c("zipcode", "fips", "state_alph"), what = "type", to = "irrelevant")
  mdf <- change(mdf, y = c("income", "edu", "con_ideology"), what = "type", to = "ordered-categorical")
  mdf_mi <- mi(mdf, seed=seed) 
  
  # switch to mitools format (no support for glmer in mi::pool)
  mdf_mi_list <- complete(mdf_mi, m=10) 
  mdf_mi_list <- lapply(mdf_mi_list, function(df) 
    sapply(df, function(v) 
      if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>% data.frame) # get rid of %&*(&^ factors
  imputationList(mdf_mi_list)
}

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

cces07_merged_mi <- multiply_impute(cces07_merged)

save(cces07_merged_mi, file = "data/cces07_merged_mi.rda")

load("data/cces07_merged_mi.rda")


# estimates the model on each of those 10 datasets and combine the results 
# and so take into account our uncertainty in the missing values of our data

m07 <- with(cces07_merged_mi,
           glmer(union_influence2~below25k*above100k+
                   median_income_zip+unemployment_rate_zip+blackpct_zip+pop_density_zip+bush04_county+union_st+
                   educ+income+age+male+black+hispanic+asian+other+parttime+unemployed+presentunion+pastunion+
                   rep_partyid+con_ideology+church_attend+south+
                   (1|fips), family=binomial(link="logit")))

m07_res <- format_mi_results(m1_all)

save(m07, m07_res, file = "data/results07.rda")

m07_inter_below25k <- interplot(m07, "below25k", "above100k") +
    labs(x = "Above $100K", 
         y = "Coefficient of Below $25K") +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
          axis.title.y = element_text(size=8)) +
    geom_hline(yintercept = 0, colour = "grey80", linetype = "dashed") +
    scale_colour_grey(start = .5)

ggsave(filename="paper/figures/m07_inter_below25k.pdf", plot = m07_inter_below25k, width = 7, height = 7)

m07_inter_above100k <- interplot(m07, "above100k", "below25k") +
    labs(x = "Below $25K", 
         y = "Coefficient of Above $100K") +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
          axis.title.y = element_text(size=8)) +
    geom_hline(yintercept = 0, colour = "grey80", linetype = "dashed") +
    scale_colour_grey(start = .5)

ggsave(filename="paper/figures/m07_inter_above100k.pdf", plot = m07_inter_above100k, width = 7, height = 7)

p <- #t2_res %>% by_2sd(hhn06x) %>%
    #rbind(t2_all_res %>% by_2sd(hhn)) %>% 
    m07_res %>% by_2sd(cces07_merged_mi[[1]]) %>% 
    dwplot() +
    # relabel_y_axis(vars_proper) +
    theme_bw() + xlab("Coefficient Estimate") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(legend.justification=c(0, 1), legend.position=c(0, 1),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5,
          legend.text = element_text(size = 9),
          legend.key.height = unit(12, "pt")) # +
# scale_colour_grey(start = .7, end = .5,
#                   name = "Data",
#                   breaks = c("t2", "t2_all"),
#                   labels = c("Pew 2006", "Pew 2005-2009"))
# g <- p %>% add_brackets(level_brackets)
# dir.create("figures", showWarnings = FALSE)
# ggsave("figures/t2_pooled.pdf", plot = g, width = 6, height = 5) 
