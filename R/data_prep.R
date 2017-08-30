library(tidyverse)
library(haven)

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
    mutate(union_influence2 = (v2071 == 1),
           union_influence3 = 4 - v2071)
    
    
    
