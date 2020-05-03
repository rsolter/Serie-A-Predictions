load(file="00 Data/archive1920.rdata")
load(file="00 Data/archive_seriea_18_19.rdata")
load(file="00 Data/archive_seriea_1516_1718.rdata")

df_raw <- rbind(archive,archive1819,archive1920)

save(df_raw,file="00 Data/full_raw_scraped.rdata")

df_raw <- df_raw %>% 
  mutate(attacks_h=(attacks_middle_h+attacks_left_h+attacks_right_h),
         attacks_a=(attacks_middle_a+attacks_left_a+attacks_right_a)) %>% 
  select(-c(attacks_middle_h,attacks_middle_a,attacks_left_a,attacks_left_h,
            attacks_right_a,attacks_right_h)) %>% 
  arrange(match_date) %>%
  mutate(match_id=row_number())


##-----------------------------------------------------------------------------##

load(file="00 Data/italy_elos.rdata")

elo_names <- italy_elos$Club %>% unique()
raw_names <- df_raw$Team_h %>% unique()


# 4 raw_names not found in elo_names
raw_names[!raw_names%in%elo_names]

#italy_elos$Club <- ifelse(italy_elos$Club=="Verona","HeallasVerona",
#                          ifelse(italy_elos$Club=="Verona","HeallasVerona",
#                          ifelse(italy_elos$Club=="Verona","HeallasVerona",
#                          ifelse(italy_elos$Club=="Verona","HeallasVerona",italy_elos$Club))

# joining elo data with df_raw



