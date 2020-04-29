load(file="00 Data/archive1920.rdata")
load(file="00 Data/archive_seriea_18_19.rdata")
load(file="00 Data/archive_seriea_1516_1718.rdata")

df_raw <- rbind(archive,archive1819,archive1920)

save(df_raw,file="00 Data/full_raw_scraped.rdata")
