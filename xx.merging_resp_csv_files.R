setwd("./resp_extracted_rates")
file_names <- dir()
lat_resp_dat <- do.call(rbind,lapply(file_names, read.csv))
save(lat_resp_dat, file="../import_files/lat_resp_dat.Rda") 
write.table(lat_resp_dat, file="../import_files/lat_resp_dat.txt", sep=" ") 
write.csv(lat_resp_dat, file="../excel_files/lat_resp_dat.csv", row.names = FALSE) 

