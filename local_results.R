
# read rds files

#rds_files <- list.files("output/sessions", pattern = "\\.rds$", full.names = TRUE)

results <- file.info(list.files("output/results", pattern = "\\.rds$", full.names = TRUE))

latest <- rownames(results)[which.max(results$mtime)]

res <- as.list(readRDS(latest))

#res

rds_list <- list.files("output/results", pattern = "\\.rds$", full.names = TRUE)

rds_file <- rds_list[length(rds_list)]

res <- as.list(readRDS(rds_file))
