#This RScript will help me to save files from the permanent disk to the Workspace Bucket

#First, obtain the name of my workspace bucket and label it as my_bucket
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")

if (my_bucket == "") {
  my_bucket <- "gs://fc-secure-240d2216-3632-4221-a2b4-a2dfd089bcaa"
}

#Then, copy the desired file (such as RDS file) into the workspace bucket using the following template
#Be sure to change "local_file.txt" into the actual name of the file I would like to be saved 
system("gsutil cp local_file.txt gs://fc-secure-240d2216-3632-4221-a2b4-a2dfd089bcaa/")

#To facilitate this, lets run a function that lists all RDS files 
list_rds_files <- function(path = "~/", full_names = TRUE) {
  rds_files <- list.files(
    path = path,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = full_names,
    ignore.case = TRUE
  )
  
  if (length(rds_files) == 0) {
    message("No .rds files found.")
  } else {
    print(rds_files)
  }
  
  return(invisible(rds_files))
}

#Now can print this: 
list_rds_files()

#Now let's copy these RDS files to the Workspace Bucket
rds_files <- list.files(
  path = "~/",
  pattern = "\\.rds$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

my_bucket <- "gs://fc-secure-240d2216-3632-4221-a2b4-a2dfd089bcaa"
destination <- paste0(my_bucket, "/rds_backup")

for (file in rds_files) {
  
  file_name <- basename(file)
  bucket_file <- paste0(destination, "/", file_name)
  
  # Skip if already uploaded
  check_cmd <- paste("gsutil -q stat", shQuote(bucket_file))
  if (system(check_cmd) == 0) {
    cat("Skipping:", file_name, "\n")
    next
  }
  
  cat("Uploading:", file_name, "\n")
  system(paste("gsutil cp", shQuote(file), shQuote(bucket_file)))
}