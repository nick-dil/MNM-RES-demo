library(googledrive)
library(googlesheets4)

# inspect input data
poc_dir_link = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT"

poc_ls = drive_ls(poc_dir_link)
poc_ls

# create easy to use (interactively) poc-folder to id mapping in list
poc_id_list = as.list(poc_ls$id)
names(poc_id_list) = poc_ls$name

# Select poc version from list
my_poc = "poc_0.10.0"
my_poc_id = poc_id_list$poc_0.10.0

# Get matching CSV file from children
my_poc_ls = drive_ls(my_poc_id, recursive = T, pattern="spatial_samples", type="csv")
my_poc_ls

# assert nrow == 1
# .....

# Read file from GDrive ID
mycsv = read.table(
  text = drive_read_string(
    as_id(my_poc_ls)
    ),
  header = T,
  sep = ","
  )


