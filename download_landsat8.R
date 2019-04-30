#get landsat8 data
pointsDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/sampling pts'
downloadfileDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/landsat8'
# Install the latest version from GitHub:
install.packages("devtools")
devtools::install_github("socialcopsdev/rLandsat")
# Load the library
library(rLandsat)
year <- '2015'
data_source <- landsat_search(min_date = paste0(year, '-01-01'), max_date = paste0(year, '-12-31'), path_master = 42, row_master = 34) #path 42 and row 34 covers soaproot watershed
dim(data_source)
colnames(data_source)
unique(data_source$product_id)
espa_creds('smdevine', 'Cienciasoil80')
test <- espa_products(data_source$product_id)
test$master #1's indicate availability

# placing an espa order
result_order <- espa_order(input_ids = data_source$product_id, product = c("sr_ndvi", "sr_evi"), projection = "lonlat", order_note = "Soaproot coverage 2014") #including source data "sr" and "toa" requires too much memory
order_id <- result_order$order_details$orderid

# getting order status
durl <- espa_status(order_id = order_id, getSize = FALSE) #session times out if getSize set to TRUE after files become available
downurl <- durl$order_details
downurl

# download; after the order is complete
landsat_download(download_url = downurl$product_dload_url, dest_file = file.path(downloadfileDir, year))
