############################################################
#' A Extract pricing information from paticular Function
#'
#' This function allows you to extract pricing information from select main sheets
#' @param Defaults to TRUE.
#' @keywords Main 
#' @export
#' @examples
#' XXX()
###########################################################


message("--------------------------*Pull Mainsheet*")


message("Extracting Pricing Information from selected brand")

#Extract the list of brands that need to have exchange rate updated and then feed to for loop below
MainFileName =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/KeystoneExchangeRateUpdate.csv", header = TRUE)
Brand_Folder_List = subset(MainFileName, Decision_Update == 1, select= (Brand_Folder_Name))

#Create black pooledmaindata sheet
PooledMainData = data.frame()

#Run For loop for selected main sheets
pb <- txtProgressBar(min = 1, max = nrow(Brand_Folder_List), style = 3)

for (i in 1:nrow(Brand_Folder_List)){
	
	print(Brand_Folder_List[i,])
	BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/TDot_Brands/",as.character(Brand_Folder_List[i,]), sep = "", collapse = NULL)
	
	print(BrandFolderLocation)
	setwd(BrandFolderLocation)
	
	print(list.files(BrandFolderLocation))
	print(fileNames <- Sys.glob("main--*.csv")) #All files with Main--.csv format

	#Identify the Main--sheet and pull it
	x <- Sys.glob("main--*.csv")
	PulledMain=read.csv(x , header = TRUE)

	MainSubset <- subset(PulledMain, delete=="N" & type=="simple", 
                           select=c(internal_sku, sku, brand_filter, usa_price, usa_retail_price, usa_cost, usa_jobber_price, 
                                    na_usa_shipping, na_exchange_rate, ca_price, ca_retail_price, ca_cost, ca_jobber_price, na_ca_shipping))

MainSubset$usa_price <- as.numeric(as.character(MainSubset$usa_price  ))
MainSubset$usa_retail_price <-  as.numeric(as.character(MainSubset$usa_retail_price ))
MainSubset$usa_cost <-  as.numeric(as.character(MainSubset$usa_cost ))
MainSubset$usa_jobber_price <- as.numeric(as.character(MainSubset$usa_jobber_price))
MainSubset$na_usa_shipping <-  as.numeric(as.character(MainSubset$na_usa_shipping ))
MainSubset$ca_price <-  as.numeric(as.character(MainSubset$ca_price ))
MainSubset$ca_retail_price <-  as.numeric(as.character( MainSubset$ca_retail_price))
MainSubset$ca_cost <-  as.numeric(as.character(MainSubset$ca_cost ))
MainSubset$ca_jobber_price <-  as.numeric(as.character( MainSubset$ca_jobber_price))
MainSubset$na_ca_shipping <-  as.numeric(as.character( MainSubset$na_ca_shipping))

	#Add Brand name and # of attribute sets to BrandAttributeSet df
	PooledMainData = rbind(PooledMainData ,MainSubset )

	Sys.sleep(0.1)
	setTxtProgressBar(pb, i)
}





###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons




