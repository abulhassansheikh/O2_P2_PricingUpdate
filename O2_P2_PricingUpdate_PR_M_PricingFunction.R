############################################################
#' A Keystone Exchange Rate Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################

KeystoneExchangeRateUpdate <- function(Final1_Test0, ER){


###########################################################

message("--------------------------*Pull Mainsheet*")


message("Extracting Pricing Information from selected brand")

#Extract the list of brands that need to have exchange rate updated and then feed to for loop below
MainFileName =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/KeystoneExchangeRateUpdate.csv", header = TRUE)
Selected_Brands = subset(MainFileName, Run_Update == 1, select= (Brand_Folder_Name))

#Create black pooledmaindata sheet
PooledMainData = data.frame()

for (i in 1:nrow(Selected_Brands)){

	
	BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(Selected_Brands[i,]), sep = "", collapse = NULL)
	
	setwd(BrandFolderLocation)

	#Identify the Main--sheet and pull it
	x <- Sys.glob("main--*.csv")
	PulledMain=read.csv(x , header = TRUE)

	MainSubset <- subset(PulledMain, delete=="N" & type=="simple", 
                           select=c(internal_sku, sku, attribute_set , usa_price, usa_retail_price, usa_cost, usa_jobber_price, 
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
	PooledMainData = rbind(PooledMainData , MainSubset )

}

###########################################################
message("--------------------------*Checking for Mainsheet Errors*")

Col_Names = array(names(PooledMainData))

for(c in 1:ncol(PooledMainData)){

	for(r in 1:nrow(PooledMainData)){

	if(is.na(PooledMainData[r, c])){
		message("**ERROR**:: Blank Cell in column **", Col_Names[c], "** in mainsheet" )
		
		}


	}

}


###########################################################
message("--------------------------*Processing Inventory File Data*")


message("Subsetting Inventory File to use selected brands")
#Identify which vendor names to subset out and their related prefix data
IF_reference =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/InventoryFileData.csv", header = TRUE)

Pull_IF_reference_Data = merge(MainFileName, IF_reference, by=c("Brand_Folder_Name"), all=TRUE)
VenderNameData = subset(Pull_IF_reference_Data, Run_Update == 1)

SubsetIF = data.frame()

for(i in 1:nrow(VenderNameData)){

	BrandIFData = subset(Pooled_IF, vendor_name == as.character(VenderNameData$vendor_name[i]))	
	BrandIFData$internal_sku = paste(VenderNameData$Internal_SKU_Prefix[i],as.character(substring(BrandIFData$mpn, 2)), sep = "-", collapse = NULL)

	SubsetIF = rbind(SubsetIF , BrandIFData)

}


###########################################################

message("--------------------------*Merging Inventory File & Main Sheet Data*")

MergedPricingData_IF = merge(PooledMainData , SubsetIF , by=c("internal_sku"), all=TRUE)

CND_MAP =data.frame(read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9a_PRCING_MAP_File/Formated_MAP_CND.csv", header = TRUE))
USA_MAP =data.frame(read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9a_PRCING_MAP_File/Formated_MAP_USA.csv", header = TRUE))

MergedPricingData_CAN = merge(MergedPricingData_IF, CND_MAP, by=c("internal_sku"), all=TRUE)
MergedPricingData_USA = merge(MergedPricingData_CAN, USA_MAP, by=c("internal_sku"), all=TRUE)


MergedPricingData = subset(MergedPricingData_USA, sku != "", select= c(vendor_name, vendor_code, internal_sku, sku, attribute_set, 
                                                                       USA_fedexable, USA_SpecialOrder, usa_price, usa_retail_price, usa_cost, 
                                                                       USA_cost, usa_jobber_price, USA_price, na_usa_shipping, na_exchange_rate, 
                                                                       CND_fedexable, CND_SpecialOrder, ca_price, ca_retail_price, ca_cost, CND_cost, 
                                                                       ca_jobber_price, CND_price, na_ca_shipping, ca_MAP_price, usa_MAP_price))

MergedPricingData$ca_MAP_price <-  as.numeric(as.character(MergedPricingData$ca_MAP_price))
MergedPricingData$usa_MAP_price <-  as.numeric(as.character(MergedPricingData$usa_MAP_price))

MergedPricingData  = data.frame(MergedPricingData)

MergedPricingData$Current_Exchange_Rate <- 0
MergedPricingData$REF_Discount <- 0
MergedPricingData$CURR_Discount <- 0
MergedPricingData$USED_Discount <- 0
MergedPricingData$NEW_ca_cost <- 0

MergedPricingData$CURR_Transaction_Fee <- 0
MergedPricingData$CURR_Actual_Cost <- 0
MergedPricingData$CURR_CND_Margin <- 0
MergedPricingData$USED_CND_Margin <- 0
MergedPricingData$NEW_ca_price <- 0
MergedPricingData$NEW_ca_retail <- 0
MergedPricingData$DECISION <- "NONE"

MergedPricingData$CURR_Discount_usa <- 0
MergedPricingData$USED_Discount_usa <- 0
MergedPricingData$NEW_usa_cost <- 0

MergedPricingData$CURR_Transaction_Fee_usa <- 0
MergedPricingData$CURR_Actual_Cost_usa <- 0
MergedPricingData$CURR_USA_Margin <- 0
MergedPricingData$USED_USA_Margin <- 0
MergedPricingData$NEW_usa_price <- 0
MergedPricingData$NEW_usa_retail <- 0
MergedPricingData$DECISION_usa <- "NONE"


###########################################################
message("--------------------------*Conducting Exchange Rate Calculations*")


for(i in 1:nrow(MergedPricingData)){
	
	#Extract necessary data depending on the attribute set of the sku
	Discount_Rate = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = Discount_Rate))
	MergedPricingData$REF_Discount[i] <- Discount_Rate
	
	#Determine if the inventory files are present
	CND_IF_Present = is.na(as.character(MergedPricingData$CND_price[i]))
	USA_IF_Present = is.na(as.character(MergedPricingData$USA_price[i]))

	#Calculate the current Exchange Rate
	if(CND_IF_Present == FALSE & USA_IF_Present == FALSE ){ #Changed this condition from OR to AND
		MergedPricingData$Current_Exchange_Rate[i] <- as.numeric(as.character(MergedPricingData$CND_price[i]))/as.numeric(as.character(MergedPricingData$USA_price[i]))
	} else { MergedPricingData$Current_Exchange_Rate[i] <- "Missing"}


	#Calculate the current discont rate CANADA
	if(CND_IF_Present == FALSE){ #Removed the OR condition that the USA_IF_Present is FALSE
		CURR_Discount <- (1-(as.numeric(MergedPricingData$CND_cost[i])/as.numeric(as.character(MergedPricingData$CND_price[i]))))*100
		MergedPricingData$CURR_Discount[i] <- CURR_Discount
			
			if(CURR_Discount <= Discount_Rate){
					MergedPricingData$USED_Discount[i] <- CURR_Discount
					MergedPricingData$NEW_ca_cost[i] <- MergedPricingData$CND_cost[i]#Should be cost
			} else{ 
					MergedPricingData$USED_Discount[i] <- Discount_Rate
					MergedPricingData$NEW_ca_cost[i] <- (1-(Discount_Rate/100))*as.numeric(as.character(MergedPricingData$CND_price[i]))
				}

	} else { 
		MergedPricingData$CURR_Discount[i] <- "Missing" 
		MergedPricingData$USED_Discount[i] <- Discount_Rate
		MergedPricingData$NEW_ca_cost[i] <- as.numeric(as.character(MergedPricingData$usa_jobber_price[i])) * ER * (1-(Discount_Rate/100))
		}


	#Calculate the current discont rate USA
	if(USA_IF_Present == FALSE ){
		CURR_Discount_usa <- (1-(as.numeric(MergedPricingData$USA_cost[i])/as.numeric(as.character(MergedPricingData$USA_price[i]))))*100
		MergedPricingData$CURR_Discount_usa[i] <- CURR_Discount_usa
			
			if(CURR_Discount_usa <= Discount_Rate){
					MergedPricingData$USED_Discount_usa[i] <- CURR_Discount_usa
					MergedPricingData$NEW_usa_cost[i] <- MergedPricingData$USA_cost[i]#Should be cost
			} else{ 
					MergedPricingData$USED_Discount_usa[i] <- Discount_Rate
					MergedPricingData$NEW_usa_cost[i] <- (1-(Discount_Rate/100))*as.numeric(as.character(MergedPricingData$USA_price[i]))
				}

	} else { 
		MergedPricingData$CURR_Discount_usa[i] <- "Missing" 
		MergedPricingData$USED_Discount_usa[i] <- Discount_Rate
		MergedPricingData$NEW_usa_cost[i] <- as.numeric(as.character(MergedPricingData$usa_jobber_price[i])) * (1-(Discount_Rate/100))
		}
}


###########################################################
message("--------------------------*Calculating ca_price*")

#Extract necessary data depending on the attribute set of the sku
CAD_Min_Margin = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = CAD_Min_Margin))
CAD_Max_Margin = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = CAD_Max_Margin))
USA_Min_Margin = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = US_Min_Margin))
USA_Max_Margin = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = US_Max_Margin))


for(i in 1:nrow(MergedPricingData)){

#CANADA CALCULATION
	if(is.na(MergedPricingData$ca_MAP_price[i]) == TRUE){

		MergedPricingData$CURR_Transaction_Fee[i] = MergedPricingData$ca_price[i]*0.03

		MergedPricingData$CURR_Actual_Cost[i] = MergedPricingData$NEW_ca_cost[i] + MergedPricingData$na_ca_shipping[i] + MergedPricingData$CURR_Transaction_Fee[i]

		CURR_CND_Margin = (((MergedPricingData$ca_price[i] - MergedPricingData$CURR_Actual_Cost[i])/ MergedPricingData$ca_price[i])*100)
		MergedPricingData$CURR_CND_Margin[i] <- CURR_CND_Margin

		if(CURR_CND_Margin < CAD_Min_Margin){ 
			MergedPricingData$USED_CND_Margin[i] = CAD_Min_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Min_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Min_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "Marked UP & Non-MAP"
		} else if(CURR_CND_Margin > CAD_Max_Margin){
			MergedPricingData$USED_CND_Margin[i] = CAD_Max_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Max_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Max_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "Marked DOWN & Non-MAP"
		} else{
			MergedPricingData$USED_CND_Margin[i] = CURR_CND_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CURR_CND_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CURR_CND_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "UNchanged & Non-MAP"
		}
	
	} else if(is.na(MergedPricingData$ca_MAP_price[i]) == FALSE){

		MergedPricingData$CURR_Transaction_Fee[i] = MergedPricingData$ca_MAP_price[i]*0.03

		MergedPricingData$CURR_Actual_Cost[i] = MergedPricingData$NEW_ca_cost[i] + MergedPricingData$na_ca_shipping[i] + MergedPricingData$CURR_Transaction_Fee[i]

		CURR_CND_Margin = (((MergedPricingData$ca_MAP_price[i] - MergedPricingData$CURR_Actual_Cost[i])/ MergedPricingData$ca_MAP_price[i])*100)
		MergedPricingData$CURR_CND_Margin[i] <- CURR_CND_Margin

		if(CURR_CND_Margin < CAD_Min_Margin){ 
			MergedPricingData$USED_CND_Margin[i] = CAD_Min_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Min_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Min_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "Marked UP & MAP"
		} else if(CURR_CND_Margin > CAD_Max_Margin){
			MergedPricingData$USED_CND_Margin[i] = CAD_Max_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Max_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CAD_Max_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "Marked DOWN & MAP"
		} else{
			MergedPricingData$USED_CND_Margin[i] = CURR_CND_Margin
			MergedPricingData$NEW_ca_price[i] = format(round((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CURR_CND_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_ca_retail[i] = format(round(((- MergedPricingData$NEW_ca_cost[i] - MergedPricingData$na_ca_shipping[i])/ ( (CURR_CND_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION[i] <- "UNchanged & MAP"
		}


	}

# USA CALCULATION
	if(is.na(MergedPricingData$usa_MAP_price[i]) == TRUE){

		MergedPricingData$CURR_Transaction_Fee_usa[i] = MergedPricingData$usa_price[i]*0.03

		MergedPricingData$CURR_Actual_Cost_usa[i] = MergedPricingData$NEW_usa_cost[i] + MergedPricingData$na_usa_shipping[i] + MergedPricingData$CURR_Transaction_Fee_usa[i]

		CURR_USA_Margin = (((MergedPricingData$usa_price[i] - MergedPricingData$CURR_Actual_Cost_usa[i])/ MergedPricingData$usa_price[i])*100)
		MergedPricingData$CURR_USA_Margin[i] <- CURR_USA_Margin


		if(CURR_USA_Margin < USA_Min_Margin){ 
			MergedPricingData$USED_USA_Margin[i] = USA_Min_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Min_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Min_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "Marked UP & Non-MAP"
		} else if(CURR_USA_Margin > USA_Max_Margin){
			MergedPricingData$USED_USA_Margin[i] = USA_Max_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Max_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Max_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "Marked DOWN & Non-MAP"
		} else{
			MergedPricingData$USED_USA_Margin[i] = CURR_USA_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (CURR_USA_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (CURR_USA_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "UNchanged & Non-MAP"
		}
	
	} else if(is.na(MergedPricingData$usa_MAP_price[i]) == FALSE){

		MergedPricingData$CURR_Transaction_Fee_usa[i] = MergedPricingData$usa_MAP_price[i]*0.03

		MergedPricingData$CURR_Actual_Cost_usa[i] = MergedPricingData$NEW_usa_cost[i] + MergedPricingData$na_usa_shipping[i] + MergedPricingData$CURR_Transaction_Fee_usa[i]

		CURR_USA_Margin = (((MergedPricingData$usa_MAP_price[i] - MergedPricingData$CURR_Actual_Cost_usa[i])/ MergedPricingData$usa_MAP_price[i])*100)
		MergedPricingData$CURR_USA_Margin[i] <- CURR_USA_Margin


		if(CURR_USA_Margin < USA_Min_Margin){ 
			MergedPricingData$USED_USA_Margin[i] = USA_Min_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Min_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Min_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "Marked UP & MAP"
		} else if(CURR_USA_Margin > USA_Max_Margin){
			MergedPricingData$USED_USA_Margin[i] = USA_Max_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Max_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (USA_Max_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "Marked DOWN & MAP"
		} else{
			MergedPricingData$USED_USA_Margin[i] = CURR_USA_Margin
			MergedPricingData$NEW_usa_price[i] = format(round((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (CURR_USA_Margin/100) - 0.97 ), 2), nsmall = 2)
			MergedPricingData$NEW_usa_retail[i] = format(round(((- MergedPricingData$NEW_usa_cost[i] - MergedPricingData$na_usa_shipping[i])/ ( (CURR_USA_Margin/100) - 0.97 ) * 1.2), 2), nsmall = 2)
			MergedPricingData$DECISION_usa[i] <- "UNchanged & MAP"
		}


	}


}


###########################################################

message("--------------------------*File Devision*")

Canada_ER = subset(MergedPricingData, select= c(vendor_code, internal_sku, sku, USA_fedexable,
                                                                       USA_SpecialOrder, usa_MAP_price, usa_price, usa_retail_price,    
                                                                       usa_cost, USA_cost, usa_jobber_price, USA_price, na_usa_shipping,    
                                                                       na_exchange_rate, Current_Exchange_Rate, CND_fedexable, CND_SpecialOrder,    
                                                                       ca_MAP_price, ca_price, ca_retail_price, ca_cost, CND_cost, NEW_ca_cost,    
                                                                       ca_jobber_price, CND_price, na_ca_shipping, CURR_Transaction_Fee,    
                                                                       CURR_Actual_Cost, CURR_CND_Margin, REF_Discount, CURR_Discount, USED_Discount))


USA_ER= subset(MergedPricingData, select= c(vendor_name, vendor_code, internal_sku, sku, USA_fedexable, 
                                                                       USA_SpecialOrder, usa_MAP_price, usa_price, usa_retail_price,    
                                                                       usa_cost, USA_cost, NEW_usa_cost, usa_jobber_price, USA_price,    
                                                                       na_usa_shipping, CURR_Transaction_Fee_usa, CURR_Actual_Cost_usa,    
                                                                       CURR_USA_Margin, REF_Discount, CURR_Discount_usa, USED_Discount_usa))

Auto_CAN_ER= subset(MergedPricingData, select= c(internal_sku, CURR_CND_Margin, USED_CND_Margin, NEW_ca_price, NEW_ca_retail, DECISION))


Auto_USA_ER= subset(MergedPricingData, select= c(internal_sku, CURR_USA_Margin, USED_USA_Margin, NEW_usa_price, NEW_usa_retail, DECISION_usa))


message("--------------------------*Create Worksheet*")

#Load openxlsx package to save excel file with tabs
library("openxlsx")

##Create the workbook for excel and add the following tabs: Canada_ER, USA_ER, Auto_CAN_ER, Auto_USA_ER
CompiledSheet <- createWorkbook()

addWorksheet(CompiledSheet , "Canada_ER")
writeData(CompiledSheet , sheet = "Canada_ER", x = Canada_ER)

addWorksheet(CompiledSheet , "USA_ER")
writeData(CompiledSheet , sheet = "USA_ER", x = USA_ER)

addWorksheet(CompiledSheet , "Auto_CAN_ER")
writeData(CompiledSheet , sheet = "Auto_CAN_ER", x = Auto_CAN_ER)

addWorksheet(CompiledSheet , "Auto_USA_ER")
writeData(CompiledSheet , sheet = "Auto_USA_ER", x = Auto_USA_ER)



message("--------------------------*Starting Exchange Rate Update*")

#Record Time of creation
##Create Folder Name with date and Time
ERfileName_canada = paste("KERU",gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)
FileName_canada = paste(ERfileName_canada, "csv", sep = ".", collapse = NULL)

#ERfileName_USA = paste("USA_KERU",gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)
#FileName_USA = paste(ERfileName_USA, "csv", sep = ".", collapse = NULL)

message("Files will be named: ", FileName_canada)
#message("Files will be named: ", FileName_USA )

message("--------------------------*Folder_File_Creation*")

#Check if FileOutput is 1
if(Final1_Test0== 1){

	BackUpFolder_CAN = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_canada, sep = "/", collapse = NULL)
	#BackUpFolder_USA = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = BackUpFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = BackUpFolder_USA, na="")

	saveWorkbook(CompiledSheet, BackUpFolder_CAN, overwrite = FALSE)

	OutputFolder_CAN = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_canada, sep = "/", collapse = NULL)
	#OutputFolder_USA = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = OutputFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = OutputFolder_USA, na="")

	saveWorkbook(CompiledSheet, OutputFolder_CAN, overwrite = FALSE)


} else{

	TEMPFolder_CAN = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_canada, sep = "/", collapse = NULL)
	#TEMPFolder_USA = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = TEMPFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = TEMPFolder_USA, na="")

	saveWorkbook(CompiledSheet, TEMPFolder_CAN, overwrite = FALSE)

} 

message("File Outputted")





message("--------------------------*Automated Price Update Complete*")
message("")
message("***If you have any issues with the output***")
message("   ***Please Contact Abul Hassan Sheikh***  ")
message("")
message("Version: 1.0")
message("Last Updated: October 1st 2018")
message("Author: Abul Hassan Sheikh")


}


###########################################################
###########################################################

###########################################################
###########################################################
Extract_US_CND_InventoryFiles <- function(){

message("--------------------------*Process Inventory File*")

message("Loading necessary files, this may take some time")
#Loading seperate Canadian & US invetory Files & Subsetting them 
IF_USA =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9_PRICING_InventoryFiles/Inventory_File_US.csv", header = TRUE)
IF_CND =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9_PRICING_InventoryFiles/Inventory_File_CAD.csv", header = TRUE)

IF_USA = subset(IF_USA , select = c("vendor_name","vendor_code","mpn","price", "cost", "fedexable", "SpecialOrderSwt"))
names(IF_USA) = c("vendor_name","vendor_code","mpn","USA_price", "USA_cost", "USA_fedexable", "USA_SpecialOrder")

IF_CND = subset(IF_CND, select = c("VendorName","VenCode","ManufacturerPartNo","JobberPrice", "Cost", "Fedexable","SpecialOrderSwt"))
names(IF_CND) = c("vendor_name","vendor_code","mpn","CND_price", "CND_cost", "CND_fedexable", "CND_SpecialOrder")

Pooled_IF <- merge(IF_USA, IF_CND, by=c("vendor_name","vendor_code", "mpn"), all=TRUE) 

message("--------------------------*Inventory File Extraction Complete*")
message("")
message("***If you have any issues with the output***")
message("   ***Please Contact Abul Hassan Sheikh***  ")
message("")
message("Version: 1.1")
message("Last Updated: Auguest 22th 2018")
message("Author: Abul Hassan Sheikh")

#Return the following data sets
return(Pooled_IF)

}



###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

#KeystoneExchangeRateUpdate(Final1_Test0 = 1, ER = 1.36)

#ER = 1.39

#Pooled_IF = Extract_US_CND_InventoryFiles()
#Pooled_IF = data.frame(Pooled_IF)
###########################################################
#Future Add Ons













