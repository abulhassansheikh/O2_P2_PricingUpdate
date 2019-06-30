############################################################
#' A ca_cost Calculations Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################

message("--------------------------*Determine ca_cost & usa_cost Calculations*")

for(i in 1:nrow(MergedPricingData)){
	
	#Extract necessary data depending on the attribute set of the sku
	Discount_Rate = as.numeric(subset(VenderNameData, as.character(MergedPricingData$attribute_set[i]) == as.character(VenderNameData$attribute_set), select = Discount_Rate))
	MergedPricingData$REF_Discount[i] <- Discount_Rate
	
	#Determine if the inventory files are present
	CND_IF_Present = is.na(as.character(MergedPricingData$CND_price[i]))
	USA_IF_Present = is.na(as.character(MergedPricingData$USA_price[i]))

	#Calculate the current Exchange Rate
	if(CND_IF_Present == FALSE | USA_IF_Present == FALSE ){
		MergedPricingData$Current_Exchange_Rate[i] <- as.numeric(as.character(MergedPricingData$CND_price[i]))/as.numeric(as.character(MergedPricingData$USA_price[i]))
	} else { MergedPricingData$Current_Exchange_Rate[i] <- "Missing"}


	#Calculate the current discont rate CANADA
	if(CND_IF_Present == FALSE | USA_IF_Present == FALSE ){
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

TEMP.fileprint(MergedPricingData)

###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

CURR_Discount <= 1

edit(MergedPricingData)

TEMP.fileprint(MergedPricingData)


###########################################################
#Future Add Ons


test = as.integer(MergedPricingData$usa_jobber_price[726])

as.numeric(as.character(MergedPricingData$usa_jobber_price[726])) * 1.39 * (1-(16/100))

#Current_Exchange_Rate
	Current_Exchange_Rate = CND_price/USA_price
	if either values missing, write "Inventory_File_Missing"

#CURR_Discount%
	CURR_Discount% = (1-(CND_cost/CND_price))*100
#USED_Discount%
	If CURR_Discount% = VenderNameData$Discount_Rate
		USED_Discount% = CURR_Discount%
		NEW_ca_cost = CND_cost
	If CURR_Discount% < VenderNameData$Discount_Rate
		USED_Discount% = CURR_Discount%
		NEW_ca_cost = CND_cost
	If CURR_Discount% > VenderNameData$Discount_Rate
		USED_Discount% = VenderNameData$Discount_Rate
		NEW_ca_cost = (1-(VenderNameData$Discount_Rate/100))*CND_price

##	CND_cost
	if missing, then NEW_ca_cost = USA_price * ER * VenderNameData$Discount_Rate
##	USA_price
	if missing, then NEW_ca_cost = usa_jobber_price * ER * VenderNameData$Discount_Rate


#NEW_ca_cost



