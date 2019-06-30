############################################################
#' A ca_price Calculations Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################


message("--------------------------*Conducting Exchange Rate Calculations*")



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
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons


###########################################################
#Rough work
format(round(1.1234, 2), nsmall = 2)

class(as.numeric(MergedPricingData$NEW_ca_cost[i]))

#CURR_Transaction_Fee
	ca_price*0.03
#CURR_Actual_Cost
	NEW_ca_cost + na_ca_shipping + CURR_Transaction_Fee
#CURR_CND_Margin
	(ca_price-CURR_Actual_Cost)/ca_price

#USED_CND_Margin
	if VenderNameData$CAD_Min_Margin < CURR_CND_Margin < VenderNameData$CAD_Max_Margin
		USED_CND_Margin = CURR_CND_Margin
	if VenderNameData$CAD_Min_Margin > CURR_CND_Margin 
		USED_CND_Margin = CAD_Min_Margin
	if CURR_CND_Margin > VenderNameData$CAD_Max_Margin
		USED_CND_Margin = CAD_Max_Margin
#NEW_ca_price
	NEW_ca_price = (-(NEW_ca_cost)-(na_ca_shipping))/((USED_CND_Margin/100)-0.97)
#USED_Transaction_Fee
	NEW_ca_price * 0.03
#USED_Actual_Cost
	NEW_ca_cost + na_ca_shipping + USED_Transaction_Fee
#NEW_ca_retail
	NEW_ca_retail = NEW_ca_price* 1.2

VenderNameData$Discount_Rate



