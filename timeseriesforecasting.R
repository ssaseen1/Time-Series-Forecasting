# CS 535 Project - Time Series Prediction
# Model Used: ARIMA(1,1,1)

# Shali Saseendran
# ssaseen1@binghamton.edu

	
	# library used for forecasting
	require(forecast)
	
	# output.txt file to print the final output file
	fileName = "output.txt"

	# remove the existing output.txt file from the folder
	if(file.exists(fileName))
	{
		file.remove(fileName)
	}
	
	# converted product_distribution_training_set.txt file to product_distribution_training_set.csv file
	# with each row containg the product sales info
	# first row is the key product id which is considered as header, hence header set to TRUE
	time = read.csv("product_distribution_training_set.csv",header = TRUE)

	# converted key_product_IDs.txt file to key_product_IDs.csv file with no header
	keyProductFile = read.csv("key_product_IDs.csv",header = FALSE)

	# number of key products (100) 
	noOfProducts =  ncol(time)
	
	# total number of days to be predicted (29)
	daysToPredict = 29

	# total sales of all the key products per day
	sum = data.frame(rowSums(time))
	
	# to check the best fit arima model
	ARIMAfit = auto.arima(sum[,1])

	# ARIMA(1,1,1) given by auto.arima is used to train the model
	ARIMAfit<-arima(sum[,1], order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))
	
	# predict the total sales of ahead 29 days using the trained model
	pred = predict(ARIMAfit, n.ahead = daysToPredict)
	dataFrame = data.frame(pred)
	
	# converting the decimal predicted values to integer values
	dataFrame[,1] = as.integer(dataFrame[,1])

	# write the output to the output.txt file
	cat("0 ", file = fileName, append = TRUE)
	write(dataFrame[,1], file = fileName, append = TRUE, ncolumns = daysToPredict, sep = " ")
	
	# predict the sales of each product of ahead 29 days using the trained model. 
	keyProduct = 1	
	while(keyProduct < noOfProducts + 1 ){
		ARIMAfit = arima(time[,keyProduct], order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))
		pred = predict(ARIMAfit, n.ahead = daysToPredict)
		dataFrame = data.frame(pred)
		dataFrame[,1] = as.integer(dataFrame[,1])

		# check the values of the prediction,
		# lesser than zero values are set to zero
		eachDay = 1
		while(eachDay < (daysToPredict+1))
		{
			if(!dataFrame[eachDay,] > 0)
				{
					dataFrame[eachDay,] = 0
				}
			eachDay = eachDay + 1
		}
		
		cat(keyProductFile[keyProduct,1] , file = fileName, append = TRUE, sep = " ")
		cat(" ", file = fileName, append = TRUE)

		write(dataFrame[,1], file = fileName, append = TRUE, ncolumns = daysToPredict, sep = " ")
		keyProduct = keyProduct + 1
	}	
	
	
