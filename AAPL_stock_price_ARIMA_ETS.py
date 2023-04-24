"""
AAPL_stock_price_ARIMA_ETS.py

Copyright 2023 Thibault Hussy <thussy@ethz.ch>
"""

"""
This python script performs ARIMA and ETS analysis on time series of Apple stock price.

+ 
+ 
+ 

"""



"""_________________________ Libraries Importation _________________________"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
# import seaborn as sns
from math import sqrt

# from sklearn.preprocessing import PolynomialFeatures
# from sklearn.linear_model import LinearRegression
# from sklearn.model_selection import train_test_split, KFold
# from sklearn.metrics import mean_squared_error
# from sklearn.metrics import accuracy_score, confusion_matrix

from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.holtwinters import SimpleExpSmoothing
from pmdarima.arima import auto_arima

# from pmdarima import ets
# import pmdarima.arima as pmdAR

# from pylab import rcParams
from sklearn import metrics



"""_________________________ Constants Definition __________________________"""

data_file = 'AAPL.csv'



"""_________________________ Functions Definition __________________________"""

def get_mape(y_true, y_pred): 
    """
    Compute mean absolute percentage error (MAPE)
    """
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100



"""___________________________ Function main() _____________________________"""

def main():
    
    """_________________________ Data Importation __________________________"""
    
    # Load time series data from a CSV file
    df = pd.read_csv(data_file)
    # df.index = df.index.to_period('M')
    
    # Print datasets details
    # print('\nDataset df\n', df.describe())
    # print(df.head())
    
    # Convert the date column to a datetime object
    df['Date'] = pd.to_datetime(df['Date'])
    print(df.head())
          
    # # Set the date column as the dataframe index
    # df.set_index('Date', inplace=True)
    # # print(len(df))
    
    
    
    """_______________ Creation of train, val, and test sets _______________"""
    
    features = df['Date']
    target = df['Close']

    data_len = df.shape[0]
    print('Historical Stock Data length is - ', str(data_len))

    # Create a chronological split for train and testing
    train_split = int(data_len * 0.88)
    print('Training Set length - ', str(train_split))

    val_split = train_split + int(data_len * 0.1)
    print('Validation Set length - ', str(val_split - train_split))

    test_split = data_len
    print('Test Set length - ', str(test_split - val_split))

    # Splitting features and target into train, validation and test set
    X_train, X_val, X_test = features[:train_split], features[train_split:val_split], features[val_split:]
    Y_train, Y_val, Y_test = target[:train_split], target[train_split:val_split], target[val_split:]
    

 
    # """_____________________________ Fit ARIMA _____________________________"""
    
    # print('\n_________________________ Fit ARIMA _____________________________')

    # # # Fit an ARIMA model with automatic parameter selection
    # # model = auto_arima(Y_train, trace=True, stationary=True, suppress_warnings=True, error_action='ignore')
    # # print(model.summary())
    
    # # # Print the selected ARIMA model parameters
    # # print("ARIMA Model Parameters: ", model.order)

    # # Fit ARIMA model to the training data with optimal parameters
    # model = ARIMA(Y_train, order=(5, 2, 0))
    # model_fit = model.fit()
    # # print(model_fit.summary())

    # # Make predictions on the test data
    # Y_train_pred = model_fit.predict(start=0, end=train_split-1, typ='levels')
    # Y_val_pred = model_fit.predict(start=train_split, end=val_split-1, typ='levels')
    # Y_test_pred = model_fit.predict(start=val_split, end=test_split-1, typ='levels')
    

    
    # """_______________________ ARIMA Error Analysis ________________________"""
    
    # print('\n___________________ ARIMA Error Analysis _______________________')
    
    # # Calculate the performances
    # print("Training R-squared: ",round(metrics.r2_score(Y_train,Y_train_pred),2))
    # print("Training Explained Variation: ",round(metrics.explained_variance_score(Y_train,Y_train_pred),2))
    # print('Training MAPE:', round(get_mape(Y_train,Y_train_pred), 2)) 
    # print('Training Mean Squared Error:', round(metrics.mean_squared_error(Y_train,Y_train_pred), 2)) 
    # print("Training RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_train,Y_train_pred)),2))
    # print("Training MAE: ",round(metrics.mean_absolute_error(Y_train,Y_train_pred),2))

    # print(' ')

    # print("Validation R-squared: ",round(metrics.r2_score(Y_val,Y_val_pred),2))
    # print("Validation Explained Variation: ",round(metrics.explained_variance_score(Y_val,Y_val_pred),2))
    # print('Validation MAPE:', round(get_mape(Y_val,Y_val_pred), 2)) 
    # print('Validation Mean Squared Error:', round(metrics.mean_squared_error(Y_train,Y_train_pred), 2)) 
    # print("Validation RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_val,Y_val_pred)),2))
    # print("Validation MAE: ",round(metrics.mean_absolute_error(Y_val,Y_val_pred),2))

    # print(' ')

    # print("Test R-squared: ",round(metrics.r2_score(Y_test,Y_test_pred),2))
    # print("Test Explained Variation: ",round(metrics.explained_variance_score(Y_test,Y_test_pred),2))
    # print('Test MAPE:', round(get_mape(Y_test,Y_test_pred), 2)) 
    # print('Test Mean Squared Error:', round(metrics.mean_squared_error(Y_test,Y_test_pred), 2)) 
    # print("Test RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_test,Y_test_pred)),2))
    # print("Test MAE: ",round(metrics.mean_absolute_error(Y_test,Y_test_pred),2))
    
    # print(' ')
    
    # # Plot the original data and the predicted values
    # plt.plot(df.index, df['Close'], label='Original Data')
    # plt.plot(Y_val.index, Y_val_pred, label='Predicted Validation Values')
    # plt.plot(Y_test.index, Y_test_pred, label='Predicted Test Values')
    # plt.legend()
    # plt.savefig('ARIMA_predictions.png', dpi=500)
    # plt.show()
    
    
    
    """__________________________ Fit Exp. Smooth. __________________________"""
    
    print('\n_____________________ Fit Exp. Smooth. _________________________')

    # Fit an Exponential Smoothing model with automatic parameter selection
    # ES_model = auto_arima(Y_train, trace=True, stepwise=True, suppress_warnings=True, error_action='ignore')
    # print(ES_model.summary())
    
    # # Print the selected Exponential Smoothing model parameters
    # print("Exponential Smoothing Model Parameters: ", ES_model.order)
    
    # # Find optimal parameters from auto_arima
    # optimal_params = ES_model.get_params()
    
    # Create Exponential Smoothing model using optimal parameters
    ES_model = SimpleExpSmoothing(Y_train)

    # Fit the model to the data
    ES_model_fit = ES_model.fit(smoothing_level=0.5) 
    # print(ES_model_fit.summary())

    # Make predictions on the test data
    Y_train_pred_ES = ES_model_fit.predict(start=0, end=train_split-1)
    Y_val_pred_ES = ES_model_fit.predict(start=train_split, end=val_split-1)
    Y_test_pred_ES = ES_model_fit.predict(start=val_split, end=test_split-1)
    

    
    """____________________ Exp. Smooth. Error Analysis _____________________"""
    
    print('\n________________ Exp. Smooth. Error Analysis ____________________')
    
    # Calculate the performances
    print("Training R-squared: ",round(metrics.r2_score(Y_train,Y_train_pred_ES),2))
    print("Training Explained Variation: ",round(metrics.explained_variance_score(Y_train,Y_train_pred_ES),2))
    print('Training MAPE:', round(get_mape(Y_train,Y_train_pred_ES), 2)) 
    print('Training Mean Squared Error:', round(metrics.mean_squared_error(Y_train,Y_train_pred_ES), 2)) 
    print("Training RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_train,Y_train_pred_ES)),2))
    print("Training MAE: ",round(metrics.mean_absolute_error(Y_train,Y_train_pred_ES),2))

    print(' ')

    print("Validation R-squared: ",round(metrics.r2_score(Y_val,Y_val_pred_ES),2))
    print("Validation Explained Variation: ",round(metrics.explained_variance_score(Y_val,Y_val_pred_ES),2))
    print('Validation MAPE:', round(get_mape(Y_val,Y_val_pred_ES), 2)) 
    print('Validation Mean Squared Error:', round(metrics.mean_squared_error(Y_train,Y_train_pred_ES), 2)) 
    print("Validation RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_val,Y_val_pred_ES)),2))
    print("Validation MAE: ",round(metrics.mean_absolute_error(Y_val,Y_val_pred_ES),2))

    print(' ')

    print("Test R-squared: ",round(metrics.r2_score(Y_test,Y_test_pred_ES),2))
    print("Test Explained Variation: ",round(metrics.explained_variance_score(Y_test,Y_test_pred_ES),2))
    print('Test MAPE:', round(get_mape(Y_test,Y_test_pred_ES), 2)) 
    print('Test Mean Squared Error:', round(metrics.mean_squared_error(Y_test,Y_test_pred_ES), 2)) 
    print("Test RMSE: ",round(np.sqrt(metrics.mean_squared_error(Y_test,Y_test_pred_ES)),2))
    print("Test MAE: ",round(metrics.mean_absolute_error(Y_test,Y_test_pred_ES),2))
    
    print(' ')
    
    # Plot the original data and the predicted values
    plt.plot(df.index, df['Close'], label='Original Data')
    plt.plot(Y_val.index, Y_val_pred_ES, label='Predicted Validation Values')
    plt.plot(Y_test.index, Y_test_pred_ES, label='Predicted Test Values')
    plt.legend()
    plt.savefig('Exp_Smooth_predictions.png', dpi=500)
    plt.show()
    


"""________________________________ Script _________________________________"""

if __name__ == '__main__':
    main()


