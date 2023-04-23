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
import seaborn as sns
from math import sqrt

from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import mean_squared_error
from sklearn.metrics import accuracy_score, confusion_matrix

from statsmodels.tsa.arima.model import ARIMA




"""_________________________ Constants Definition __________________________"""

data_file = 'AAPL.csv'
obs_threshold = 5000



"""___________________________ Function main() _____________________________"""

def main():
    
    """_________________________ Data Importation __________________________"""
    
    # Reads the .csv file
    data = pd.read_csv(data_file)
    data.index = data.index.to_period('M')
    
    # Prints datasets details
    print('\nDataset data\n', data.describe())
    
    
    """_____________________________ Fit Model _____________________________"""
    
    model = ARIMA(data, order=(5, 1, 0))
    model_fit = model.fit()
    
    print(model_fit.summary())
    
    # Line plot of residuals
    residuals = pd.DataFrame(model_fit.resid)
    residuals.plot()
    plt.show()
    
    # Density plot of the residuals
    residuals.plot(kind='kde')
    plt.show()
    
    # Summary stats of residuals
    print(residuals.describe())
    
    # Split into train and test sets (0.5/0.5)
    X = data['High'].to_numpy()
    size = int(len(X) * 0.5)
    train, test = X[0:size], X[size:len(X)]
    history = [x for x in train]
    predictions = list()
    
    # Walk-forward validation
    for t in range(len(test)):
        model = ARIMA(history, order=(5, 1, 0))
        model_fit = model.fit()
        output = model_fit.forecast()
        yhat = output[0]
        predictions.append(yhat)
        obs = test[t]
        history.append(obs)
        print(f'predicted={yhat}, expected={obs}')
    
    # Evaluate the forecasts
    rmse = sqrt(mean_squared_error(test, predictions))
    print('Test RMSE: {rmse}')
    
    # Plot forecasts against actual outcomes
    
    
    
    
    # """__________________________ Error Analysis ___________________________"""
    
    # print('\n______________________ Error Analysis __________________________')

    # # Prints training and validation errors, for each degree without KFold
    # print(f'\nErrors for degree 1 to {regr_max_degree} without KFold.')
    # print(f'Training errors:\t{tr_errors}',
    #     f'\nValidation errors:\t{val_errors}\n')
    
    # # Graphs of training and validation errors
    # plt.figure(figsize=(6, 4))

    # plt.plot(degrees, tr_errors, color='b', label = 'Training')
    # plt.plot(degrees, val_errors, color='r', label = 'Validation')
    # plt.legend()

    # plt.xlabel('Degree', fontdict={'fontsize':12})
    # plt.ylabel('Loss', fontdict={'fontsize':12})
    # plt.ylim((0, 100))
    # plt.title('Training and Validation Loss', fontdict={'fontsize':14})
    
    # # plt.savefig('Train_val_error.svg')
    # # plt.savefig('Train_val_error.png', dpi=500)
    # plt.show()
    
    # # Applies polynomial regression of degree with the smallest loss (i.e., 1)
    # poly = PolynomialFeatures(degree=1)
    # X_train_poly = poly.fit_transform(X_train)
    # lin_regr.fit(X_train_poly, y_train)

    # # Computes the test error
    # X_test_poly = poly.fit_transform(X_test)
    # y_pred_test = lin_regr.predict(X_test_poly)
    # test_error = mean_squared_error(y_test, y_pred_test)
    
    # # Prints testing error
    # print(f'Testing errors:\t{test_error}')
    
    # # Prints the equation of the curve
    # print(f'\nEquation of the model:\ty = {lin_regr.coef_} · X + ',
    #     '{lin_regr.intercept_}\n\n')
    


"""________________________________ Script _________________________________"""

if __name__ == '__main__':
    main()


