import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
import random
from tqdm import tqdm
from scipy.optimize import curve_fit
import warnings
from scipy.optimize import minimize
from scipy.stats import poisson
from scipy.stats import fisk
import pickle
warnings.filterwarnings('ignore')
import copy
from scipy.stats import norm, gamma

# Name for outputs and visualization
NCDs = ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer']

NCD_full_name = {'DM1': 'Diabetes mellitus type 1 (E10)',
                 'DM1': 'Diabetes mellitus type 2 (E11)',
                 'CKD': 'Chronic kidney disease (N18)',
                 'IHD': 'Ischaemic heart diseases (I20-25)',
                 'CDIS': 'Cerebrovascular diseases including stroke (I63 to I66)',
                 'HD': 'Hypertensive diseases (I11-12)',
                 'HS': 'Haemorrhagic Stroke',
                 'IS': 'Ischemic Stroke',
                 'COPD': 'chronic obstructive pulmonary disease (J44)',
                 'Bcancer': 'Breast cancer',
                 'Ccancer': 'Colorectal cancer',
                 'Lcancer': 'Lung cancer'
                 }

NCD_cata = {'DM1': 'DM',
            'DM1': 'DM',
            'CKD': 'CKD',
            'IHD': 'CVD',
            'CDIS': 'CVD',
            'HD': 'CVD',
            'HS': 'CVD',
            'IS': 'CVD',
            'COPD': 'COPD',
            'Bcancer': 'Cancer',
            'Ccancer': 'Cancer',
            'Lcancer': 'Cancer',

            }

# Initialize tables to record the fitting goodness and parameters
fit_goodness = pd.DataFrame()
survival_curve_para = {}

# Method 1 - Directly fit the survival curve
NCDs = ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer']
df = pd.read_excel('NCD_M2_model_para_1.26.2024.xlsx', sheet_name = 'Treatment')
df.iloc[0] = df.iloc[0]/100
first_row = df.iloc[0]
curve_vals = df.iloc[1:].mul(df.iloc[0].values)/100
curve_vals.index = curve_vals.index + 1
curve_vals.loc[0] = first_row
curve_vals = curve_vals.sort_index()
curve_vals['Time (Month)'] = range(121)


# Survival functions
def weibull_survival(t, lambda_, kappa):
    return acute * np.exp(-(t / lambda_) ** kappa)


def log_normal_survival(t, mu, sigma):
    return acute * (1 - norm.cdf(np.log(t), mu, sigma))


def log_logistic_survival(t, alpha, beta):
    return acute / (1 + (t / alpha) ** beta)


def gamma_survival(t, shape, scale):
    return acute * (1 - gamma.cdf(t, shape, scale=scale))


def exponential_survival(t, lambda_):
    return acute * np.exp(-lambda_ * t)


curve = 'log_logistic_survival'  # select the distribution for fitting here

if (curve == 'weibull_survival') | (curve == 'log_logistic_survival'):
    param_bounds = ([0.001, 0.001], [np.inf, np.inf])
    initial_guess = [0.01, 0.01]
else:
    param_bounds = None
    initial_guess = None

ncd_error = []
ncd_name = []
for ncd in NCDs:
    survival_curve_para[ncd] = {}
    for cond in ['_treated_lb', '_treated_ub', '_untreated_lb', '_untreated_ub']:
        info = curve_vals[['Time (Month)', ncd + cond]].dropna()
        months = info['Time (Month)'].values[1:]
        survival_rates = info[ncd + cond].values[1:]
        acute = info[ncd + cond].values[0]

        if (curve == 'weibull_survival') | (
                curve == 'log_logistic_survival'):  # if weibull survival or log_logistic_survival we set bounds for the parameters
            para, pcov = curve_fit(globals()[curve], months, survival_rates, p0=initial_guess, bounds=param_bounds)
        else:
            para, pcov = curve_fit(globals()[curve], months, survival_rates)

        # Use the obtained parameters to predict the y values
        fitted_ydata = globals()[curve](months, *para)

        # Calculate the mean squared error
        mse = np.mean((survival_rates - fitted_ydata) ** 2)
        para1, para2 = para
        survival_curve_para[ncd][cond[1:]] = [para1, para2, acute]

        ncd_name.append(ncd + cond)
        ncd_error.append(mse)

fit_goodness['NCD'] = ncd_name
fit_goodness[curve] = ncd_error