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

NCDs = ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer']
# Input parameters for Survival Function
ncd_survival_function = {'CKD': 'log_normal_hazard_rate',
                         'IHD': 'log_normal_hazard_rate',
                         'HS' : 'log_logistic_hazard_rate',
                         'IS' : 'log_logistic_hazard_rate',
                         'Bcancer': 'log_logistic_hazard_rate',
                         'Lcancer': 'log_logistic_hazard_rate',
                         'Ccancer': 'log_logistic_hazard_rate',
                         'DM1': 'negative_expoential_hazard_rate'
                         }

survival_curve_para = {'CKD': {     'treated_lb': [100, 3.99524753, 1.68136759],
                                    'treated_ub': [100, 3.99524753, 1.68136759],
                                  'untreated_lb': [100, 3.14329388, 1.58136759],
                                  'untreated_ub': [100, 3.25922182, 1.58136759]},
                       'IHD': {     'treated_lb': [94.3,8.1029755 , 5.19497573],
                                    'treated_ub': [97.2,8.10297547, 5.19497571],
                                  'untreated_lb': [60,  2.90401215, 4.50269156],
                                  'untreated_ub': [60,  2.90401215, 4.50269156]},
                       'HS': {      'treated_lb': [68,  0.70166823, 0.12837254],
                                    'treated_ub': [68,  20.71026698,0.32401575],
                                  'untreated_lb': [51,  0.24012431, 0.17809074],
                                  'untreated_ub': [51,  0.24012431, 0.17809074]},
                       'IS': {      'treated_lb': [93,  63.17745322,0.58047708],
                                    'treated_ub': [93,  63.17745322,0.58047708],
                                  'untreated_lb': [88,  18.55677113,0.58644473],
                                  'untreated_ub': [88,  18.55677113,0.58644473]},
                       'Bcancer': { 'treated_lb': [100, 221.35196983, 1.06114687],
                                    'treated_ub': [100, 221.35196983, 1.06114687],
                                  'untreated_lb': [100, 146.6025383, 1.06114687],
                                  'untreated_ub': [100, 146.6025383, 1.06114687]},
                       'Ccancer': { 'treated_lb': [100, 84.40664198, 0.72789679],
                                    'treated_ub': [100, 84.40664198, 0.72789679],
                                  'untreated_lb': [100, 64.49123835, 0.65451315],
                                  'untreated_ub': [100, 64.49123835, 0.65451315]},
                       'Lcancer': { 'treated_lb': [100, 23.64434258, 0.6601755],
                                    'treated_ub': [100, 23.64434258, 0.6601755],
                                  'untreated_lb': [100, 7.15460378, 0.6601755],
                                  'untreated_ub': [100, 7.15460378, 0.6601755]},
                       'DM1':    {'treated_lb': [100,100,1/75.6/12],
                                  'treated_ub': [100,100,1/75.6/12],
                                  'untreated_lb': [100,100,1/1.3/12],
                                  'untreated_ub': [100,100,1/2.6/12]}}

def log_logistic_hazard_rate(t, alpha, beta):
    pdf_log_logistic = (beta/alpha) * (t/alpha)**(beta-1) / (1 + (t/alpha)**beta)**2
    cdf_log_logistic = 1 / (1 + (alpha/t)**beta)
    return pdf_log_logistic/(1-cdf_log_logistic)

def log_normal_hazard_rate(t, mu, sigma):
    pdf = (1 / (t * sigma * np.sqrt(2 * np.pi))) * np.exp(- (np.log(t) - mu)**2 / (2 * sigma**2))
    cdf = norm.cdf(np.log(t), mu, sigma)
    return pdf / (1 - cdf)

def negative_expoential_harzard_rate(t, acute, h):
    return h


# We want to generate the hazard rate before we run the main simulation
HR_ncds = {}
for ncd_name in NCDs:

    survival_curve = survival_curve_para[ncd_name]

    acute_tlb, para1_tlb, para2_tlb = survival_curve['treated_lb']
    acute_tub, para1_tub, para2_tub = survival_curve['treated_ub']
    acute_utlb, para1_utlb, para2_utlb = survival_curve['untreated_lb']
    acute_utub, para1_utub, para2_utub = survival_curve['untreated_ub']

    HR_t_all = []

    for t in range(500):
        if t == 0:
            # Acute Phase when tau = 0
            HR_t_tlb = 1 - acute_tlb / 100
            HR_t_tub = 1 - acute_tub / 100
            HR_t_utlb = 1 - acute_utlb / 100
            HR_t_utub = 1 - acute_utub / 100
            HR_t_all.append([HR_t_tlb, HR_t_tub, HR_t_utlb, HR_t_utub])
        else:
            # Continue Survive when tau > 0
            HR_t_tlb = globals()[ncd_survival_function[ncd_name]](t, para1_tlb, para2_tlb) 
            HR_t_tub = globals()[ncd_survival_function[ncd_name]](t, para1_tub, para2_tub)
            HR_t_utlb = globals()[ncd_survival_function[ncd_name]](t, para1_utlb, para2_utlb)
            HR_t_utub = globals()[ncd_survival_function[ncd_name]](t, para1_utub, para2_utub)

            HR_t_all.append([HR_t_tlb, HR_t_tub, HR_t_utlb, HR_t_utub])
    df = pd.DataFrame(HR_t_all, columns=['HR_t_tlb', 'HR_t_tub', 'HR_t_utlb', 'HR_t_utub'])

    HR_ncds[ncd_name] = df
  
# Save the Hazard rate as the excel file
with pd.ExcelWriter('HR_new_logistic_normal.xlsx', engine='openpyxl') as writer:
    # Iterate over the dictionary and create a DataFrame from each set of values
    # Each key represents a sheet name
    for sheet_name, records in  HR_ncds.items():
        # Write each DataFrame to a specific sheet
        records.to_excel(writer, sheet_name=sheet_name, index=False)
