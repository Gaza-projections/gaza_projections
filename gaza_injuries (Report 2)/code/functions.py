import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import re
from datetime import datetime, timedelta, date, time
from tqdm import tqdm
from sklearn.preprocessing import OneHotEncoder
import miceforest as mf
from sklearn.preprocessing import LabelEncoder
from random import choices
from sklearn.utils import resample
import warnings
warnings.filterwarnings("ignore")

# Color coding for plots
colors = {
    'North Gaza': '#1f77b4',    # Blue
    'Gaza': '#ff7f0e',          # Orange
    'Deir Al-Balah': '#2ca02c', # Green
    'Khan Younis': '#d62728',   # Red
    'Rafah': '#9467bd'          # Purple
}

def re_encoding(df, code_book):
    df['event_date'] = pd.to_datetime(df['event_date'])
    labels, values_mice = label_texts(df['notes'], df['fatalities'], code_book)
    df['labels'] = labels
    df['fata_mice'] = values_mice

    # set up all the predictors used for mice imputation
    col = ['disorder_type', 'event_type', 'sub_event_type', 'actor1',
           'assoc_actor_1', 'inter1', 'interaction', 'civilian_targeting', 'iso', 'admin2', 'latitude', 'longitude',
           'fata_mice']
    df_mice = df[col]
    # check the data type and as for those non mumerical predictors, we label encoder it in to catagorical type
    for c in col:
        if (df_mice[c].dtype != 'float64') and (df_mice[c].dtype != 'int64'):
            label_encoder = LabelEncoder()
            df_mice[c] = label_encoder.fit_transform(df_mice[c])
    return df_mice

def mice_imputation(df_mice, df, c):
    # Create kernel for the mice imputation and do 200 individual chains
    kds = mf.ImputationKernel(
        df_mice,
        datasets=c,
        save_all_iterations=False
    )

    # Run the MICE algorithm for 5 iterations
    kds.mice(5)

    # Return the completed dataset.
    for i in tqdm(range(c)):
        df_mice_complete = kds.complete_data(dataset=i)
        df['fata_mice_' + str(i)] = df_mice_complete['fata_mice']

    # Save the imputed MICE into output file
    df.to_csv('../output/RAW_ACLED_with_MICE_200Chain.csv')
    return df

def label_texts(text_list, value_list, code_book):
    labels = []
    values_mice = []
    for i in range(len(text_list)):
        text = text_list[i]
        v = value_list[i]
        if v > 0:
            label = [str(1)]
        else:
            label = [str(0)]
        for key, value in code_book.items():
            if key in text.lower():
                label.append(value)
        if len(label) > 1:
            l = list(np.unique(label))
            label = ','.join(l)
            labels.append(label)
        else:
            label.append('real')
            label = ','.join(label)
            labels.append(label)

        if ('inflated' in label) or ('unknow' in label):
            values_mice.append(np.nan)
        else:
            values_mice.append(v)
    return labels, values_mice

def uncounted_scaler_moh_unwra():
    data = pd.read_excel('../input/MOH_UNWRA_Death_Rate.xlsx')
    # Perform linear interpolation for the missing values in MOH and UNWRA columns
    data['MOH_iner'] = data['MOH'].interpolate(method='linear')
    data['UNWRA_inter'] = data['UNWRA'].interpolate(method='linear')
    data['Date'] = pd.to_datetime(data['Date'])
    # Add a third column with the interpolated results for UNWRA/MOH
    data['MOH/UNWRA_real'] = data['MOH_iner'] / data['UNWRA_inter']
    data['MOH/UNWRA_model'] = np.where(data['MOH/UNWRA_real'] < 1, data['MOH/UNWRA_real'], 1)
    uncounted_scaler = data['MOH/UNWRA_model'].iloc[70:160].values

    return uncounted_scaler

def population_bootstrapping_base(population_clean):
    KY_noise = population_clean['Khan Younis'] - population_clean['Khan Younis' + 'smooth']
    RF_noise = population_clean['Rafah'] - population_clean['Rafah' + 'smooth']
    KY_nm = np.mean(KY_noise)
    KY_std = np.std(KY_noise)
    RF_nm = np.mean(RF_noise.values[21:])
    RF_std = np.std(RF_noise.values[21:])
    KY_pop_mean = 0.659181
    RF_pop_mean = 0.584297
    return KY_nm, KY_std, KY_pop_mean, RF_nm, RF_std, RF_pop_mean
    # RF_pop_mean = 0.80

def MOH_adjustment(MOH_reference, micedf):
    # Perform linear interpolation for the missing values in MOH and UNWRA columns
    MOH_reference['MOH_iner'] = MOH_reference['MOH'].interpolate(method='linear')
    MOH_reference['MOH_diff'] = MOH_reference['MOH_iner'].diff() * 2226544 / 1000
    MOH_dpd = MOH_reference.iloc[70:160][['Date', 'MOH_diff']]

    micedf['event_date'] = pd.to_datetime(micedf['event_date'])
    mice_all = pd.DataFrame()
    td_all = []
    for i in range(200):
        fatalities_columns = 'fata_mice_' + str(i)
        micedf['total_fatalities'] = micedf[fatalities_columns]
        td_all.append(np.sum(micedf[fatalities_columns]))
        total_fatalities = micedf.groupby(['event_date'])['total_fatalities'].sum().reset_index()
        mice_all['date'] = total_fatalities['event_date'].values
        mice_all['fata_mice_' + str(i)] = total_fatalities['total_fatalities'].values
    mice_all_cut = mice_all.iloc[70:160].reset_index()
    mice_columns = [col for col in mice_all.columns if 'fata_mice' in col]
    mice_moh_ratio = pd.DataFrame()
    for c in mice_columns:
        mice_moh = mice_all_cut[c].values / MOH_dpd['MOH_diff'].values
        mice_moh_ratio[c] = mice_moh

    # moh_scaler = np.where(mice_moh_ratio.mean(axis = 1).values >1, 1, mice_moh_ratio.mean(axis = 1).values)
    moh_scaler_m = mice_moh_ratio.mean(axis=1).values
    moh_scaler_std = mice_moh_ratio.std(axis=1).values

    return moh_scaler_m, moh_scaler_std

def KY_death_per_day(micedf):
    micedf['event_date'] = pd.to_datetime(micedf['event_date'])
    mice_ky = pd.DataFrame()
    for i in range(200):
        fatalities_columns = 'fata_mice_' + str(i)
        micedf['total_fatalities'] = micedf[fatalities_columns]
        total_fatalities = micedf.groupby(['admin2', 'event_date'])['total_fatalities'].sum().reset_index()
        mice_ky['date'] = total_fatalities[total_fatalities['admin2'] == 'Khan Yunis']['event_date'].values
        mice_ky['fata_mice_' + str(i)] = total_fatalities[total_fatalities['admin2'] == 'Khan Yunis'][
            'total_fatalities'].values
    mice_ky_cut = mice_ky.iloc[67:157].reset_index()
    mice_columns = [col for col in mice_ky_cut.columns if 'fata_mice' in col]
    mice_dpd_boots = mice_ky_cut[mice_columns].values
    return mice_dpd_boots

def Bootstrap(mice_dpd_boots, n_boots, KY_std, KY_nm, RF_std, RF_nm, moh_scaler_m, moh_scaler_std, uncounted_scaler):
    KY_pop_mean = 0.659181 # KY population at December 16 2023
    RF_pop_mean = 0.584297 # RF population at May 20 2024

    death_initial = []
    death_counted = []
    death_uncounted = []
    death_total = []
    pop_ratios = []
    for i in range(n_boots):
        pop_ky_n = np.random.randn() * KY_std
        pop_rf_n = np.random.randn() * RF_std
        pop_ratio = (pop_rf_n + RF_nm + RF_pop_mean) / (pop_ky_n + KY_nm + KY_pop_mean)
        pop_ratios.append(pop_ratio)
        dpds_i = []
        dpds_c = []
        dpds_uc = []
        dpds_t = []
        for t in range(90):
            dpd = mice_dpd_boots[t]
            d = choices(dpd, k=1)[0] * pop_ratio # i f we comment line the populatin then we plor the kY estimation
            # print(pop_ratio)
            # d = choices(dpd, k=1)[0]
            dpds_i.append(d)

            moh_scaler = moh_scaler_m[t] + np.random.randn() * moh_scaler_std[t]
            moh_scaler = min(1, moh_scaler)
            moh_scaler = max(0.2992, moh_scaler)
            d_moh = d / moh_scaler
            dpds_c.append(d_moh)

            d_moh_uncounted = d_moh / uncounted_scaler[t]
            dpds_uc.append(d_moh_uncounted - d_moh)

            dpds_t.append(d_moh_uncounted)

        death_initial.append(dpds_i)
        death_counted.append(dpds_c)
        death_uncounted.append(dpds_uc)
        death_total.append(dpds_t)
    return death_initial, death_total, death_uncounted, death_uncounted, death_total

def plot_boots_res(death):
    # mean = np.mean(death_total,axis = 0)
    # lb = np.quantile(death_total, q = 0.025, axis = 0)
    # ub = np.quantile(death_total, q = 0.975, axis = 0)

    start_date = pd.to_datetime('2024-5-20')
    end_date = start_date + pd.Timedelta(days=89)
    time_array = pd.date_range(start=start_date, end=end_date, freq='D')

    mean = np.mean(death, axis=0)
    lb = np.quantile(death, q=0.025, axis=0)
    ub = np.quantile(death, q=0.975, axis=0)

    # Adjusting the plot with smaller text size
    plt.figure(figsize=(11, 5), dpi=300)
    # plt.plot(data['Date'], data['MOH/UNWRA_real'], label='Proportion ', linestyle='-', color='b', linewidth=1)
    plt.plot(time_array, mean, linestyle='-', color='b', linewidth=1, label='Projected Deaths')
    plt.fill_between(time_array, lb, ub, linestyle='-', color='k', linewidth=1, alpha=0.2,
                     label='95% Uncertainty Interval ')

    plt.axvline(pd.to_datetime('2024-5-20'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2024-5-20'), plt.ylim()[1]*0.95, '2024-5-20', color='r', fontsize=8, ha='center')

    plt.axvline(pd.to_datetime('2024-08-17'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2024-08-17'), plt.ylim()[1]*0.95, '2024-08-17', color='r', fontsize=8, ha='center')

    plt.xlabel('Date', fontsize=12)
    plt.ylabel('Deaths', fontsize=12)
    plt.legend(fontsize=12, loc=1, bbox_to_anchor=(1, 0.9))
    plt.grid(True)
    plt.xticks(rotation=0, fontsize=12)
    plt.yticks(fontsize=12)
    plt.tight_layout()

    # Display the plot
    plt.savefig('../output/Bootstrapping_result.jpeg')


def plot_population(population_clean):
    population_clean['population_date'] = pd.to_datetime(population_clean['date'])
    plt.figure(figsize=(11, 5), dpi=300)
    for region in ['North Gaza', 'Gaza', 'Deir Al-Balah', 'Khan Younis', 'Rafah']:
        population = population_clean[region]
        smoothed_population = population_clean[region + 'smooth']
        plt.plot(population_clean['population_date'], population, linestyle='-', label=region, linewidth=1,
                 color=colors[region])
        plt.plot(population_clean['population_date'], smoothed_population, linestyle='--', linewidth=1,
                 color=colors[region])

    plt.axvline(pd.to_datetime('2023-12-16'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2023-12-16'), plt.ylim()[1] * 0.95, '2023-12-16', color='r', fontsize=8, ha='center')

    plt.axvline(pd.to_datetime('2024-05-20'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2024-05-20'), plt.ylim()[1] * 0.95, '2024-05-20', color='r', fontsize=8, ha='center')

    plt.xlabel('Date', fontsize=12)
    plt.ylabel('Population (M)', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True)
    plt.xticks(rotation=0, fontsize=12)
    plt.yticks(fontsize=12)

    plt.tight_layout()
    # save the plot
    plt.savefig('../output/population.jpeg')

def plot_uncounted_scaler(data):
    # Adjusting the plot with smaller text size
    data['MOH_iner'] = data['MOH'].interpolate(method='linear')
    data['UNWRA_inter'] = data['UNWRA'].interpolate(method='linear')
    data['Date'] = pd.to_datetime(data['Date'])
    # Add a third column with the interpolated results for UNWRA/MOH
    data['MOH/UNWRA_real'] = data['MOH_iner'] / data['UNWRA_inter']
    data['MOH/UNWRA_model'] = np.where(data['MOH/UNWRA_real'] < 1, data['MOH/UNWRA_real'], 1)

    plt.figure(figsize=(11, 5), dpi=300)
    plt.plot(data['Date'], data['MOH_iner'], label='MOH Deaths', linestyle='-', color='b', linewidth=1)
    plt.plot(data['Date'], data['UNWRA_inter'], label='UNWRA Deaths', linestyle='-', color='g', linewidth=1)
    # Adding vertical lines
    plt.axvline(pd.to_datetime('2023-12-16'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2023-12-16'), plt.ylim()[1] * 0.95, '2023-12-16', color='r', fontsize=8, ha='center')

    plt.axvline(pd.to_datetime('2024-03-14'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2024-03-14'), plt.ylim()[1] * 0.95, '2024-03-14', color='r', fontsize=8, ha='center')

    # Enhancing the plot

    # Enhancing the plot
    # Enhancing the plot with smaller text size
    # plt.title('MOH_iner and UNWRA_inter Over Time', fontsize=12)
    plt.xlabel('Date', fontsize=12)
    plt.ylabel('Cumulative Rate per 1000 person', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True)
    plt.xticks(rotation=0, fontsize=12)
    plt.yticks(fontsize=12)
    plt.tight_layout()

    # Display the plot
    plt.savefig('../output/MOH_UNWRA_death_rates.jpeg')

    # Adjusting the plot with smaller text size
    plt.figure(figsize=(11, 5), dpi=300)
    # plt.plot(data['Date'], data['MOH/UNWRA_real'], label='Proportion ', linestyle='-', color='b', linewidth=1)
    plt.plot(data['Date'], data['MOH/UNWRA_model'], linestyle='-', color='b', linewidth=1)
    # Adding vertical lines
    plt.axvline(pd.to_datetime('2023-12-16'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2023-12-16'), plt.ylim()[1] * 0.95, '2023-12-16', color='r', fontsize=8, ha='center')

    plt.axvline(pd.to_datetime('2024-03-14'), color='r', linestyle='--', linewidth=1)
    plt.text(pd.to_datetime('2024-03-14'), plt.ylim()[1] * 0.95, '2024-03-14', color='r', fontsize=8, ha='center')

    # Enhancing the plot

    # Enhancing the plot
    # Enhancing the plot with smaller text size
    # plt.title('MOH_iner and UNWRA_inter Over Time', fontsize=12)
    plt.xlabel('Date', fontsize=12)
    plt.ylabel('Proportion of Counted Death', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True)
    plt.xticks(rotation=0, fontsize=12)
    plt.yticks(fontsize=12)
    plt.tight_layout()

    # Save the figure
    plt.savefig('../output/proportion_counted.jpeg')

