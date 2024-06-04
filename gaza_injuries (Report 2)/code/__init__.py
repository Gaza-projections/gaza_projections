required_packages = ['numpy', 'pandas', 'statsmodels', 'random', 'scipy', 'sklearn', 'matplotlib', 'miceforest',
                     'warnings']
missing_packages = []

for package in required_packages:
    try:
        __import__(package)
    except ImportError:
        missing_packages.append(package)

if missing_packages:
    print(f"Missing packages: {', '.join(missing_packages)}")
    print("Please install the missing packages using:")
    print(f"pip install {' '.join(missing_packages)}")
    exit(1)

from functions import *
import warnings
warnings.filterwarnings("ignore")

# read the raw data from input
acled = pd.read_excel('../input/ACLED_FATALITIES.xlsx')
death_rate = pd.read_excel('../input/MOH_UNWRA_Death_Rate.xlsx')
population = pd.read_csv('../input/OXFORD_POPULATION.csv')
# dictionary for labeling the data
code_book = {'casualties unknown': 'unknow',
             'fatalities unknown': 'unknow',
             'unknown fatalities': 'unknow',
             'there were no casualties': 'zero',
             'injury': 'injury',
             'injuries': 'injury',
             'injured': 'injury',
             'injuring': 'injury',
             'palestinian ministry of health reported': 'inflated'}

## MICE Imputation
# re_encoding of the original ACLED dataset
df_mice = re_encoding(acled, code_book)
# mice imputation with 200 individual chains and store the imputed data
acled_mice = mice_imputation(df_mice, acled, 200)

## Preparing for the Bootstrapping
# calculate proportion of counted
uncounted_scaler = uncounted_scaler_moh_unwra()
# calculate the base for bootstrapping population ratio
KY_nm, KY_std, KY_pop_mean, RF_nm, RF_std, RF_pop_mean = population_bootstrapping_base(population)
# calculate the base for bootstrapping moh ratio
moh_scaler_m, moh_scaler_std = MOH_adjustment(death_rate, acled_mice)
# calculate imputed death per day in Khan Younis
ky_dpd = KY_death_per_day(acled_mice)

## Bootstrapping
n_boots = 10000
death_initial, death_total, death_uncounted, death_uncounted, death_total = Bootstrap(ky_dpd, n_boots, KY_std, KY_nm, RF_std, RF_nm, moh_scaler_m, moh_scaler_std, uncounted_scaler)

## Plots
plot_uncounted_scaler(death_rate) # plot proportion counted and death rates
plot_population(population) # plot population over time
plot_boots_res(death_total) # plot bootstrapping results over time
