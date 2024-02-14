import numpy as np
import pandas as pd

# Age Distribution for ncd death
ncd_d = pd.read_excel('NCD_M2_model_para_1.26.2024.xlsx', sheet_name = 'Death')

def cum_1346_by_age_sce(ncd_name, res_total):
    distribution = ncd_d[ncd_name + '_Age_Distribution']

    df1 = pd.DataFrame()

    for s in ['Escalation', 'Status Quo', 'Ceasefire']:
        total_excess_13 = sum(res_total[s][4:7])
        values = distribution[:, np.newaxis] * total_excess_13

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month1-3_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month1-3_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month1-3_ub'] = np.round(upper_bounds, 2)

        total_excess_46 = sum(res_total[s][7:10])
        values = distribution[:, np.newaxis] * total_excess_46

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month4-6_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month4-6_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month4-6_ub'] = np.round(upper_bounds, 2)

        total_excess = sum(res_total[s][4:10])
        values = distribution[:, np.newaxis] * total_excess

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_All_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_All_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_All_ub'] = np.round(upper_bounds, 2)

    df1.insert(0, 'Age', ncd_d['Age'])

    with pd.ExcelWriter(
            'outputs/' + ncd_name + '/scenario_total_13-46M_by_age_.xlsx',
            engine='xlsxwriter') as writer:
        df1.to_excel(writer, sheet_name=ncd_name)


def cum_1346_by_age_base(ncd_name, baseline):
    df1 = pd.DataFrame()

    distribution = ncd_d[ncd_name + '_Age_Distribution'].values

    total_excess_13 = sum(baseline[4:7])
    values = distribution[:, np.newaxis] * total_excess_13

    means = [np.mean(v) for v in values]
    lower_bounds = [np.percentile(v, 2.5) for v in values]
    upper_bounds = [np.percentile(v, 97.5) for v in values]

    df1[ncd_name + '_Month1-3_mean'] = np.round(means, 2)
    df1[ncd_name + '_Month1-3_lb'] = np.round(lower_bounds, 2)
    df1[ncd_name + '_Month1-3_ub'] = np.round(upper_bounds, 2)

    total_excess_46 = sum(baseline[7:10])
    values = distribution[:, np.newaxis] * total_excess_46

    means = [np.mean(v) for v in values]
    lower_bounds = [np.percentile(v, 2.5) for v in values]
    upper_bounds = [np.percentile(v, 97.5) for v in values]

    df1[ncd_name + '_Month4-6_mean'] = np.round(means, 2)
    df1[ncd_name + '_Month4-6_lb'] = np.round(lower_bounds, 2)
    df1[ncd_name + '_Month4-6_ub'] = np.round(upper_bounds, 2)

    total_excess = sum(baseline[4:10])
    values = distribution[:, np.newaxis] * total_excess

    means = [np.mean(v) for v in values]
    lower_bounds = [np.percentile(v, 2.5) for v in values]
    upper_bounds = [np.percentile(v, 97.5) for v in values]

    df1[ncd_name + '_All_mean'] = np.round(means, 2)
    df1[ncd_name + '_All_lb'] = np.round(lower_bounds, 2)
    df1[ncd_name + '_All_ub'] = np.round(upper_bounds, 2)

    df1.insert(0, 'Age', ncd_d['Age'])

    # with pd.ExcelWriter('Output_Final/' + ncd_name + '13-46M_by_age.xlsx', engine='xlsxwriter') as writer:

    # df1.to_excel(writer, sheet_name=ncd_name)

    with pd.ExcelWriter(
            'outputs/' + ncd_name + '/baseline_total_13-46M_by_age_.xlsx',
            engine='xlsxwriter') as writer:
        df1.to_excel(writer, sheet_name=ncd_name)


def cum_1346_by_age_excess(ncd_name, excess):
    distribution = ncd_d[ncd_name + '_Age_Distribution']

    df1 = pd.DataFrame()

    for s in ['Escalation', 'Status Quo', 'Ceasefire']:
        total_excess_13 = sum(excess[s][4:7])
        values = distribution[:, np.newaxis] * total_excess_13

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month1-3_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month1-3_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month1-3_ub'] = np.round(upper_bounds, 2)

        total_excess_46 = sum(excess[s][7:10])
        values = distribution[:, np.newaxis] * total_excess_46

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month4-6_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month4-6_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month4-6_ub'] = np.round(upper_bounds, 2)

        total_excess = sum(excess[s][4:10])
        values = distribution[:, np.newaxis] * total_excess

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_All_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_All_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_All_ub'] = np.round(upper_bounds, 2)

    df1.insert(0, 'Age', ncd_d['Age'])

    # with pd.ExcelWriter('Output_Final/' + ncd_name + '13-46M_by_age.xlsx', engine='xlsxwriter') as writer:

    # df1.to_excel(writer, sheet_name=ncd_name)

    with pd.ExcelWriter(
            'outputs/' + ncd_name + '/excess_death_13-46M_by_age_.xlsx',
            engine='xlsxwriter') as writer:
        df1.to_excel(writer, sheet_name=ncd_name)


def cum_1346_by_age_all_ncds(ncd_name, total_ncd):
    distribution = ncd_d[ncd_name + '_Age_Distribution']

    df1 = pd.DataFrame()

    for s in ['Escalation', 'Status Quo', 'Ceasefire']:
        total_excess_13 = sum(total_ncd[s][4:7])
        values = distribution[:, np.newaxis] * total_excess_13

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month1-3_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month1-3_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month1-3_ub'] = np.round(upper_bounds, 2)

        total_excess_46 = sum(total_ncd[s][7:10])
        values = distribution[:, np.newaxis] * total_excess_46

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_Month4-6_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_Month4-6_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_Month4-6_ub'] = np.round(upper_bounds, 2)

        total_excess = sum(total_ncd[s][4:10])
        values = distribution[:, np.newaxis] * total_excess

        means = [np.mean(v) for v in values]
        lower_bounds = [np.percentile(v, 2.5) for v in values]
        upper_bounds = [np.percentile(v, 97.5) for v in values]

        df1[ncd_name + s + '_All_mean'] = np.round(means, 2)
        df1[ncd_name + s + '_All_lb'] = np.round(lower_bounds, 2)
        df1[ncd_name + s + '_All_ub'] = np.round(upper_bounds, 2)

    df1.insert(0, 'Age', ncd_d['Age'])

    # with pd.ExcelWriter('Output_Final/' + ncd_name + '13-46M_by_age.xlsx', engine='xlsxwriter') as writer:

    # df1.to_excel(writer, sheet_name=ncd_name)

    with pd.ExcelWriter('outputs/Total_ncd_1346M.xlsx',
                        engine='xlsxwriter') as writer:
        df1.to_excel(writer, sheet_name=ncd_name)


ndc_data_dict = {}
for s in ['Escalation', 'Status Quo', 'Ceasefire']:
    ndc_data_s = {}
    for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
        excel_file = 'outputs/Raw/scenario_excess_' + ncd_name + '.xlsx'
        xls = pd.ExcelFile(excel_file)
        df = pd.read_excel(xls, s, header=None)
        ndc_data_s[ncd_name] = df.values.T
    ndc_data_dict[s] = ndc_data_s

total_ncd = {}
for s in ['Escalation', 'Status Quo', 'Ceasefire']:
    total = []
    for run in range(1000):
        per_run = np.zeros(10)
        for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
            per_run += ndc_data_dict[s][ncd_name][run]

        total.append(per_run)

    total_ncd[s] = np.array(total).T

# Generate Simulation Results
for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
    baseline = np.genfromtxt('TI Model Out/baseline_' + ncd_name + '.csv', delimiter=',')
    cum_1346_by_age_base(ncd_name, baseline)

for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
    excel_file = 'outputs/Raw/scenario_total_%s.xlsx' % ncd_name
    xls = pd.ExcelFile(excel_file)
    # Initialize an empty dictionary to store your arrays
    res_total = {}

    # Iterate through each sheet in the Excel file
    for sheet_name in xls.sheet_names:
        # Read the sheet into a DataFrame
        df = pd.read_excel(xls, sheet_name, header=None)

        # Convert the DataFrame into a numpy array and store it in the dictionary
        res_total[sheet_name] = df.values

    cum_1346_by_age_sce(ncd_name, res_total)

for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
    excel_file = 'outputs/Raw/scenario_excess_' + ncd_name + '.xlsx'
    xls = pd.ExcelFile(excel_file)
    # Initialize an empty dictionary to store your arrays
    excess = {}

    # Iterate through each sheet in the Excel file
    for sheet_name in xls.sheet_names:
        # Read the sheet into a DataFrame
        df = pd.read_excel(xls, sheet_name, header=None)

        # Convert the DataFrame into a numpy array and store it in the dictionary
        excess[sheet_name] = df.values

    cum_1346_by_age_excess(ncd_name, excess)

cum_1346_by_age_all_ncds(ncd_name, total_ncd)

ndc_data_dict = {}
for s in ['Escalation', 'Status Quo', 'Ceasefire']:
    ndc_data_s = {}
    for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
        excel_file = 'Output/Raw/scenario_excess_' + ncd_name + '.xlsx'
        xls = pd.ExcelFile(excel_file)
        df = pd.read_excel(xls, s, header=None)

        distribution = ncd_d[ncd_name + '_Age_Distribution']
        ndc_data_s_a = {}
        for a in range(len(distribution)):
            ndc_data_s_a[ncd_d['Age'][a]] = df.values.T * distribution[a]
        ndc_data_s[ncd_name] = ndc_data_s_a
    ndc_data_dict[s] = ndc_data_s

df_age_total = pd.DataFrame()

for s in ['Ceasefire', 'Status Quo', 'Escalation']:
    total = []
    age_all = []
    for a in ncd_d['Age']:
        total_age = []
        for run in range(1000):
            per_run = np.zeros(10)
            for ncd_name in ['CKD', 'IHD', 'HS', 'IS', 'Bcancer', 'Ccancer', 'Lcancer', 'DM1']:
                per_run += ndc_data_dict[s][ncd_name][a][run]
            total_age.append(per_run)

        m_13 = sum(np.array(total_age).T[4:7])

        means = np.mean(m_13)
        lower_bounds = np.percentile(m_13, 2.5)
        upper_bounds = np.percentile(m_13, 97.5)

        res13 = f"{means:.0f} ({lower_bounds:.0f} to {upper_bounds:.0f})"

        m_46 = sum(np.array(total_age).T[7:10])

        means = np.mean(m_46)
        lower_bounds = np.percentile(m_46, 2.5)
        upper_bounds = np.percentile(m_46, 97.5)

        res46 = f"{means:.0f} ({lower_bounds:.0f} to {upper_bounds:.0f})"

        m_total = sum(np.array(total_age).T[4:10])

        means = np.mean(m_total)
        lower_bounds = np.percentile(m_total, 2.5)
        upper_bounds = np.percentile(m_total, 97.5)

        restotal = f"{means:.0f} ({lower_bounds:.0f} to {upper_bounds:.0f})"

        age_all.extend(np.repeat(a, 3))
        total.extend([res13, res46, restotal])
    df_age_total['AGE'] = age_all
    df_age_total[s] = total

df_age_total.to_excel('outputs/Total_ncd_Age.xlsx')

