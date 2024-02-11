function simple_simulation()

set(0,'defaultAxesFontSize',18)

% DATA
Scenario_names = {'Escalation','Status Quo','Ceasefire'};
months         = {'23 Oct','23 Nov','23 Dec','24 Jan',...
    '24 Feb','24 Mar','24 Apr','24 May','24 Jun','24 Jul'};
Nt             = length(months);
periods        = {'Months 1-3 (Feb-Apr)','Months 4-6 (May-Jul)'};
ts_past        = (2015:2022)';                     % years of death data
Ds_past        = {
    'CKD',    [NaN NaN 107 153 169 192 211  233 ]  % CKD
    'IHD',    [NaN NaN NaN NaN NaN NaN 1173 1532]  % IHD
    'HS',     [NaN NaN 312 382 329 336 310  351 ]  % HS
    'IS',     [NaN NaN 255 313 269 275 253  287 ]  % IS
    'Bcancer',[182 147 159 192 195 213 210  216 ]  % Bcancer
    'Lcancer',[270 264 261 311 278 294 293  287 ]  % Lcancer
    'Ccancer',[45  43  36  40  49  44  51   51  ]  % Ccancer
    'DM1',    [0   0   0   0   0   0   0    0   ]  % DM1
    };

age_distribution = struct(...
    'CKD',     [0.000, 0.000, 0.001, 0.002, 0.004, 0.008, 0.013, 0.009, 0.015, 0.031, 0.043, 0.029, 0.107, 0.107, 0.117, 0.103, 0.137, 0.274, 1.000], ...
    'Bcancer', [0, 0, 0, 0, 0.001, 0.003, 0.010, 0.028, 0.056, 0.071, 0.102, 0.132, 0.134, 0.120, 0.103, 0.090, 0.077, 0.073, 1.000], ...
    'Ccancer', [0, 0, 0.001, 0.001, 0.003, 0.005, 0.009, 0.012, 0.019, 0.033, 0.055, 0.095, 0.112, 0.116, 0.138, 0.138, 0.123, 0.141, 1.000], ...
    'Lcancer', [0, 0, 0, 0.001, 0.003, 0.004, 0.006, 0.010, 0.016, 0.028, 0.057, 0.095, 0.125, 0.147, 0.146, 0.139, 0.122, 0.101, 1.000], ...
    'DM2',     [0.000, 0.000, 0.000, 0.000, 0.000, 0.001, 0.002, 0.002, 0.004, 0.009, 0.019, 0.036, 0.073, 0.102, 0.129, 0.154, 0.166, 0.299, 1.000], ...
    'IHD',     [0.000, 0.000, 0.000, 0.000, 0.001, 0.002, 0.003, 0.006, 0.008, 0.018, 0.034, 0.050, 0.078, 0.094, 0.099, 0.115, 0.130, 0.361, 1.000], ...
    'COPD',    [0.000, 0.003, 0.000, 0.001, 0.001, 0.000, 0.001, 0.003, 0.006, 0.012, 0.015, 0.044, 0.085, 0.094, 0.115, 0.135, 0.139, 0.345, 1.000], ...
    'IS',      [0.000, 0.000, 0.000, 0.000, 0.000, 0.001, 0.002, 0.002, 0.004, 0.008, 0.012, 0.026, 0.044, 0.064, 0.079, 0.129, 0.156, 0.475, 1.000], ...
    'HS',      [0.000, 0.000, 0.000, 0.000, 0.000, 0.001, 0.002, 0.002, 0.004, 0.008, 0.012, 0.026, 0.044, 0.064, 0.079, 0.129, 0.155, 0.475, 1.000], ...
    'DM1',     [0.007	0.028	0.030	0.029	0.102	0.083	0.083	0.075	0.055	0.120	0.100	0.075	0.068	0.049	0.040	0.029	0.016	0.012	1.000]);
age_groups = {'<1', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80', 'Total'};
n_age      = length(age_groups);

cs_names   = {'CKD','CVD','Cancer','DM'};

cs_all_LBs = {              % Oct-Jul
    [                       % CKD
    0.8	0.8	0.8
    0.4	0.4	0.4
    0.3	0.3	0.3
    0.3	0.3	0.3
    0.05	0.15	0.5
    0.05	0.15	0.5
    0.05	0.15	0.5
    0.05	0.15	0.6
    0.05	0.15	0.6
    0.05	0.15	0.6
    ],
    [                       % CVD
    0.5	0.5	0.5
    0.2	0.2	0.2
    0.1	0.1	0.1
    0.1	0.1	0.1
    0.05	0.1	0.3
    0.05	0.1	0.3
    0.05	0.1	0.3
    0.05	0.1	0.4
    0.05	0.1	0.4
    0.05	0.1	0.4 ],
    [                       % Cancer
    0.3	0.3	0.3
    0.01	0.01	0.01
    0.01	0.01	0.01
    0.01	0.01	0.01
    0.01	0.01	0.2
    0.01	0.01	0.2
    0.01	0.01	0.2
    0.01	0.01	0.4
    0.01	0.01	0.4
    0.01	0.01	0.4
    ],
    [                       % DM
    0.9	0.9	0.9
    0.9	0.9	0.9
    0.9	0.9	0.9
    0.9	0.9	0.9
    0.6	0.7	0.9
    0.6	0.7	0.9
    0.6	0.7	0.9
    0.6	0.7	0.9
    0.6	0.7	0.9
    0.6	0.7	0.9
    ]
    };

cs_all_UBs = {              % Oct-Jul
    [                       % CKD
    0.8	0.8	0.8
    0.4	0.4	0.4
    0.3	0.3	0.3
    0.3	0.3	0.3
    0.05	0.2	0.5
    0.05	0.2	0.5
    0.05	0.2	0.5
    0.05	0.2	0.6
    0.05	0.2	0.6
    0.05	0.2	0.6
    ],
    [                       % CVD
    0.5	0.5	0.5
    0.2	0.2	0.2
    0.1	0.1	0.1
    0.1	0.1	0.1
    0.05	0.1	0.3
    0.05	0.1	0.3
    0.05	0.1	0.3
    0.05	0.1	0.4
    0.05	0.1	0.4
    0.05	0.1	0.4 ],
    [                       % Cancer
    0.3	0.3	0.3
    0.05	0.05	0.05
    0.05	0.06	0.06
    0.05	0.07	0.07
    0.05	0.05	0.4
    0.05	0.05	0.4
    0.05	0.05	0.4
    0.05	0.05	0.5
    0.05	0.05	0.5
    0.05	0.05	0.5
    ],
    [                       % DM
    1	1	1
    1	1	1
    1	1	1
    1	1	1
    0.7	0.8	1
    0.7	0.8	1
    0.7	0.8	1
    0.7	0.8	1
    0.7	0.8	1
    0.7	0.8	1
    ]
    };


NCD_params = { % HS is the only one with different BL values, 
    {'CKD',...
    [100, 100, 0.0151],[100, 100, 0.0151],[100, 100, 0.0301],[100, 100, 0.0283]},
    {'Bcancer',...
    [100, 100, 0.0037],[100, 100, 0.0037],[100, 100, 0.0067],[100, 100, 0.0067]},
    {'Ccancer',...
    [100, 100, 0.0098],[100, 100, 0.0098],[100, 100, 0.0118],[100, 100, 0.0118]},
    {'Lcancer',...
    [100, 100, 0.0194],[100, 100, 0.0194],[100, 100, 0.0399],[100, 100, 0.0399]},
    {'IHD',...
    [94.3, 94, 0.0079],[97.2, 94, 0.0079],[60,  100, 0.0108],[60,  100, 0.0108]},
    {'HS',...
    [68,   50, 0.0075],[68,   70, 0.0075],[51,   42, 0.0079],[51,   42, 0.0079]},
    {'IS',...
    [93,   88, 0.0084],[93,   88, 0.0084],[88,   82, 0.0135],[88,   82, 0.0135]},
    {'DM1',...
    [100,100,1/75.6/12],[100,100,1/75.6/12],[100,100,1/1.3/12],[100,100,1/2.6/12]}
    };
% 75.6 here is the average life expectancy for the gaza population



NCD_catagories = {
    'DM1',    'DM'     % 1 
    'DM2',    'DM'
    'CKD',    'CKD'    % 3 
    'IHD',    'CVD'    % 4 
    'CDIS',   'CVD'    % 5
    'HD',     'CVD'
    'HS',     'CVD'
    'IS',     'CVD'    % 8
    'COPD',   'COPD'
    'Bcancer','Cancer' % 10
    'Ccancer','Cancer'
    'Lcancer','Cancer' % 12
    };

% Choose the NCD using the 1st column of NCD_categories
k_NCD_categories = 4;
NCD              = NCD_catagories{k_NCD_categories,1}; % name of the NCD

% Read all HR by time
filename = 'HR_new_logistic_normal.xlsx';
sheetName = NCD;
HR_ALL_t = readtable(filename, 'Sheet', sheetName);

HR_tlb  = HR_ALL_t.HR_t_tlb;
HR_tub  = HR_ALL_t.HR_t_tub;
HR_utlb = HR_ALL_t.HR_t_utlb;
HR_utub = HR_ALL_t.HR_t_utub;

if strcmp(NCD,'DM1')
    flag_single_cohort = 1;                            % put prevalence into one cohort
else
    flag_single_cohort = 0;
end

% collect names of NCDs from each dictionary
n_NCD_params = length(NCD_params);
for k=1:n_NCD_params
    NCD_params_names{k} = NCD_params{k}{1};
end

n_Ds_past = length(Ds_past);
for k=1:n_Ds_past
    Ds_past_names{k} = Ds_past{k,1};
end

% Nsims need to be modified so the D_mult is approximately 1 (see below)
Nsims  = {'CKD',25;'IHD',473;'HS',29;'IS',24;'Bcancer',30;'Ccancer',6;...
    'Lcancer',25;'DM1',2947}; % array of Nsim (number per cohort)
n_Nsims = size(Nsims,1);
for k=1:n_Nsims
    Nsims_names{k} = Nsims{k,1};             % collect the names corresponding to each Nsim
end

% Extract the data and other information
NCD_category = NCD_catagories{k_NCD_categories,2};          % category of the NCD
k_NCD_params = find(string(NCD_params_names)==string(NCD));
h0           = NCD_params{k_NCD_params}{2}(end);            % hazard rates
hL           = NCD_params{k_NCD_params}{4}(end);
hU           = NCD_params{k_NCD_params}{5}(end);
hAs          = zeros(4,2);                                  % acute rates
for j=1:4
    hAs(j,:) = NCD_params{k_NCD_params}{j+1}(1:2);
end

k_cs         = find(string(cs_names)==string(NCD_category));
cs_all_LB    = cs_all_LBs{k_cs};                            % coverage
cs_all_UB    = cs_all_UBs{k_cs};

k_Ds_past   = find(string(Ds_past_names)==NCD);
D_past      = Ds_past{k_Ds_past,2}';                        % death rate
D_keep      = ~isnan(D_past);                               % keep only the good dates
D_past      = D_past( D_keep);
t_past      = ts_past(D_keep);                              % keep the corresponding years
n_past      = length(t_past);
D_regress   = regress(D_past,[ones(n_past,1) t_past]);      % regress
D_2023      = D_regress(1) + 2023*D_regress(2);             % use regression to predict 2023

flag_plot_D_past = 0;                                       % set to 1 to plot the deaths
if flag_plot_D_past
    fig = figure(805); fig.Name = 'Deaths past';
    plot(t_past,D_past,'bo-'), xlabel('year'), ylabel('deaths')
    hold on, plot(2023,D_2023,'r*',t_past,D_regress(1)+D_regress(2)*t_past), hold off
    title(sprintf('%s, estimate for 2023: %.0f',fig_ttl,D_2023))
    keyboard
end

k_Nsims = find(string(Nsims_names)==string(NCD));           % get Nsim
Nsim    = round(Nsims{k_Nsims,2});

% Simulation parameters
% number of simulation runs
nsim = 1000;        
if flag_single_cohort
    tC   = 1;                % DM1 start with everyone in the initial cohort
else
    tC   = round(12*30);     % time of conflict (after burn-in)
end
tnow = 5;                    % from start of conflict to current month
nt   = tC + tnow + 6 + 1;    % use larger number to see long-term trends

for k_Scenario=1:3                                  % 1-worst, 2-central, 3-best
    fig_ttl = sprintf('%s, %s',NCD,Scenario_names{k_Scenario});
    fig_no  = 809+k_Scenario;
    [D_mean0, D_mean_BL0, D_C0, D_BL0, D_exc0] = f_D(nt,tC,Nsim,nsim,h0,hL,hU,hAs,...
        cs_all_LB,cs_all_UB,flag_single_cohort,k_Scenario,fig_no,fig_ttl, ...
        HR_tlb,HR_tub,HR_utlb,HR_utub);
    if flag_single_cohort
        D_mult = 1;                                 % prevalance in t0=1 cohort
    else
        D_mult   = D_2023/mean(D_mean_BL0)/12;      % see fprint message below
        fprintf('D_mult = %.2f IF THIS IS NOT 1.0 THEN MULTIPLY Nsim BY D_mult\n',D_mult)
%       keyboard
    end
    D_mean   = D_mean0   *D_mult;                   % scale the results
    D_mean_BL= D_mean_BL0*D_mult;
    D_C      = D_C0      *D_mult;
    D_C_BL   = D_BL0     *D_mult;
    D_C_exc  = D_exc0    *D_mult;
    D_Cs     = sort(D_C,2);                         % sort over simulations for each time t
    D_C_BLs  = sort(D_C_BL,2);                      % sorted values used to find the CI
    D_C_CI   = max(0,D_Cs(:,   round(nsim*[0.025 0.975]))); % get the CI
    D_C_BL_CI= max(0,D_C_BLs(:,round(nsim*[0.025 0.975])));
    D_exc{k_Scenario} = D_mean - D_mean_BL;         % excess deaths
    % D_C_exc{k_Scenario}= D_C - D_C_BL;            % same result
    D_C_total{k_Scenario} = D_C;                    % total death for each scenario
    D_C_excs{k_Scenario} = sort(D_C_exc,2);
    D_exc_CI{k_Scenario} = max(0,D_C_excs{k_Scenario}(:,round(nsim*[0.025 0.975])));
end

filename = sprintf('TI Model Out/baseline_%s.csv', NCD);
writematrix(D_C_BL, filename);
filename = sprintf('TI Model Out/scenario_total_%s.xlsx', NCD);
% Loop through each cell in the cell array
for i = 1:length(D_C_total)
    % Write each table to a different sheet named 'Sheet1', 'Sheet2', ...
    writematrix(D_C_total{i}, filename, 'Sheet', Scenario_names{i});
end

filename = sprintf('TI Model Out/scenario_excess_%s.xlsx', NCD);
% Loop through each cell in the cell array
for i = 1:length(D_C_excs)
    % Write each table to a different sheet named 'Sheet1', 'Sheet2', ...
    writematrix(D_C_excs{i}, filename, 'Sheet', Scenario_names{i});
end





Y = [D_exc{1} D_exc_CI{1} D_exc{2} D_exc_CI{2} D_exc{3} D_exc_CI{3} D_mean_BL D_C_BL_CI];
fprintf('\n----- %s: Monthly baseline death -----\n',NCD)
fprintf('month, Baseline, lower CI, upper CI\n')
for t=1:Nt
    fprintf('%s, %6.2f, %6.2f, %6.2f\n',...
    months{t},Y(t,end-2:end))
end
fprintf('\n----- %s: Monthly excess death -----\n',NCD)
fprintf('month, Escalation,  Escalation lower CI,  Escalation upper CI, Status Quo, Status Quo lower CI, Status Quo upper CI, Ceasefire, Ceasefire lower CI, Ceasefire upper CI\n')
for t=1:Nt
    fprintf('%s, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f\n',...
    months{t},Y(t,1:end-3))
end
Y_periods(1,:) = sum(Y(tnow:tnow+2,  [1:3:3*3]));
Y_periods(2,:) = sum(Y(tnow+3:tnow+5,[1:3:3*3]));

for t_period=1:2
    fprintf('\n----- %s: Excess death by age, %s -----\n',NCD,periods{t_period})
    fprintf('age, Escalation, Escalation lower CI, Escalation upper CI, Status Quo, Status Quo lower CI, Status Quo upper CI, Ceasefire, Ceasefire lower CI, Ceasefire upper CI\n')
    for a=1:n_age
        age_dist = getfield(age_distribution,NCD);
        age_D    = Y_periods(t_period,:)*age_dist(a);
        age_D_CI = zeros(3);
        for j=1:3
            Dj   = age_D(j);
            age_D_CI(:,j) = max(0,...
                [Dj (Dj-1.96*sqrt(Dj)) (Dj+1.96*sqrt(Dj))]);
        end
        fprintf('%s, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f\n',...
            age_groups{a},age_D_CI)
    end
end

 keyboard


function [D_mean, D_mean_BL, D_C, D_BL, D_exc] = f_D(...
    nt,tC,Nsim,nsim,h0,hL,hU,hAs,cs_all_LB,cs_all_UB,flag_single_cohort,k_Scenario,fig_no,fig_ttl, ...
        HR_tlb,HR_tub,HR_utlb,HR_utub)

%----------------- SIMPLE HAZARD ----------------------------
HRL    = hL/h0;                % hazard ratio lower
HRU    = hU/h0;                %              upper

nc     = size(cs_all_LB,1);    % number of months in cs_all
cs_LB  = zeros(nt,1);
cs_UB  = zeros(nt,1);
cs_LB(tC:tC+nc-1) = cs_all_LB(:,  k_Scenario);
cs_UB(tC:tC+nc-1) = cs_all_UB(:,  k_Scenario);
cs_LB(1:tC-1)     = 1;                          % baseline
cs_UB(1:tC-1)     = 1;
cs_LB(tC+nc:end)  = cs_all_LB(end,k_Scenario);  % final value
cs_UB(tC+nc:end)  = cs_all_UB(end,k_Scenario);

Psim    = zeros(nt,nsim);
Dsim    = zeros(nt,nsim);
P_BLsim = zeros(nt,nsim);
D_BLsim = zeros(nt,nsim);
D_excsim= zeros(nt,nsim);

for k=1:nsim
    % Print progress
    D   = zeros(nt,1);  % deaths at every time step, D(nt) is at the start of the war
    D_BL= zeros(nt,1);
    D_exc = zeros(nt,1);

    if flag_single_cohort
        P    = zeros(nt); P(1) = Nsim;
        P_BL = P;
    else
        P    = eye(nt)*Nsim; % incidence at t=t0, assume the same every month
        P_BL = eye(nt)*Nsim; % Nsim is the number of people in each cohort, need BL cohort
    end

    cs  = cs_LB + rand*(cs_UB - cs_LB);  % coverage  ~ U(cs_LB,cs_UB)
    
%     hs  = h0*(cs + HR*(1-cs));           % hazard rate: cs=1 => h = h0, cs=0 => h = h0*HR
    
%     hAt  = 1 - (hAs(1, 1) + rand*diff(hAs(1:2, 1)))/100;
%     hAut = 1 - (hAs(3, 1) + rand*diff(hAs(3:4, 1)))/100;
%     
%     hA = hAt*cs + hAut*(1-cs);
%     
%     mAt  = 1 - (hAs(1, 2) + rand*diff(hAs(1:2, 2)))/100;
%     mAut = 1 - (hAs(3, 2) + rand*diff(hAs(3:4, 2)))/100;
%     
%     mA = mAt * cs + mAut * (1-cs);
    
    %hA_BL = 1 - (hAs(1,1)            + rand*diff(hAs(1,:))           )/100;  % acute hazard BL
    %hA    = 1 - (hAs(k_Scenario+1,1) + rand*diff(hAs(k_Scenario+1,:)))/100;

    for t0=1:nt               % for all incidence
        % begin by subtracting the deaths from acute hazard
%         r     = rand(Nsim,1);
%         dA    = 0;
%         dA_BL = 0;
%         if hA>0               % acute hazard > 0
%             dA       = sum(r<hA(t0));
%             D(t0)    = D(t0)    + dA;
%             P(t0,t0) = P(t0,t0) - dA;
%         end
%         if hAt>0            % acute hazard at BL > 0
%             dA_BL       = sum(r<hAt);
%             D_BL(t0)    = D_BL(t0)    + dA_BL;
%             P_BL(t0,t0) = P_BL(t0,t0) - dA_BL;
%         end
%         D_exc(t0) = D_exc(t0) + dA - dA_BL;      % excess in this cohort (same as D_BL - D)

        for t=t0:nt                              % for every time step after t0
            
            cs_t = cs(t);
 
            HR_t_tlb  = HR_tlb(t-t0+1);
            HR_t_tub  = HR_tub(t-t0+1);
            HR_t_utlb = HR_utlb(t-t0+1);
            HR_t_utub = HR_utub(t-t0+1);
            
            
            HR_with  = HR_t_tlb   + rand*(HR_t_tlb   - HR_t_tub);
            HR_without  = HR_t_utlb   + rand*(HR_t_utlb   - HR_t_utub);
            
            h0 = HR_with;
            h = cs_t * HR_with + (1-cs_t) * HR_without;  % hazard rate at t
            
            h = max(h0, h);
                                    
            nP   = P(   t0,t);                   % number of people in cohort t0
            nP_BL= P_BL(t0,t);                   % number of people in the BL cohort
            nh   = max(nP,nP_BL);                % number of people is either cohort
            d    = 0;                            % deaths in cohort t0
            d_BL = 0;                            % BL: same people, different treatment
            if nh>0                              % if anyone left in either cohort
                % same random numbers for both cohorts: important for counterfactual analysis!
                r = rand(nh,1);                  % random numbers
                if nP                            % if at least one person in the cohort
                    d       = sum(r(1:nP)   <h); % number in cohort who die
                    D(t)    = D(t) + d;          % add to deaths at time t
                end
                if nP_BL
                    d_BL    = sum(r(1:nP_BL)<h0);% number in BL cohort who die
                    D_BL(t) = D_BL(t) + d_BL;
                end
                D_exc(t) = D_exc(t) + d - d_BL;  % excess in this cohort (same as D_BL - D)
            else
                continue                         % go to next cohort if no one left
            end
            if t<nt
                P(   t0,t+1) = P(   t0,t) - d;   % reduce cohort t0 by deaths d
                P_BL(t0,t+1) = P_BL(t0,t) - d_BL;
            end
        end
    end

    Dsim(   :,k) = D;                   % deaths over time for simulation k
    D_BLsim(:,k) = D_BL;
    D_excsim(:,k)= D_exc;
    Psim(   :,k) = sum(P)';             % prevalence
    P_BLsim(:,k) = sum(P_BL)';
end



Pavg    = mean(Psim,   2);              % average over simulations
Davg    = mean(Dsim,   2);
P_BLavg = mean(P_BLsim,2);              % average over simulations
D_BLavg = mean(D_BLsim,2);

tCs       = tC:tC+nc-1;
P_mean    = Pavg(tCs);                  % results over the the time of conflict and projection
D_mean    = Davg(tCs);
D_C       = Dsim(tCs,:);
P_mean_BL = P_BLavg(tCs);               % baseline
D_mean_BL = D_BLavg(tCs);
D_BL      = D_BLsim(tCs,:);
D_exc     = D_excsim(tCs,:);
ts  = 1:nt;                             % time
idx = ts>tC-12;                         % past 12 months before start

fig = figure(fig_no); fig.Name = fig_ttl; tile = tiledlayout(2,2);
nexttile
plot(ts(idx),Davg(idx),'b-',ts(idx),D_BLavg(idx),'r-')
xlabel('time (months)'), ylabel('Number of deaths')
nexttile
plot(ts,Davg,'b-',ts,D_BLavg,'r-'), title(sprintf('Area = %.2f',sum(D)))
nexttile
plot(ts,Pavg,'b-',ts,P_BLavg,'r-'), title('Prevalence')
nexttile
tC_cdf = tC + 3;
ecdf(D_BLsim(tC_cdf,:)), grid on, hold on
ecdf(Dsim(   tC_cdf,:))
ecdf(D_excsim(tC_cdf,:)), hold off
title('Empirical CDF'), ylabel('CDF'), xlabel('Deaths per month')

return

