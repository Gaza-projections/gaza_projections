function ncd_simulation()

set(0,'defaultAxesFontSize',18)
% Before run the main simulation please make sure you change the name into valid name for MATLAB
% DATA
Scenario_names = {'Escalation','Status Quo','Ceasefire'};
months         = {'23 Oct','23 Nov','23 Dec','24 Jan','24 Feb','24 Mar','24 Apr','24 May','24 Jun','24 Jul'};
Nt             = length(months); % total simulation period = 4 (current war till Feb) and 6 mo projection period
periods        = {'Months 1-3 (Feb-Apr)','Months 4-6 (May-Jul)'}; % two main periods 

%-------------------- IMPORT NCD BASELINE DEATH ---------------------------
ts_past        = (2015:2022)';                     % years of death data
Ds_past        = {
    'CKD',    [NaN NaN 107 153 169 192 211  233 ]  % CKD
    'IHD',    [NaN NaN NaN NaN NaN NaN 1173 1532]  % IHD
    'HS',     [NaN NaN 312 382 329 336 310  351 ]  % HS
    'IS',     [NaN NaN 255 313 269 275 253  287 ]  % IS
    'Bcancer',[182 147 159 192 195 213 210  216 ]  % Bcancer
    'Lcancer',[270 264 261 311 278 294 293  287 ]  % Lcancer
    'Ccancer',[45  43  36  40  49  44  51   51  ]  % Ccancer
    };

%----------------- IMPORT COVERAGE RATE OVER TIME -------------------------
cs_names   = {'CKD','CVD','Cancer','DM'};

filePath = 'inputs/NCD_M2_model_para_1.26.2024.xlsx';
sheetName = 'coverage rate'; % Specify the sheet name

% Read the entire sheet
dataTable = readtable(filePath, 'Sheet', sheetName);

% Initialize cell arrays for LBs and UBs
cs_all_LBs = cell(4, 1); 
cs_all_UBs = cell(4, 1);

for i = 1:4
    
    row_name = 'Worst' + "_" + cs_names(i) + "_" + 'lb';
    r1 = dataTable.(row_name);
    row_name = 'Central' + "_" + cs_names(i) + "_" + 'lb';
    r2 = dataTable.(row_name);
    row_name = 'Best' + "_" + cs_names(i) + "_" + 'lb';
    r3 = dataTable.(row_name);
    cs_all_LBs{i} = [r1, r2, r3];
    
end

for i = 1:4
    
    row_name = 'Worst' + "_" + cs_names(i) + "_" + 'ub';
    r1 = dataTable.(row_name);
    row_name = 'Central' + "_" + cs_names(i) + "_" + 'ub';
    r2 = dataTable.(row_name);
    row_name = 'Best' + "_" + cs_names(i) + "_" + 'ub';
    r3 = dataTable.(row_name);
    cs_all_UBs{i} = [r1, r2, r3];
    
end

%----------------- LINK EACH NCD WITH ITS CATEGORY ------------------------

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

%-------------------- PREPARE FOR THE MAIN SIMULATION ---------------------
% Choose the NCD for simulation
k_NCD_categories = 8;
% name of the NCD
NCD              = NCD_catagories{k_NCD_categories,1}; 

% Read Harzard Rate over time
filename = 'inputs/HR_new_logistic_normal.xlsx'; % pre-calculated HR (tau = 0-500mo)
sheetName = NCD;
HR_ALL_t = readtable(filename, 'Sheet', sheetName);

HR_tlb  = HR_ALL_t.HR_t_tlb;
HR_tub  = HR_ALL_t.HR_t_tub;
HR_utlb = HR_ALL_t.HR_t_utlb;
HR_utub = HR_ALL_t.HR_t_utub;

% In the simulation, we have two strategies:
% For DM1 we start the simulation from Prevalence = 2023 OCT DM1 prevalence
% For other NCDs, we start the simulation from Prevalence = 0
if strcmp(NCD,'DM1')
    flag_single_cohort = 1;                            
else
    flag_single_cohort = 0;
end

% Read Treatment Coverage Rate for NCD
NCD_category = NCD_catagories{k_NCD_categories,2};          % category of the NCD
k_cs         = find(string(cs_names)==string(NCD_category));
cs_all_LB    = cs_all_LBs{k_cs};                            % coverage
cs_all_UB    = cs_all_UBs{k_cs};

% Linear Regression to get 2023 Baseline Death
n_Ds_past = length(Ds_past);
for k=1:n_Ds_past
    Ds_past_names{k} = Ds_past{k,1};
end

k_Ds_past   = find(string(Ds_past_names)==NCD);
D_past      = Ds_past{k_Ds_past,2}';                        % death rate
D_keep      = ~isnan(D_past);                               % keep only the good dates
D_past      = D_past( D_keep);
t_past      = ts_past(D_keep);                              % keep the corresponding years
n_past      = length(t_past);
D_regress   = regress(D_past,[ones(n_past,1) t_past]);      % regress
D_2023      = D_regress(1) + 2023*D_regress(2);             % use regression to predict 2023

% Visualize the baseline death trend
flag_plot_D_past = 0;                                       % set to 1 to plot the deaths
if flag_plot_D_past
    fig = figure(805); fig.Name = 'Deaths past';
    plot(t_past,D_past,'bo-'), xlabel('year'), ylabel('deaths')
    hold on, plot(2023,D_2023,'r*',t_past,D_regress(1)+D_regress(2)*t_past), hold off
    title(sprintf('%s, estimate for 2023: %.0f',fig_ttl,D_2023))
    keyboard
end

% Dummy Incidence Rate to align simulated baseline death with actual baseline death
% Nsims (monthly incidence mean) need to be modified so the D_mult is approximately 1
Nsims  = {'CKD',25;'IHD',473;'HS',58;'IS',26;'Bcancer',30;'Ccancer',6;'Lcancer',25;'DM1',2947}; 
n_Nsims = size(Nsims,1);
for k=1:n_Nsims
    Nsims_names{k} = Nsims{k,1};             % collect the names corresponding to each Nsim
end

k_Nsims = find(string(Nsims_names)==string(NCD));           % get Nsim
Nsim    = round(Nsims{k_Nsims,2});


%-------------------- SIMULATION FOR THREE SCENARIOS ----------------------
% Simulation parameters
% number of simulation runs
nsim = 100;

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
    [D_mean0, D_mean_BL0, D_C0, D_BL0, D_exc0] = f_D(nt,tC,Nsim,nsim,...
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

% Save all runs results for Baseline
filename = sprintf('outputs/Raw/baseline_%s.csv', NCD);
writematrix(D_C_BL, filename);

% Save all runs results for Total death by scenarios
filename = sprintf('outputs/Raw/scenario_total_%s.xlsx', NCD);
% Loop through each cell in the cell array
for i = 1:length(D_C_total)
    % Write each table to a different sheet named 'Sheet1', 'Sheet2', ...
    writematrix(D_C_total{i}, filename, 'Sheet', Scenario_names{i});
end

% Save all runs results for Excess Death by scenarios
filename = sprintf('outputs/Raw/scenario_excess_%s.xlsx', NCD);
% Loop through each cell in the cell array
for i = 1:length(D_C_excs)
    % Write each table to a different sheet named 'Sheet1', 'Sheet2', ...
    writematrix(D_C_excs{i}, filename, 'Sheet', Scenario_names{i});
end



%-------------------- MAIN SIMULATION FUNCTION ----------------------------
function [D_mean, D_mean_BL, D_C, D_BL, D_exc] = f_D(...
    nt,tC,Nsim,nsim,cs_all_LB,cs_all_UB,flag_single_cohort,k_Scenario,fig_no,fig_ttl, ...
        HR_tlb,HR_tub,HR_utlb,HR_utub)

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
    

    for t0=1:nt               % for all incidence

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

