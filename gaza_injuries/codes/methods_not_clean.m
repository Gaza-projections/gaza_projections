function methods_not_clean()

% INITIALIZE
rng('default')                         % initialize random number generator
% p_DOW = number of deaths in the next 6 months due to injuries in the past
% p_DOW   = f_p_DOW();                 % generate p_DOW by curve fitting
%                                        curve-fit results are below
p_DOW   = [ 8.3602 ; 5.6896 ; 3.8535 ; 2.4114 ; 1.2787 ; 0.3890 ];
m       = length(p_DOW);               % number of months for the scenario

I_counted_Dec_12 = 42804;    % MoH reports number injured counted and missing on Dec 12
I_missing_Dec_12 = 7540;     % use as a measure of uncertainty for the confidence bound
I_c_o_v          = I_missing_Dec_12/I_counted_Dec_12/1.96; % c.o.v.

N_p          = 6745;           % N used for D_p_repr, D_p_neo (Sept 19, 2023 MoH report)

n_sim        = 10000; 
I_conflict_0 = nan(1,3,n_sim); % all months same
D_im_0       = nan(1,3,n_sim);
D_DOW_0      = nan(m,3,n_sim); % each month different
D_dow_0      = nan(m,3,n_sim);
D_DOW_after_0= nan(m,3,n_sim);
D_unc_0      = nan(1,3,n_sim);
D_mines_S1_0 = nan(1,1,n_sim); % S1 only
I_mines_S1_0 = nan(1,1,n_sim); % S1 only
D_repr_0     = nan(m,3,n_sim);
D_neo_0      = nan(m,3,n_sim);
D_preg_0     = nan(m,3,n_sim);
D_preg_post_0= nan(m,3,n_sim);
D_still_0    = nan(m,3,n_sim);

I_current = 9204;         % 30-day average number of injured as of Jan 29, 2024
D_current = 4539;         %                          deaths

I_max_end = 1.79;         % ratio of maximum to current
D_max_end = 1.67;
p_mod_sev = 0.561;        % proportion of moderate and severe injuries used in CFR
p_mod_upper_sev = 0.370;  % upper half of moderate to severe (for comparison with Mosul injuries)
CFR       = [0.251 0.316 0.362];  % CFR of moderate to severe injuries for the 3 scenarios

D_D_to_I  = 206 + 132;            % number of deaths in Mosul survey
I_D_to_I  =  77 +  69;            % number of injured

flag_use_count_repr_neo = 2;      % 0-uncounted, 1-counted, 2-uncounted+DOW
flag_use_count_repr_neo_msg = ...
    {'COUNTED + UNCOUNTED','COUNTED ONLY','COUNTED + UNCOUNTED + LAGGED DOW'};
fprintf('***** D_repr, D_neo: %s *****\n',flag_use_count_repr_neo_msg{flag_use_count_repr_neo+1})

% BEGIN SIMULATION
for i=1:n_sim
    [D_a,D_p_repr,D_p_neo] = generate_D_a(N_p); % generate random ages

    % 'P' = Poisson distribution, e.g., D_mines ~ Poisson(13)
    D_mines   = lnrand(13,[],'P');      % death due to mines (2014)
    I_mines   = lnrand(56,[],'P');      % injuries

    % GENERATE INJURIES AND DEATHS FOR ONE SIMULATION
    % INPUT ARGUMENTS ARE RANDOM NUMBERS
    % 'c' = lognormal using c.o.v as input
    % 'P' = Poisson, 'p' = Poisson divided by N to get proportions
    [I_conflict_0(:,:,i),D_im_0(:,:,i),D_DOW_0(:,:,i),D_dow_0(:,:,i),D_DOW_after_0(:,:,i),D_unc_0(:,:,i),...
        D_mines_S1_0(:,:,i),I_mines_S1_0(:,:,i),D_repr_0(:,:,i),D_neo_0(:,:,i),...
        D_preg_post_0(:,:,i),D_preg_0(:,:,i),D_still_0(:,:,i)] = ...
        fDI(...
        flag_use_count_repr_neo,...
        lnrand(I_current,I_c_o_v,'c'),...
        lnrand(D_current,I_c_o_v,'c'),...
        lnrand(I_current*I_max_end,I_c_o_v,'c'),...
        lnrand(D_current*D_max_end,I_c_o_v,'c'),...
        CFR,p_mod_sev,p_mod_upper_sev,...
        p_DOW,...
        lnrand(D_mines,  [],'P'),...
        lnrand(I_mines,  [],'P'),...
        lnrand(D_p_repr,N_p,'p'),...                       % D_repr
        lnrand(D_p_neo, N_p,'p'),...                       % D_neo
        lnrand(D_p_repr*0.1134,N_p,'p'),...                % D_preg_post
        lnrand(D_p_repr*0.0972,N_p,'p'),...                % D_preg
        lnrand(D_p_repr*0.0291,N_p,'p'),...                % D_still   
        lnrand(D_D_to_I, [],'P')/lnrand(I_D_to_I,[],'P')); % D_to_I
end

% Make all results the same size array (m,3,n_sim) where m = months, 3 = number of scenarios
D_mines_S1_0 = cat(2,D_mines_S1_0,zeros(1,2,n_sim)); % deaths by mines incl in I_conflict in S2,S3
I_mines_S1_0 = cat(2,I_mines_S1_0,zeros(1,2,n_sim));
I_conflict_0 = repmat(I_conflict_0,m,1,1);
D_im_0       = repmat(D_im_0,      m,1,1);
D_unc_0      = repmat(D_unc_0,     m,1,1);
D_mines_S1_0 = repmat(D_mines_S1_0,m,1,1);
I_mines_S1_0 = repmat(I_mines_S1_0,m,1,1);

% printf USES THE n_sim SIMULATIONS TO GENERATE CIs
%        USING SORTED SIMULATIONS (SELECTING THE 2.5% and 97.5% LARGEST VALUES)
%        SIMULATION CONDUCTED FOR EACH AGE BRACKET AND GENDER EXCEPT FOR preg_post, preg, still, neo
%        SIMULATION RESULTS ARE AGGREGATED TO RECOMPUTE THE CIs FOR BOTH GENDERS AND ALL AGES
I_conflict = printf(D_a,'I_conflict', I_conflict_0, 1);      % I_conflict_0(1,3,n_sim)
D_im       = printf(D_a,'D_im',       D_im_0,       1);
D_DOW      = printf(D_a,'D_DOW',      D_DOW_0,      3);      % D_DOW_0     (6,3,n_sim)
D_dow      = printf(D_a,'D_dow',      D_dow_0,      3);
D_DOW_after= printf(D_a,'D_DOW_after',D_DOW_after_0,3);
D_unc      = printf(D_a,'D_unc',      D_unc_0,      1, 1);   % flag for D_unc, some parts won't work
D_mines_S1 = printf(D_a,'D_mines_S1', D_mines_S1_0, 0);      % D_mines_S1_0(1,n_sim)
I_mines_S1 = printf(D_a,'I_mines_S1', I_mines_S1_0, 1);
D_preg_post= printf(D_a,'D_preg_post',D_preg_post_0,3, -1);  % Don't need D_repr_0
D_preg     = printf(D_a,'D_preg',     D_preg_0,     3, -1);
D_still    = printf(D_a,'D_still',    D_still_0,    3, -1);
D_neo      = printf(D_a,'D_neo',      D_neo_0,      3, -1);

% need to repeat D_im_0 over m months (m=6) because D_im_0 is the same every month
%                D_unc_0
D_tot                     = printf(D_a,'D_tot',      D_im_0           + D_DOW_0,3);
[ D_tot_unc,Neo_tot_unc ] = printf(D_a,'D_tot',      D_im_0 + D_unc_0 + D_DOW_0,3);

times = {'Feb-24','Mar-24','Apr-24','May-24','Jun-24','Jul-24'};

% COLLECT RESULTS AND OUTPUT INTO TABLES
% Include mines for Injured: {1,3,4} = mean, lower CI, upper CI
clear X
for im=1:m
    X{im} = [ I_mines_S1{1}(1,im) I_conflict{1}(2:3,im)'
              I_mines_S1{2}(1,im) I_conflict{2}(2:3,im)'
              I_mines_S1{3}(1,im) I_conflict{3}(2:3,im)'
              I_mines_S1{4}(1,im) I_conflict{4}(2:3,im)' ];
end

% f_CI_age('Injured',X,times,D_a,0)
f_CI_a(  'Injured0', times,D_a,0,I_mines_S1,1,I_conflict,2:3);

% Include mines, residual DOW from S2 for S1, DOW from S2 for other scenarios
clear X
D_mines_S1_DOW_S2 = printf(D_a,'D_mines_S1_DOW_S2', ...
    D_mines_S1_0 + repmat(D_DOW_after_0(:,2,:),1,3,1),3);
for im=1:m
    X{im} = [ D_mines_S1_DOW_S2{1}(1,im) D_tot{1}(2:3,im)'    % only mines and DOW for S1
              D_mines_S1_DOW_S2{2}(1,im) D_tot{2}(2:3,im)'
              D_mines_S1_DOW_S2{3}(1,im) D_tot{3}(2:3,im)'
              D_mines_S1_DOW_S2{4}(1,im) D_tot{4}(2:3,im)' ]; % more narrow CIs
%   X{im} = [ D_mines_S1{1}+D_DOW_after{1}(2,im) D_DOW{1}(2:3,im)'+D_im{1}(2:3)'
%             D_mines_S1{3}+D_DOW_after{3}(2,im) D_DOW{3}(2:3,im)'+D_im{3}(2:3)'
%             D_mines_S1{4}+D_DOW_after{4}(2,im) D_DOW{4}(2:3,im)'+D_im{4}(2:3)' ];
end
% f_CI_age('Deaths_counted_by_MoH',X,times,D_a,0)
f_CI_a(  'Deaths_counted_by_MoH0', times,D_a,0,D_mines_S1_DOW_S2,1,D_tot,2:3);

clear X
X{1} = [ D_unc{1}(1:3,1)'
         D_unc{2}(1:3,1)'
         D_unc{3}(1:3,1)'
         D_unc{4}(1:3,1)' ];
% f_CI_age('Deaths_uncounted',X,times,D_a,0)
f_CI_a(  'Deaths_uncounted0', times,D_a,2,[],[],D_unc,1:3); % use flag = 2

clear X
for im=1:m
    X{im} = [ D_mines_S1_DOW_S2{1}(1,im) D_tot_unc{1}(2:3,im)'
              D_mines_S1_DOW_S2{2}(1,im) D_tot_unc{2}(2:3,im)'
              D_mines_S1_DOW_S2{3}(1,im) D_tot_unc{3}(2:3,im)'
              D_mines_S1_DOW_S2{4}(1,im) D_tot_unc{4}(2:3,im)' ];
end
% f_CI_age('Deaths_total_with_uncounted',X,times,D_a,0)
f_CI_a(  'Deaths_total_with_uncounted0', times,D_a,0,D_mines_S1_DOW_S2,1,D_tot_unc,2:3);

clear X
for im=1:m
    X{im} = [ D_DOW_after{1}(2,im) D_DOW{1}(2:3,im)'
              D_DOW_after{2}(2,im) D_DOW{2}(2:3,im)'
              D_DOW_after{3}(2,im) D_DOW{3}(2:3,im)'
              D_DOW_after{4}(2,im) D_DOW{4}(2:3,im)' ];
end
% f_CI_age('Died_of_wounds',X,times,D_a,0)
f_CI_a(  'Died_of_wounds0', times,D_a,0,D_DOW_after,2,D_DOW,2:3);

clear X
for im=1:m
    X{im} = [ D_mines_S1{1}(1,im) D_im{1}(2:3,im)'
              D_mines_S1{2}(1,im) D_im{2}(2:3,im)'
              D_mines_S1{3}(1,im) D_im{3}(2:3,im)'
              D_mines_S1{4}(1,im) D_im{4}(2:3,im)' ];
end
% f_CI_age('Immediate_deaths',X,times,D_a,0)
f_CI_a(  'Immediate_deaths0', times,D_a,0,D_mines_S1,1,D_im,2:3);

clear X
for im=1:m
    X{im} = [ D_dow{1}(1:3,im)'
              D_dow{2}(1:3,im)'
              D_dow{3}(1:3,im)'
              D_dow{4}(1:3,im)' ];
end
% f_CI_age('Lack_hospital_services',X,times,D_a,0)
f_CI_a(  'Lack_hospital_services0', times,D_a,0,[],[],D_dow,1:3);

clear X
for im=1:m
    X{im} = [ D_preg_post{1}(1:3,im)'
              D_preg_post{2}(1:3,im)'
              D_preg_post{3}(1:3,im)'
              D_preg_post{4}(1:3,im)'];
end
% f_CI_age('Pregnant_post_partum',X,times,D_a,1)
f_CI_a(  'Pregnant_post_partum0', times,D_a,0,[],[],D_preg_post,1:3);

clear X
for im=1:m
    X{im} = [ D_preg{1}(1:3,im)'
              D_preg{2}(1:3,im)'
              D_preg{3}(1:3,im)'
              D_preg{4}(1:3,im)'];
end
% f_CI_age('Pregnant',X,times,D_a,1)
f_CI_a(  'Pregnant0', times,D_a,0,[],[],D_preg,1:3);

clear X
for im=1:m
    X{im} = [ D_still{1}(1:3,im)'
              D_still{2}(1:3,im)'
              D_still{3}(1:3,im)'
              D_still{4}(1:3,im)'];
end
% f_CI_age('Stillbirths',X,times,D_a,2)
f_CI_a(  'Stillbirths0', times,D_a,0,[],[],D_still,1:3); % try flag = 0

clear X
for im=1:m
    X{im} = [ D_neo{1}(1:3,im)'
              D_neo{2}(1:3,im)'
              D_neo{3}(1:3,im)'
              D_neo{4}(1:3,im)'];
end
% f_CI_age('Neonates',X,times,D_a,2)
% f_CI_a(  'Neonates0', times,D_a,2,[],[],D_neo,1:3);
f_CI_a(  'Neonates1', times,D_a,2,[],[],Neo_tot_unc,1:3); % use flag = 2

% SUBSEQUENT PROCESSING
% Gaza_report_format.m
%   generate plots and graphs
%   for neonates:
%     it will not use the Neonates file generated above
%     instead it will read Deaths_uncounted0
%     and pick out the first line (neonates)

keyboard


function p_DOW = f_p_DOW()

% DATA SOURCE
% Chronic consequences of acute injuries, Worse survival after discharge
%   Shafi, Shahid MD, MPH; Renfro, Lindsay A. MS; Barnes, Sunni PhD; Rayan, Nadine MHS;
%   Gentilello, Larry M. MD; Fleming, Neil PhD; Ballard, David MD, PhD, MSPH
%   Journal of Trauma and Acute Care Surgery 73(3):p 699-703, September 2012"																	

% INITIALIZE
% Use S(1) = S_initial(1)
%     S(2)   is close to S_initial(2)
t_month   = 30;                  % one month after the initial time (t=1)
S_initial = [ 74.00 40.70 30.10 25.90 ]/100; % S(first few days)

flag_plot     = 0;               % set to 1 to see plot of curve-fitting

% curve fit the survival curve
n_months  = 6;               % number of months
S_data = [ 280 114 97 76 60 36 ]/280;
t_data = round([ 0 0.5 1 2 3 6 ]*30);
h_data = -diff(S_data)./diff(t_data);
n_data = length(h_data);                     % n - 1

X_data = [ones(n_data,1) t_data(2:end)'];    % n - 1
b_data = regress(log(h_data(2:end))',X_data(2:end,:)) % skip 1, start from 2nd point

b_fit  = lsqcurvefit(@(b,xdata) f_survival(b,xdata,S_data(2)),...
    b_data(2),t_data(2:end)'-t_data(2),S_data(2:end)')
S      = f_survival(b_fit,0:n_months*t_month,S_data(2));
S(1)   = S_initial(1);

if flag_plot
    figure(99), tiledlayout(2,1)
    nexttile, plot(t_data,S_data,'bo-'), title('S_{data}'), xlims = xlim; hold on
    plot(1:n_months*t_month+1,S,'r-'), hold off
    nexttile, plot(t_data(2:end),log(h_data),'bo-'), title('log h_{data}'), hold on
    plot(t_data(2:end),X_data*b_data,'ro-'), hold off
    xlim(xlims)
end

% S = survival over 181 days starting at S(1) 0.74 (26% die in the first 24 hours)
% D = probability of deaths for each day after injury
% D_tot = conditional probability of survival assuming death before 181 days
%         assumption: CFR deaths occurs within 6 months
% D_month = same as D_tot except aggregated over days
% p_DOW   = number of deaths for 6 months after ceasefire start with 1 injury per day

D       = -diff(S);                                      
D_tot   = cumsum(D,'reverse');
D_month = nan(n_months,1);

for i=1:n_months
    % D_month(i) = probability of death during ith month after injury, i=1 is first month
    D_month(i)   = sum(D_tot(1+t_month*(i-1):t_month*i)); % sum over 30-day months
end

p_DOW = d_month;


function S = f_survival(h,ts,S0)

% Survival function: S(t) = S(0)*exp(-h*t)
% ts are times to evaluate S starting at 0

n        = ts(end)+1;             % shift by 1
S_all    = nan(n,1);
S_all(1) = S0;                    % start at t=0
for t=2:n
    S_all(t) = S_all(1)*exp(-h*(t-1));  % for each successive t
end
S = S_all(ts+1);                  % evaluate at t=ts, with shift by 1


function f_CI_a(ttl,times,D_a,flag,X1,nS1,X2,nS2)

% X1(3,21,2,2) 21 corresponds to 19 ages, total
% X{k}(i,kS) for month k, scenario kS, and statistic i (mean, std, lower/upper CI)

% for k=1:4, X1,X2(nS,m)
%     k=5:8       (nS,n_age_tot,n_gender_tot,n_period)

[ nS,m ] = size(X2{1});
n_stat_all = length(X2);
n_stat     = n_stat_all/2; % n_stat = 4: mean,sd,CI_lower,CI_upper
n_S_stat   = (n_stat-1)*nS;    % S1: mean,CI_lower,CI_upper, S2: (n_stat), S3: (n_stat)

for im=1:m
    Z{im} = [];
    for i_stat=1:n_stat % mean, std, CI-lower, CI-upper
        if isempty(X1)
            Z{im} = cat( 1, Z{im},                             X2{i_stat}(nS2,im)'   );
        else
            Z{im} = cat( 1, Z{im}, cat( 2, X1{i_stat}(nS1,im), X2{i_stat}(nS2,im)' ) );
        end
    end
end

head1 = {'month','Ceasefire','Ceasefire lower CI','Ceasefire upper CI',...
    'Status Quo','Status Quo lower CI','Status Quo upper CI',...
    'Escalation','Escalation lower CI','Escalation upper CI'};
T1 = table('Size',[6 10],'VariableTypes',...
    {'string','double','double','double','double','double','double','double','double','double'},...
    'VariableNames',head1);

fprintf('\n----- %s -----\n',ttl)
fprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n',string(head1))
for im=1:m                                 % for each month
    fprintf(['%s, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f\n'],...
        times{im},Z{im}([1 3 4],:))
    T1{im,1}     = string(times{im});
    T1{im,2:end} = round(reshape(Z{im}([1 3 4],:),1,n_S_stat),2); % save mean, CIs
end

writetable(T1,[ttl '.xlsx'],'Sheet','Scenario Excess Death')

if flag==2, return, end  % stillbirth and neonates

[ nS,n_age_tot,n_gender_tot,n_period_tot ] = size(X2{end});
n_period = n_period_tot;                       % include all
szXk = [ 1,nS,n_age_tot,n_gender_tot ];        % used for forming the 4 rows of Z{ip}
% for  k=1:4, X1,X2{k}(nS,m)
%      k=5:8       {k}(nS,n_age_tot,n_gender_tot,n_period)
% for ip=1:2,    Z{ip}(nS,n_age_tot,n_gender_tot)

clear Z
for ip=1:n_period
    Z{ip} = [];
    for k=5:8 % mean, std, CI-lower, CI-upper
        X2k = reshape(X2{k}(:,:,:,ip),szXk); % add singleton dim
        if isempty(X1)
            Z{ip} = cat( 1, Z{ip},                         X2k(1,nS2,:,:)   );
        else
            X1k   = reshape(X1{k}(:,:,:,ip),szXk);
            Z{ip} = cat( 1, Z{ip}, cat( 2, X1k(1,nS1,:,:), X2k(1,nS2,:,:) ) );
        end
    end
end

head1{1} = 'age';

% USE SMALLER NUMBER OF AGE GROUPS
% age_groups0 = string({'<1', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80', 'Total'});
% if flag==1, age_idx = [5:11 19]; else age_idx = 1:19; end
% age_groups = string({'neonates','<1 not neonates', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80', 'Total'});
age_groups   = string({'0mo','1 to 11mo', '12 to 59mo', '5 to 9yo', '10 to 14yo', '15 to 19yo', '20 to 29yo', '30 to 39yo', '40 to 49yo', '50 to 59yo', '60 to 69yo', '70 to 79yo', '>80', 'Total'});

if flag==1                         % F reproductive age and total
    age_idx = [6:9 14];            % 15-49yo
elseif flag==0
    age_idx = 1:14;                % all ages
elseif flag==2
    age_idx = 1;
end

age_groups = age_groups(age_idx);  % take the subset
n_age      = length(age_groups);

for g=1:n_gender_tot
    for ip=1:n_period
        f_Table(n_age,head1,Z,age_idx,D_a,flag,age_groups,ttl,ip,g)
    end
end


function f_Table(n_age,head1,Z,age_idx,D_a,flag,age_groups,ttl,ip,g)

T3 = table('Size',[n_age 10],'VariableTypes',...
    {'string','double','double','double','double','double','double','double','double','double'},...
    'VariableNames',head1);
ttl_ip     = {'Months 1-3 (Feb-Apr)','Months 4-6 (May-Jul)','All Months (Feb-Jul)'};
ttl_sheets = {'M1-M3','M4-M6','M1-M6'};
g_str  = {' F',' M',''};

Xall   = age_X0(Z{ip},age_idx,D_a,flag); % months 4-6, age_idx for select ages

fprintf('\n----- %s, %s -----\n',ttl,ttl_ip{ip})
fprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n',string(head1))

for ia=1:n_age
    Xall_ia = Xall(ia,:,g);        % g = F,M,total
    fprintf(['%s, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f\n'],...
        age_groups(ia),Xall_ia)
    T3{ia,1}     = string(age_groups{ia});
    T3{ia,2:end} = round(Xall_ia,2);    
end

writetable(T3,[ttl '.xlsx'],'Sheet',['Excess Death Age ' ttl_sheets{ip} g_str{g}])


% function f_CI_age(ttl,X,times,D_a,flag)
%
% OLD VERSION USING NORMAL APPROXIMATION FOR THE CI
% 
% % X{k}(i,kS) for month k, scenario kS, and statistic i (mean, std, lower/upper CI)
% 
% kmax  = numel(X);   % Number of months,sometimes only 1 if constant for every month
% head1 = {'month','Ceasefire','Ceasefire lower CI','Ceasefire upper CI',...
%     'Status Quo','Status Quo lower CI','Status Quo upper CI',...
%     'Escalation','Escalation lower CI','Escalation upper CI'};
% T1 = table('Size',[6 10],'VariableTypes',...
%     {'string','double','double','double','double','double','double','double','double','double'},...
%     'VariableNames',head1);
% 
% fprintf('\n----- %s -----\n',ttl)
% fprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n',string(head1))
% for k=1:6                                  % for each month
%     im = min(k,kmax);                      % if X{k} ranges from 1 to kmax, use kmax for k>kmax
%     fprintf(['%s, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f\n'],...
%         times{k},X{im}([1 3 4],:))
%     T1{k,1}     = string(times{k});
%     T1{k,2:end} = round(reshape(X{im}([1 3 4],:),1,9),2); % save mean, CIs
% end
% 
% writetable(T1,[ttl '.xlsx'],'Sheet','Scenario Excess Death')
% 
% if flag==2, return, end  % stillbirth and neonates
% 
% head1{1} = 'age';
% % age_groups0 = string({'<1', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80', 'Total'});
% % if flag==1, age_idx = [5:11 19]; else age_idx = 1:19; end
% 
% % age_groups = string({'neonates','<1 not neonates', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80', 'Total'});
% age_groups = string({'0mo','1 to 11mo', '12 to 59mo', '5 to 9yo', '10 to 14yo', '15 to 19yo', '20 to 29yo', '30 to 39yo', '40 to 49yo', '50 to 59yo', '60 to 69yo', '70 to 79yo', '>80', 'Total'});
% 
% if flag==1                         % F reproductive age and total
%     age_idx = [6:9 14];            % 15-49yo
% elseif flag==0
%     age_idx = 1:14;                % all ages
% elseif flag==2
%     age_idx = 1;
% end
% 
% age_groups = age_groups(age_idx);  % take the subset
% n_age      = length(age_groups);
% 
% T2 = table('Size',[n_age 10],'VariableTypes',...
%     {'string','double','double','double','double','double','double','double','double','double'},...
%     'VariableNames',head1);
% 
% Xall = age_X(X,n_age,1:3,kmax,age_idx,D_a,flag); % months 1-3, age_idx for select ages
% fprintf('\n----- %s, Months 1-3 (Feb-Apr) -----\n',ttl)
% fprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n',string(head1))
% for k=1:n_age
%     fprintf(['%s, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f\n'],...
%         age_groups(k),Xall(k,:))
%     T2{k,1}     = string(age_groups{k});
%     T2{k,2:end} = round(Xall(k,:),2);    
% end
% 
% writetable(T2,[ttl '.xlsx'],'Sheet','Excess Death Age M1-M3')
% 
% T3 = table('Size',[n_age 10],'VariableTypes',...
%     {'string','double','double','double','double','double','double','double','double','double'},...
%     'VariableNames',head1);
% 
% Xall = age_X(X,n_age,4:6,kmax,age_idx,D_a,flag); % months 4-6, age_idx for select ages
% fprintf('\n----- %s, Months 4-6 (May-Jul) -----\n',ttl)
% fprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n',string(head1))
% for k=1:n_age
%     fprintf(['%s, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f, %8.2f\n'],...
%         age_groups(k),Xall(k,:))
%     T3{k,1}     = string(age_groups{k});
%     T3{k,2:end} = round(Xall(k,:),2);    
% end
% 
% writetable(T3,[ttl '.xlsx'],'Sheet','Excess Death Age M4-M6')


% function Xall = age_X(X,n_age,idx_month,kmax,age_idx,D_a,flag)
% 
% OLD VERSION USING mean +/- 1.96 std FOR THE CI
%
% n_gender = 1;
% if flag==0
%     D_a = sum(D_a,2);                           % add M and F
% elseif flag==1
%     fprintf('***** proportion neonatal: %.3f *****\n',sum(D_a(1,:)))
%     D_a = D_a(age_idx(1:end-1),1);              % only F reproductive age, total included later
%     fprintf('***** proportion reproductive age: %.3f *****\n',sum(D_a))
%     D_a = D_a/sum(D_a(:));                      % renormalize
% elseif flag==3                                  % flag==2 does not call this function
%     fprintf('***** generate random deaths for each age/gender *****\n')
%     n_gender = 2;
% end
% D_a(end+1,:) = 1;                               % total, index end+1 = age_idx(end)
% 
% Xtot    = 0;
% Xtotvar = 0;
% Xtotsum = 0;
% iM      = 1:4:12; % mean of each scenario (sum over months)
% iS      = 2:4:12; % std
% % iCI1  = 3:4:12; % upper CI (sum variances over months before sqrt)
% % iCI2  = 4:4:12; % lower CI
% 
% % compute sum over months: mean,variance = Xtotsum,Xtotvar
% for k_month=idx_month
%     im      = min(k_month,kmax);
%     Xk      = reshape(X{im},1,prod(size(X{im})));     % make it a row vector
%     Xkmean  = Xk(iM);
% %   Xkstd   = (Xk(iCI1)-Xk(iCI2))/2/1.96;             % get std (NG)
%     Xkstd   = Xk(iS);                                 % get std
%     Xkvar   = Xkstd.^2;
%     Xtotsum = Xtotsum + Xkmean;                       % add means over months
%     Xtotvar = Xtotvar + Xkvar;                        % add variances
% end
% % Xtot(i1)= Xtotsum;
% % Xtot(i2)= max(0,Xtotsum - 1.96*sqrt(Xtotvar));      % lower CI in 2:3:9
% % Xtot(i3)= max(0,Xtotsum + 1.96*sqrt(Xtotvar));      % upper       3:3:9
% 
% Xasum = zeros(n_age,3,n_gender);
% Xavar = zeros(n_age,3,n_gender);
% Xall  = zeros(n_age,9,n_gender);
% 
% jM    = 1:3:9;                                        % index for the mean (3 scenarios)
% jCI1  = 2:3:9;                                        %           lower CI
% jCI2  = 3:3:9;                                        %           upper CI
% 
% % scale total over months by age proportion to get mean,CI
% for a=1:n_age
%     for j=1:n_gender
%         Xasum(a,:,j)   = Xtotsum*D_a(a,j);
% %       Xavar(a,:,j)   = Xtotvar*D_a(a)^2;
%         Xavar(a,:,j)   = Xasum(a,:,j);
%         Xall(a,jM,  j) = Xasum(a,:,j);
%         Xall(a,jCI1,j) = max(0,Xasum(a,:,j) - 1.96*sqrt(Xavar(a,:,j)));
%         Xall(a,jCI2,j) = max(0,Xasum(a,:,j) + 1.96*sqrt(Xavar(a,:,j)));
%     end
% end


function [D_a,D_p_repr,D_p_neo] = generate_D_a(N_p)

% NUMBER OF DEATHS FOR EACH AGE GROUP AND GENDER
% FROM THE Sept 19, 2023 MoH REPORT
N_a = [                            % F/M
   [5.4	6.1]*   0.2069*   0.0833   % <1 mo
   [5.4	6.1]*   0.2069*(1-0.0833)  % 1 yr
   [5.4	6.1]*(1-0.2069)            % 1-4
    5.6	6                          % 5-9
    4.8	5.9                        % 10-14
    3.8	5.3  %  6
    3.5	5.6
    3.9	5.7
    3.9	6.3
    2.5	4.2
    2	2.8  % 11                  % 40-44
    1.6	2    % 12
    1.6	1.8                        % 50-54
    1.3	1.6
    1.3	1.3
    0.9	1
    0.4	0.7
    0.3	0.4
    0.4	0.3  % 19                  % 80+
    ]/100*N_p;

% GENERATE POISSON RANDOM VARIABLES AND DIVIDE BY N_p TO GET PROPORTIONS
D_a      = lnrand(N_a,[],'P')/N_p;

% AGGREGATE FOR REPRODUCTIVE AGED WOMEN, NEONATAL, AND (20-24,25-29) -> (20-29), ETC.
D_p_repr = sum(D_a(6:12,1));       % F 15-49
D_p_neo  = sum(D_a(1,:));          % Both <1 mo

for i=7:2:18 % 7:8, 9:10, ..., 17:18 ranges from 20 to 79 years old with index up to 12
    D_a(7+(i-7)/2,:) = sum(D_a(i:i+1,:));
end
D_a(13,:) = D_a(end,:);
D_a       = D_a(1:13,:);           % overwrite

% D_p_preg_post = 0.1134*D_p_repr; % multiplication done later
% D_p_preg      = 0.0972*D_p_repr;
% D_p_still     = 0.0291*D_p_repr;


function Xall = age_X0(X,age_idx,D_a,flag)

% period selected in input X = Z{ip}
% X(n_stat,nS,n_gender_tot,n_age_tot,n_period) % n_stat = 4 (mean,sd,CI_lower,CI_upper)

[ n_stat,nS,n_age_tot,n_gender_tot ] = size(X);
n_S_stat   = (n_stat-1)*nS;    % S1: mean,CI_lower,CI_upper, S2: (n_stat), S3: (n_stat)
Xall = zeros(n_age_tot,n_S_stat,n_gender_tot);
% for k_month=idx_month
%     im      = min(k_month,kmax);
%     Xk      = reshape(X{im},1,prod(size(X{im})));     % make it a row vector

for a=1:n_age_tot
    for g=1:n_gender_tot
        for kS=1:nS
            Xall(a,1+(kS-1)*(n_stat-1),g) = X(1,kS,a,g);       % mean
            Xall(a,2+(kS-1)*(n_stat-1),g) = X(3,kS,a,g);       % CI-lower
            Xall(a,3+(kS-1)*(n_stat-1),g) = X(4,kS,a,g);       % CI-upper
        end
    end
end


function [ Z,Neo ] = printf(D_a,str,X0,n,flag_unc)

% Z{1,2,3,4} mean, std, CI-lower, upper                  from X0
% Z{5,6,7,8}                             ages and gender
Z = cell(1,8);

[ m,n_S,n_sim ] = size(X0);

Xs = sort(X0,3);

% find mean and standard deviation
Y = (std( X0,[],3))'; % Y(n_S,m)
X = (mean(X0,   3))'; % X(n_S,m)

% use the normal approximation for the CI
Z{1} = X;                                 % (n_S,m)
Z{2} = Y;

try flag_unc
    flag_normal = flag_unc;               % unc is sometimes negative
catch
    flag_unc    = 0;
    flag_normal = 0;                      % 0-consistent with other analysis
end

if flag_normal==1
    Z{3} = max(0, X - 1.96*Y);            % CI is wider but is more accurate
    Z{4} =       (X + 1.96*Y);            % but there may be negative values
else
    % try the empirical results
    Z{3} = Xs(:,:,round(n_sim*0.025))';
    Z{4} = Xs(:,:,round(n_sim*0.975))';
end

fprintf('\n%s\n',str)
if n==3
    fprintf('  %8.2f %8.2f %8.2f\n',X)
else
    if 3==3
        fprintf('  %8.2f %8.2f %8.2f\n',X)
    else
        fprintf('  %8.2f\n',X)
    end
end
fprintf('----------\n')
if n==3
    fprintf('  %8.2f %8.2f %8.2f\n',Y)
else
    if 3==3
        fprintf('  %8.2f %8.2f %8.2f\n',Y)
    else
        fprintf('  %8.2f\n',Y)
    end
end

if flag_normal==1, return, end

% D_a(end+1,:)       = sum(D_a);                   % total for each gender
% D_a(end+1,:)       = [1 1];                      % grand total
if flag_unc==-1
    D_a = D_a*0; D_a(1) = 1;     % use dummy variable for convenience
end 
[ n_age,n_gender ] = size(D_a);
if isempty(n_S)
    n_S = 1;
    X0 = reshape(X0,m,n_S,n_sim);
end
n_period_tot = 3;
n_gender_tot = n_gender + 1; % need total
n_age_tot    = n_age    + 1; % need total
X_a_avg = zeros([n_S n_age n_gender_tot n_period_tot]);
X_a_std = X_a_avg; X_a_CI1 = X_a_avg; X_a_CI2 = X_a_avg;
X_g_ip_sims = {zeros(n_S,n_sim),zeros(n_S,n_sim),zeros(n_S,n_sim);
               zeros(n_S,n_sim),zeros(n_S,n_sim),zeros(n_S,n_sim);
               zeros(n_S,n_sim),zeros(n_S,n_sim),zeros(n_S,n_sim)}; % (g,ip)

X_neo_m_sims= zeros(m,n_S,n_sim);
X_S_m_sims  = zeros(m,n_S,n_sim); % same as X0

for a=1:n_age
    for ip=n_period_tot:-1:1                  % go backwards and do all 6 months first
        X_ag_sims{n_gender_tot} = 0;
        for g=1:n_gender_tot
            if g<n_gender_tot
                if ip==1, k_months=min(1:3,m); elseif ip==2, k_months=min(4:6,m); ...
                else k_months=min(1:6,m); end
                X_ag_sims{g}  = zeros(n_S,n_sim);
                for im=k_months % sum over all months in ip
                    if ip==n_period_tot           % generate only once
                        % X_a_sims(n_S,n_sim), X0,X_S_m_sims(m,n_S,n_sim)
                        X_a_sims{g,im} = poissrnd( reshape(X0(im,:,:),n_S,n_sim)*D_a(a,g) );
                        X_a_sims_im    = reshape(X_a_sims{g,im},1,n_S,n_sim);
                        X_S_m_sims(im,:,:)       = X_S_m_sims(  im,:,:) + X_a_sims_im;
                        if a==1 % neo, restrict to neo age, sum over g
                            X_neo_m_sims(im,:,:) = X_neo_m_sims(im,:,:) + X_a_sims_im;
                        end
                    end
                    X_ag_sims{g}   = X_ag_sims{g} + X_a_sims{g,im};    % sum over months
                    % sum over a,g
                end
                X_ag_sims{n_gender_tot} = X_ag_sims{n_gender_tot} + X_ag_sims{g};
            end
            X_g_ip_sims{g,ip}   = X_g_ip_sims{g,ip} + X_ag_sims{g}; % (n_S,n_sim) sum over ages
            X_a_avg(:,a,g,ip)   = mean(X_ag_sims{g},   2); % mean over simulations
            X_a_std(:,a,g,ip)   = std( X_ag_sims{g},[],2);
            X_ag_sims{g}        = sort(X_ag_sims{g},2);
            X_a_CI1(:,a,g,ip)   = X_ag_sims{g}(:,round(n_sim*0.025))';
            X_a_CI2(:,a,g,ip)   = X_ag_sims{g}(:,round(n_sim*0.975))';
%           X_g_ip_sims{n_gender_tot,ip} = X_g_ip_sims{n_gender_tot,ip} + X_g_ip_sims{g,ip};
        end
    end
end

for ip=1:n_period_tot
%   X_all_g_ip_sims = 0;
    for g=1:n_gender_tot
%       X_all_g_ip_sims = [X_all_g_ip_sims X_g_ip_sims{g,ip}]; % (nS,
        X_a_avg(:,n_age_tot,g,ip) = mean(X_g_ip_sims{g,ip},   2);
        X_a_std(:,n_age_tot,g,ip) = std( X_g_ip_sims{g,ip},[],2);
        X_g_ip_sims{g,ip}         = sort(X_g_ip_sims{g,ip},   2);
        X_a_CI1(:,n_age_tot,g,ip) = X_g_ip_sims{g,ip}(:,round(n_sim*0.025));
        X_a_CI2(:,n_age_tot,g,ip) = X_g_ip_sims{g,ip}(:,round(n_sim*0.975));
    end
%   X_a_avg(:,:,n_gender_tot,:) = sum(X_a_avg(:,:,1:n_gender,:),3); % add over gender
end

Z{5} = X_a_avg;
Z{6} = X_a_std;
Z{7} = X_a_CI1;
Z{8} = X_a_CI2;

% RECOMPUTE MONTHLY STATS
% [ m,n_S,n_sim ] = size(X0);

Xs = sort(X_S_m_sims,3);

% find mean and standard deviation
Y = (std( Xs,[],3))'; % Y(n_S,m)
X = (mean(Xs,   3))'; % X(n_S,m)

% use the normal approximation for the CI
Z{1} = X;                                 % (n_S,m)
Z{2} = Y;

flag_normal = 0;                          % 0-consistent with other analysis

if flag_normal
    Z{3} = max(0, X - 1.96*Y);            % CI is wider but is more accurate
    Z{4} =       (X + 1.96*Y);            % but there may be negative values
else
    % try the empirical results
    Z{3} = Xs(:,:,round(n_sim*0.025))';
    Z{4} = Xs(:,:,round(n_sim*0.975))';
end

fprintf('\n%s\n',str)
if n==3
    fprintf('  %8.2f %8.2f %8.2f\n',X)
else
    if 3==3
        fprintf('  %8.2f %8.2f %8.2f\n',X)
    else
        fprintf('  %8.2f\n',X)
    end
end
fprintf('----------\n')
if n==3
    fprintf('  %8.2f %8.2f %8.2f\n',Y)
else
    if 3==3
        fprintf('  %8.2f %8.2f %8.2f\n',Y)
    else
        fprintf('  %8.2f\n',Y)
    end
end

% NEO MONTHLY STATS
% [ m,n_S,n_sim ] = size(X0);

Xs = sort(X_neo_m_sims,3);

% find mean and standard deviation
Y = (std( Xs,[],3))'; % Y(n_S,m)
X = (mean(Xs,   3))'; % X(n_S,m)

% use the normal approximation for the CI
Neo    = cell([1 8]);
Neo{1} = X;                                 % (n_S,m)
Neo{2} = Y;

flag_normal = 0;                          % 0-consistent with other analysis

if flag_normal
    Neo{3} = max(0, X - 1.96*Y);            % CI is wider but is more accurate
    Neo{4} =       (X + 1.96*Y);            % but there may be negative values
else
    % try the empirical results
    Neo{3} = Xs(:,:,round(n_sim*0.025))';
    Neo{4} = Xs(:,:,round(n_sim*0.975))';
end


function y = lnrand(x1,x2,flag)

switch flag
    case 'n'                 % normal
        m     = x1; v = x2;
        dist  = 'N';
    case 'c'                 % normal with x2 = c.o.v.
        m     = x1; v = (m*x2)^2;
        dist  = 'N';
    case 'p'                 % var(p)   = p*(1-p)/n
        p     = x1; n = x2;
%       m     = p;
%       v     = p*(1-p)/n;
        m     = n*p;
        dist  = 'P';
    case 'P'                 % Poisson
        m     = x1;
        dist  = 'P';
end

switch dist
    case 'N'
        mu    = log((m^2)/sqrt(v+m^2));
        sigma = sqrt(log(v/(m^2)+1));

        y     = lognrnd(mu,sigma);
    case 'P'
        y     = poissrnd(m);
        if flag=='p'
            y = y/n;
        end
end


function [I_conflict,D_im,D_DOW,D_dow,D_DOW_after,D_unc,D_mines_S1,I_mines_S1,D_repr,D_neo,...
    D_preg_post,D_preg,D_still] = fDI(...
    flag_use_count_repr_neo,...
    I_current,D_current,I_max,D_max,CFR,p_mod_sev,p_mod_upper_sev,...
    p_DOW,D_mines,I_mines,D_p_repr,D_p_neo,D_p_preg_post,D_p_preg,D_p_still,D_to_I)

% INITIALIZE
mo         = 30;                   % number of days in a month

D_2014     = 2251;                 % total number of deaths, Gaza 2014, used for D by mines
D_2023     = 26637;                %                              2023/24
I_2023     = 65387;
m          = length(p_DOW);        % number of months for time-varying DOW

% ESTIMATE I AND D
I_conflict = [ 0 I_current I_max ];       % I_conflict and D_conflict are uncorrelated =>
D_conflict = [ 0 D_current D_max ];       %   increasing CIs for uncounted D
D_DOW_0    = I_conflict.*CFR*p_mod_sev;   % DOW, CFR applies to moderate-to-severe
D_im       = D_conflict - D_DOW_0;        % immediate deaths is same for every month
D_DOW_after= p_DOW*D_DOW_0/mo;            % DOW after end of previous month
%                                         %   p_DOW is for daily I so divided by mo
%                                         % D_DOW_S3 is negative because of fewer D_DOW in the
D_DOW_S3   = D_DOW_after(:,2) - D_DOW_after(:,3); % beginning of the transition to S3
D_DOW      = ones(m,1)*D_DOW_0;           % start with constant DOW every month
D_DOW(:,3) = D_DOW(:,3) + D_DOW_S3;       % reduce DOW in S3 as explained above

% ESTIMATE DOW USING REDUCED CFR FROM S1
D_dow_after= D_DOW_after .*(CFR-CFR(1))./CFR;     % non adjusted DOW with reduced CFR from S1
D_dow_S3   = D_dow_after(:,2) - D_dow_after(:,3); % adjusted DOW
D_dow      = ones(m,1)*D_DOW_0.*(CFR-CFR(1))./CFR;% start with constant DOW every month with reduced CFR
D_dow(:,3) = D_dow(:,3) + D_dow_S3;               % reduce DOW in S3

% UNCERTAIN DEATHS
D_unc      = I_conflict*p_mod_upper_sev.*(1-CFR   )*D_to_I - D_conflict;  % monthly D,I
D_unc_2023 = I_2023    *p_mod_upper_sev.*(1-CFR(2))*D_to_I - D_2023;      % total D,I for mines

% ADD DIFFERENT TYPES OF DEATHS
D_count    = D_im + D_DOW;       % add adjusted DOW to immediate deaths to get counted D
D_tot      = D_count + D_unc;    % add uncounted D to get total D
D_tot_DOW_lagged      = D_tot;   % initialize lagged DOW
D_tot_DOW_lagged(:,1) = D_tot_DOW_lagged(:,1) + D_DOW_after(:,2); % add lagged DOW from S2 to S1

% DEATHS DUE TO MINES, ONLY FOR S1
D_mines_S1 = D_mines*(D_2023+D_unc_2023)/D_2014/12;  % monthly, include uncounted in scaling
I_mines_S1 = I_mines*(D_2023+D_unc_2023)/D_2014/12;

% USE SCALING TO GET D_repr, etc. BASED ON 
if flag_use_count_repr_neo==2
    D = D_tot_DOW_lagged;
elseif flag_use_count_repr_neo==1
    D = D_count;
else
    D = D_tot;
end
% D_p_repr, D_p_neo are random numbers
D_repr      = D_p_repr      * D;
D_neo       = D_p_neo       * D;
D_preg_post = D_p_preg_post * D;
D_preg      = D_p_preg      * D;
D_still     = D_p_still     * D;

% Scale factors from Demographic-OONA.xlsx > Sheet 2
% D_preg_post, D_preg, D_still proportional to D_repr
% D_preg_post = 0.1134*D_repr;
% D_preg      = 0.0972*D_repr;
% D_still     = 0.0291*D_repr;
