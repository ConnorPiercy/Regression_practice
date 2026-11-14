"""
Synthetic health & lifestyle cohort (N = 10,000 adults, 100 clinics) for practising the
regression and multivariate flowcharts. Every relationship is planted on purpose.
"""
import json
import os
import numpy as np
import pandas as pd
from scipy import special, stats

SEED = 20261107
rng = np.random.default_rng(SEED)
N, S = 10_000, 100
expit = special.expit
TRUTH = {}          # true parameters, dumped to the answer key


def calibrate_binary(lp, target, extra=0.0, n_forced=0):
    """intercept b so that sum(expit(lp+b)) + n_forced = target*N"""
    lo, hi = -25.0, 25.0
    for _ in range(80):
        mid = (lo + hi) / 2
        if expit(lp + mid).sum() + n_forced > target * len(lp):
            hi = mid
        else:
            lo = mid
    return (lo + hi) / 2


def zt_nb(mu, theta):
    """zero-truncated negative binomial draws"""
    x = rng.negative_binomial(theta, theta / (theta + mu))
    bad = x == 0
    while bad.any():
        x[bad] = rng.negative_binomial(theta, theta / (theta + mu[bad]))
        bad = x == 0
    return x


def rand_corr(d, strength):
    A = rng.normal(size=(d, d)) * strength
    C = A @ A.T + np.eye(d)
    D = np.sqrt(np.diag(C))
    return C / np.outer(D, D)


# ------------------------------------------------------------------------------------------ sites
sizes = 50 + rng.multinomial(N - 50 * S, rng.dirichlet(np.ones(S) * 2.0))
site = np.repeat(np.arange(S), sizes)
site_region = rng.choice(4, size=S, p=[0.30, 0.25, 0.25, 0.20])
site_type = rng.choice(3, size=S, p=[0.45, 0.35, 0.20])           # urban / suburban / rural
REG = np.array(['North', 'South', 'East', 'West'])
STY = np.array(['urban', 'suburban', 'rural'])
region = site_region[site]
urban = (site_type[site] == 0).astype(float)
rural = (site_type[site] == 2).astype(float)

RE = dict(sbp_int=4.0, sbp_age_slope=0.10, hyp=0.35, er=0.30, adh=0.25, cost=0.15, fit=1.5, frailty=0.25)
u = {k: rng.normal(0, v, S) for k, v in RE.items()}
TRUTH['site_random_effect_sd'] = RE

# ------------------------------------------------------------------------------------------ demographics
age = np.clip(np.round(18 + 72 * rng.beta(2.0, 3.0, N)), 18, 90)
male = (rng.random(N) < 0.48).astype(float)
age_c = age - 45
age_z = (age - age.mean()) / age.std()

z_adip = 0.2 * age_z + np.sqrt(1 - 0.04) * rng.normal(size=N)
bmi_t = np.clip(np.exp(np.log(26) + 0.004 * age_c + 0.02 * male + 0.165 * z_adip), 15, 55)
height = np.round(np.clip(rng.normal(162 + 14 * male, 6.5 + 0.5 * male) - 0.05 * np.maximum(0, age - 55), 140, 205), 1)
weight = np.round(bmi_t * (height / 100) ** 2, 1)
bmi = np.round(weight / (height / 100) ** 2, 1)
waist = np.round(38 + 2.1 * bmi + 6 * male + 0.1 * age_c + rng.normal(0, 3.5, N), 1)
bmi_c = bmi - 26

edu_lat = -0.012 * age_c + 0.15 * (1 - male) + 0.10 * urban + rng.normal(size=N)
edu_lvl = np.digitize(edu_lat, [-0.9, 0.25, 1.15])                  # 0..3
EDU = np.array(['secondary or less', 'vocational', 'bachelor', 'postgraduate'])

# ------------------------------------------------------------------------------------------ hidden lifestyle segments
p_seg = np.exp(np.log([0.32, 0.28, 0.25, 0.15]) + np.outer(age_z, [0.25, 0.0, -0.35, 0.0]))
p_seg /= p_seg.sum(1, keepdims=True)
seg = (p_seg.cumsum(1) > rng.random((N, 1))).argmax(1)
SEGN = np.array(['active_balanced', 'sedentary_screen', 'social_drinkers_smokers', 'busy_short_sleepers'])
#                  act   diet  sleep screen alc   smoke
M = 2.0 * np.array([[0.9, 0.7, 0.1, -0.5, -0.2, -0.4],
                    [-0.9, -0.4, -0.2, 1.0, 0.0, 0.1],
                    [0.1, -0.7, -0.5, 0.2, 1.2, 0.9],
                    [-0.2, -0.2, -1.1, 0.4, 0.3, 0.2]])
sd_seg = 0.8 * np.array([[0.9, 0.9, 0.9, 0.9, 0.9, 0.9],
                   [0.9, 1.0, 1.0, 0.8, 1.0, 1.0],
                   [1.0, 0.9, 1.0, 1.0, 0.8, 0.8],
                   [1.1, 1.0, 0.8, 1.0, 1.0, 1.0]])
lat = np.zeros((N, 6))
for k in range(4):
    idx = np.where(seg == k)[0]
    C = rand_corr(6, 0.35) * np.outer(sd_seg[k], sd_seg[k])
    lat[idx] = M[k] + rng.multivariate_normal(np.zeros(6), C, len(idx))
act, diet_l, sleep_l, screen_l, alc_l, smk_l = lat.T
act = act + 0.25 * urban
TRUTH['segments'] = dict(names=SEGN.tolist(), proportions=[0.32, 0.28, 0.25, 0.15],
                         latent_means_act_diet_sleep_screen_alc_smoke=M.tolist())

# ------------------------------------------------------------------------------------------ lifestyle measures
diet_score = np.round(np.clip(55 + 12 * diet_l + 2 * edu_lvl + rng.normal(0, 5, N), 0, 100), 1)
sleep_hours = np.round(np.clip(7.1 + 0.9 * sleep_l + 0.01 * age_c + rng.normal(0, 0.3, N), 3.5, 11), 1)
screen_hours = np.round(np.clip(4.3 + 1.6 * screen_l - 0.015 * age_c + rng.normal(0, 0.5, N), 0.3, 14), 1)
famhx = (rng.random(N) < expit(-1.4 + 0.015 * age_c)).astype(int)
rare_variant = (rng.random(N) < 0.006).astype(int)

# smoking
p_cur = expit(-1.9 + 0.8 * smk_l - 0.01 * age_c + 0.25 * male - 0.2 * edu_lvl + 0.15 * (region == 1))
cur = rng.random(N) < p_cur
frm = (~cur) & (rng.random(N) < expit(-1.3 + 0.03 * age_c + 0.2 * male))
smoker = np.where(cur, 'current', np.where(frm, 'former', 'never'))
sm_cur = cur.astype(float)
sm_frm = frm.astype(float)

# cigarettes per day: structural zeros (non-smokers) + occasional smokers (many sampling zeros) + regular smokers
occasional = cur & (rng.random(N) < 0.30)
regular = cur & ~occasional
cigs = np.zeros(N, dtype=int)
cigs[occasional] = rng.poisson(1.5, occasional.sum())
mu_cig = 12 * np.exp(0.12 * smk_l + 0.3 * male)
cigs[regular] = rng.negative_binomial(6, 6 / (6 + mu_cig[regular]))

# alcohol units per week: abstainers (one zero process) + zero-truncated NB
p_drink = expit(0.9 + 0.6 * male - 0.015 * age_c + 0.75 * alc_l + 0.1 * edu_lvl)
drinker = rng.random(N) < p_drink
alc = np.zeros(N, dtype=int)
mu_alc = np.exp(1.9 + 0.55 * alc_l + 0.35 * male)
alc[drinker] = zt_nb(mu_alc[drinker], 3.0)

# activity: exercise hours (left-censored at 0), training minutes, steps
ex_star = 2.4 + 1.7 * act - 0.02 * age_c - 0.05 * bmi_c + 0.15 * edu_lvl + rng.normal(0, 1.8, N)
exercise = np.round(np.maximum(0, ex_star) * 4) / 4
training = np.round(np.maximum(0, 50 * ex_star + rng.normal(0, 45, N)) / 5) * 5
days_worn = 1 + rng.binomial(13, 0.55, N)
steps_true = np.exp(np.log(7.0) + 0.22 * act + 0.08 * urban - 0.007 * age_c - 0.012 * bmi_c)
steps = np.round(np.clip(steps_true + rng.normal(0, 1, N) * 4.5 / np.sqrt(days_worn), 0.3, None), 2)

diet_type_lp = np.log([0.72, 0.08, 0.14, 0.06])[None, :] + np.outer(diet_l, [-0.25, 0.10, 0.20, 0.30]) \
    + np.outer(1 - male, [-0.15, 0.0, 0.25, 0.30])
pdt = np.exp(diet_type_lp)
pdt /= pdt.sum(1, keepdims=True)
diet_type = np.array(['omnivore', 'pescatarian', 'vegetarian', 'vegan'])[(pdt.cumsum(1) > rng.random((N, 1))).argmax(1)]

# employment & income
p_stud = np.where(age < 30, 0.5 * np.exp(-np.maximum(0, age - 18) / 4), 0.0)
p_ret = expit((age - 64) / 2.5)
u_emp = rng.random(N)
emp = np.empty(N, dtype=object)
rest = rng.random(N)
emp[:] = np.where(rest < 0.62 + 0.07 * edu_lvl, 'employed',
                  np.where(rest < 0.82 + 0.05 * edu_lvl, 'self-employed', 'unemployed'))
emp = np.where(u_emp < p_stud, 'student', np.where(rng.random(N) < p_ret, 'retired', emp))
inc_log = (3.4 + 0.18 * edu_lvl + 0.012 * age_c - 0.0003 * age_c ** 2 + 0.06 * male
           + np.where(emp == 'retired', -0.43, 0) + np.where(emp == 'student', -0.9, 0)
           + np.where(emp == 'unemployed', -0.5, 0) + rng.normal(0, 0.45, N))
income_k_true = np.round(np.exp(inc_log), 1)

# ------------------------------------------------------------------------------------------ survey factors & items
R_f = np.array([[1, -0.55, 0.40], [-0.55, 1, -0.25], [0.40, -0.25, 1]])
Fz = rng.multivariate_normal(np.zeros(3), R_f, N)
Fz[:, 0] += 0.4 * (seg == 0) - 0.2 * (seg == 3) + 0.01 * age_c + 0.1 * (act)
Fz[:, 1] += 0.6 * (seg == 3) - 0.3 * (seg == 0) - 0.012 * age_c + 0.4 * (emp == 'unemployed')
Fz[:, 2] += 0.2 * (seg == 2) + 0.15 * (emp != 'retired') - 0.1 * (rural)
WB, ST, SS = Fz.T
ITEMS = [  # name, factor index, loading, threshold offset, cross-loading (on ST)
    ('wb1', 0, 0.78, -0.3, 0), ('wb2', 0, 0.74, -0.3, 0), ('wb3', 0, 0.70, -0.3, -0.20), ('wb4', 0, -0.62, 0.2, 0),
    ('st1', 1, 0.80, 0.0, 0), ('st2', 1, 0.76, 0.1, 0), ('st3', 1, 0.70, 0.0, 0), ('st4', 1, 0.66, 0.2, 0),
    ('ss1', 2, 0.82, -0.3, 0), ('ss2', 2, 0.74, -0.2, 0), ('ss3', 2, 0.68, -0.1, 0), ('ss4', 2, -0.60, 0.3, 0)]
items = {}
for name, f, lam, off, cross in ITEMS:
    ystar = lam * Fz[:, f] + cross * Fz[:, 1] + np.sqrt(max(0.08, 1 - lam ** 2 - cross ** 2)) * rng.normal(size=N)
    items[name] = 1 + np.digitize(ystar, np.array([-1.4, -0.5, 0.35, 1.25]) + off)
TRUTH['likert_items'] = {n: dict(factor=['wellbeing', 'stress', 'support'][f], loading=l, cross_loading_on_stress=c)
                         for n, f, l, o, c in ITEMS}
TRUTH['factor_correlations_wb_st_ss'] = R_f.tolist()

# ------------------------------------------------------------------------------------------ biomarkers
z_met = 0.55 * z_adip + 0.83 * rng.normal(size=N) - 0.15 * act - 0.1 * diet_l
exercise_hr = exercise
log_tg = 4.65 + 0.025 * bmi_c + 0.22 * z_met + 0.004 * age_c + 0.10 * male + 0.003 * alc + rng.normal(0, 0.40, N)
tg_t = np.exp(log_tg)
ldl_t = 112 + 0.35 * age_c + 1.0 * bmi_c + 9 * z_met + 5 * sm_cur + rng.normal(0, 22, N)
hdl_t = 55 - 0.9 * bmi_c * 0.6 + 6 * (1 - male) - 6 * 0.6 * z_met + rng.normal(0, 10, N)
glu_t = np.exp(np.log(95) + 0.008 * bmi_c + 0.10 * z_met + 0.003 * age_c + rng.normal(0, 0.12, N))
a1c_t = (glu_t + 46.7) / 28.7 + rng.normal(0, 0.28, N)
crp_t = np.exp(0.2 + 0.07 * bmi_c + 0.25 * z_met + 0.2 * sm_cur + 0.15 * ST + rng.normal(0, 0.9, N))
hr_t = 70 + 0.12 * bmi_c - 0.06 * age_c + 2.0 * ST - 1.2 * act + rng.normal(0, 8, N)

# lab markers: 3 blocks of 5 (within r = .65, between r = .12)
B = np.full((15, 15), 0.12)
for b in range(3):
    B[b * 5:(b + 1) * 5, b * 5:(b + 1) * 5] = 0.65
np.fill_diagonal(B, 1.0)
Zm = rng.multivariate_normal(np.zeros(15), B, N)
markers = np.round(100 + 15 * Zm, 1)

# systolic BP (baseline): the 'clean' quantitative outcome with clustered structure
SBP = dict(intercept=118.0, age_c=0.40, bmi_c=0.80, male=4.5, smoker_current=1.5, alcohol_units=0.12,
           sleep_hours_minus7=-0.8, stress_latent_sd=2.0, exercise_hours=-0.35,
           lab_marker_03_z=3.0, lab_marker_07_z=2.2, lab_marker_12_z=-2.5, resid_sd=13.0)
TRUTH['sbp'] = SBP
sbp_t = (SBP['intercept'] + SBP['age_c'] * age_c + SBP['bmi_c'] * bmi_c + SBP['male'] * male
         + SBP['smoker_current'] * sm_cur + SBP['alcohol_units'] * alc + SBP['sleep_hours_minus7'] * (sleep_hours - 7)
         + SBP['stress_latent_sd'] * ST + SBP['exercise_hours'] * exercise
         + SBP['lab_marker_03_z'] * Zm[:, 2] + SBP['lab_marker_07_z'] * Zm[:, 6] + SBP['lab_marker_12_z'] * Zm[:, 11]
         + u['sbp_int'][site] + u['sbp_age_slope'][site] * age_c + rng.normal(0, SBP['resid_sd'], N))
sbp_t = np.clip(sbp_t, 80, 230)
dbp_t = 25 + 0.45 * sbp_t + rng.normal(0, 7, N)

# ------------------------------------------------------------------------------------------ trial arm & follow-up
arm = np.zeros(N, dtype=int)
for s_ in range(S):
    idx = np.where(site == s_)[0]
    a = np.tile([0, 1, 2], int(np.ceil(len(idx) / 3)))[:len(idx)]
    arm[idx] = rng.permutation(a)
ARM = np.array(['control', 'program_A', 'program_B'])
effect = {'sbp6': [-1.0, -4.0, -2.0], 'sbp12': [-1.5, -8.5, -4.0], 'ldl': [-1.0, -4.0, -14.0],
          'a1c': [0.0, -0.10, -0.35], 'wt': [-0.4, -2.2, -1.0]}
TRUTH['trial_effects_by_arm_control_A_B'] = effect
# correlated follow-up noise (4 outcomes); programme B has more variable lipids/HbA1c (unequal covariance)
Cn = np.array([[1, .25, .15, .30], [.25, 1, .25, .15], [.15, .25, 1, .10], [.30, .15, .10, 1]])
sd_arm = {0: np.array([8, 14, 0.25, 2.0]), 1: np.array([8, 14, 0.25, 2.0]), 2: np.array([8, 22, 0.40, 2.0])}
noise = np.zeros((N, 4))
for k in range(3):
    idx = np.where(arm == k)[0]
    noise[idx] = rng.multivariate_normal(np.zeros(4), np.outer(sd_arm[k], sd_arm[k]) * Cn, len(idx))
TRUTH['followup_noise_sd_by_arm_sbp_ldl_a1c_weight'] = {ARM[k]: sd_arm[k].tolist() for k in range(3)}
e6 = 0.6 * noise[:, 0] + 0.8 * rng.normal(0, 6, N)
sbp6_t = sbp_t + np.array(effect['sbp6'])[arm] - 0.15 * (sbp_t - 125) + e6
sbp12_t = sbp_t + np.array(effect['sbp12'])[arm] - 0.25 * (sbp_t - 125) + noise[:, 0]
ldl12_t = ldl_t + np.array(effect['ldl'])[arm] - 0.20 * (ldl_t - 112) + noise[:, 1]
a1c12_t = a1c_t + np.array(effect['a1c'])[arm] - 0.20 * (a1c_t - 5.4) + noise[:, 2]
wt12_t = weight + np.array(effect['wt'])[arm] + noise[:, 3] * np.sqrt(weight / 75)
dropout = rng.random(N) < expit(-2.9 - 0.02 * age_c + 0.04 * bmi_c + 0.3 * (arm == 0))

# ------------------------------------------------------------------------------------------ binary outcomes
HYP = dict(age_c=0.045, bmi_c=0.07, male=0.5, smoker_current=0.35, alcohol_units=0.02, family_history=0.5,
           stress_latent_sd=0.25, bmi_over_32_and_age_over_50=0.9, exercise_hours=-0.08, site_re_sd=RE['hyp'])
lp_h = (HYP['age_c'] * age_c + HYP['bmi_c'] * bmi_c + HYP['male'] * male + HYP['smoker_current'] * sm_cur
        + HYP['alcohol_units'] * alc + HYP['family_history'] * famhx + HYP['stress_latent_sd'] * ST
        + HYP['bmi_over_32_and_age_over_50'] * ((bmi > 32) & (age > 50)) + HYP['exercise_hours'] * exercise
        + u['hyp'][site])
HYP['intercept'] = calibrate_binary(lp_h, 0.27)
hypertension = (rng.random(N) < expit(lp_h + HYP['intercept'])).astype(int)
TRUTH['hypertension_logit'] = HYP

RARE = dict(age_c=0.02, smoker_current=0.35, family_history=0.5, bmi_c=0.03, rare_variant='all carriers have the event (quasi-complete separation)')
lp_r = RARE['age_c'] * age_c + RARE['smoker_current'] * sm_cur + RARE['family_history'] * famhx + RARE['bmi_c'] * bmi_c
nc = rare_variant == 0
RARE['intercept'] = calibrate_binary(lp_r[nc], (0.011 * N - rare_variant.sum()) / nc.sum())   # total events ~ 1.1% incl. carriers
rare_event = (rng.random(N) < expit(lp_r + RARE['intercept'])).astype(int)
rare_event[rare_variant == 1] = 1
TRUTH['rare_event_logit'] = RARE

# ------------------------------------------------------------------------------------------ primary condition (multinomial)
sbp_c, ldl_c, a1c_c = sbp_t - 125, ldl_t - 112, a1c_t - 5.4
lp_met = 0.15 * bmi_c + 1.6 * a1c_c + 0.01 * age_c + 1.8 * ((bmi > 30) & (age > 45))
lp_cv = 0.06 * age_c + 0.040 * sbp_c + 0.018 * ldl_c + 0.9 * sm_cur + 0.7 * famhx + 1.5 * ((sbp_t > 140) & (ldl_t > 130))
lp_rs = 1.8 * sm_cur + 0.9 * sm_frm + 0.03 * age_c + 0.30 * np.log(crp_t)
LP = np.column_stack([np.zeros(N), lp_met, lp_cv, lp_rs])
tgt = np.array([0.62, 0.17, 0.13, 0.08])
b_int = np.zeros(4)
for _ in range(200):
    P = np.exp(LP + b_int)
    P /= P.sum(1, keepdims=True)
    b_int += 0.8 * (np.log(tgt) - np.log(P.mean(0)))
    b_int -= b_int[0]
P = np.exp(LP + b_int)
P /= P.sum(1, keepdims=True)
cond = (P.cumsum(1) > rng.random((N, 1))).argmax(1)
COND = np.array(['none', 'metabolic', 'cardiovascular', 'respiratory'])
TRUTH['primary_condition_multinomial_logit_vs_none'] = dict(
    intercepts=dict(zip(COND[1:], b_int[1:].round(3).tolist())),
    metabolic='0.15*bmi_c + 1.6*(hba1c-5.4) + 0.01*age_c + 1.8*(bmi>30 & age>45)',
    cardiovascular='0.06*age_c + 0.040*(sbp-125) + 0.018*(ldl-112) + 0.9*current_smoker + 0.7*family_history + 1.5*(sbp>140 & ldl>130)',
    respiratory='1.8*current_smoker + 0.9*former_smoker + 0.03*age_c + 0.30*log(crp)')
cond_any = (cond > 0).astype(float)

# ------------------------------------------------------------------------------------------ ordinal outcome
lat_srh = (0.75 * WB - 0.25 * ST - 0.020 * age_c - 0.03 * bmi_c + 0.08 * exercise - 0.55 * cond_any
           + 0.006 * (income_k_true - 40) - 0.3 * sm_cur + rng.logistic(size=N))
cuts = np.quantile(lat_srh, [0.05, 0.22, 0.56, 0.85])
srh = 1 + np.digitize(lat_srh, cuts)
TRUTH['self_rated_health_ordinal_logit'] = dict(
    coefficients=dict(wellbeing_latent=0.75, stress_latent=-0.25, age_c=-0.020, bmi_c=-0.03, exercise_hours=0.08,
                      any_condition=-0.55, income_k_minus40=0.006, current_smoker=-0.3),
    thresholds=cuts.round(3).tolist(), model='proportional odds (logistic errors)')

# ------------------------------------------------------------------------------------------ counts
exposure = np.round(rng.uniform(0.5, 3.0, N), 2)
GPC = dict(age_c=0.012, bmi_c=0.02, any_condition=0.35, male=-0.20, stress_latent_sd=0.10)
lp_gp = (GPC['age_c'] * age_c + GPC['bmi_c'] * bmi_c + GPC['any_condition'] * cond_any + GPC['male'] * male
         + GPC['stress_latent_sd'] * ST)
GPC['intercept'] = np.log(3.2) - np.log(np.mean(np.exp(lp_gp)))
gp = rng.poisson(exposure * np.exp(lp_gp + GPC['intercept']))
TRUTH['gp_visits_poisson_log_rate'] = dict(GPC, offset='log(exposure_years)', dispersion='variance = mean (Poisson)')

ERC = dict(age_c=0.01, smoker_current=0.3, resp_condition=0.5, stress_latent_sd=0.2, urban=0.1, site_re_sd=RE['er'], nb_theta=0.8)
lp_er = (ERC['age_c'] * age_c + ERC['smoker_current'] * sm_cur + ERC['resp_condition'] * (cond == 3)
         + ERC['stress_latent_sd'] * ST + ERC['urban'] * urban + u['er'][site])
ERC['intercept'] = np.log(0.5) - np.log(np.mean(np.exp(lp_er)))
mu_er = exposure * np.exp(lp_er + ERC['intercept'])
er = rng.negative_binomial(ERC['nb_theta'], ERC['nb_theta'] / (ERC['nb_theta'] + mu_er))
TRUTH['er_visits_negbin_log_rate'] = dict(ERC, offset='log(exposure_years)')
TRUTH['cigs_per_day'] = 'zero-inflated: never/former smokers are structural zeros; 30% of current smokers are occasional (Poisson mean 1.5); regular smokers NB(mean ~12, theta 6)'
TRUTH['alcohol_units_week'] = 'hurdle: P(drink)=expit(0.9+0.6*male-0.015*age_c+0.75*alc_latent+0.1*edu); positives zero-truncated NB(theta 3)'

# ------------------------------------------------------------------------------------------ positive-skewed cost, fitness, proportions
COST = dict(intercept=7.0, age_c=0.012, bmi_c=0.03, smoker_current=0.22, any_condition=0.35, cardiovascular_extra=0.25,
            site_re_sd=RE['cost'], gamma_shape=1.4)
mu_cost = np.exp(COST['intercept'] + COST['age_c'] * age_c + COST['bmi_c'] * bmi_c + COST['smoker_current'] * sm_cur
                 + COST['any_condition'] * cond_any + COST['cardiovascular_extra'] * (cond == 2) + u['cost'][site])
cost = np.maximum(1, np.round(rng.gamma(COST['gamma_shape'], mu_cost / COST['gamma_shape'])))
TRUTH['annual_cost_gamma_log_link'] = COST

FIT = dict(base=28, vmax=52, K=110, age_quad=-0.009, age_peak=32, bmi_c=-0.45, male=3.0, site_re_sd=RE['fit'], resid_sd=4.0)
fitness = np.round(np.clip(FIT['base'] + FIT['vmax'] * training / (FIT['K'] + training) + FIT['age_quad'] * (age - FIT['age_peak']) ** 2
                   + FIT['bmi_c'] * (bmi - 25) + FIT['male'] * male + u['fit'][site] + rng.normal(0, FIT['resid_sd'], N), 0, 100), 1)
TRUTH['fitness_score'] = dict(FIT, form='base + vmax*training/(K+training) + age_quad*(age-age_peak)^2 + bmi_c*(bmi-25) + male*male')

ADH = dict(intercept=0.45, age_c=0.015, edu_level_0to3=0.10, stress_latent_sd=-0.15, program_A=0.30, program_B=0.45,
           site_re_sd=RE['adh'], beta_phi=8)
mu_adh = expit(ADH['intercept'] + ADH['age_c'] * age_c + ADH['edu_level_0to3'] * edu_lvl + ADH['stress_latent_sd'] * ST
               + ADH['program_A'] * (arm == 1) + ADH['program_B'] * (arm == 2) + u['adh'][site])
adh = np.clip(np.round(rng.beta(mu_adh * ADH['beta_phi'], (1 - mu_adh) * ADH['beta_phi']), 4), 0.0005, 0.9995)
TRUTH['adherence_prop_beta_logit_mean'] = ADH

BPT = dict(p0='expit(-2.3 + 0.03*(sbp-125))', p1='expit(-1.9 - 0.04*(sbp-125))',
           beta_mean='expit(0.2 - 0.025*(sbp-125) + 0.4*programA + 0.3*programB)', beta_phi=5)
p0 = expit(-2.3 + 0.03 * sbp_c)
p1 = expit(-1.9 - 0.04 * sbp_c)
mu_bt = expit(0.2 - 0.025 * sbp_c + 0.4 * (arm == 1) + 0.3 * (arm == 2))
ub = rng.random(N)
bpt = np.where(ub < p0, 0.0, np.where(ub < p0 + p1, 1.0,
                                      np.clip(rng.beta(mu_bt * 5, (1 - mu_bt) * 5), 0.0005, 0.9995)))
bpt = np.round(bpt, 4)
TRUTH['bp_in_target_prop_zero_one_inflated_beta'] = BPT

# ------------------------------------------------------------------------------------------ survival
SURV = dict(age_c=0.035, bmi_c=0.03, smoker_current=0.45, smoker_former=0.20, sbp_minus125=0.004, family_history=0.30,
            hba1c_minus5_4=0.25, exercise_hours=-0.04, cardiovascular_condition=0.7, metabolic_condition=0.3,
            program_A=-0.35, program_B=-0.15, frailty_sd=RE['frailty'], weibull_shape=1.3,
            admin_censor_months=60, dropout_fraction_by_60m=0.12)
xb = (SURV['age_c'] * age_c + SURV['bmi_c'] * bmi_c + SURV['smoker_current'] * sm_cur + SURV['smoker_former'] * sm_frm
      + SURV['sbp_minus125'] * sbp_c + SURV['family_history'] * famhx + SURV['hba1c_minus5_4'] * a1c_c
      + SURV['exercise_hours'] * exercise + SURV['cardiovascular_condition'] * (cond == 2)
      + SURV['metabolic_condition'] * (cond == 1) + SURV['program_A'] * (arm == 1) + SURV['program_B'] * (arm == 2)
      + u['frailty'][site])
Ue = rng.random(N)
Ud = rng.random(N)
shape = SURV['weibull_shape']
drop_rate = -np.log(1 - SURV['dropout_fraction_by_60m']) / 60
t_drop = -np.log(Ud) / drop_rate


def event_rate(loglam):
    t_ev = (-np.log(Ue) / (np.exp(loglam) * np.exp(xb))) ** (1 / shape)
    t_obs = np.minimum(np.minimum(t_ev, t_drop), 60)
    return (t_ev <= np.minimum(t_drop, 60)).mean()


lo, hi = -15.0, 0.0
for _ in range(60):
    mid = (lo + hi) / 2
    if event_rate(mid) > 0.28:
        hi = mid
    else:
        lo = mid
loglam = (lo + hi) / 2
t_ev = (-np.log(Ue) / (np.exp(loglam) * np.exp(xb))) ** (1 / shape)
t_cens = np.minimum(t_drop, 60)
time_m = np.round(np.maximum(0.1, np.minimum(t_ev, t_cens)), 1)
event = (t_ev <= t_cens).astype(int)
SURV['log_baseline_lambda'] = round(loglam, 4)
TRUTH['time_to_event_weibull_ph'] = SURV

# ------------------------------------------------------------------------------------------ recorded values + planted problems
# biomarker panel as recorded (rounded)
panel = pd.DataFrame(dict(sbp=np.round(sbp_t), dbp=np.round(np.clip(dbp_t, 45, 130)), ldl=np.round(np.clip(ldl_t, 35, 260)),
                          hdl=np.round(np.clip(hdl_t, 20, 120)), tg=np.round(np.clip(tg_t, 30, 900)),
                          glu=np.round(np.clip(glu_t, 60, 400)), a1c=np.round(np.clip(a1c_t, 4.0, 13.0), 1),
                          crp=np.round(np.clip(crp_t, 0.1, 60), 2), hr=np.round(np.clip(hr_t, 40, 130))))

# (1) joint multivariate outliers in the panel (excluding sbp): 80 rows shifted along the smallest principal axes
out_type = np.zeros(N, dtype=int)
cols = ['dbp', 'ldl', 'hdl', 'tg', 'glu', 'a1c', 'crp', 'hr']
T = panel[cols].copy()
T['tg'], T['glu'], T['crp'] = np.log(T['tg']), np.log(T['glu']), np.log(T['crp'])
mu_T, sd_T = T.mean(), T.std()
Zp = ((T - mu_T) / sd_T).values
evals, evecs = np.linalg.eigh(np.corrcoef(Zp.T))
cand = rng.choice(N, 80, replace=False)
for i in cand:
    shift = (np.sqrt(60 * evals[0]) * rng.choice([-1, 1]) * evecs[:, 0] + np.sqrt(40 * evals[1]) * rng.choice([-1, 1]) * evecs[:, 1])
    Zp[i] += shift
out_type[cand] = 1
Tn = pd.DataFrame(Zp * sd_T.values + mu_T.values, columns=cols)
panel['dbp'] = np.round(np.clip(Tn['dbp'], 45, 130))
panel['ldl'] = np.round(np.clip(Tn['ldl'], 35, 260))
panel['hdl'] = np.round(np.clip(Tn['hdl'], 20, 120))
panel['tg'] = np.round(np.clip(np.exp(Tn['tg']), 30, 900))
panel['glu'] = np.round(np.clip(np.exp(Tn['glu']), 60, 400))
panel['a1c'] = np.round(np.clip(Tn['a1c'], 4.0, 13.0), 1)
panel['crp'] = np.round(np.clip(np.exp(Tn['crp']), 0.1, 60), 2)
panel['hr'] = np.round(np.clip(Tn['hr'], 40, 130))
total_chol = np.round(panel['ldl'] + panel['hdl'] + panel['tg'] / 5 + rng.normal(0, 3, N))

# (2) unit errors: 60 follow-up weights recorded in pounds
wt12 = np.round(wt12_t, 1)
cand2 = rng.choice(np.where(~dropout & (out_type == 0))[0], 60, replace=False)
wt12[cand2] = np.round(wt12[cand2] * 2.2046, 1)
out_type[cand2] = 2

# ------------------------------------------------------------------------------------------ assemble
df = pd.DataFrame({
    'id': np.arange(1, N + 1),
    'site_id': ['S%03d' % (s_ + 1) for s_ in site],
    'region': REG[region], 'site_type': STY[site_type[site]], 'treatment_arm': ARM[arm],
    'age': age.astype(int), 'sex': np.where(male == 1, 'male', 'female'), 'education': EDU[edu_lvl],
    'employment': emp, 'income_k': income_k_true,
    'height_cm': height, 'weight_kg': weight, 'bmi': bmi, 'waist_cm': waist,
    'smoker': smoker, 'diet_type': diet_type, 'diet_score': diet_score, 'sleep_hours': sleep_hours,
    'screen_hours': screen_hours, 'family_history': famhx, 'rare_variant': rare_variant,
    'training_min_week': training.astype(int), 'exercise_hours_week': exercise, 'days_worn': days_worn,
    'mean_daily_steps_k': steps, 'cigs_per_day': cigs, 'alcohol_units_week': alc,
})
for name in items:
    df[name] = items[name]
df['sbp'] = panel['sbp'].astype(int)
df['dbp'] = panel['dbp'].astype(int)
df['total_chol'] = total_chol.astype(int)
df['ldl'] = panel['ldl'].astype(int)
df['hdl'] = panel['hdl'].astype(int)
df['triglycerides'] = panel['tg'].astype(int)
df['glucose'] = panel['glu'].astype(int)
df['hba1c'] = panel['a1c']
df['crp'] = panel['crp']
df['resting_hr'] = panel['hr'].astype(int)
for j in range(15):
    df['lab_marker_%02d' % (j + 1)] = markers[:, j]
fu = dict(sbp_wk6=np.round(sbp6_t), sbp_wk12=np.round(sbp12_t), ldl_wk12=np.round(ldl12_t),
          hba1c_wk12=np.round(a1c12_t, 1), weight_wk12=wt12)
for k, v in fu.items():
    v = np.asarray(v, dtype=float)
    v[dropout] = np.nan
    df[k] = v
df['hypertension'] = hypertension
df['rare_event'] = rare_event
df['primary_condition'] = COND[cond]
df['self_rated_health'] = srh
df['exposure_years'] = exposure
df['gp_visits'] = gp
df['er_visits'] = er
df['annual_cost'] = cost.astype(int)
df['fitness_score'] = fitness
df['adherence_prop'] = adh
df['bp_in_target_prop'] = bpt
df['time_to_event_months'] = time_m
df['event_observed'] = event

# ------------------------------------------------------------------------------------------ missing data
miss_rng = np.random.default_rng(SEED + 1)
p_inc = expit(-2.7 + 0.02 * age_c - 0.3 * edu_lvl)                         # MAR (depends on observed age, education)
df.loc[miss_rng.random(N) < p_inc, 'income_k'] = np.nan
for col, p in [('waist_cm', 0.04), ('crp', 0.05), ('hdl', 0.03)]:         # MCAR
    df.loc[miss_rng.random(N) < p, col] = np.nan
for col in ['wb1', 'wb2', 'wb3', 'wb4', 'ss1', 'ss2', 'ss3', 'ss4']:      # MCAR 1.5%
    df.loc[miss_rng.random(N) < 0.015, col] = np.nan
p_st = expit(-3.6 + 0.03 * age_c)                                          # MAR: older respondents skip stress items
for col in ['st1', 'st2', 'st3', 'st4']:
    df.loc[miss_rng.random(N) < p_st, col] = np.nan
for col in ['wb1', 'wb2', 'wb3', 'wb4', 'st1', 'st2', 'st3', 'st4', 'ss1', 'ss2', 'ss3', 'ss4']:
    df[col] = df[col].astype('Int64')

# ------------------------------------------------------------------------------------------ answer key
key = pd.DataFrame({'id': np.arange(1, N + 1), 'true_segment': SEGN[seg], 'outlier_type': out_type,
                    'latent_wellbeing': np.round(WB, 3), 'latent_stress': np.round(ST, 3), 'latent_support': np.round(SS, 3)})
key['outlier_type'] = key['outlier_type'].map({0: 'none', 1: 'joint_biomarker_outlier', 2: 'weight_wk12_in_pounds'})

# shuffle row order so that ids are not sorted by clinic
perm = rng.permutation(N)
df = df.iloc[perm].reset_index(drop=True)
df['id'] = np.arange(1, N + 1)
key = key.iloc[perm].reset_index(drop=True)
key['id'] = np.arange(1, N + 1)

# ------------------------------------------------------------------------------------------ daily time series (companion file)
T_days = 1096
dates = pd.date_range('2022-01-01', periods=T_days, freq='D')
t = np.arange(T_days)
doy = dates.day_of_year.values
temp = 11 - 9 * np.cos(2 * np.pi * (doy - 15) / 365.25)
e_t = np.zeros(T_days)
for i in range(1, T_days):
    e_t[i] = 0.7 * e_t[i - 1] + rng.normal(0, 1.8)
temp = np.round(temp + e_t, 1)
flu = np.round(np.maximum(0, 40 + 35 * np.cos(2 * np.pi * (doy - 20) / 365.25) + rng.normal(0, 8, T_days)), 1)
dow = dates.day_of_week.values
holiday = ((dates.month == 12) & (dates.day.isin([24, 25, 26, 31])) | ((dates.month == 1) & (dates.day == 1))).astype(int)
phi = 0.7
ar = np.zeros(T_days)
for i in range(1, T_days):
    ar[i] = phi * ar[i - 1] + rng.normal(0, 6)
visits = (80 + 0.012 * t + 0.35 * (flu - 40) - 0.9 * (temp - 11) + np.where(dow == 0, 12, np.where(dow >= 5, -22, 0))
          - 25 * holiday + ar)
series = pd.DataFrame({'date': dates.strftime('%Y-%m-%d'), 'day_of_week': dates.day_name(),
                       'mean_temp_c': temp, 'flu_index': flu, 'public_holiday': holiday,
                       'daily_clinic_visits': np.round(np.maximum(5, visits)).astype(int)})
TRUTH['daily_series'] = dict(trend_per_day=0.012, flu_index_per_unit=0.35, temp_per_degC=-0.9, monday=12, weekend=-22,
                             holiday=-25, ar1_phi=phi, ar1_innovation_sd=6)

# ------------------------------------------------------------------------------------------ write
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'Data')
os.makedirs(OUT, exist_ok=True)
df.to_csv(os.path.join(OUT, 'practice_data_10000.csv'), index=False, na_rep='NA')
key.to_csv(os.path.join(OUT, 'answer_key_labels.csv'), index=False)
series.to_csv(os.path.join(OUT, 'daily_series.csv'), index=False)
with open(os.path.join(OUT, 'truth.json'), 'w') as truth_file:
    json.dump(TRUTH, truth_file, indent=1, default=lambda o: o.tolist() if hasattr(o, 'tolist') else str(o))
print(df.shape, key.shape, series.shape)