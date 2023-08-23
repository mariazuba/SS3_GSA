#V3.30.19.00;_safe;_compile_date:_Apr  4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Fri Jan 20 09:27:26 2023
#_bootdata:_3
#C data file for model showing different selectivities
#_bootstrap file: 1  irand_seed: 1674203246 first rand#: 1.49313
#V3.30.19.00;_safe;_compile_date:_Apr  4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
2001 #_StartYr
2012 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
20 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
6 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2) 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 1 1 1 0 Type12_age_logistic  # 1
 1 1 1 1 0 Type14_age_non-parametric  # 2
 1 1 1 1 0 Type17_age_random-walk  # 3
 1 1 1 1 0 Type20_age_double-normal  # 4
 1 1 1 1 0 Type25_age_exponential-logistic  # 5
 1 1 1 1 0 Type27_age_cubic-spline  # 6
#Bycatch_fleet_input_goes_next
#a:  fleet index
#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)
#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years
#d:  F or first year of range
#e:  last year of range
#f:  not used
# a   b   c   d   e   f 
#_catch_biomass(mtons):_columns_are_fisheries,year,season
#_catch:_columns_are_year,season,fleet,catch,catch_se
#_Catch data: yr, seas, fleet, catch, catch_se
-999 1 1 0 0.1
2001 1 1 1090.22 0.1
2002 1 1 971.375 0.1
2003 1 1 943.644 0.1
2004 1 1 837.048 0.1
2005 1 1 1153.8 0.1
2006 1 1 1022.21 0.1
2007 1 1 1112.22 0.1
2008 1 1 1203.14 0.1
2009 1 1 960.504 0.1
2010 1 1 1133.63 0.1
2011 1 1 909.932 0.1
2012 1 1 995 0.1
-999 1 2 0 0.1
2001 1 2 1110.69 0.1
2002 1 2 1050.32 0.1
2003 1 2 953.185 0.1
2004 1 2 962.268 0.1
2005 1 2 1025.41 0.1
2006 1 2 953.577 0.1
2007 1 2 896.586 0.1
2008 1 2 1050.38 0.1
2009 1 2 950.028 0.1
2010 1 2 887.781 0.1
2011 1 2 1000.12 0.1
2012 1 2 1014.65 0.1
-999 1 3 0 0.1
2001 1 3 922.277 0.1
2002 1 3 945.463 0.1
2003 1 3 871.254 0.1
2004 1 3 1012.62 0.1
2005 1 3 923.346 0.1
2006 1 3 1068.26 0.1
2007 1 3 993.19 0.1
2008 1 3 1028.11 0.1
2009 1 3 1049.79 0.1
2010 1 3 1029.38 0.1
2011 1 3 1003.14 0.1
2012 1 3 893.892 0.1
-999 1 4 0 0.1
2001 1 4 1081.36 0.1
2002 1 4 1024.5 0.1
2003 1 4 1026.53 0.1
2004 1 4 1148.07 0.1
2005 1 4 891.84 0.1
2006 1 4 909.305 0.1
2007 1 4 1107.27 0.1
2008 1 4 896.165 0.1
2009 1 4 909.041 0.1
2010 1 4 1018.31 0.1
2011 1 4 1101.62 0.1
2012 1 4 1011.68 0.1
-999 1 5 0 0.1
2001 1 5 993.275 0.1
2002 1 5 997.465 0.1
2003 1 5 1026.34 0.1
2004 1 5 1057.4 0.1
2005 1 5 936.581 0.1
2006 1 5 984.602 0.1
2007 1 5 889.9 0.1
2008 1 5 979.877 0.1
2009 1 5 1012.36 0.1
2010 1 5 1041.45 0.1
2011 1 5 924.681 0.1
2012 1 5 982.628 0.1
-999 1 6 0 0.1
2001 1 6 0 0.1
2002 1 6 0 0.1
2003 1 6 0 0.1
2004 1 6 0 0.1
2005 1 6 0 0.1
2006 1 6 0 0.1
2007 1 6 0 0.1
2008 1 6 0 0.1
2009 1 6 0 0.1
2010 1 6 0 0.1
2011 1 6 0 0.1
2012 1 6 0 0.1
-9999 0 0 0 0
#
 #_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet Units Errtype SD_Report
1 1 0 0 # Type12_age_logistic
2 1 0 0 # Type14_age_non-parametric
3 1 0 0 # Type17_age_random-walk
4 1 0 0 # Type20_age_double-normal
5 1 0 0 # Type25_age_exponential-logistic
6 1 0 0 # Type27_age_cubic-spline
#_year month index obs err
2001 7 1 9.47286 0.1 #_orig_obs: 10 Type12_age_logistic
2002 7 1 8.98021 0.1 #_orig_obs: 9 Type12_age_logistic
2003 7 1 8.31431 0.1 #_orig_obs: 8 Type12_age_logistic
2004 7 1 6.06298 0.1 #_orig_obs: 7 Type12_age_logistic
2005 7 1 6.54322 0.1 #_orig_obs: 6 Type12_age_logistic
2006 7 1 5.27935 0.1 #_orig_obs: 5 Type12_age_logistic
2007 7 1 5.5266 0.1 #_orig_obs: 5 Type12_age_logistic
2008 7 1 5.43675 0.1 #_orig_obs: 4 Type12_age_logistic
2009 7 1 3.60969 0.1 #_orig_obs: 4 Type12_age_logistic
2010 7 1 3.78594 0.1 #_orig_obs: 3.5 Type12_age_logistic
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_Fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_0 #_DF_for_meanbodysize_T-distribution_like
# note:  type=1 for mean length; type=2 for mean body weight 
#_yr month fleet part type obs stderr
#  -9999 0 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
10 # minimum size in the population (lower edge of first bin and size at age 0.00) 
98 # maximum size in the population (lower edge of last bin) 
1 # use length composition data (0/1)
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_ParmSelect:  parm number for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 1 0 0 0 1 #_fleet:1_Type12_age_logistic
-1 0.001 1 0 0 0 1 #_fleet:2_Type14_age_non-parametric
-1 0.001 1 0 0 0 1 #_fleet:3_Type17_age_random-walk
-1 0.001 1 0 0 0 1 #_fleet:4_Type20_age_double-normal
-1 0.001 1 0 0 0 1 #_fleet:5_Type25_age_exponential-logistic
-1 0.001 1 0 0 0 1 #_fleet:6_Type27_age_cubic-spline
33 #_N_LengthBins
 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_yr month fleet sex part Nsamp datavector(female-male)
 2010 7 -1 3 0 5000  16 8 7 8 9 7 11 21 29 32 47 71 89 92 162 161 194 224 182 232 218 184 145 104 77 48 48 23 12 8 10 4 7 0 9 16 11 18 19 21 46 41 61 107 157 186 202 270 232 249 205 177 157 119 67 43 21 23 17 6 5 8 1 2 5 9
 2010 7 -2 3 0 5000  43 12 12 11 9 13 17 11 32 30 44 68 86 115 139 165 183 215 213 220 177 170 132 108 80 56 50 18 24 10 8 7 6 0 16 10 12 12 15 22 33 60 88 111 136 185 219 201 267 227 225 201 161 104 75 48 28 20 11 7 4 6 3 2 4 3
 2010 7 -3 3 0 5000  9 3 3 7 10 12 13 19 32 34 64 64 86 114 137 180 189 211 233 209 170 190 134 96 76 44 53 33 13 18 7 7 11 0 7 5 7 15 14 23 42 55 75 108 142 166 209 231 247 233 228 188 172 111 68 57 23 30 17 10 6 10 4 3 6 7
 2010 7 -4 3 0 5000  6 9 7 4 14 8 11 15 26 41 38 67 94 110 143 204 187 225 218 200 211 184 140 103 103 51 39 25 17 13 10 8 2 0 7 9 9 9 21 29 38 51 83 84 124 181 205 256 259 213 236 188 140 107 79 45 30 14 10 7 8 4 8 4 5 4
 2010 7 -5 3 0 5000  22 10 11 9 20 15 16 17 12 45 50 77 82 103 128 168 197 210 229 221 183 191 130 93 84 53 51 21 22 12 15 10 3 0 9 10 10 10 20 33 44 52 70 116 138 150 185 242 242 250 225 209 120 111 78 56 30 17 11 12 7 4 6 6 7 10
 2010 7 -6 3 0 5000  17 7 7 12 15 14 18 33 41 47 50 59 113 128 104 177 206 239 229 208 220 164 144 102 90 55 34 17 18 13 9 11 5 0 8 11 8 13 17 34 53 56 77 128 133 177 176 229 244 213 194 172 136 100 81 37 31 14 14 6 11 6 2 6 1 6
-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#
15 #_N_age_bins
 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
1 #_N_ageerror_definitions
 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5
 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_ParmSelect:  parm number for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 0 0 0 0 1 #_fleet:1_Type12_age_logistic
-1 0.001 0 0 0 0 1 #_fleet:2_Type14_age_non-parametric
-1 0.001 0 0 0 0 1 #_fleet:3_Type17_age_random-walk
-1 0.001 0 0 0 0 1 #_fleet:4_Type20_age_double-normal
-1 0.001 0 0 0 0 1 #_fleet:5_Type25_age_exponential-logistic
-1 0.001 0 0 0 0 1 #_fleet:6_Type27_age_cubic-spline
3 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
2010  7 1  3 0 1 -1 -1 250  2 1 3 9 24 26 18 21 11 3 0 0 2 3 2 2 1 9 19 20 24 22 12 3 3 2 0 1 1 6
2010  7 2  3 0 1 -1 -1 250  4 4 9 9 22 25 14 10 5 1 0 3 0 0 1 6 2 10 31 17 27 18 14 6 6 2 4 0 0 0
2010  7 3  3 0 1 -1 -1 250  2 2 5 23 24 32 10 9 13 2 3 0 0 1 2 1 3 8 21 24 17 15 11 8 4 3 1 2 2 2
2010  7 4  3 0 1 -1 -1 250  2 4 7 19 24 20 22 16 7 4 3 0 0 3 2 1 5 9 18 23 17 14 16 5 2 3 2 0 1 1
2010  7 5  3 0 1 -1 -1 250  4 4 12 11 19 22 15 13 9 6 2 1 1 2 0 1 4 11 18 24 30 12 10 6 2 3 0 4 0 4
2010  7 6  3 0 1 -1 -1 250  1 6 6 19 31 23 25 12 9 8 2 1 1 0 0 0 4 13 12 16 19 12 15 5 6 0 1 0 1 2
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_Use_MeanSize-at-Age_obs (0/1)
#
0 #_N_environ_variables
# -2 in yr will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#Yr Variable Value
#
0 # N sizefreq methods to read 
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#  yr, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
 # Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd
 # feature not yet implemented
#
999

