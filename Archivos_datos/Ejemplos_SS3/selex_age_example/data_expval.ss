#V3.30.19.00;_safe;_compile_date:_Apr  4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Fri Jan 20 09:27:26 2023
#_expected_values
#C data file for model showing different selectivities
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
#_catch:_columns_are_year,season,fleet,catch,catch_se
#_Catch data: yr, seas, fleet, catch, catch_se
-999 1 1 0 0.1
2001 1 1 1000 0.1
2002 1 1 1000 0.1
2003 1 1 1000 0.1
2004 1 1 1000 0.1
2005 1 1 1000 0.1
2006 1 1 1000 0.1
2007 1 1 1000 0.1
2008 1 1 1000 0.1
2009 1 1 999.999 0.1
2010 1 1 999.999 0.1
2011 1 1 999.999 0.1
2012 1 1 999.998 0.1
-999 1 2 0 0.1
2001 1 2 1000 0.1
2002 1 2 1000 0.1
2003 1 2 1000 0.1
2004 1 2 1000 0.1
2005 1 2 1000 0.1
2006 1 2 1000 0.1
2007 1 2 1000 0.1
2008 1 2 1000 0.1
2009 1 2 999.999 0.1
2010 1 2 999.999 0.1
2011 1 2 999.999 0.1
2012 1 2 999.998 0.1
-999 1 3 0 0.1
2001 1 3 1000 0.1
2002 1 3 1000 0.1
2003 1 3 1000 0.1
2004 1 3 1000 0.1
2005 1 3 1000 0.1
2006 1 3 1000 0.1
2007 1 3 1000 0.1
2008 1 3 1000 0.1
2009 1 3 999.999 0.1
2010 1 3 999.999 0.1
2011 1 3 999.999 0.1
2012 1 3 999.998 0.1
-999 1 4 0 0.1
2001 1 4 1000 0.1
2002 1 4 1000 0.1
2003 1 4 1000 0.1
2004 1 4 1000 0.1
2005 1 4 1000 0.1
2006 1 4 1000 0.1
2007 1 4 1000 0.1
2008 1 4 1000 0.1
2009 1 4 999.999 0.1
2010 1 4 999.999 0.1
2011 1 4 999.999 0.1
2012 1 4 999.998 0.1
-999 1 5 0 0.1
2001 1 5 1000 0.1
2002 1 5 1000 0.1
2003 1 5 1000 0.1
2004 1 5 1000 0.1
2005 1 5 1000 0.1
2006 1 5 1000 0.1
2007 1 5 1000 0.1
2008 1 5 1000 0.1
2009 1 5 999.999 0.1
2010 1 5 999.999 0.1
2011 1 5 999.999 0.1
2012 1 5 999.998 0.1
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
2001 7 1 10.1193 0.1 #_orig_obs: 10 Type12_age_logistic
2002 7 1 8.79331 0.1 #_orig_obs: 9 Type12_age_logistic
2003 7 1 7.66139 0.1 #_orig_obs: 8 Type12_age_logistic
2004 7 1 6.7186 0.1 #_orig_obs: 7 Type12_age_logistic
2005 7 1 5.94356 0.1 #_orig_obs: 6 Type12_age_logistic
2006 7 1 5.30674 0.1 #_orig_obs: 5 Type12_age_logistic
2007 7 1 4.77687 0.1 #_orig_obs: 5 Type12_age_logistic
2008 7 1 4.32724 0.1 #_orig_obs: 4 Type12_age_logistic
2009 7 1 3.93898 0.1 #_orig_obs: 4 Type12_age_logistic
2010 7 1 3.59924 0.1 #_orig_obs: 3.5 Type12_age_logistic
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
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
33 #_N_LengthBins
 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90
#_yr month fleet sex part Nsamp datavector(female-male)
 2010 7 -1 3 0 5000  15.9557 7.54365 7.24568 7.65674 9.29057 11.6409 14.3216 17.8692 23.4914 32.3041 45.0745 62.4679 84.9586 112.195 142.43 172.504 198.382 216.028 222.42 216.398 198.991 173.09 142.624 111.56 83.0885 59.1927 40.6257 27.1749 18.0457 12.2219 8.7223 6.7389 6.44378 0 7.91903 8.34812 10.1796 13.3003 17.5039 23.7956 34.0285 49.8895 72.5404 102.421 138.418 177.025 212.538 238.419 249.287 242.731 220.141 186.171 147.102 108.928 75.9434 50.2301 31.9297 19.9486 12.697 8.62618 6.50256 5.47211 5.00696 4.81173 4.7356 4.71352
 2010 7 -2 3 0 5000  42.7592 13.8164 11.1537 9.30079 10.0233 12.3262 15.2246 19.0783 25.0993 34.3934 47.6708 65.5303 88.3617 115.66 145.457 174.406 198.466 213.857 218.018 210.285 192.026 166.199 136.539 106.672 79.4549 56.6453 38.9057 26.0343 17.2946 11.7314 8.40868 6.54575 6.21104 0 13.4916 11.2002 11.395 14.2114 18.6779 25.4233 36.2324 52.7302 75.9586 106.184 142.035 179.713 213.369 236.675 244.838 236.169 212.58 178.817 140.847 104.163 72.6196 48.0532 30.5572 19.1004 12.1813 8.32133 6.33058 5.38115 4.96245 4.79172 4.72738 4.70891
 2010 7 -3 3 0 5000  5.08066 4.96014 5.50411 6.62871 8.41133 10.7302 13.6399 17.7824 24.1931 33.7594 47.1238 64.9294 87.6416 114.849 144.703 173.985 198.726 215.115 220.468 213.928 196.665 171.479 142.016 111.909 84.0983 60.4802 41.8657 28.1761 18.7512 12.6647 8.97286 6.8678 6.54708 0 5.51442 6.83116 9.10537 12.3738 16.9864 24.0946 35.3043 52.0147 75.2465 105.381 141.216 179.144 213.471 237.901 247.493 240.282 217.873 184.771 146.841 109.633 77.1768 51.5381 33.0132 20.7011 13.149 8.86505 6.61485 5.51945 5.02498 4.81796 4.73757 4.71429
 2010 7 -4 3 0 5000  10.411 6.20685 6.27659 6.93802 8.50429 10.7364 13.5543 17.5773 23.8959 33.4644 46.9311 64.8495 87.5491 114.55 144.093 173.175 198.041 214.938 221.013 215.099 198.076 172.639 142.553 111.708 83.3042 59.3795 40.7468 27.2343 18.0641 12.2199 8.71384 6.73079 6.43187 0 6.61657 7.374 9.27172 12.3689 16.8517 23.8336 34.9891 51.7573 75.0785 105.178 140.79 178.457 212.77 237.614 247.933 241.386 219.196 185.736 147.065 109.091 76.1402 50.3741 32.0047 19.9738 12.6974 8.61892 6.49564 5.46766 5.00464 4.81069 4.73519 4.7133
 2010 7 -5 3 0 5000  25.442 9.87924 9.11353 9.42294 11.6889 14.9226 18.2089 22.013 27.7621 36.6207 49.0847 65.4958 86.2183 111.044 138.564 166.099 190.148 207.1 214.08 209.724 194.593 171.025 142.488 112.702 84.8373 61.0186 42.2055 28.3769 18.8717 12.7419 9.02495 6.90262 6.59415 0 10.3542 10.6052 12.981 17.0724 22.0063 28.6214 38.8935 54.3631 75.7934 103.414 136.331 171.64 204.438 228.931 240.111 235.385 215.492 184.263 147.33 110.4 77.8324 51.9736 33.2669 20.844 13.2328 8.91651 6.64613 5.53724 5.03416 4.82221 4.73933 4.71525
 2010 7 -6 3 0 5000  21.262 8.91373 8.56235 9.32708 11.9287 15.5007 19.2868 23.8443 30.5936 40.5679 54.046 71.1436 92.032 116.362 142.684 168.455 190.484 205.546 211.082 205.879 190.459 167.001 138.79 109.408 81.9649 58.5729 40.1953 26.8026 17.7095 11.939 8.50793 6.59305 6.25838 0 9.54575 10.3489 13.2463 17.8721 23.5691 31.2857 42.9046 59.6801 82.0031 109.769 141.883 175.461 205.912 227.976 237.183 231.249 210.918 179.813 143.307 106.918 74.9208 49.6376 31.4934 19.5874 12.4102 8.42207 6.37413 5.40046 4.97131 4.7958 4.72918 4.71008
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
2010  7 1  3 0 1 -1 -1 250  1.98488 3.33233 7.56131 15.5292 24.3308 25.2469 18.5145 11.3234 6.3832 3.62839 2.19994 1.41329 0.969852 0.716035 1.86603 1.98488 3.33233 7.56131 15.5292 24.3308 25.2469 18.5145 11.3234 6.3832 3.62839 2.19994 1.41329 0.969852 0.716035 1.86603
2010  7 2  3 0 1 -1 -1 250  3.32417 3.70363 7.9155 16.7472 25.239 24.3768 15.2489 10.9806 6.8651 3.925 2.36723 1.51225 1.02926 0.746704 1.01857 3.32417 3.70363 7.9155 16.7472 25.239 24.3768 15.2489 10.9806 6.8651 3.925 2.36723 1.51225 1.02926 0.746704 1.01857
2010  7 3  3 0 1 -1 -1 250  1.21278 3.63038 7.97408 16.1286 25.4067 24.2737 15.1182 11.0756 9.17948 3.71115 2.24228 1.43758 0.984745 0.72569 1.89908 1.21278 3.63038 7.97408 16.1286 25.4067 24.2737 15.1182 11.0756 9.17948 3.71115 2.24228 1.43758 0.984745 0.72569 1.89908
2010  7 4  3 0 1 -1 -1 250  1.47893 3.4371 8.35812 16.1332 23.3094 24.45 19.1442 11.6264 6.37883 3.59631 2.17607 1.39802 0.960177 0.709698 1.84353 1.47893 3.4371 8.35812 16.1332 23.3094 24.45 19.1442 11.6264 6.37883 3.59631 2.17607 1.39802 0.960177 0.709698 1.84353
2010  7 5  3 0 1 -1 -1 250  3.11088 4.31089 8.4012 15.0323 21.9372 23.3727 18.3993 11.8747 6.87365 3.94317 2.39138 1.52928 1.04178 0.762507 2.01907 3.11088 4.31089 8.4012 15.0323 21.9372 23.3727 18.3993 11.8747 6.87365 3.94317 2.39138 1.52928 1.04178 0.762507 2.01907
2010  7 6  3 0 1 -1 -1 250  2.96372 4.95892 9.73356 16.0973 21.3426 22.0092 18.1147 12.3565 7.23558 3.96117 2.19729 1.2723 0.804898 0.573782 1.37851 2.96372 4.95892 9.73356 16.0973 21.3426 22.0092 18.1147 12.3565 7.23558 3.96117 2.19729 1.2723 0.804898 0.573782 1.37851
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

