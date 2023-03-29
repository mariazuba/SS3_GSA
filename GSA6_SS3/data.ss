#V3.30
#C data file for simple example
#
2003 #_styr
2021 #_endyr
1 #_nseas
12 #_months_per_seas
2 #_Nsubseasons
6 #_spawn_month
1 #_Nsexes
6 #_Nages
1 #_N_areas
4 #_Nfleets
#_fleetinfo
#_type	surveytiming	area	units	need_catch_mult	fleetname
1	-1	1	1	0	FISHERY	#_1
3	 1	1	2	0	SURVEY1	#_2
3	 1	1	2	0	SURVEY2	#_3
3	 1	1	2	0	SURVEY3	#_4
#_Catch data
#_year	season	fleet	catch	catch_se
 -999	1	1	    0	0.01	#_1         
 2003	1	1	353.4	0.01	#_2         
 2004	1	1	464.5	0.01	#_3         
 2005	1	1	322.9	0.01	#_4         
 2006	1	1	  333	0.01	#_5         
 2007	1	1	310.2	0.01	#_6         
 2008	1	1	293.4	0.01	#_7         
 2009	1	1	570.7	0.01	#_8         
 2010	1	1	530.8	0.01	#_9         
 2011	1	1	652.7	0.01	#_10        
 2012	1	1	447.7	0.01	#_11        
 2013	1	1	337.7	0.01	#_12        
 2014	1	1	248.2	0.01	#_13        
 2015	1	1	175.1	0.01	#_14        
 2016	1	1	170.5	0.01	#_15        
 2017	1	1	295.7	0.01	#_16        
 2018	1	1	425.4	0.01	#_17        
 2019	1	1	274.5	0.01	#_18        
 2020	1	1	170.4	0.01	#_19        
 2021	1	1	287.5	0.01	#_20        
-9999	0	0	    0	   0	#_terminator
#_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; >=30 for special types
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet	Units	Errtype	SD_Report
1	1	0	0	#_FISHERY
2	1	0	0	#_SURVEY1
3	1	0	0	#_SURVEY2
4	1	0	0	#_SURVEY3
#
#_CPUE_data
#_year	seas	index	obs	se_log
 2003	1	2	242.2	  0.2	#_1         
 2004	1	2	187.9	  0.2	#_2         
 2005	1	2	168.1	  0.2	#_3         
 2006	1	2	350.2	  0.3	#_4         
 2007	1	2	356.6	  1.2	#_5         
 2008	1	2	306.3	  0.6	#_6         
 2009	1	2	329.6	  0.3	#_7         
 2010	1	2	  147	  0.2	#_8         
 2011	1	2	116.9	  0.2	#_9         
 2012	1	2	 62.9	  0.5	#_10        
 2013	1	2	 38.4	  0.2	#_11        
 2014	1	2	383.7	  1.7	#_12        
 2015	1	2	161.7	  0.6	#_13        
 2016	1	2	238.7	  0.2	#_14        
 2017	1	2	 80.7	  0.3	#_15        
 2018	1	2	143.9	  0.4	#_16        
 2019	1	2	 28.6	  0.5	#_17        
 2020	1	2	 50.6	  0.3	#_18        
 2021	1	2	104.9	  0.2	#_19        
 2003	1	3	 33.6	  0.1	#_20        
 2004	1	3	 25.2	  0.1	#_21        
 2005	1	3	 20.2	  0.1	#_22        
 2006	1	3	 34.7	  0.2	#_23        
 2007	1	3	 27.9	  0.2	#_24        
 2008	1	3	 36.5	  0.1	#_25        
 2009	1	3	 78.8	  0.1	#_26        
 2010	1	3	125.6	  0.1	#_27        
 2011	1	3	 47.7	  0.1	#_28        
 2012	1	3	 17.4	  0.3	#_29        
 2013	1	3	 22.3	  0.1	#_30        
 2014	1	3	 23.1	  0.8	#_31        
 2015	1	3	 24.9	  0.2	#_32        
 2016	1	3	 37.8	  0.1	#_33        
 2017	1	3	 64.4	  0.2	#_34        
 2018	1	3	 96.3	  0.1	#_35        
 2019	1	3	 42.5	  0.1	#_36        
 2020	1	3	 40.7	  0.1	#_37        
 2021	1	3	 71.7	  0.1	#_38        
 2003	1	4	  2.7	0.001	#_39        
 2004	1	4	  3.3	0.001	#_40        
 2005	1	4	  2.8	0.001	#_41        
 2006	1	4	  3.6	0.001	#_42        
 2007	1	4	    2	0.001	#_43        
 2008	1	4	  4.5	0.001	#_44        
 2009	1	4	  5.2	0.001	#_45        
 2010	1	4	   11	0.001	#_46        
 2011	1	4	    3	0.001	#_47        
 2012	1	4	  2.9	0.001	#_48        
 2013	1	4	  2.5	0.001	#_49        
 2014	1	4	  3.1	0.001	#_50        
 2015	1	4	  4.9	0.001	#_51        
 2016	1	4	  1.3	0.001	#_52        
 2017	1	4	  2.4	0.001	#_53        
 2018	1	4	  1.7	0.001	#_54        
 2019	1	4	  2.6	0.001	#_55        
 2020	1	4	  1.3	0.001	#_56        
 2021	1	4	  1.9	0.001	#_57        
-9999	0	0	    0	    0	#_terminator
0 #_N_discard_fleets
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal
#
#_discard_fleet_info
#
#_discard_data
#
#_meanbodywt
0 #_use_meanbodywt
 #_DF_for_meanbodywt_T-distribution_like
#
#_population_length_bins
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp
10 # minimum size in the population (lower edge of first bin and size at age 0.00)
70 # maximum size in the population (lower edge of last bin)
0 #_use_lencomp
6 #_N_agebins
#
#_agebin_vector
0 1 2 3 4 5 #_agebin_vector
#
#_ageing_error
1 #_N_ageerror_definitions
#_	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1	   -1
0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001	0.001
#
#_age_info
#_mintailcomp	addtocomp	combine_M_F	CompressBins	CompError	ParmSelect	minsamplesize
-1	0.001	0	0	0	0	1	#_FISHERY
-1	0.001	0	0	0	0	1	#_SURVEY1
-1	0.001	0	0	0	0	1	#_SURVEY2
-1	0.001	0	0	0	0	1	#_SURVEY3
1 #_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths
 #_combine males into females at or below this bin number
#_Yr	Seas	FltSvy	Gender	Part	Ageerr	Lbin_lo	Lbin_hi	Nsamp	E0	E1	E2	E3	E4	E5
 2003	7	-1	3	0	2	-1	-1	75	 746.7	  1502	  216	  44	2.1	0.5	#_1         
 2004	7	-1	3	0	2	-1	-1	75	1425.9	2837.4	231.9	20.5	1.7	0.5	#_2         
 2005	7	-1	3	0	2	-1	-1	75	 212.8	1132.5	  277	36.6	1.2	0.7	#_3         
 2006	7	-1	3	0	2	-1	-1	75	2848.6	 861.6	256.2	30.9	1.2	1.4	#_4         
 2007	7	-1	3	0	2	-1	-1	75	   421	1409.5	163.7	25.2	4.6	1.2	#_5         
 2008	7	-1	3	0	2	-1	-1	75	 306.3	   986	208.9	28.1	4.7	1.2	#_6         
 2009	7	-1	3	0	2	-1	-1	75	 650.6	2902.1	  291	37.6	2.1	0.8	#_7         
 2010	7	-1	3	0	2	-1	-1	75	 197.6	  1962	404.5	18.4	2.2	0.3	#_8         
 2011	7	-1	3	0	2	-1	-1	75	 278.9	3410.3	324.9	15.8	1.1	0.1	#_9         
 2012	7	-1	3	0	2	-1	-1	75	 133.8	2464.1	198.4	11.7	0.7	0.1	#_10        
 2013	7	-1	3	0	2	-1	-1	75	  36.6	1360.9	223.6	19.4	0.3	0.1	#_11        
 2014	7	-1	3	0	2	-1	-1	75	  48.2	1086.1	136.4	13.3	0.3	0.2	#_12        
 2015	7	-1	3	0	2	-1	-1	75	  61.7	 791.1	106.1	 7.7	0.7	0.3	#_13        
 2016	7	-1	3	0	2	-1	-1	75	  79.5	 989.3	 78.1	 2.7	0.1	0.1	#_14        
 2017	7	-1	3	0	2	-1	-1	75	 809.8	1750.2	126.9	 4.6	0.2	0.1	#_15        
 2018	7	-1	3	0	2	-1	-1	75	 754.5	  2320	189.8	13.7	0.3	0.1	#_16        
 2019	7	-1	3	0	2	-1	-1	75	 107.4	1171.8	197.6	 5.5	0.2	0.1	#_17        
 2020	7	-1	3	0	2	-1	-1	75	 123.8	 996.9	 78.3	   3	0.3	0.1	#_18        
 2021	7	-1	3	0	2	-1	-1	75	 731.8	2299.1	 87.5	 4.6	0.1	0.1	#_19        
-9999	0	 0	0	0	0	 0	 0	 0	     0	     0	    0	   0	  0	  0	#_terminator
#
#_MeanSize_at_Age_obs
0 #_use_MeanSize_at_Age_obs
#_Yr	Seas	FltSvy	Gender	Part	AgeErr	Ignore	L0	L1	L2	L3	L4	L5	nmL0	nmL1	nmL2	nmL3	nmL4	nmL5
# -9999	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_1         
-9999  	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_terminator
0 #_N_environ_variables
0 #_N_sizefreq_methods
0 #_do_tags
0 #_morphcomp_data
0 #_use_selectivity_priors
#
999
