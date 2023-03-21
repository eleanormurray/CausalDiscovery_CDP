libname cdp "<raw data location>";

libname autoDAG '<working data location>';

data autoDAG.cdp_binary;
	set cdp.binary;
keep age_bin nonwhite mi_bin niha_bin1 rbw_bin 
			chf aci ap ic icia dig diur irk antiarr antihyp oralhyp  
			cardiom stelev hifastgluc cig inact anyqqs anystdep 
			anytwave fveb vcd hiheart hisysbp hidiasbp hibili hiserchol  
			hisertrigly hiseruric hiseralk hiplasurea hionegluc hiwhitecell 
			hineut hihemat  
			adhx15bin dth5;
run;

proc contents data = autoDAG.cdp_binary;
run;

PROC EXPORT DATA= autoDAG.cdp_binary
            OUTFILE= "<path>\cdp_binary.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
