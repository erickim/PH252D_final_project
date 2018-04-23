rscript code/g_computation.R bootstrap=TRUE type=\"continuous\" B=500 n=2500 > output/g_comp_continuous.txt
rscript code/g_computation.R bootstrap=TRUE type=\"binary\" B=500 n=2500 > output/g_comp_binary.txt
rscript code/iptw.R bootstrap=TRUE type=\"continuous\" B=500 n=2500 > output/iptw_continuous.txt
rscript code/iptw.R bootstrap=TRUE type=\"binary\" B=500 n=2500 > output/iptw_binary.txt

