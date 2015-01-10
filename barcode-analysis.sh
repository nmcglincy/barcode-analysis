# BARCODE ANALYSIS
# 
mkdir barcode-analysis
grep -A 1 "@HS3:*" 112A.fastq > barcode-analysis/112A-lite.fastq
# 
# SEEMS TO WORK, BUT HAS THIS WEIRD "--" BETWEEN EACH ENTRY, AND IS LACKING IT FROM THE END OF THE FILE
# THIS COMMAND ADDS "--" TO THE END OF THE FILE
echo -- >> barcode-analysis/112A-lite.fastq
# 
# WOULD BE COOLER IF I COULD DELETE THEM
head -n15 barcode-analysis/112A-lite.fastq | sed '/^--/d'
# 
# FOR THE WHOLE FILE
sed -i '/^--/d' barcode-analysis/112A-lite.fastq
# 
# awk 'BEGIN{RS=">"}{gsub("\n","\t",$0); print ">"$0}' file
head -n8 112A-lite.fastq | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}'
# 
# IT WORKED!
# TIE IT ALL TOGETHER INTO A PIPE
grep -A 1 "@HS3:*" 112A.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/112A-lite.txt
grep -A 1 "@HS3:*" 112B.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/112B-lite.txt
grep -A 1 "@HS3:*" D19A.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19A-lite.txt
grep -A 1 "@HS3:*" D19B.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19B-lite.txt