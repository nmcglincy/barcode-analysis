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
grep -A 1 "@HS3:*" 112B.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/112B-lite.txt &
grep -A 1 "@HS3:*" D19A.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19A-lite.txt &
grep -A 1 "@HS3:*" D19B.fastq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19B-lite.txt &
# 
# DIMENSIONS CHECK OUT: THE NUMBER OF LINES OF 112A-LITE.TXT IS EXACTLY 0.25 OF THE NUMBER OF LINES IN THE .FASTQ FILE
# 
# TIDY IT UP INTO A MORE MANAGABLE TABLE
head -n10 112A-lite.txt | sed '/^$/d' | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}'
# 
sed '/^$/d' 112A-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > 112A-liteT.txt &
sed '/^$/d' 112B-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > 112B-liteT.txt &
sed '/^$/d' D19A-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > D19A-liteT.txt &
sed '/^$/d' D19B-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > D19B-liteT.txt &
# 
# LOOKS EXACTLY AS I WANT IT
# 
# MOVING IT INTO MY LOCAL FOLDER FOR ANALYSIS IN R
scp mcglincy@compute1.ingolia-lab.org:/mnt/ingolialab/mcglincy/NINM001/Sample_NINM01_index1/split3/barcode-analysis/*liteT.txt .
# 
# DOING THE SAME THING FOR THE *norrna.fq FILES TO SEE IF ALL THE DEPLETION IS COMING FROM THE RRNA READS
grep -A 1 "@HS3:*" 112A_norrna.fq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/112A_norrna-lite.txt &
grep -A 1 "@HS3:*" 112B_norrna.fq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/112B_norrna-lite.txt &
grep -A 1 "@HS3:*" D19A_norrna.fq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19A_norrna-lite.txt &
grep -A 1 "@HS3:*" D19B_norrna.fq | sed '/^--/d' | awk 'BEGIN{RS="@"}{gsub("\n","\t",$0); print $0}' > barcode-analysis/D19B_norrna-lite.txt &
# 
# 
sed '/^$/d' 112A-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > 112A-liteT.txt &
sed '/^$/d' 112B-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > 112B-liteT.txt &
sed '/^$/d' D19A-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > D19A-liteT.txt &
sed '/^$/d' D19B-lite.txt | awk 'BEGIN{FS="[ ]|\t|#";OFS="\t"}{print $1,$2,$3,$4,$5}' > D19B-liteT.txt &
