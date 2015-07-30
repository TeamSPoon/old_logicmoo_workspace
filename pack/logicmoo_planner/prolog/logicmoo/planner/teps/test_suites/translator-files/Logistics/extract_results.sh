
for i in p1 p2 p3 p4 p5 p6 p6a p6b p7 p7a p8 p9 p10 p11 p12 p12a p13 p14 p15 p16 p17 p18 p19
do
 #echo "$i-----------------"
  grep "total time" ${i}_out.txt | sed -e 's/ //g' | sed -e 's/secondstotaltime//g'
 # grep -B 3 "time spent" ${i}_out.txt | grep ":" | grep -v "time spent" | sed -e 's/ //g' | sed -e 's/:.*$/\1/g'
done