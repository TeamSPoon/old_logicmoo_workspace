
for i in p1 p2 p3 p4 p5 p6 p6a p6b p7 p7a p8 p9 p10 p11 p12 p12a p13 p14 p15 p16 p17 p18 p19
do
  echo "Planning for $i"
  ../../ffx/ff -o logistics_${i}_dp.pddl -f ${i}_dp.pddl > ${i}_out.txt 2> ${i}_err.txt
done