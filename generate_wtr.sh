#!/bin/bash

output_name='weather'
elevacio=10
mes=6

for quantil in 0.75 0.85 0.95 0.98
do
    output_file="${output_name}_${quantil}.txt"
    echo "METRIC" > "$output_file"
    for dia in 15 16 17
    do
        h1=$(awk -v dia="$dia" -v q="$quantil" 'BEGIN{FS=","} $1 == dia && $6 == q {print $2}' temperatures.csv)
        h2=$(awk -v dia="$dia" -v q="$quantil" 'BEGIN{FS=","} $1 == dia && $6 == q {print $3}' temperatures.csv)
        t1=$(awk -v dia="$dia" -v q="$quantil" 'BEGIN{FS=","} $1 == dia && $6 == q {print int($4+0.5)}' temperatures.csv)
        t2=$(awk -v dia="$dia" -v q="$quantil" 'BEGIN{FS=","} $1 == dia && $6 == q {print int($5+0.5)}' temperatures.csv)
        
        printf "%d %d %d %s %s %d %d %d\n" "$mes" "$dia" 00 "$h1" "$h2" "$t1" "$t2" "$elevacio" >> "$output_file"

    done
done
