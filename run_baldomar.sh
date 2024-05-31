#!/bin/bash

ll_dia=(0 1)
ll_ratio=(0.75 0.85 0.95 0.98)
ll_dir_wx=('SW' 'W' 'E')
ll_dir_x6=('S' 'SE' 'SW')

for dia in ${ll_dia[@]}; do
	for ratio in ${ll_ratio[@]}; do
		for dir_wx in ${ll_dir_wx[@]}; do
			for dir_x6 in ${ll_dir_x6[@]}; do

				# Executa el script de Python amb les variables com a arguments
				python3 generate_csv.py $ratio $dia "$dir_wx" "$dir_x6"

				# Executa WindNinja
				echo " "
				echo "========================================================"
				echo "ratio=$ratio dia=$dia dir_wx=$dir_wx dir_x6=$dir_x6"
				echo "========================================================"
				
				if [ $dia -eq 1 ]
				then
					/home/wrf-chem/Desktop/build/src/cli/WindNinja_cli dia.cfg
				else
					/home/wrf-chem/Desktop/build/src/cli/WindNinja_cli nit.cfg
				fi

				# Canviar nom
				cd output
				for i in Baldomar*; do
					mv "$i" "${ratio}_${dir_wx}_${dir_x6}_${dia}_${i}"
				done
				
				
				cd ..

			done
		done
	done
done
