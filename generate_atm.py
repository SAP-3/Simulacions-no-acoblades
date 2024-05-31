import itertools
#vel = 50
velocities = [0.75, 0.85, 0.95, 0.98]
directions1 = ['W', 'SW', 'E']
directions2 = ['S', 'SE', 'SW']
times = ["0000", "0600", "1200", "1800"]
dates = ["06 15", "06 16", "06 17"]
combinations = list(itertools.product(velocities, directions1, directions2))

final_file = f"/home/wrf/Desktop/ProjecteBaldomar/Vientos_{vel}mph.atm"

with open(final_file, 'w') as the_file:
    the_file.write("WINDS_AND_CLOUDS\n")
    the_file.write("ENGLISH\n")
    
    for date in dates:
        for time in times:
            for i, (vel, dir1, dir2) in enumerate(combinations):
                base_filename = f"{vel}_{dir1}_{dir2}_{i % 2}"
                str_add_1 = f"/home/wrf/Desktop/ProjecteBaldomar/{base_filename}_vel.asc"
                str_add_2 = f"/home/wrf/Desktop/ProjecteBaldomar/{base_filename}_ang.asc"
                str_add_3 = f"/home/wrf/Desktop/ProjecteBaldomar/{base_filename}_cld.asc"
                
                new_line = f"{date} {time} {str_add_1} {str_add_2} {str_add_3}\n"
                the_file.write(new_line)

print(f"File created: {final_file}")
