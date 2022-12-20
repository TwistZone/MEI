import os
import numpy as np
import sys

def run(executable1, executable2,executable3, home_dir, result_dir, seed, max_time, file, input, n, p,r):
    os.chdir(home_dir)

    out = os.popen(f'./{executable1} {max_time} "{input}/{file}"').read()
    arcs = out.split("\n")[0]
    output1 = out.split("\n")[1]
    out = os.popen(f'./{executable2} {max_time} "{input}/{file}"').read()
    output2 = out.split("\n")[1]
    out = os.popen(f'./{executable3} {max_time} "{input}/{file}"').read()
    output3 = out.split("\n")[1]
    os.chdir(result_dir)
    with open("results.txt", "a") as f:
        f.write(f"{arcs} {output1} {output2} {output3} {n} {p} {r} {seed}\n")
    f.close()    
    if "-1" in output1 or "-1" in output2 or "-1" in output3:
        return 1
    return 0        

def log(string):
    with open("log.txt", "a") as f:
        f.write(string+"\n")
    print(string)

vertexes = 1000
max_cap = 500
seed = 6969 #2342135 #341253
max_time = 10
start1 = 100
start2 = 50
step = 100
step2 = 50


exec1 = "Dinic.cpp"
exec2 = "MPM.cpp"
exec3 = "EK.cpp"
out1 = "Dinic"
out2 = "MPM"
out3 = "EK"
data_dir = "dataset"
result_dir = "results"

#compile files
log(f"Compiling {exec1}...")
os.system(f"g++ -O3 -DNDEBUG {exec1} -o {out1}")
log(f"{exec1} compiled!")

log(f"Compiling {exec2}...")
os.system(f"g++ -O3 -DNDEBUG {exec2} -o {out2}")
log(f"{exec2} compiled!")

log(f"Compiling {exec2}...")
os.system(f"g++ -O3 -DNDEBUG {exec3} -o {out3}")
log(f"{exec3} compiled!")

#create directory

dirs = [x.replace("test", "") for x in os.listdir('.') if os.path.isdir(x) and 'test' in x]
log(f"Directories found: {dirs}")
home_path = os.getcwd()

test_dir = None
dataset_dir = None

#no test directory yet
if dirs == []:
    test_dir = os.path.join(home_path, f"test0")
    dataset_dir = os.path.join(test_dir,"dataset")
    os.mkdir(test_dir)
    os.mkdir(dataset_dir)

#create next index test directory
else:
    index = int(dirs[-1]) + 1
    test_dir = os.path.join(home_path, f"test{index}")
    dataset_dir = os.path.join(test_dir, "dataset")
    os.mkdir(test_dir)
    os.mkdir(dataset_dir)

with open("seeds.txt","a") as f:
    f.write(str(seed) + "\n")

os.chdir(test_dir)
log(f"Output results to {test_dir}")
#create results file
with open("results.txt", "w") as f:
    f.write("Arcs Dinic MPM EK vertexes probability capacity seed\n")    


for p in np.arange(0.1,1,0.1):
    p = p.round(2)
    fails = 0    
    for n in range(start1,vertexes +1,step):
        for r in range(start2,max_cap+1,step2):
            seed = seed + 10
            log(f"\nNew test data\nProbability: {p} \nVertexes: {n} \nMaximum Capacity: {r}\nSeed: {seed}")
            #generate data
            os.chdir(home_path)
            file = f"{n}_{p}_{r}_{seed}.txt"
            os.system(f'python3 gen.py {n} {p} {r} {seed} "{dataset_dir}/{file}"')
            log("Data generated!")
            #run c code
            fp =  dataset_dir + "/" + file
            with open(fp,"r") as f:
                if f.read() == "-1":
                    os.remove(fp)
                    continue 
            run(out1, out2,out3, home_path, test_dir, seed, max_time, file, dataset_dir, n, p,r)
            '''  fails = fails + 1    
                if fails > 4: 
                    with open('results.txt','r') as f:
                        lines = f.readlines()  
                    with open('results.txt','w') as f:
                        f.writelines(lines[:-5])
                    break
            else: 
                fails = 0    
            '''