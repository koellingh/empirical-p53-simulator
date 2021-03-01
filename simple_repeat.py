#a script to run several replicates of several treatments locally

directory = "trial1/"
seeds = range(1, 7)
mut_malig = [0.0, 0.25, 0.5, 0.75, 0.9]
mut_benig = [0.0, 0.25, 0.5, 0.75, 0.9]
p_53 = [0.0, 0.05, 0.06, 0.08, 0.13, 0.15, 0.2]

import subprocess

def cmd(command):
    '''This wait causes all executions to run in sieries.                          
    For parralelization, remove .wait() and instead delay the                      
    R script calls unitl all neccesary data is created.'''
    return subprocess.Popen(command, shell=True).wait()

def silent_cmd(command):
    '''This wait causes all executions to run in sieries.                          
    For parralelization, remove .wait() and instead delay the                      
    R script calls unitl all neccesary data is created.'''
    return subprocess.Popen(command, shell=True, stdout=subprocess.PIPE).wait()

print("Copying MySettings.cfg to "+directory)
cmd("cp MySettings.cfg "+directory)

for a in seeds:
    for b in mut_malig:
        for c in mut_benig:
            for d in p_53:
                command_str = './evo-algo -SEED '+str(a)+' -MUT_MALIG '+str(b)+' -MUT_BENIG '+str(c)+' -INIT_P53 '+str(d)+' -OUTPUT_FILE trial1/SP'+str(a)+'malig'+str(b)+'benig'+str(c)+'p53'+str(d)
        
                print(command_str)
                cmd(command_str)