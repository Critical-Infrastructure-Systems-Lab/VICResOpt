# Update 09 Mar 2020 - separate indices calculation from main codes
# Calculate NSE - Nash-Sutcliffe Efficiency coefficient
def NSE3(obs,mol,k):
    numerator = 0
    denominator = 0
    meangauge = 0
    for i in range(len(obs)):
        numerator+=pow(abs(mol[i][k])-obs[i],2)
        meangauge+=obs[i]
    meangauge=meangauge/len(obs)
    for i in range(len(obs)):
        denominator+=pow(obs[i]-meangauge,2)
    return 1-numerator/denominator

def NSE(obs,mol):
    numerator = 0
    denominator = 0
    meangauge = 0
    for i in range(len(obs)):
        numerator+=pow(abs(mol[i])-obs[i],2)
        meangauge+=obs[i]
    meangauge=meangauge/len(obs)
    for i in range(len(obs)):
        denominator+=pow(obs[i]-meangauge,2)
    return 1-numerator/denominator

# Calculate TRMSE - The Box-Cox transformed root mean squared error
def TRMSE3(obs,mol,k):
    trmse = 0
    for i in range(len(obs)):
        zmdl = (pow((abs(mol[i][k])+1),0.3)-1)/0.3
        zobs = (pow((abs(obs[i])+1),0.3)-1)/0.3
        trmse+=pow(zmdl-zobs,2)
    return pow(trmse/len(obs),0.5)

def TRMSE(obs,mol):
    trmse = 0
    for i in range(len(obs)):
        zmdl = (pow((abs(mol[i])+1),0.3)-1)/0.3
        zobs = (pow((abs(obs[i])+1),0.3)-1)/0.3
        trmse+=pow(zmdl-zobs,2)
    return pow(trmse/len(obs),0.5)

# Calculate MSDE - Mean square derivative error
def MSDE(obs,mol,k):
    msde = 0
    for i in range(1,len(obs)):
        msde+=pow(((obs[i]-obs[i-1])-(mol[i][k]-mol[i-1][k])),2)
    return msde/len(obs)

# Calculate ROCE - the runoff coefficient error
def ROCE(obs,mol,k):
    summdl = 0
    sumobs = 0
    for i in range(len(obs)):
        summdl+=mol[i][k]
        sumobs+=obs[i]
    return abs(summdl-sumobs)/sumobs