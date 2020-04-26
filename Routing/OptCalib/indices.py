# Update 09 Mar 2020 - separate indices calculation from main codes
# Calculate NSE - Nash-Sutcliffe Efficiency coefficient
def NSE(obs,mol):
    numerator = 0
    denominator = 0
    meangauge = 0
    count = 0
    for i in range(len(obs)):
        if (obs[i]>=0):
            numerator+=pow(abs(mol[i])-obs[i],2)
            meangauge+=obs[i]
            count+=1
    meangauge=meangauge/count
    for i in range(len(obs)):
        if (obs[i]>=0):
            denominator+=pow(obs[i]-meangauge,2)
    return 1-numerator/denominator

# Calculate TRMSE - The Box-Cox transformed root mean squared error
def TRMSE(obs,mol):
    trmse = 0
    count = 0
    for i in range(len(obs)):
        if (obs[i]>=0):
            zmdl = (pow((abs(mol[i])+1),0.3)-1)/0.3
            zobs = (pow((abs(obs[i])+1),0.3)-1)/0.3
            trmse+=pow(zmdl-zobs,2)
            count+=1
    return pow((trmse/count),0.5)

# Calculate MSDE - Mean square derivative error
def MSDE(obs,mol):
    msde = 0
    count = 0
    for i in range(1,len(obs)):
        if ((obs[i]>=0) and (obs[i-1]>=0)):
            msde+=pow(((obs[i]-obs[i-1])-(mol[i]-mol[i-1])),2)
            count+=1
    return msde/count

# Calculate ROCE - the runoff coefficient error
def ROCE(obs,mol):
    summdl = 0
    sumobs = 0
    count = 0
    for i in range(len(obs)):
        if (obs[i]>=0):
            summdl+=mol[i]
            sumobs+=obs[i]
            count+=1
    return abs(summdl-sumobs)/count