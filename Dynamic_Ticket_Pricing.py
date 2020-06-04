#!/usr/bin/env python
# coding: utf-8

# In[98]:


import pymysql
import pandas as pd


# In[99]:


### CONNECT TO THE DATABASE

with pymysql.connect( host='datamine.rcac.purdue.edu', user='', passwd = '', database='', port = 3306) as cursor:
    unsold_inv = pd.read_sql("select * from 2017_2019_Unsold_Inventory", cursor.connection)
    primary = pd.read_sql("select * from 2020_Purdue_Project_Primary", cursor.connection)
    secondary = pd.read_sql("select * from 2020_Purdue_Project_Secondary", cursor.connection)
    opp = pd.read_sql("select * from Opponent_Data_Variable", cursor.connection)


# In[100]:


### TURN DATAFRAMES TO LIST FOR EASY MANIPULATION

primarylist = primary.values.tolist()
unsold_invlist = unsold_inv.values.tolist()
secondarylist = secondary.values.tolist()
opplist = opp.values.tolist()


# In[101]:


# SPLIT TEAM NAMES INTO SEPARATE LISTS 
for i in range(len(primarylist)):
    primarylist[i][4] = primarylist[i][4].split()


# In[102]:


### LIST OF EVERY NFL TEAM
nfl = [["Arizona", "Cardinals"],
["Atlanta", "Falcons"],
["Baltimore", "Ravens"],
["Buffalo", "Bills"],
["Carolina", "Panthers"],
["Chicago", "Bears"],
["Cincinnati", "Bengals"],
["Cleveland", "Browns"],
["Dallas", "Cowboys"],
["Denver", "Broncos"],
["Detroit", "Lions"],
["Green", "Bay", "Packers"],
["Houston", "Texans"],
["Indianapolis", "Colts"],
["Jacksonville", "Jaguars"],
["Kansas", "City", "Chiefs"],
["Los", "Angeles", "Chargers"],
["Los", "Angeles", "Rams"],
["Miami", "Dolphins"],
["Minnesota", "Vikings"],
["New", "England", "Patriots"],
["New", "Orleans", "Saints"],
["New", "York", "Giants"],
["New", "York", "Jets"],
["Oakland", "Raiders"],
["Philadelphia", "Eagles"],
["Pittsburgh", "Steelers"],
["San", "Francisco", "49ers"],
["Seattle", "Seahawks"],
["Tampa", "Bay", "Buccaneers"],
["Tennessee", "Titans"],
["Washington", "Redskins"],
      ["San", "Diego","Chargers"],
      ["St.", "Louis","Rams"]]


# In[103]:


# REMOVE COLTS FROM COLUMN TO CREATE MERGABLE COLUMN OF OPPONENT NAME ONLY


for i in range(len(primarylist)):
    for j in range(len(primarylist[i][4])):
        if primarylist[i][4][j] in ["Indianapolis","Indianpolis","Colts","vs.","vs"]:
            primarylist[i][4][j] = ""
                
    truelist = []
    for j in range(len(primarylist[i][4])):
        if primarylist[i][4][j] != "":
            truelist.append(primarylist[i][4][j])

    for j in range(len(truelist)):
        check = 0
        for k in range(len(nfl)):
            if len(truelist) == 2:
                if j > 0:
                    if truelist[j] == nfl[k][1] and truelist[j - 1] == nfl[k][0]:
                        primarylist[i][4] = str(nfl[k][0]) + " " + str(nfl[k][1])
                        check = 1
                        break
            elif len(truelist) == 3:
                if j > 1 and len(nfl[k]) == 3:

                    if truelist[j] == nfl[k][2] and truelist[j - 1] == nfl[k][1]:

                        if truelist[j - 2] == nfl[k][0]:
                            primarylist[i][4] = str(nfl[k][0]) + " " + str(nfl[k][1]) + " " + str(nfl[k][2])
                            check = 1
                            break
            elif len(truelist) == 1: 
### ACCOUNT FOR CHARGERS AND RAMS MOVE TO LA 
                if truelist[0] == "Chargers" and int(primarylist[i][13]) > 2016:
                    primarylist[i][4] = "Los Angeles Chargers"
                    check = 1
                    break
                elif truelist[0] == "Chargers":
                    primarylist[i][4] = "San Diego Chargers"
                    check = 1
                    break
                elif truelist[0] =="Rams" and int(primarylist[i][13]) > 2015:
                    primarylist[i][4] = "Los Angeles Rams"
                    check = 1
                    break
                elif truelist[0] == "Rams":
                    primarylist[i][4] = "St. Louis Rams"
                    check = 1
                    break
                elif truelist[0] in nfl[k]:
                    if len(nfl[k]) == 3:
                        primarylist[i][4] =  nfl[k][0] + " " + nfl[k][1] + " " + nfl[k][2]
                    else:
                        primarylist[i][4] = nfl[k][0] + " " + nfl[k][1]
                    check = 1
                    break
        if check == 1:
            break
            
            


# In[104]:


### FIX LACK OF SPACING IN SOME LINES
for i in range(len(opplist)):
    if opplist[i][4] == "St.Louis Rams":
        opplist[i][4]  = "St. Louis Rams"
oppPD = pd.DataFrame(opplist) 
oppPD.columns = opp.columns
oppPD = oppPD.iloc[:,1:]
oppPD


# In[105]:


### FIX ERROR OF TWO DIFFERENT GAMES HAVING SAME EVENT ID


for i in range(len(primarylist)):
    if primarylist[i][3] == "CLT1214" and primarylist[i][4] == "Houston Texans":
        primarylist[i][3] = "CLT1214a"
primary1 = pd.DataFrame(primarylist)
primary1.columns = primary.columns
primary = primary1
secondarylist = secondary.values.tolist()
for i in range(len(secondarylist)):
    if secondarylist[i][1] == "CLT1214" and secondarylist[i][14] == 2014:
        secondarylist[i][1] = "CLT1214a"
secondary1 = pd.DataFrame(secondarylist)
secondary1.columns = secondary.columns
secondary = secondary1


# In[106]:


### TURN ADJUSTED PRIMARY LIST INTO DATAFRAME

primary1 = pd.DataFrame(primarylist)
primary1
primary1.columns = primary.columns
primary = primary1


# In[107]:


### MERGE PRIMARY AND OPP DATASETS

primaryopp = primary.merge(oppPD,left_on = ["EventDesc","Season"],right_on = ["VisitingTeam","Season"], how = "outer")


# In[108]:


### CREATE DATAFRAME OF ONE VALUE PER UNIQUE EVENT CODE TO ALLOW FOR MERGING OF SECONDARY AND OPP

primaryopp1 = primaryopp.groupby('EventCode', group_keys=False).apply(lambda df: df.sample(1))
primaryopp1 = primaryopp1[["EventCode","EventDesc"]]


# In[110]:


###MERGE SECONDARY WITH ADJUSTED DATAFRAME
secondaryopp = secondary.merge(primaryopp1,left_on = "event_name",right_on = "EventCode", how = "outer")


# In[111]:


###MERGE NEW SECONDARY DATAFRAME WITH OPP
secondaryopp1 = secondaryopp.merge(oppPD,left_on = ["EventDesc","season_year"],right_on = ["VisitingTeam","Season"], how = "outer")


# In[112]:


### SAVE DATAFRAMES TO CSV FOR USE IN R

secondaryopp1.to_csv("secondary_opp.csv")
primaryopp.to_csv("primary_opp.csv")


# In[113]:





# In[ ]:





# In[ ]:





# In[ ]:




