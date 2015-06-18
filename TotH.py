import requests
from bs4 import BeautifulSoup
import csv
import time
# Host: towerofthehand.com
# User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:38.0) Gecko/20100101 Firefox/38.0
# Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
# Accept-Language: en-US,en;q=0.5
# Accept-Encoding: gzip, deflate
# DNT: 1
# Referer: http://towerofthehand.com/books/101/003/index.html
# Cookie: __cfduid=dce5d857394f93acb1be5b21854b809451434639006; PHPSESSID=k3dkqipojjuauo5c8l7tmejma7; scopes=1=0&2=0&3=0&4=0
# Connection: keep-alive

ratings = []

for chapter in range(1, 74):
    headers = { 'Host': 'towerofthehand.com', \
                'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:38.0) Gecko/20100101 Firefox/38.0', \
                'Referer': "http://towerofthehand.com/books/105/" + "%03d" % (chapter) + "/index.html", \
                'Cookie': '__cfduid=dce5d857394f93acb1be5b21854b809451434639006; PHPSESSID=k3dkqipojjuauo5c8l7tmejma7; scopes=1=0&2=0&3=0&4=0', \
                'Connection': 'keep-alive', \
              }    
    
    url = "http://towerofthehand.com/books/105/" + "%03d" % (chapter) + "/boxscore.html"

    print url
    
    try:
        response = requests.post(url, headers=headers)
    except ConnectionError:
        time.sleep(2)
        try:
            response = requests.post(url, headers=headers)
        except ConnectionError:
            time.sleep(10)
            response = requests.post(url, headers=headers)
    parsedres = BeautifulSoup(response.text)    

    stattable = parsedres.find(attrs={'class':"stattable"})    

    rawRatings = stattable.tbody.tr.findAll('td')
    parsedRatings = map(lambda x: int(x.div.contents[0]), rawRatings)

    row = ["ADWD", chapter] + parsedRatings
    print row
    ratings.append(row)

    time.sleep(10)

with open("TotHratingsADWD.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(ratings)
