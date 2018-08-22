import csv
import re
import requests
import os

# Sources referenced:
# [1] https://github.com/joshtemple/nypd-cases/blob/master/scrape.py
# [2] https://stackoverflow.com/questions/8024248/telling-python-to-save-a-txt-file-to-a-certain-directory-on-windows-and-mac
# [3] https://stackoverflow.com/questions/34503412/download-and-save-pdf-file-with-python-requests-module


def get_asset_url(url): # from [1]
    url = re.sub('www.', 'assets.', url)
    url = re.sub('.html$', '.pdf', url)
    url = re.sub('-(?=nypd-cases-)', '/', url)
    return url

list_of_urls = []
with open('nypd-discipline.csv') as file:
    reader = csv.reader(file)
    for row in reader:
        case = row[0]
        url = row[3]
        list_of_urls.append(url)
        asset_url = get_asset_url(url)
        response = requests.get(asset_url)
        with open(os.path.join('/Users/saahithi/Desktop/PyProjects/nypd','nypd-case'+ case), 'wb') as f: # from [2]
            f.write(response.content) # from [3]