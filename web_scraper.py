'''
Get trumps speaches.
author: Mike Jaron
date: 4/1/17
'''

# import the library used to query a website
import urllib2
# import the BeautifulSoup functions to parse data returned from the website
from bs4 import BeautifulSoup
import os
import pandas as pd

# get current working directory
cwd = os.getcwd()
directory = cwd + '/data/'
if not os.path.exists(directory):
    os.makedirs(directory)

# specify the url
base_url = "http://www.presidency.ucsb.edu/"
link_url = "2016_election_speeches.php?candidate=45&campaign=2016TRUMP&doctype=5000"
page_url = base_url + link_url
# Query the website and return the html to the variable 'page'
page = urllib2.urlopen(page_url)
# print page.read()

# Parse the html in the 'page' variable, and store it in Beautiful Soup format
soup = BeautifulSoup(page, "html.parser")

# this actually shows you the html that we are parsing, good to print out once
# to understand it then don't need to do it again
# print soup.prettify(encoding='utf-8')
# print soup.title.string

# now I will find all links in the file
page_links = []
all_links = soup.find_all('a')
for link in all_links:
	link = link.get('href')
	# setting condition of types of links
	if '../ws/' in link:
		page_links.append(link)
	# get only unique page links, aka get rid of duplicates
	# also make it back to a list so it can be looped through
	page_links = list(set(page_links))
print len(page_links), " total page links to go through"


# start a counter to see how many articles we actually got
count = 0
titles = []
dates = []
locations = []
links = []
for link in page_links:
	link = base_url + link[3:]
	links.append(link)
	print link
	# do the same thing as above
	# try the link, if the link doesnt exist break for loop and go to next link
	try:
		page = urllib2.urlopen(link)
		count += 1  # if the page works add 1 to the count
	except:
		continue

	soup = BeautifulSoup(page, "html.parser")
	# print soup.prettify(encoding='utf-8')

	title = soup.title.string
	titles.append(title)
	# find the meta that has the date, and get the content of that tag 
	meta_title = soup.find('meta', attrs={'name': 'title'})['content']
	
	# find the '-' that devides the dates
	date_idx = meta_title.find(' - ')
	date = meta_title[date_idx + 3:]
	dates.append(date)

	# find the 'in' that divides the dates
	location_idx = meta_title.find(' in ')
	location = meta_title[location_idx + 4:date_idx]
	locations.append(location)

	# the text is all together, so find the div and then use .getText
	text = soup.find('span', attrs={'class': 'displaytext'}).getText()
	
	# write everything to file
	text_file = open(directory + title + ".txt", "w")
	text_file.write(text.encode('utf-8'))
	text_file.close()

meta_df = pd.DataFrame()
meta_df['title'] = titles
meta_df['date'] = dates
meta_df['location'] = locations
meta_df['link'] = links
meta_df.to_csv(directory + 'meta_data.csv', index=False)
			
print count, "total pages actually scraped"
print len(page_links), "total possible pages"


