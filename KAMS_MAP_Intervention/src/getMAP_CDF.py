"""
Python Script to pull CDF zip file from NWEA's MAP reporting site
    Need to authenticate to , then navigate to export page and save file with a an appropriate name
    An R script should then be called which culls that last two weeks data and reformat per Kate Mazurek's
    Two lines per student (one line for negative paycheck deductions and another for postive create points (well
    these acutally have zero value)
"""

import mechanize
import cookielib
import shutil
import os
from datetime import datetime

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)

# Follows refresh 0 but not hangs on refresh > 0
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

# Want debugging messages?
#br.set_debug_http(True)
#br.set_debug_redirects(True)
#br.set_debug_responses(True)

# User-Agent (this is cheating, ok?)
br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

# Open Kickboard's authenticate webpage
r=br.open('https://pdx-map01.mapnwea.org/cas/nweaLogin?service=https%3A%2F%2Fpdx-map01.mapnwea.org%2Fadmin%2F')

# Select form
br.select_form(nr=0)

#Add authentication details and submit
br.form['password']='haiKIPP1'
br.form['username']='chaid@kippchicago.org'
br.submit()

#navigate to csv page and save file
r=br.open('https://kippcreate.kickboardforteachers.com/culture-analysis/index')
f=br.retrieve('https://kippcreate.kickboardforteachers.com/culture-analysis/export/')[0] #f is path to downloaded file

#Now to save the file
#get current wowkring directory
dest_dir=os.getcwd()
#use to make file name
dest_file=dest_dir+'/culture-analysis_'+datetime.now().strftime('%y%m%d')+'.csv'

# Move and rename file in one line
os.rename(f,dest_file)


