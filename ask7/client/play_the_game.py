#!/usr/bin/env python

from sys import exit
from os import mkdir, unlink
from os.path import isdir, isfile
from subprocess import call, Popen, PIPE
from lxml import etree, html
import requests

def die (msg):
    print "Error:", msg
    print "Aborting..."
    exit(1)

def solve(number):

    # TODO: Branch solution so that it is less than 80 lines
    binary = str(bin(number))[2:]
    
    reversed_binary = binary[::-1]
    #print reversed_binary

    result = "!"
    if(reversed_binary[0] == '1'):
        result += ":"
    result += ":+"
    for b in reversed_binary[1:]:
        if(b == '1'):
            result += ":"
        result += ":+"
    cnt = len(filter(lambda x: x == '1', reversed_binary))

    result += "$" + cnt * "+" + ".@"
    return result

def main():

    # Global Constants
    url = "http://courses.softlab.ntua.gr/pl2/2016b/exercises/funge.php"
    html_file = "temp_resources/temp.html"

    # Initialize Session
    session = requests.Session()
    headers = {'User-Agent': 'Mozilla/5.0'}

    # Get request
    r = session.get(url)
    cookies = requests.utils.cookiejar_from_dict(requests.utils.dict_from_cookiejar(session.cookies))

    for i in xrange(1,11):

        root = html.fromstring(r.text)
        entry = root.xpath('//span[@class="question"]')[0]
        question_number = int(entry.text)
        
        print "Round " + str(i) + ", number " + str(question_number)

        # Solve the problem
        result = solve(question_number)
        print "Submitted solution: "
        print result

        # Send the POST request
        data = {'submit':'Submit!', 'program':result}
        r = session.post(url, headers=headers, data=data, cookies=cookies)
        #print r.text
    
        root = html.fromstring(r.text)
        entry = root.xpath('//p[@class="right"]')[0]
        print entry.text

        data = {'again':'Play again!', 'reset':'reset'}
        r = session.post(url, headers=headers, data=data, cookies=cookies)
        #print r.text

main()
#print solve(432475369)
