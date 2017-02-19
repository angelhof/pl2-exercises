#!/usr/bin/env python


from lxml import etree, html
import requests
import sys

def die (msg):
    print "Error:", msg
    print "Aborting..."
    exit(1)

def solve(number):

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

    # Wrap the program
    # Hack: Wraps programs that are maximum two lines long
    if(len(result) > 78):
        #print len(result)
        first = result[:len(result)/2]
        second = result[len(result)/2:]
        if len(second) > len(first):
            first = first + " "
        first += "v\r\n"
        second += ">"
        result = first + second
    return result

def main():

    if(len(sys.argv) != 2):
        die('Wrong arguments')

    # Global Constants
    url = "http://courses.softlab.ntua.gr/pl2/2016b/exercises/funge.php"
    url = sys.argv[1]
    html_file = "temp_resources/temp.html"

    # Initialize Session
    session = requests.Session()
    headers = {'User-Agent': 'Mozilla/5.0'}

    # Get request
    r = session.get(url)
    cookies = requests.utils.cookiejar_from_dict(requests.utils.dict_from_cookiejar(session.cookies))

    # Counter
    right_counter = 0

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
        # Debug
        #print r.text
        #print html.fromstring(r.text).xpath('//span[@class="question"]')[0].text
    
        root = html.fromstring(r.text)
        
        right_span = root.xpath('//p[@class="right"]')
        if(len(right_span) > 0):
            entry = right_span[0]
            right_counter += 1
        else:
            entry = root.xpath('//p[@class="wrong"]')[0]
        print entry.text

        data = {'again':'Play again!'}
        r = session.post(url, headers=headers, data=data, cookies=cookies)
        #print r.text

    print "You got " + str(right_counter) + " out of 10 right!!"

main()
#print solve(523529267)
