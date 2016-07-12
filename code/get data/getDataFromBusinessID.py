import argparse
import json
import pprint
import sys
import urllib
import urllib2

import oauth2
import codecs
import time


API_HOST = 'api.yelp.com'
BUSINESS_PATH = '/v2/business/'

# OAuth credential placeholders that must be filled in by users.
CONSUMER_KEY = 'bmWMQaBMFl-3_Vo1TgAMsA'
CONSUMER_SECRET = 'OZZFgxJoFgH5VaVOSdsFjVwA7UE'
TOKEN = '4Hj0Nz-nNo7qccOdNRbdFmrCljxu5f5b'
TOKEN_SECRET = 'iMBDI5z6BvOc9DA3HTSRm5e50ow'


def request(host, path, url_params=None):
    """Prepares OAuth authentication and sends the request to the API.
    Args:
        host (str): The domain host of the API.
        path (str): The path of the API after the domain.
        url_params (dict): An optional set of query parameters in the request.
    Returns:
        dict: The JSON response from the request.
    Raises:
        urllib2.HTTPError: An error occurs from the HTTP request.
    """
    url_params = url_params or {}
    url = 'http://{0}{1}?'.format(host, urllib.quote(path.encode('utf8')))

    consumer = oauth2.Consumer(CONSUMER_KEY, CONSUMER_SECRET)
    oauth_request = oauth2.Request(method="GET", url=url, parameters=url_params)

    oauth_request.update(
        {
            'oauth_nonce': oauth2.generate_nonce(),
            'oauth_timestamp': oauth2.generate_timestamp(),
            'oauth_token': TOKEN,
            'oauth_consumer_key': CONSUMER_KEY
        }
    )
    token = oauth2.Token(TOKEN, TOKEN_SECRET)
    oauth_request.sign_request(oauth2.SignatureMethod_HMAC_SHA1(), consumer, token)
    signed_url = oauth_request.to_url()
    
    #print u'Querying {0} ...'.format(url)

    conn = urllib2.urlopen(signed_url, None)
    try:
        response = json.loads(conn.read())
    finally:
        conn.close()

    return response


def get_business(business_id):
    """Query the Business API by a business ID.
    Args:
        business_id (str): The ID of the business to query.
    Returns:
        dict: The JSON response from the request.
    """
    business_path = BUSINESS_PATH + business_id

    return request(API_HOST, business_path)

    


def main():

    file = codecs.open('sac_id.txt','r', encoding='UTF-8')
    data = file.readlines()
    for i in range(len(data)):
        data[i] = data[i].replace('\n', '')

    businesses = data
    

    for i in range(len(businesses)):
        business_id = businesses[i]
        response = get_business(business_id)
        print json.dumps(response, ensure_ascii=False, encoding = 'utf-8').encode('utf-8')
        time.sleep(1)
 


if __name__ == '__main__':
    main()