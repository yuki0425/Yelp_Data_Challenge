import json
import pandas as pd
import matplotlib.pyplot as plt
import nltk
import nltk.tag.sequential as seq

file_path = '/Users/Yuki_Liu/Documents/Spring2015/YelpProject/Yelp_data/'
business = 'business.json'
yelp_business = []

review = 'review.json'
yelp_review = []

tip = 'tip.json'
yelp_tip = []

user = 'user.json'
yelp_user = []

checkin = 'checkin.json'
yelp_checkin = []

def read_in_data(file, listname):
    data = open(file_path+file, "r")
    for line in data:
        try:
            tmp = json.loads(line)
            listname.append(tmp)
        except:
            continue

read_in_data(business, yelp_business)

read_in_data(review, yelp_review)

file_path = '/Users/Yuki_Liu/Documents/Spring2015/YelpProject/'

f = open(file_path+'positive-words.txt')
pos_wd = [line[:-(line[-1] == '\n') or len(line)+1] for line in f]
f.close()

f = open(file_path+'negative-words.txt', 'r')
neg_wd = [line[:-(line[-1] == '\n') or len(line)+1] for line in f]
f.close()

def extract(key, listname):
    return [i[key] if key in i else '' for i in listname]

def extract2(key1, key2, listname):
    return[{key1:i[key1], key2:i[key2]} if key2 in i else {key1:i[key1]} for i in listname]

city = extract('city', yelp_business)
state = extract('state', yelp_business)

rev_busID = extract('business_id', yelp_review)

bus_busID = extract('business_id', yelp_business)

business_info['business_ID'] = bus_busID
business_info['categories'] = categories
business_info['price range'] = bus_price
business_info['stars'] = stars
business_info['city'] = city
business_info['state'] = state

business_expensive = business_info[business_info['price range']==4]
business_cheap = business_info[business_info['price range']==1]
business_regular = business_info[business_info['price range']==2]
business_regular2 = business_info[business_info['price range']==3]


import numpy as np
np.mean(business_expensive)


import itertools
reviews_dict = {}
for key, group in itertools.groupby(yelp_review, lambda item: item["business_id"]):
    reviews_dict.update({key: [item["text"] for item in group]})

stars_dict = {}
for key, group in itertools.groupby(yelp_review, lambda item: item["business_id"]):
    stars_dict.update({key:[item["stars"] for item in group]})


cat_id = extract2('business_id', 'categories', yelp_business)
res_dict = [i for i in cat_id if 'Restaurants' in i['categories']]
res_id = [i['business_id'] for i in res_dict]

rest_dict = {}
for key in res_id:
    if key in reviews_dict.keys():
        rest_dict[key] = reviews_dict[key]
    else:
        continue

from nltk.tokenize import sent_tokenize
from nltk.tokenize import word_tokenize


rest_sen = []
for key in rest_dict.keys():
    review_sen = []
    for i in rest_dict[key]:
        review_sen.append(sent_tokenize(i))
    rest_sen.append({key:review_sen})


from nltk.tokenize import RegexpTokenizer
tokenizer = RegexpTokenizer('\s+', gaps=True)

for i in rev_sen[2000:10000]:
    rev_word1.append([word_tokenize(k) if not k== [] else [] for k in i])


vegas_id = [i['business_id'] for i in city_res if i['city'] == 'Las Vegas']

def extract_id(key1, value1):
    return [i['business_id'] for i in city_res if i[key1] == value1]

vegas_review, pheonix_review, charlotte_review, pittsburgh_review, montreal_review = ({} for i in range(5))

pheonix_id = extract_id('city', 'Phoenix')
charlotte_id = extract_id('city', 'Charlotte')
pittsburgh_id = extract_id('city', 'Pittsburgh')
montreal_id = extract_id('city', 'Montréal')


def extract_review(r_dict, list_id):
    for key in list_id:
        if key in reviews_dict.keys():
            r_dict[key] = reviews_dict[key]
        else:
            continue

extract_review(vegas_review, vegas_id)
extract_review(pheonix_review, pheonix_id)
extract_review(pittsburgh_review, pittsburgh_id)
extract_review(charlotte_review, charlotte_id)
extract_review(montreal_review, montreal_id)


vegas_sen=[]
for key in vegas_review.keys():
    review_sen = []
    for i in vegas_review[key]:
        review_sen.append(sent_tokenize(i))
    vegas_sen.append({key:review_sen})


pheonix_sen=[]
def tok_sen(list_sen, dict_review):
    for key in dict_review.keys():
        review_sen = []
        for i in dict_review[key]:
            review_sen.append(sent_tokenize(i))
        list_sen.append({key:review_sen})


charlotte_sen, pittsburgh_sen, montreal_sen = ([] for i in range(3))
vegas_word, pheonix_word, charlotte_word, pittsburgh_word, montreal_word = ({} for i in range(5))


tok_sen(charlotte_sen, charlotte_review)
tok_sen(pittsburgh_sen, pittsburgh_review)
tok_sen(montreal_sen, montreal_review)



vegas_word, pheonix_word, charlotte_word, pittsburgh_word, montreal_word = ({} for i in range(5))




word = []
def tok_word(dict_words,list_sen):
    for item in list_sen:
        for key,value in item.items():
            word = []
            for list_sents in value:
                sents = []
                for sent in list_sents:
                    sents.append(word_tokenize(sent))
                word.append(sents)
            dict_words.update({key:word})


tok_word(pheonix_word, pheonix_sen)
tok_word(vegas_word, vegas_sen)
tok_word(charlotee_word, charlotte_sen)
tok_word(montreal_word, montreal_sen)
tok_word(pittsburgh_word, pittsburgh_sen)

from itertools import chain

def unpack_words(words_dict):
    return list(chain.from_iterable(chain.from_iterable(chain.from_iterable(words_dict.values()))))

vegas_ts = unpack_words(vegas_words)

pittsburg_ts = unpack_words(pittsburgh_word)
montreal_ts = unpack_words(montreal_word)
charlotte_ts = unpack_words(charlotte_word)
pheonix_ts = unpack_words(montreal_word)

                                 
wrd = nltk.corpus.words.words("en")

u_tagger = seq.UnigramTagger(t_corpus)
tagged_popular = u_tagger.tag(pop)


def nonwords(list):
    return[w for w in list if w not in wrd]

def cull(list):
    return[w for w in list if w not in wrd_s]

def clean(list_ts):
    return [i for i in list_ts if (len(i)>1 or i == 'I')]

charlotte_cl = clean(charlotte_ts)
montreal_cl = clean(montreal_ts)
pittsburgh_cl = clean(pittsburg_ts)
pheonix_cl = clean(pheonix_ts)
vegas_cl = clean(vegas_ts)

tagged_all = u_tagger.tag(pheonix_cl+vegas_cl+charlotte_cl+montreal_cl+pittsburgh_cl)


#### Keywords selection
all_pop = nltk.FreqDist(tagged_all).most_common(1000)
pop_nn = [w_freq for w_freq in all_pop if w_freq[0][1] in ['NN', 'NNS', None]]

v_pop = [w_freq for w_freq in all_pop if w_freq[0][1] in ['VBD', 'VBP', 'VB', 'VBN']]
pop_jj = [w_freq for w_freq in all_pop if w_freq[0][1] in ['JJ']]


trash = ['food', 'something', 'excellent', 'decent', 'Very', 'et', 'perfect', 'Our', 'Good',
         'ate', 'Chicken', 'delicious', 'amazing']
pop_ns = [v for i,v in enumerate(pop_nns) if v not in trash][:120]
pop_ns_freq = [v for v in pop_nn[:150] if v[0][0] not in trash][:120]



alphab = pop_ns[:30]
frequencies = [f[1] for f in pop_ns_freq][:30]
pos = np.arange(len(alphab))
width = 1.0     # gives histogram aspect to the bar diagram

ax = plt.axes()
ax.set_xticks(pos + (width / 2))
ax.set_xticklabels(alphab, rotation = 'vertical')

plt.title('Noun keywords')
plt.xlabel('Top 30 nouns in restaurant reviews')
plt.ylabel('Frequencies in restaurant reviews')
plt.bar(pos, frequencies, width, color='blue')
plt.grid(True)
plt.show()


### Noun Phrases selection

def subcatter(x, y, mycorp_pos):
    tt = []
    for i, t in enumerate(mycorp_pos[:len(mycorp_pos)-1]):
        if ((t[1] != None) and (mycorp_pos[i+1][1] != None)):
            if t[1].startswith(x):
                if mycorp_pos[i+1][1].startswith(y):
                    tt.append(t[0] + ' '+ mycorp_pos[i+1][0])
        elif (t[1] == None) and (mycorp_pos[i+1][1] == None):
            tt.append(t[0] + ' '+ mycorp_pos[i+1][0])
        elif (t[1] == None) and (mycorp_pos[i+1][1] != None):
            if mycorp_pos[i+1][1].startswith(y):
                tt.append(t[0] + ' '+ mycorp_pos[i+1][0])
        else:
            if t[1].startswith(x):
                tt.append(t[0]+ ' ' + mycorp_pos[i+1][0])


return set(tt)


set_nns = subcatter('NN', 'NN', tagged_all)

nns = nltk.FreqDist(set_nns).most_common(100)


tagged_vegas = {}
tagged_pheonix, tagged_montreal, tagged_pittsburgh, tagged_charlotte = [{} for i in range(4)]


def tag_wd(tagged_dict, dict_wd):
    for key, list_reviews in dict_wd.items():
        tagged_reviews=[]
        for review in list_reviews:
            tagged_sen = []
            for sen in review:
                tagged_sen.append(u_tagger.tag(sen))
            tagged_reviews.append(tagged_sen)
        
        tagged_dict.update({key:tagged_reviews})

tag_wd(tagged_vegas, vegas_words)
tag_wd(tagged_pheonix, pheonix_word)



# Opinions related to keywords 
test_pos = []
test_neg = []
for w in pop_ns:
    a = 0
    b = 0
    for key, value in pheonix_word.items():
        cp = 0
        cn = 0
        
        for reviews in value:
            p = 0
            n = 0
            for sen in reviews:
                if w in sen:
                    i = 0
                    j = 0
                    for wd in sen:
                        #print(wd)
                        if wd in pos_wd:
                            
                            i+=1
                        if wd in neg_wd:
                            j +=1
                p +=i
                    n+=j
        cp = cp+p
            cn = cn +n
        a = cp+a
        b = cn+b
        test_pos.append((w,cp))
    test_neg.append((w,cn))




row_names = [x[0] for x in pos_nn[124:155]]
data = {'positive': ppdata,'negative': nndata}

df = pd.DataFrame(data, index = row_names, columns = ['positive', 'negative'])
fig, ax = plt.subplots()
df.plot(ax=ax, kind='bar',
        title = 'Positve counts VS Negative counts of keywords for a restaurants in Pheonix',
        grid = True)
plt.show()































