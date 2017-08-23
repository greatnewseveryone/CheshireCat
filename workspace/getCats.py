
import re, requests, os, time, random, csv


def parseIndex(fn= './data/GUTINDEX.ALL.iso-8859-1.txt'):
    gut_all= []
    book_block= []
    for line in open(fn):
        if not line.rstrip():  # book end
            if book_block: 
                gut_all.append(book_block)
            book_block= []
            continue
        if line.rstrip()[-1].isdigit():  # new book
            book_block.append(line.rstrip())
        elif book_block:
            book_block.append(line.rstrip())
    return gut_all


def getGutTexts(gut_all, keywords= []):
    books= []
    outf= open('cheshire_ind.csv', 'w')
    outc= csv.DictWriter(outf, fieldnames='ind,title,cat'.split(','))
    outc.writeheader()
    for bb_ind, book_block in enumerate(gut_all):
        book_ind= re.findall('\W+(\d+)', book_block[0])[0]
        base_url= 'http://gutenberg.readingroo.ms/{path}/{ind}/'.format(path='/'.join(book_ind[:-1]), ind=book_ind)
        text_fnl= re.findall('>(\S+.txt)', requests.get(base_url).text)
        if text_fnl:
            text_url= os.path.join(base_url, text_fnl[0])
            req= requests.get(text_url)
            matches= [s for s in keywords if s.lower() in req.text.lower()]
            if matches:
                open('./data/texts/{book_ind}.txt'.format(book_ind=book_ind), 'w').write(req.text.encode('ascii', 'ignore').decode())
                outc.writerow({'ind':int(book_ind), 'title':' '.join(book_block[0].split()[:-1]).encode('ascii', 'ignore').decode()})
        if not req.status_code==200:
            print( req.status_code)
            print( text_url)
            print( book_block[0])
        time.sleep(random.random()/10)
        if bb_ind%100==0: 
            print(bb_ind, )
    outf.close()

gut_ind= parseIndex()
keywords= set(['Cheshire %s'%i for i in 'cat,feline,tomcat,tom,kitten,mouser,puss,kitty,furball'.split(',')])
getGutTexts(gut_ind, keywords)


