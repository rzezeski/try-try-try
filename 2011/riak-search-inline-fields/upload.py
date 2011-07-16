# Python isn't my 1st, 2nd, 3rd, or even 10th language, so please
# forgive me if it's not pretty/idiomatic.

# Usage: head -10 earthquake | python upload.py [PORT]
#        python upload.py [PORT] < earthquake
import sys
import json
import riak
import datetime

def main():
    if len(sys.argv) == 1:
        inf = sys.stdin
        port = 8087
    else:
        inf = sys.stdin
        port = int(sys.argv[1])

    i = 0
    c = riak.RiakClient(port=port, transport_class=riak.RiakPbcTransport)
    b = c.bucket('tweets')

    for line in inf:

        try:
            j = json.loads(line)
            ts = datetime.datetime.strptime(j['created_at'], '%a %b %d %H:%M:%S +0000 %Y')
            j['created_at'] = ts.strftime('%Y%m%dT%H%M%S')
            (b.new(str(i), data=j)).store()
        except ValueError as (strerror):
            print "Couldn't decode: %s" % line
            print "Error: ", strerror

        i += 1
        if i % 10 == 0:
            sys.stdout.write('.')
            sys.stdout.flush()

    print 'OK'

if __name__ == '__main__':
    main()
