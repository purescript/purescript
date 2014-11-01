import json

with open("issues.json") as of:
    data = json.load(of)

for issue in data:
    t = issue['title']
    n = issue['number']
    print "- %s (#%s)" % (t, n)
