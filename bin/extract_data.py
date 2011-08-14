#!/usr/bin/env python2

import sys
from lxml import etree

parser = etree.HTMLParser()
for fn in sys.argv[1:]:
    with open(fn) as f:
        tree = etree.parse(f, parser)

    with open(fn + ".digest", "w") as f:
        f.write('\n'.join(etree.tostring(x) for x in tree.xpath("//body/*")))


