#!/usr/bin/env python

import json
import os
import sys

runners = json.loads(os.getenv('runners'))

sys.stderr.write(repr(runners))
