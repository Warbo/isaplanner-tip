#!/usr/bin/env python

import json
import os
import signal
import subprocess32
import sys
import timeit

runners = json.loads(os.getenv('runners'))

timeout_secs = int(os.getenv('timeout_secs'))

sample_env                = os.environ.copy()
sample_env['DUMP_SAMPLE'] = '1'

results = {}
for size in runners:
    results[size] = {}
    for rep in runners[size]:
        sample = subprocess32.check_output([runners[size][rep]],
                                           env=sample_env)
        start  = timeit.default_timer()
        try:
            sys.stderr.write('Running size {0} rep {1}\n'.format(size, rep))
            proc     = subprocess32.Popen([runners[size][rep]],
                                          stdout=subprocess32.PIPE,
                                          stderr=subprocess32.PIPE,
                                          start_new_session=True)
            out, err = proc.communicate(timeout=timeout_secs)
            code     = proc.poll()
            results[size][rep] = {
                'size':      int(size),
                'rep':       int(rep),
                'sample':    sample,
                'stdout':    out,
                'stderr':    err,
                'time':      timeit.default_timer() - start,
                'timeout':   timeout_secs,
                'timed out': False,
                'error':     code != 0,
                'exit code': code
            }
        except subprocess32.TimeoutExpired:
            # Kill the process group, which will include all children
            os.killpg(os.getpgid(proc.pid), signal.SIGTERM)

            out, err = proc.communicate()
            results[size][rep] = {
                'size':      int(size),
                'rep':       int(rep),
                'sample':    sample,
                'stdout':    out,
                'stderr':    err,
                'time':      timeit.default_timer() - start,
                'timeout':   timeout_secs,
                'timed out': True,
                'error':     True,
                'exit code': None
            }

print(json.dumps(results))
