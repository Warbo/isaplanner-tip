#!/usr/bin/env python

from os           import getenv, getpgid, killpg, setsid
from json         import loads
from signal       import SIGTERM
from subprocess32 import check_output, PIPE, Popen, TimeoutExpired
from timeit       import default_timer

missing = [var for var in ['runners', 'timeout_secs'] if not getenv(var)]
if missing:
    raise Exception('Missing environment variables: ' + repr(missing))
del(missing)

timeout_secs = int(getenv('timeout_secs'))

def get_runners():
    with open(getenv('runners'), 'r') as f:
        return loads(f.read())

runners = get_runners()

def run(cmd, stdin=None):
    proc = Popen(cmd, stdin=PIPE if stdin else None, stdout=PIPE, stderr=PIPE)
    (stdout, stderr) = proc.communicate(input=stdin)
    return {'stdout': stdout, 'stderr': stderr}

def run_timed(cmd, stdin=None, timeout=None):
    '''Run the given command 'cmd', with optional stdin, and return its stdout,
    stderr and the time it took to run (in seconds). Also report whether or not
    the command was killed by going over the (optional) timeout.'''
    start = default_timer()
    proc  = Popen(cmd, stdin=PIPE if stdin else None, stdout=PIPE, stderr=PIPE,
                  preexec_fn=setsid)

    try:
        (stdout, stderr) = proc.communicate(input=stdin, timeout=timeout)
        result = {'stdout': stdout, 'stderr': stderr, 'killed': False}
    except TimeoutExpired:
        # Kill the process group, which will include all children
        killpg(getpgid(proc.pid), SIGTERM)
        result = {'stdout': None, 'stderr': None, 'killed': True}

    proc.wait()  # Reaps zombies

    end = default_timer()
    result['time'] = end - start
    return result

def setup_cache():
    cache = get_runners()
    for size in cache:
        for rep in cache[size]:
            runner     = cache[size][rep]['runner']
            result     = run_timed(runner, timeout=timeout_secs)
            to_analyse = '[]' if result['killed'] else result['stdout']
            try:
                analysis = run([cache[size][rep]['analyser']],
                               stdin=to_analyse);
                analysis['analysed'] = True
            except:
                analysis = {'analysed':       False,
                            'analysis error': repr(exc_info())}

            if analysis['analysed']:
                try:
                    parsed             = loads(analysis['stdout'])
                    parsed['analysed'] = True
                    analysis           = parsed
                except:
                    analysis['analysed']       = False,
                    analysis['analysis error'] = repr(exc_info())
            cache[size][rep]['result']   = result
            cache[size][rep]['analysis'] = analysis

    return cache
setup_cache.timeout = max(3600,
                          timeout_secs          * \
                          len(runners.values()) * \
                          len(runners.values()[0]))

def track_data(cache, _):
    return cache
track_data.repeat      = 1
track_data.number      = 1
track_data.params      = (["dummy"],)
track_data.param_names = ["dummy"]
