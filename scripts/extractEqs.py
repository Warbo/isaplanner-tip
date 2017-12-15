#!/usr/bin/env python
import sys

msg = lambda s: sys.stderr.write(s + '\n')

content = ''
begin   = False
end     = False

for line in sys.stdin:
    end   = end   or (begin and line.find('END OUTPUT')   != -1)

    if begin and not end:
        print('\n'.join(line.strip().split(','))),

    begin = begin or            line.find('BEGIN OUTPUT') != -1

    content += line

print('')

if not begin:
    msg("No 'BEGIN OUTPUT' sentinel found. Dumping whole output:")
    msg(content)
    msg("End of dump")
    sys.exit(1)

if not end:
    msg("No 'END OUTPUT' sentinel found. Dumping whole output:")
    msg(content)
    msg("End of dump")
    sys.exit(1)
