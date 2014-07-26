# This script acts on the contents of orig. It transforms the source
# to call "rrtmerr" instead of any stop commands.
#
# 1) STOP -> CALL RRTMERR('<file:line>:', len)
# 2) STOP '<message>' -> CALL RRTMERR('<file:line>:<message>', len)
#
# Also handles IF (...) STOP ... lines.
#
# <message> is truncated to give a line length of at most 72 chars.
# Indent of 6 is always used, and is copied from original line.

import re
import os
import glob

def transform_file(fin, fout):
    output = []
    with open(fin, 'r') as f:
        for i, l in enumerate(f.readlines()):
            lineid = "%s:%d:" % (os.path.basename(fin), i+1)
            output.append(transform_line(l, lineid))
    with open(fout, 'w') as f:
        f.write(''.join(output))

def transform_line(line, lineid):
    if len(line) < 7: return line
    if len(line) > 73: line = line[:72] + '\n'
    if line[0].lower() in ['*', 'c']: return line
    indent, line = line[:6], line[6:]
    rex = """(?ix)                        # flags
             ^\s*
             (?P<ifsmt> IF \s* \(.+\))?   # optional IF statement
             \s*STOP                      # matches STOP
             (\s* '(?P<msg>[^']*)' )?     # optional message
             \s*$                         # makes sure no code follows
    """
    mo = re.match(rex, line)
    if mo is not None:
        # we can transform this line.
        call = "CALL RRTMERR('%s%%s',%%d)" % lineid
        min_line_length = len(indent + call % ('', 99))
        if min_line_length > 72:
            Exception("Call string too long for line! `%s`" % lineid)

        # truncate the message so the call fits on one line.
        message = mo.group('msg')
        if message:
            message_length = 72 - min_line_length
            call = call % (message[:message_length], 
                           len(lineid) + len(message))
        else:
            call = call % ('', len(lineid))

        if_statement = mo.group('ifsmt')
        if if_statement:
            # split into two lines if we have to!
            if len(indent + if_statement + ' ' + call) > 72:
                cont_char = indent[5] if indent[5] in ['&', '*'] else '&'
                second_indent = indent[:5] + cont_char
                return indent + if_statement + '\n' \
                    + second_indent + call + '\n'
            else:
                return indent + if_statement + ' ' + call + '\n'
        else:
            return indent + call + '\n'

    else:
        if re.search('(?i)STOP', line):
            print 'WARNING! `%s`' % line.strip()
        return indent + line

for f in glob.glob('orig/*.f'):
    transform_file(f, 'fort/' + os.path.basename(f))
