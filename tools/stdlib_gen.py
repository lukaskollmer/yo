import glob
import datetime
import os
import sys
import subprocess

CMAKE_BINARY_DIR = sys.argv[1]
STDLIB_PATH = os.path.join(os.getcwd(), sys.argv[2])
STDLIB_PARENT = os.path.dirname(STDLIB_PATH)


f = open(os.path.join(CMAKE_BINARY_DIR, 'src', 'stdlib_sources.h'), 'w')

f.write(
f"""//
//  stdlib_sources.h
//  yo
//
//  Created by Lukas Kollmer on {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}.
//  Copyright © {datetime.datetime.now().year} Lukas Kollmer. All rights reserved.
//
""")

for filename in glob.iglob(f'{STDLIB_PATH}/**/*.yo', recursive=True):
     f.write(f"\n\n// File at {filename}\n")
     filename = filename.replace(STDLIB_PARENT + '/', '')
     x = subprocess.check_output(['/usr/bin/xxd', '-i', filename], encoding='ascii')
     f.write(x)

f.close();
