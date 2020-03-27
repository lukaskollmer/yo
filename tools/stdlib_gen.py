import glob
import datetime
import os
import sys
import re
import subprocess

STDLIB_DIRECTORY =  sys.argv[1]
OUTPUT_FILE_PATH =  sys.argv[2]
STDLIB_PARENT = os.path.dirname(STDLIB_DIRECTORY)

filename_pattern = re.compile(r'/|\.')

f = open(OUTPUT_FILE_PATH, 'w')

f.write(
f"""//
//  {os.path.basename(OUTPUT_FILE_PATH)}
//  yo
//
//  Generated {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}.
//  Copyright © {datetime.datetime.now().year} Lukas Kollmer. All rights reserved.
//

#include <map>
#include <string_view>
""")

module_symbols = {}

for filename in glob.iglob(f'{STDLIB_DIRECTORY}/**/*.yo', recursive=True):
     f.write(f"\n\n// File at {filename}\n")
     filename = filename.replace(STDLIB_PARENT + '/', '')
     module_symbols[filename] = filename_pattern.sub('_', filename)
     x = subprocess.check_output(['/usr/bin/xxd', '-i', filename], encoding='ascii', cwd=STDLIB_PARENT)
     f.write(x)

f.write('static const std::map<std::string_view, std::string_view> stdlibModules = {\n')

for filename, symbol in module_symbols.items():
     import_name = filename[:-3].replace('stdlib/', ':')
     f.write(f'    {{ "{import_name}", std::string_view(reinterpret_cast<const char*>({symbol}), {symbol}_len) }},\n')

f.write('};\n')

f.close()
