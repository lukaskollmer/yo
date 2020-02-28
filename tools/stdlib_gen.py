import glob
import datetime
import os
import sys
import re
import subprocess

CMAKE_BINARY_DIR = sys.argv[1]
STDLIB_PATH = os.path.join(os.getcwd(), sys.argv[2])
STDLIB_PARENT = os.path.dirname(STDLIB_PATH)

filename_pattern = re.compile(r'/|\.')

print(CMAKE_BINARY_DIR)
f = open(os.path.join(CMAKE_BINARY_DIR, 'lib', 'yo', 'stdlib_sources.cpp'), 'w')

f.write(
f"""//
//  stdlib_sources.cpp
//  yo
//
//  Created by Lukas Kollmer on {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}.
//  Copyright © {datetime.datetime.now().year} Lukas Kollmer. All rights reserved.
//

#include <map>
#include <string_view>

""")

module_symbols = {}

for filename in glob.iglob(f'{STDLIB_PATH}/**/*.yo', recursive=True):
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
