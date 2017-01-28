import os
from re import *

def read_file(name):
    with open(name, 'r') as f:
        return f.read()

usage_regex = compile(r'new\s+(\w+)')
def usages(file_name):
    """Given a file, find all scala classes it uses"""
    if not file_name:
        return set()
    file = read_file(file_name)
    objects = findall(usage_regex, file)
    return set(objects)

def to_file(obj):
    """Given a scala class, find the file in which it resides"""
    decl_regex = compile(f'class\s+{obj}\(')
    dir_files = os.listdir()
    possible_filename = f'{obj}.scala'
    if possible_filename in dir_files:
        return possible_filename

    for file_name in dir_files:
        file = read_file(file_name)
        if findall(decl_regex, file):
            return file_name

def find_all(start):
    seen = set()
    frontier = set()
    frontier.add(start)

    while frontier:
        e = frontier.pop()
        seen.add(e)
        new = usages(to_file(e)).difference(seen)
        frontier.update(new)
    return seen

if __name__ == '__main__':
    import sys
    import itertools
    if len(sys.argv) < 1:
        print('Usage: `python find-unused.py <entry class>`')
        sys.exit(1)
    classes = find_all(sys.argv[1])
    files = list(set([e for e in [to_file(c) for c in classes] if e]))
    if files:
        files.sort()
        print('# Used files:')
        for file in files:
            print(f' - {file}')
        dir_files = set(os.listdir())
        used_files = set(files)
        diff = list(dir_files.difference(used_files))
        if diff:
            diff.sort()
            print('\n# Unused files:')
            for file in diff:
                if '.scala' in file:
                    print(f' - {file}')
