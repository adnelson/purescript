import argparse
import json
import os
import sqlite3

def initialize_db(root):
    conn = sqlite3.connect(os.path.join(root, 'manifest.db'))

    with conn:
        conn.execute('''
CREATE TABLE IF NOT EXISTS modules (
    package TEXT,
    module TEXT NOT NULL,
    prebuilt INT NOT NULL,
    externs BLOB NOT NULL,
    CONSTRAINT module_pkg_unique UNIQUE (package, module)
)''')
        conn.execute('''
CREATE TABLE IF NOT EXISTS local_modules (
    module TEXT NOT NULL UNIQUE,
    timestamp INT NOT NULL,
    path TEXT NOT NULL
)''')
    return conn

parser = argparse.ArgumentParser()
parser.add_argument('package')
parser.add_argument('--root', default=os.getcwd())
args = parser.parse_args()

def read_dir(root, package):
    modules = {}
    (_, module_names, _) = next(os.walk(os.path.join(root, package)))
    for module_name in module_names:
        path = os.path.join(root, package, module_name, 'externs.json')
        if not os.path.exists(path):
            continue
        with open(path, 'rb') as f:
            contents = f.read()
            # Ensure it's json
            json.loads(contents.decode())
            modules[module_name] = contents
    return modules

def seed_manifest(root, package, conn):
    modules = read_dir(root, package)

    with conn:
        values = [(
            package,
            module_name,
            True, # not prebuilt
            externs,
        ) for (module_name, externs) in modules.items()]

        result = conn.executemany(
            'INSERT OR REPLACE INTO modules VALUES (?, ?, ?, ?)',
            values
        )

conn = initialize_db(args.root)
seed_manifest(args.root, args.package, conn)
