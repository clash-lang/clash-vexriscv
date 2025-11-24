#!/usr/bin/env python3
"""
Usage:

    python3 build-vexriscv.py <repo> <commit_hash>
"""
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import glob
import os
import shutil
import subprocess
import sys
import tempfile

def main(repo, commit_hash):
    if glob.glob(f"vexriscv_*-{commit_hash}.jar"):
        print(f"JAR for commit {commit_hash} already exists, not rebuilding.")
        return

    with tempfile.TemporaryDirectory() as temp_dir:
        # Build new JAR
        subprocess.check_call(["git", "clone", repo, temp_dir])
        subprocess.check_call(["git", "checkout", commit_hash], cwd=temp_dir)
        subprocess.check_call(["flock", "/tmp/lock", "sbt", "package"], cwd=temp_dir)

        # Find and copy new JAR
        jar_path = glob.glob(os.path.join(temp_dir, "target", "scala-*", "vexriscv_*.jar"))[0]
        jar_filename = os.path.basename(jar_path)
        jar_base, jar_ext = os.path.splitext(jar_filename)
        new_jar_filename = f"{jar_base}-{commit_hash}{jar_ext}"
        shutil.move(jar_path, new_jar_filename)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
