#!/usr/bin/env python
# -*- coding:utf-8 -*-

#
# Install symlink and packages.
#

import os
import sys
import shutil


UIX_RC_DIR = os.path.expanduser('~/.unixrc')


def usage():
    print "usage: {0} install".format(sys.argv[0])
    sys.exit(1)


def print_info(msg):
    color = '\033[36m'
    reset_color = '\033[m'
    print color + '[*] {0} '.format(msg) + reset_color


def create_dir(name):
    if not os.path.exists(name):
        print_info("creating dir: {0} ...".format(name))
        os.makedirs(name)


def create_link(src_path, dst_path):
    # Test if the symlink is already there.
    if os.path.exists(dst_path) or os.path.islink(dst_path):
        if os.path.realpath(dst_path) == src_path:
            return
        else:
            ans = raw_input("path {0} already exists, remove it? [y/n]: "
                            .format(dst_path))
            if ans != 'y':
                return
            if os.path.isfile(dst_path) or os.path.islink(dst_path):
                os.remove(dst_path)
            else:
                shutil.rmtree(dst_path)
    # Create parent directory if needed.
    create_dir(os.path.dirname(dst_path))
    # Create the symlink.
    print_info("linking {0} to {1} ...".format(src_path, dst_path))
    os.symlink(src_path, dst_path)


def install_homelink(src):
    src_path = os.path.join(UIX_RC_DIR, src)
    dst_path = os.path.expanduser('~/{0}'.format(src))
    create_link(src_path, dst_path)


def install_all():
    # Link install following files or directory to home directory.
    map(install_homelink, [
        '.zshrc',
        '.emacs.d',
    ])


def main():
    if len(sys.argv) != 2:
        usage()
    elif sys.argv[1] == 'install':
        install_all()
    else:
        usage()


if __name__ == '__main__':
    main()
