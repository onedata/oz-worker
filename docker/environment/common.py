# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A custom utils library used across docker scripts.
"""

from __future__ import print_function

import argparse
import inspect
import json
import os
import time
import sys

import dns


try:
    import xml.etree.cElementTree as eTree
except ImportError:
    import xml.etree.ElementTree as eTree

try:  # Python 2
    from urllib2 import urlopen
    from urllib2 import URLError
    from httplib import BadStatusLine
except ImportError:  # Python 3
    from urllib.request import urlopen
    from urllib.error import URLError
    from http.client import BadStatusLine


def nagios_up(ip, port=None):
    url = 'https://{0}{1}/nagios'.format(ip, (':' + port) if port else '')
    try:
        fo = urlopen(url, timeout=5)
        tree = eTree.parse(fo)
        healthdata = tree.getroot()
        status = healthdata.attrib['status']
        return status == 'ok'
    except URLError:
        return False
    except BadStatusLine:
        return False


def wait_until(condition, containers, timeout):
    deadline = time.time() + timeout
    for container in containers:
        while not condition(container):
            if time.time() > deadline:
                warning = 'WARNING: timeout while waiting for condition {0}'
                print(warning.format(condition.__name__), file=sys.stderr)
                break

            time.sleep(1)


def standard_arg_parser(desc):
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description=desc)

    parser.add_argument(
        '-i-', '--image',
        action='store',
        default='onedata/worker',
        help='docker image to use for the container',
        dest='image')

    parser.add_argument(
        '-b', '--bin',
        action='store',
        default=os.getcwd(),
        help='path to the code repository (precompiled)',
        dest='bin')

    parser.add_argument(
        '-d', '--dns',
        action='store',
        default='auto',
        help='IP address of DNS or "none" - if no dns should be started or \
             "auto" - if it should be started automatically',
        dest='dns')

    parser.add_argument(
        '-u', '--uid',
        action='store',
        default=generate_uid(),
        help='uid that will be concatenated to docker names',
        dest='uid')

    parser.add_argument(
        'config_path',
        action='store',
        help='path to json configuration file')

    return parser


def merge(d, merged):
    """Merge the dict merged into dict d by adding their values on
    common keys
    """
    for key, value in iter(merged.items()):
        d[key] = d[key] + value if key in d else value


def set_up_dns(config, uid):
    """Sets up DNS configuration values, starting the server if needed."""
    if config == 'auto':
        dns_config = dns.up(uid)
        return [dns_config['dns']], dns_config

    if config == 'none':
        return [], {}

    return [config], {}


def get_file_dir(file_path):
    """Returns the absolute path to directory containing given file"""
    return os.path.dirname(os.path.realpath(file_path))


def get_script_dir():
    """Returns the absolute path to directory containing the caller script"""
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    return get_file_dir(caller_mod.__file__)


def parse_json_file(path):
    """Parses a JSON file and returns a dict."""
    with open(path, 'r') as f:
        return json.load(f)


def format_hostname(node_name, uid):
    """Formats hostname for a docker based on node name and uid.
    node_name can be in format 'somename@' or 'somename'.
    """
    (name, _, hostname) = node_name.partition('@')
    if hostname:
        return '{0}.{1}.dev.docker'.format(hostname.replace('.', '-'), uid)
    else:
        return '{0}.{1}.dev.docker'.format(name, uid)


def format_nodename(node_name, uid):
    """Formats full node name for a docker based on node name and uid
    node_name can be in format 'somename@' or 'somename'.
    This is needed so different components are resolvable through DNS.
    """
    (name, _, _) = node_name.partition('@')
    return '{0}@{1}'.format(name, format_hostname(node_name, uid))


def format_dockername(node_name, uid):
    """Formats docker name based on node name and uid
    node_name can be in format 'somename@' or 'somename'.
    This is needed so different components are resolvable through DNS.
    """
    (name, _, hostname) = node_name.partition('@')
    if hostname:
        return hostname.replace('.', '_')
    else:
        return '{0}_{1}'.format(name, uid)


def generate_uid():
    """Returns a uid (based on current time),
    that can be used to group dockers in DNS
    """
    return str(int(time.time()))
