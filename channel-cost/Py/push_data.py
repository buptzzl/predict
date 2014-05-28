#coding=utf-8
"""
@func  数据推送到HIVE库表
@stream 检验本地数据，配置|构造 远端路径， 检验远端路径并推送， 远端推送结果检验
@author zhouliaoming01@baidu.com
@ref InstJobStream.py
"""

import os, sys
from subprocess import *
import logging
from datetime import datetime, timedelta

class DataPush(object):
    def __init__(self, path_src, path_dst, plog='./log'):
        pass
    
    def check_src(self):
        """ 检验本地数据： 文件or目录中是否存在 """
        pass
    
    def build_remote_path(self):
        return False
    def push(self):
        return None
    def check_dst(self):
        return True
    
class PushHive(DataPush):
    """ 将本地数据推送 到HIVE 表  """
    def __init__(self, path_src, path_dst, plog='./log'):
        pass
    
    def check_src(self):
        # 检验数据的列数是否一致
        if super(PushHive, self).check_src():
            pass
    
    def build_remote_path(self):
        # 
        pass
    def _path_partition(self):
        # 构造分区路径
        pass
        