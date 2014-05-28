 # -*- coding=utf-8 -*-
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/03/27 15:57:40
 * @brief 
 *  
 **/
'''

import os
import sys
import logging
import shutil

import MySQLdb

from InstCommon import *

reload(sys)
sys.setdefaultencoding("utf-8")

# 作业基础类
class InstJob(object):
    # 初始化作业名称，运行时目录，参数名列表
    def __init__(self, name, path, parmList=[]):
        self.jobName = name
        self.jobPath = path
        self.parmList = parmList
        pass

    # 返回作业日志文件完整路径文件名
    def logFile(self):
        return self.getFullPath(self.jobName+".log")

    # 根据输入文件名生成文件完整路径文件名
    def getFullPath(self, fileName):
        return "%s/%s" % (self.jobPath.rstrip("/"), fileName)

    # 获取作业参数字典
    def getParmDict(self, parmDict):
        parm = {}
        for key in self.parmList:
            parm[key] = parmDict[key]
        return parm
    
    # 获取作业参数值列表
    def getParmList(self, parmDict):
        parm = []
        for key in self.parmList:
            parm.append(parmDict[key])
        return parm

    # 构建作业执行上下文
    def BuildContext(self, confDict):
        pass
    
    # 执行作业过程
    def ExecuteJob(self, jobParm):
        pass




'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
