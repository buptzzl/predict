#coding=utf-8

"""
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstRJob.py
 * @author zhouliaoming01(com@baidu.com)
 * @date 2014/04/21 14:15:33
 * @brief 执行shell 命令； 为对 Popen的简单包装
 *  
 **/
"""

from InstJob import *
from .InstJobStream import InstJobStream


class InstCMDJob(InstJob):
    """ 执行命令， 对Popen的包装. 采用RJob 的参数处理方式。  """
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.cmd = None
        self.args = ''
        self.outFile = None
        
    def BuildContext(self, confDict={}):
        """ 构建当前任务的参数环境  """
        self.cmd = confDict['CMD']
        try:
            self.args = confDict['args_last'] 
            self.outFile = confDict['outFile']
        except Exception, e:
            self.logger.warn('paramerter not complete. [MSG]: ' + e.message)
        if not os.path.exists(self.outFile):
            self.outFile = self.getFullPath(self.outFile)
    
    def ExecuteJob(self, parmDict):
        """ 执行当前任务 """
        myParmList = self.getParmList(parmDict)
        myParmList.append(self.args)
        cmd = '%s %s' % (self.cmd, ' '.join(myParmList))
        stat = INST_SHELL_EXEC('', cmd, retry=0)
        return stat






