# -*- coding=utf-8 -*-  
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstRJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/04 14:15:33
 * @brief 
 *  
 **/
'''

from InstJob import *

# R作业类
class InstRJob(InstJob):
    # 初始化R作业的执行器
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.executor = "/home/yupeng01/tools/R-3.0.3/bin/Rscript"
    
    # 通过执行输入的R脚本返回执行结果数据、日志信息
    def RCompute(self, RFile, parmList, logFile):
        stat,dat,log = INST_R_COMPUTE(RFile, self.executor, parmList)
        if stat == False:
            logging.error("R Script Compute Failed\n")
            return False
        return True

    # 构建R作业执行上下文空间
    def BuildContext(self, confDict={}):
        try:
            RFile = confDict["rfile"]
            outFile = confDict["outfile"]
            args = confDict["args"]
            tmpRFile = self.getFullPath(os.path.basename(RFile))
            shutil.copyfile(RFile, tmpRFile)
        except:
            logging.error("Build R Job Context Failed\n") 
            return False
        self.argList = args.split(",")
        self.RFile = tmpRFile
        self.outFile = outFile
        return True

    # 执行R作业
    def ExecuteJob(self, parmDict):
        logFile = self.logFile()
        argList = [self.jobPath]
        myParmList = self.getParmList(parmDict)
        argList.extend(myParmList)
        argList.extend(self.argList)
        stat = self.RCompute(self.RFile, argList, logFile)
        if stat == False:
            logging.error("Excecute R Job Failed, detail in %s\n" % logFile)
            return False
        return True


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
