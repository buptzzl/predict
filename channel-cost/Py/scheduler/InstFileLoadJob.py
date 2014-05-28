# -*- coding=utf-8 -*-  
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstFileLoadJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/04 14:15:33
 * @brief 
 *  
 **/
'''

from InstJob import *

# 数据文件加载作业类
class InstFileLoadJob(InstJob):
    # 初始化数据文件加载作业参数
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
    
    # 将输入的数据文件加载到目标路径
    def LoadFile(self, dataFile, outPath):
        if not os.path.exists(dataFile) and os.path.isfile(dataFile):
            logging.error("Data File %s Not Exists\n" % dataFile)
            return False
        if not os.path.exists(outPath):
            logging.info("Create New Directory %s\n" % outPath)
            os.makedirs(outPath)
        outFile = "%s/%s" % (outPath, self.outFile)
        shutil.copyfile(dataFile, outFile)
        return True
    
    # 构建加载作业执行上下文空间
    def BuildContext(self, confDict={}):
        try:
            inFile = confDict["infile"]
            outPath = confDict["outpath"]
            outFile = confDict["outfile"]
        except:
            logging.error("Build Hive Job Context Failed\n")
            return False
        self.inFile = self.getFullPath(inFile)
        self.outPath = outPath
        self.outFile = outFile
        return True

    # 执行加载作业
    def ExecuteJob(self, parmDict):
        logFile = self.logFile()
        myParmDict = self.getParmDict(parmDict)
        outPath = "%s/%s" % (self.outPath, myParmDict["predict_start"])
        stat = self.LoadFile(self.inFile, outPath)
        if stat == False:
            logging.error("Excecute Load Job Failed\n") 
            return False
        return True


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
