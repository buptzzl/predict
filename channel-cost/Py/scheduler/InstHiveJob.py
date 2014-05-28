# -*- coding=utf-8 -*-  
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstHiveJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/04 14:15:33
 * @brief 
 *  
 **/
'''

from InstJob import *

# Hive作业类
class InstHiveJob(InstJob):
    # 初始化Hive作业的执行器
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.executor = "/home/yupeng01/tools/queryengine-client-1.6.10-online/queryengine/bin/queryengine"
    
    # 通过执行输入的hql脚本返回执行结果数据
    def FetchData(self, hqlScript, parmDict={}):
        stat,dat,log = INST_QE_QUERY(hqlScript, self.executor, self.parmDict, retry=3)
        if stat == False:
            logging.error("Excecute HQL Scrite Failed\n")
            logging.error(log)
            return False,[]
        return True,dat
    
    # 通过执行输入的hql脚本文件，将查询数据输出文件，并打印日志
    def FetchDataToFile(self, hqlFile, datFile, logFile, parmDict={}):
        stat = INST_QE_QUERY2(hqlFile, self.executor, parmDict, datFile, logFile, retry=3)
        if stat == False:
            logging.error("Excecute HQL Scrite Failed\n")
            return False
        return True

    # 构建Hive作业执行上下文空间
    def BuildContext(self, confDict={}):
        try:
            hqlFile = confDict["hqlfile"]
            outFile = confDict["outfile"]
            tmpHqlFile = self.getFullPath(os.path.basename(hqlFile))
            shutil.copyfile(hqlFile, tmpHqlFile)
        except:
            logging.error("Build Hive Job Context Failed\n")
            return False
        self.hqlFile = tmpHqlFile
        self.dataFile = outFile
        return True

    # 执行Hive作业
    def ExecuteJob(self, parmDict, toFile=True):
        if toFile:
            datFile = self.getFullPath(self.dataFile)
            logFile = self.logFile()
            myparmDict = self.getParmDict(parmDict)
            stat = self.FetchDataToFile(self.hqlFile, datFile, logFile, myparmDict)
        else:
            pass
        if stat == False:
            logging.error("Excecute HQL Job Failed\n")
            return False, logFile


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
