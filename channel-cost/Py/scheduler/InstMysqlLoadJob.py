# -*- coding=utf-8 -*-  
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstMysqlLoadJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/04 14:15:33
 * @brief 
 *  
 **/
'''

from InstJob import *

# 数据文件加载作业类
class InstMysqlLoadJob(InstJob):
    # 初始化Mysql数据文件加载作业参数
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.executor = "/home/yupeng01/tools/mysql/install/bin/mysql" 
    
    # 将数据文件导入参数传入的Mysql表
    def LoadData(self, tableName, fileName):
        parmStr = self.connStr
        stat = INST_MYSQL_LOAD(self.executor, parmStr, tableName, fileName)
        return True
    
    # 构建Mysql加载作业执行上下文空间
    def BuildContext(self, confDict={}):
        try:
            host = confDict["host"]
            port = confDict["port"]
            user = confDict["user"]
            pswd = confDict["pswd"]
            dbname = confDict["dbname"]
            inFile = confDict["infile"]
            tblName = confDict["tablename"]
        except:
             logging.error("Build Mysql Load Job Context Failed\n")
             return False
        self.connStr = " -h%s -u%s -p%s -P%s %s" % (host, user, pswd, port, dbname)
        self.inFile = inFile
        self.tableName = tblName
        return True

    # 执行加载作业
    def ExecuteJob(self, parmDict):
        logFile = self.logFile()
        myParmDict = self.getParmDict(parmDict)
        inFile = self.getFullPath(self.inFile)
        tableName = self.tableName
        stat = self.LoadData(tableName, inFile)
        return stat


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
