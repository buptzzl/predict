# -*- coding=utf-8 -*-  
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstMysqlQueryJob.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/04 14:15:33
 * @brief 
 *  
 **/
'''
import MySQLdb 

from InstJob import *

# MYSQL查询作业类
class InstMysqlQueryJob(InstJob):
    # 初始化Mysql查询作业参数
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.executor = "/home/qerd/tools/mysql/install/bin/mysql" 
    
    # 将SQL脚本中的变量替换为值
    def GenerateSQL(self, sqlScript, parmDict):
        sqlScript = sqlScript.replace("\n", " ")
        for key,val in parmDict.items():
            pattern = "${mysqlvar:%s}" % key
            if pattern in sqlScript:
                sqlScript = sqlScript.replace(pattern, val)
        return sqlScript

    # 读取SQL文件中的脚本，执行脚本返回结果数据
    def FetchTSData(self, sqlFile, parmDict):
        try:
            rawScript = open(sqlFile).read()
            sqlScript = self.GenerateSQL(rawScript, parmDict)
            cur = self.conn.cursor(cursorclass= MySQLdb.cursors.DictCursor)
            cur.execute(sqlScript)
            fieldDesc = [field[0] for field in cur.description]
            fieldVal = []
            for row in cur:
                fieldVal.append(row)
            cur.close()
        except:
            return False,[],[]
            logging.error("Fetch Data From Mysql Failed\n")
        return True, fieldDesc, fieldVal

    # 将数据写入文件中
    def WriteToFile(self, lines, fileName):
        fp = open(fileName, "w")
        for line in lines:
            fp.write(line +"\n")
        return True

    # 构建Mysql查询作业执行上下文空间
    def BuildContext(self, confDict={}):
        logging.info("Start build Job(%s) Context" % self.jobName)
        try:
            host = confDict["host"]
            port = confDict["port"]
            user = confDict["user"]
            pswd = confDict["pswd"]
            dbname = confDict["dbname"]
            outFile = confDict["outfile"]
            sqlFile = confDict["sqlfile"]
            tmpSqlFile = self.getFullPath(os.path.basename(sqlFile))
            shutil.copyfile(sqlFile, tmpSqlFile)
        except:
            logging.error("Build Mysql Job(%s) Context Failed" % self.jobName)
            return False
        self.conn = MySQLdb.connect(host=host,port=int(port),user=user,passwd=pswd,db=dbname,charset="utf8")
        self.outFile = outFile
        self.sqlFile = os.path.basename(sqlFile)
        logging.info("Finish Build Job(%s) Context: Success" % self.jobName)
        return True

    # 执行加载作业
    def ExecuteJob(self, parmDict, toFile=True):
        logging.info("Start Execute Job(%s) Process" % self.jobName)
        logFile = self.logFile()
        myParmDict = self.getParmDict(parmDict)
        outFile = self.getFullPath(self.outFile)
        sqlFile = self.getFullPath(self.sqlFile)
        stat,head,dat = self.FetchTSData(sqlFile, myParmDict)
        if stat == False:
            logging.error("Build Mysql Job(%s) Context Failed" % self.jobName)
            return False
        if toFile:
            stat, lines = INST_WRITE_DATAFRAME(dat, head, sep=",", header=True)
            self.WriteToFile(lines, outFile)
        logging.info("Finish Excecute Job(%s) Process: Success" % self.jobName)
        return True


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
