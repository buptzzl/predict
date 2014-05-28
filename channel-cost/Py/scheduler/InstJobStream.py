# -*- coding=utf-8 -*- 
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstJobStream.py
 * @author qerd(com@baidu.com)
 * @date 2014/03/28 12:08:04
 * @brief 
 *  
 **/
'''
import os
import sys
import imp
import ConfigParser

from InstHiveJob import *
from InstFileLoadJob import *
from InstMysqlLoadJob import *
from InstMysqlQueryJob import *
from InstRJob import *

# 作业流基础类
class InstJobStream(object):
    def __init__(self):
        self.parmDict = {}
        self.jobConfList = []
        self.jobList = []
        self.logger = logging.getLogger()
        self.config = ""
        pass
    
    # 初始化日志模块
    def InitLogger(self, logFile, debug=False):
        fileHandler = logging.FileHandler(logFile)
        contHandler = logging.StreamHandler()
        formatter = logging.Formatter('%(asctime)s - %(levelname)s: %(message)s')
        fileHandler.setFormatter(formatter)
        contHandler.setFormatter(formatter)
        self.logger.addHandler(fileHandler)
        self.logger.addHandler(contHandler)
        self.logger.setLevel(logging.DEBUG)
    
    # 更新作业流配置变量
    def UpdateVariables(self, config, fileName):
        var = "variables"
        for option in config.options(var):
            if self.period == "day":
                olddate = config.get(var,option)
                stat, newdate = INST_DATE_TRANS(olddate, 1)
                config.set(var, option, newdate)
            elif self.period == "halfmonth":
                olddate = config.get(var,option)
                stat, newdate = INST_DATE_TRANS(olddate, 1, "halfmonth")
                config.set(var, option, newdate)
            else:
                pass
        config.write(open(fileName, "w"))
        return True
            
    # 加载作业流元数据信息
    def LoadMetas(self, metaFile):
        logging.info("Start Load Meta From File %s" % metaFile)
        self.metaFile = metaFile
        try:
            config=ConfigParser.ConfigParser()
            config.read(metaFile)
            self.config = config
            self.jobstream=config.get("jobstream","name")
            self.workPath=config.get("jobstream","workpath")
            self.period=config.get("jobstream", "period")
            jobCount = config.get("jobstream","jobcount")
        except:
            sys.stderr.write("Load JobStream Config File %s Failed, Please Check!\n" % metaFile)
            return False
        
        logFile = "%s/log/%s.log" % (self.workPath,self.jobstream)
        if not os.path.exists(os.path.dirname(logFile)):
            os.makedirs(os.path.dirname(logFile))
        self.InitLogger(logFile)
        
        for option in config.options("variables"):
            self.parmDict[option] = config.get("variables",option)
        
        for i in range(int(jobCount)):
            jobDict = {}
            step = "job%d" % (i+1)
            for option in config.options(step):
                jobDict[option] = config.get(step, option)
            self.jobConfList.append(jobDict)
        logging.info("Finish Load Meta From File")
        return True

    # 构建作业流执行上下文环境
    def BuildContext(self):
        logging.info("Start Build JobStream Context:")
        workDir = self.jobstream+"_"
        workDir += "_".join(self.parmDict.values())
        runtimePath = self.workPath+"/"+workDir

        self.CleanContext(runtimePath)
        os.makedirs(runtimePath)

        for jobConf in self.jobConfList:
            #if jobConf["jobname"] != "baiduboxapp_android_1": 
            #    continue
            try:
                jobClass = "Inst" + jobConf["jobtype"]
                jobName = jobConf["jobname"]
                jobParmList = jobConf["parmlist"].split(",") if "parmlist" in jobConf else []
                constructor = globals()[jobClass]
            except StandardError,e:
                return False
            job = constructor(jobName, runtimePath, jobParmList)
            stat = job.BuildContext(jobConf)
            if stat == False:
                logging.error("Build Job %s Context: Failed" % jobName)
                return False
            self.jobList.append(job)
        logging.info("Finish Build JobStream Context: Success")
        return runtimePath
    
    # 析构作业流执行上下文环境
    def CleanContext(self, workPath):
        logging.info("Clean Old JobStream Context")
        if os.path.exists(workPath):
            shutil.rmtree(workPath)
        pass

    # 执行作业流
    def RunStream(self, runtimePath, updateConf=False):
        logging.info("Start Run %d Jobs Process" % len(self.jobList))
        for job in self.jobList:
            stat = job.ExecuteJob(self.parmDict)
            if stat == False:
                logging.error("Job Stream Running Failed\n")
                return False
        logging.info("Finish Run All Jobs Process: Success")
        if updateConf:
            self.UpdateVariables(self.config, self.metaFile)
        return True


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.stderr.write("incorrect number of arguments")
        sys.exit(-1)
    metaFile=sys.argv[1]
    js = InstJobStream()
    stat = js.LoadMetas(metaFile)
    print "------------------------------------"
    workpath = js.BuildContext()
    if workpath != False:
        stat = js.RunStream(workpath, True)

    #js.CleanContext(workpath)

















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
