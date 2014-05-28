# -*- coding=utf-8 -*-
'''
/***************************************************************************
 * 
 * Copyright (c) 2013 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file InstCommon.py
 * @author qerd(com@baidu.com)
 * @date 2013/12/08 10:22:20
 * @brief 
 *  
 **/
'''

import os
import re
import sys
import copy
import logging
from subprocess import *
from datetime import datetime,timedelta

import MySQLdb
reload(sys)
sys.setdefaultencoding("utf-8")

# @INST_DATE_TRANS: 根据偏移量平移日期字符串
# @date: 输入日期字符串
# @format: 输入日期字符串格式
# @retval: (执行状态，偏移后的日期字符串)
def INST_DATE_TRANS(date, days, period="day", format="%Y%m%d"):
    try:
        dt = datetime.strptime(date, format)
    except:
        sys.stderr.write("Input Date Formate unmatch\n")
        return False,date
    if period == "day":
        newdt = dt + timedelta(days=days)
        newdate = datetime.strftime(newdt, format)
    elif period == "halfmonth":
        olddate = datetime.strftime(dt, format)
        if olddate[6:] == "01":
            newdate = "%s16" % olddate[:6]
        else:
            if olddate[4:6] == "12":
                newdate = "%d0101" % (int(olddate[:4])+1)
            else:
                newdate = "%d01" % (int(olddate[:6])+1)
    return True,newdate

# @INST_LOAD_FILE: 从文件中读取结构化数据
# @fp: 数据读句柄
# @sep: 数据字段分隔符
# @header: 文件中是否包含字段名称
# @retval: (执行状态,返回数据表头LIST,结构化数据LIST[DICT{key:val}])
def INST_READ_DATAFRAME(fp, sep='\t', header=True):
    dataframe = []
    firstline = True
    for line in fp:
        fields = line.strip().split(sep)
        if firstline:
            if header:
                datahead = fields
            else:
                datahead = ["v%d" % (i+1) for i in range(len(fields))]
                dataframe.append(fields)
            datalength = len(fields)
            firstline = False
            continue
        if len(fields) != datalength:
            sys.stderr.write("Error: Fields Count Conflict\n")
            return False, datahead, dataframe
        tmpdict={datahead[i]:fields[i] for i in range(len(fields))}
        dataframe.append(tmpdict)
    return True,datahead,dataframe

# @INST_WRITE_DATAFRAME: 将结构化数据写入文件
# @sep: 数据字段分隔符
# @header: 是否写入数据表头
# @dataHead: 结构化数据字段头
# @dataFrame: 结构化数据
# @retval: (执行状态)
def INST_WRITE_DATAFRAME(dataFrame, dataHead, sep='\t', header=True):
    outlist = []
    if header == True:
        outlist.append(sep.join(dataHead))
    for dat in dataFrame:
        if len(dataHead) != len(dat):
            sys.stderr.write("Error: Fields Count Conflict\n")
            return False, outlist
        fields = [str(dat[h]) for h in dataHead]
        outlist.append(sep.join(fields))
    return True, outlist


# @INST_QE_QUERY: 通过QE接口获取数据,适合小量数据获取(<1000)
# @hql: HiveQL script
# @exc: QE接口访问字符串
# @parm: HiveQL script动态加载的变量
# @retry: 失败重试次数
# @retval: (执行状态, 数据接口返回数据)
def INST_QE_QUERY(hql, exc, parmDict, retry=0):
    parmstr = ""
    for key,val in parmDict.items():
        parmstr += "%s=%s " % (key,val)
    hql = hql.replace("\n","")
    hql = hql.replace("'","\"")
    command = "%s --hivevar %s -f '%s'" % (exc, parmstr, hql)
    stat = -1
    while retry >= 0 and stat != 0:
        sys.stderr.write(command+"\n")
        p = Popen(command, stdout=PIPE, stderr=PIPE,shell=True)
        stat = p.wait()
        log = p.stderr.read()
        res = p.stdout.read()
        retry -= 1
    if stat == 0:
        return True, res, log
    return False, res, log

# @INST_QE_QUERY2: 通过QE接口获取数据，并输出到文件,适合大量数据获取
# @hql: HiveQL script
# @exc: QE接口访问字符串
# @parm: HiveQL script动态加载的变量
# @retry: 失败重试次数
# @retval: (执行状态)
def INST_QE_QUERY2(hql, exc, parmDict, outFile, logFile, retry=0):
    parmstr = ""
    for key,val in parmDict.items():
        parmstr += "%s=%s " % (key,str(val))
    command = "%s --hivevar %s -f '%s' >%s 2>>%s" % (exc, parmstr, hql, outFile, logFile)
    stat = -1
    while retry >= 0 and stat != 0:
        logging.info(command+"\n")
        p = Popen(command, shell=True)
        stat = p.wait()
        retry -= 1
    if stat == 0:
        return True
    return False

# @INST_SHELL_EXEC: 直接执行shell命令
# @exc: shell命令字符串
# @content: shell命令参数字符串
# @retry: 失败重试次数
# @retval: (执行状态)
def INST_SHELL_EXEC(script, exc, retry=0):
    command = "%s %s" % (exc, script)
    stat = -1 
    while retry >= 0 and stat != 0:
        logging.info(command+"\n")
        p = Popen(command, shell=True)
        stat = p.wait()
        retry -= 1
    if stat == 0:
        return True
    return False

# @INST_R_COMPUTE: 执行R命令
# @exc: R命令字符串
# @script: R script文件字符串
# @parm: R命令参数字符串列表
def INST_R_COMPUTE(script, exc, parmList):
    command = '%s %s %s' % (exc, script, " ".join(parmList))
    logging.info(command+"\n")
    p = Popen(command, stdout=PIPE, stderr=PIPE,shell=True)
    stat = p.wait()
    log = p.stderr.read()
    res = p.stdout.read()
    if stat == 0:
        return True, res, log
    else:
        return False, res, log

# @INST_MYSQL_QUERY: 执行SQL查询MYSQL数据
# @conn：数据库连接
# @query: sql查询语句
def INST_MYSQL_QUERY(conn, query):
    cursor = conn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()
        cursor.close()
    except StandardError, e:
        sys.stderr.write("Execute Query Exception:%s\n" % e)
        return False
    return result

# @INST_MYSQL_COMMAND: 执行SQL命令
# @conn：数据库连接
# @command: sql执行命令
def INST_MYSQL_COMMAND(conn, command):
    cursor = conn.cursor()
    try:
        cursor.execute(command)
        conn.commit()
        cursor.close()
    except StandardError, e:
        sys.stderr.write("Execute Command Exception:%s\n" % e)
        return False
    return True

# @INST_MYSQL_LOAD: 通过MYSQL客户端加载数据文件到目标表
# @exc: MYSQL客户端
# @parmStr: 参数字符串
# @tableName: 目标表名
# @fileName: 导入数据文件名
def INST_MYSQL_LOAD(exc, parmStr, tableName, fileName):
    sql = "load data local infile \"%s\" replace into table %s" % (fileName, tableName)
    sql += " FIELDS TERMINATED BY \"\\t\";"
    command = parmStr +" --local-infile=1 -e '%s'" % sql
    return INST_SHELL_EXEC(command, exc)


















'''
/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
'''
