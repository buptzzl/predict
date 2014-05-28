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
 * @brief 更新biglog 上的HIVE 表；必须有相应权限。
 *  
 **/
"""

from InstJob import *
from .InstJobStream import InstJobStream

class INST_HIVE(object):
    """ HIVE表的常用操作; 要求当前运行环境至少支持 QE or Hadoop 之一  """
    def __init__(self, exc, queryengine):
        if not os.path.exists(exc) and not os.path.exists(queryengine):
            assert ValueError("queryengine is unreachable.")
        self.logger = logging.getLogger()
        self.exc = None
        if self._cmd_test(exc):
            self.exc = exc  # Hadoop
        self.qe = None
        if self._cmd_test(queryengine):
            self.qe = queryengine # query engine
    def set_logger(self, logger):
        if isinstance(logger, logging.Logger):
            self.logger = logger
        
    def _cmd_test(self, cmd):
        cmd, bad_str, = '%s --help', 'command not found',
        p = Popen(cmd, stdout=PIPE, stderr=PIPE,shell=True)
        stat = p.wait()
        log = p.stderr.read()
        self.logger.warn('test commend is valide or not. [cmd] %s\t[outpu] %s' % (cmd, log))
        
        if bad_str in log:
            return False
        return True
         
    def INST_HIVE_LOAD(self, tableName, fileName):
        """ push本地数据到HIVE """  
        if self.exc is None or self.qe is None:
            self.logger.error('enviroment is uncompleted. hadoop:%s, queryengine:%s' % (str(self.exc), str(self.qe)))
            return False
        htable = HiveTable(tableName)
        
        self.INST_EXEC_HADOOP('', 'dfs -rmr %s'%(htable.ptn_path))
        self.INST_EXEC_QE('', 'alter table %s  drop partition (%s);' % (','.join(htable.ptn_kv)))
        
        p_hive = self.INST_HIVE_TABLE_PATH(None, htable.table)
        p_hive = '%s/%s' % (p_hive, htable.ptn_path) 
        stat, dat = self.INST_EXEC_HADOOP('', 'dfs -mkdir "%s"' % p_hive)
        if not stat:
            return False
        h_cmd = 'dfs -put "%"s "%s"' % (fileName, p_hive)
        stat, dat = self.INST_EXEC_HADOOP('', h_cmd)
        if not stat:
            return False
        t_cmd = 'alter table %s add partition (%s) location "%s"' % \
                (htable.table, htable.ptn_str, p_hive)
        stat, dat = self.INST_EXEC_QE('', t_cmd)
        if not stat:
            return False
        return True
        
    def INST_HIVE_TABLE_PATH(self, tableName):
        """ 获取HIVE表的HDFS物理路径 """
        cmd1 = 'show create table %s' % tableName
        #stat, data = self.INST_EXEC_QE('', cmd)
        cmd2 = ' grep "hdfs" | sed "s/[ \']//g" '
        stat, data, _ = self._exec_pipline(self.qe, cmd1, '', cmd2,)
        if stat != 0:
            return None
        return data
    
    def INST_EXEC_HADOOP(self, p_before, cmd, *p_last):
        """ 执行对应的 hadoop 命令； 终将parmStr 看作cmd 的参数 """
        h_cmd = self._parm_format(p_before)
        h_cmd = '%s %s' % (h_cmd, cmd)
        if len(p_last) > 0:
            h_cmd = '%s %s' % (h_cmd,' '.join(p_last).strip())
        
        stat, data, log = self._exec(cmd, self.exc)
        if not stat:
            return (False, None)
        return (True, data)
        
    def INST_EXEC_QE(self, p_before, cmd, *p_last):
        """ 执行QE 命令； 将parmStr 看着QE的参数（在CMD前面） k-v结果看着hivevar； args为末尾的参数 """
        q_cmd = ''
        _cmd = self._parm_format(p_before)
        if isinstance(p_before, dict) and len(p_before) != 0:
            # K-V 结构看作 hivevar 变量； 否则视为命令后的参数部分
            q_cmd = '--hivevar %s' % (_cmd)
        if isinstance(cmd, str) and os.path.isfile(cmd):
            q_cmd = '%s -f %s' % (q_cmd, cmd)
        else:
            q_cmd = '%s -e "%s"' % (q_cmd, cmd)
        if len(p_last) > 0:
            q_cmd = '%s %s' % (q_cmd, ' '.join(p_last).strip())
            
        stat, data, log = self._exec(q_cmd, self.qe)
        if not stat:
            return (False, None)
        return (True, data)
    
    def _exec(self, cmd, bin_):
        """ 检验执行命令与参数，并合并为可执行字符串 """
        if not isinstance(cmd, str) or len(cmd.strip()) == 0:
            self.logger.warn('command str is invalid: %s' % str(cmd))
            return (False, None, None)
        #cmd = self._parm_format(parmStr)
        
        cmd = '%s %s' % (bin_, cmd)
        p = Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True)
        stat = p.wait()
        flag, data_, err_ = True, p.stdout.read(), p.stderr.read()
        self.logger.info('%s\nstate: %s,\nlog:%s' % (cmd, str(stat), err_))
        
        if stat != 0:
            flag = False
        return (flag, data_, err_)
    
    def _exec_pipline(self, *args):
        """ 使用管线执行多个命令, 每个命令的格式为 cmd, arg-str;整体为偶数  """
        if len(args) < 2 or len(args) % 2 != 0:
            return (False, None, None)
        cmd_ = '%s %s' % (args[0], self._parm_format(args[1]))
        p_e = Popen(cmd_, stdout=PIPE, shell=True)
        stat = p_e.wait()
        if stat != 0:
            return (False, None, None) 
        for i in range(2, len(args), 2):
            cmd_ = '%s %s' % (args[i], self._parm_format(args[i+1]))
            p_e = Popen(cmd_, stdin=p_e.stdout, stdout=PIPE, shell=True)
            stat = p_e.wait()
            if stat != 0:
                return (False, None, None)
        return (stat, p_e.stdout.read(), p_e.stderr.read())
        
    def _parm_format(self, parmStr):
        cmd = ""
        if parmStr is not None:
            if isinstance(parmStr, str):
                cmd = parmStr.strip()
            elif isinstance(parmStr, list) or isinstance(parmStr, tuple):
                cmd = ' '.join(parmStr)
            elif isinstance(parmStr, dict):
                cmd = ' '.join(['%s=%s'%(k,v) for k,v in dict.items()])
        return cmd

class HiveTable(object):
    """ HIVE 对象基础操作类  """
    def __init__(self, table, partitions=None):
        self.table = table
        
        self.ptn_kv, self.ptn_str, self.ptn_path  = {}, '', ''
        if isinstance(partitions, dict):
            self.ptn_kv = partitions
            self.ptn_str = ['%s=%s'%(k.strip(),v.strip()) for (k,v) in partitions.items()]
            self.ptn_path = '/'.join(self.ptn_str)
        
    def build_all(self, exc):
        # 构造完整的Table 周围信息 @To be update. 
        self.path = exc.INST_HIVE_TABLE_PATH(self.table)  # HDFS 路径
    
    
class InstHivePushJob(InstJob):
    """ 将本地数据推送到指定的HIVE 表格 ；当前QE账号必须有对应的权限。 """
    def __init__(self, name, path, parmList):
        InstJob.__init__(self, name, path, parmList)
        self.executor = '/home/yupeng01/tools/queryengine-client-1.6.10-online/queryengine/bin/queryengine'
        self.hadoop = '/home/qerd/tools/hadoop/bin/hadoop'
        self.exc = INST_HIVE(self.hadoop, self.executor)
        self.path_hive = None
        self.state = False
        self.logger = logging.getLogger()
        
    def set_logger(self, logger):
        if isinstance(logger, logging.Logger):
            self.logger = logger
    
    def LoadData(self, tableName, fileName):
        stat = self.exc.INST_HIVE_LOAD(tableName, fileName)
        if not stat:
            self.logger.error('insert data to HDFS failed.')
            return False
        return True
    
    def BuildContext(self, confDict={}):
        try:
            t_inFile = confDict['infile']
            self.inFile = self.getFullPath(t_inFile)
            
            self.table = confDict['tablename']
            ptn_tmp = confDict['partitions'].strip().split(',')
            ptn_dict = {}
            for k in ptn_tmp:
                if k in confDict:
                    ptn_dict[k] = confDict[k]
            self.htable = HiveTable(self.table, ptn_dict)  
            
        except Exception,e:
            logging.error('build hive table param-list fail: ' + e.message)
            self.state = False
        self.state = True
        return self.state
    
    def ExecuteJob(self, parmDict):
        if not self.state:
            logging.error('run enviroment build failed.')
            return False
        self.state = self.LoadData(self.htable.table, self.inFile)
        return self.state
        
if __name__ == '__main__':
    metaFile, = 'test/test_meta.conf', 
    js = InstJobStream()
    stat = js.LoadMetas()
    workpath = js.BuildContext()
    t_obj = js.jobList[0]  # InstHivePushJob
    stat = t_obj.ExecuteJob(js.parmDict)
    

    