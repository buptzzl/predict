#coding=utf-8


import os, logging, logging.config
from subprocess import *
from datetime import datetime, timedelta

URL_DIM_DATA = 'http://ex.ops.baidu.com/exchange/index.php?r=ApiDownloadAction&platform=channelrating&type=csv&title'
P_ROOT = r'/home/qerd/channel_predict/CBC/'
if os.name == 'nt':
    P_ROOT = r'D:/project/channel-cost/'
F_DIM_TMP = P_ROOT + 'data/tmp.dim_data'

logging.config.fileConfig(P_ROOT + 'conf/logging.conf')
logger = logging.getLogger('root')

# 数据流： wget拉取文件； 清洗； 备份历史； 覆盖当前文件；

class PullData(object):
    ''' 数据拉取， 解析类 '''
    def __init__(self, Finit=1, FType=""):
        self.state = Finit  # 数据抽取状态
        self._doc = FType # 解析类型标志
        self.cmd = None # 待执行的命令
        self.f_save = None # 拉取结果存储路径
        self.f_tmp = None # 临时文件路径 
        
    def __enter__(self):
        ''' 状态准备 '''
        return None
    def pull(self, content, n_retry=1):
        ''' 拉取、检验数据 '''
        return True
    def clean(self, content, *args):
        ''' 数据清洗 '''
        return True
    def backup(self, content, rdir=None, model='w', t_format='%Y%m%d', **kvargs):
        ''' 数据备份 默认按tmp目录下的日期为编号保存 ; 强制使用时间命名文件; @content 文件的路径or内容'''
        if content is None:
            logger.warn('backup source is None')
            return False
        if rdir is None:
            if self.f_save is None:
                logger.warn('backup failed of DIR is None.')
                return False
            rdir = self.f_save
            
        s_col, s_line = '\t', '\n'
        if 's_col' in kvargs:
            s_col = kvargs['s_col']
        if 's_line' in kvargs:
            s_line = kvargs['s_line']
            
        pdir = os.path.dirname((rdir))
        if not os.path.exists(pdir):
            os.makedirs(pdir)
        now_ = datetime.now()
        rdir = os.path.join(pdir, now_.strftime(t_format))
        self.f_save = rdir
        
        if isinstance(content, str):
            if os.path.isfile(content) and content != rdir:
                with open(content, 'rb') as f:
                    f_w = open(rdir, model)
                    f_w.write(''.join(f.readlines()))
                    f_w.close()
                logger.info('copy file to FILE:%s' % self.f_save)
            else:
                f_w = open(rdir, model)
                f_w.write(content)
                f_w.close()
                logger.info('write string to FILE:%s' % self.f_save)
        elif isinstance(content, list):
            f_w = open(rdir, model)
            f_w.write('%s' % (s_line.join([s_col.join(ai) for ai in content])))
            f_w.close()
            logger.info('write list to FILE:%s' % self.f_save)
        else:
            logger.error("data unparserable, type:" + str(type(content)))
            return  False
        # 文件备份成功后，才删除原始文件
        if isinstance(content, str) and os.path.isfile(content):
            os.remove(content)
        return True
    
    def __exit__(self):
        ''' 释放资源 '''
        return True 

class ChannelPull(PullData):
    ''' 拉取运营渠道信息;  '''
    def __init__(self, f_tmp, url_dim, head='ID,'):
        super(ChannelPull, self).__init__(-1, "cop_channel_dim_info");
        self.cmd = 'wget -O %s "%s"' % (f_tmp, url_dim)
        self.f_tmp =f_tmp
        # 设置备份子目录路径
        self.f_save = os.path.join(os.path.dirname(f_tmp), 'channel_dim/')
        self.f_dst = os.path.join(os.path.dirname(f_tmp), 'cbc_data_dim.csv')
        self.str_head = head
        self.state = -1
    
    def pull(self, content, n_retry=1):
        self.state = -1
        while n_retry != 0 and self.state != 0:
            p = Popen(self.cmd, shell=True)
            self.state = p.wait()
            n_retry -= 1
        logger.info('class::ChannelPull cmd:%s return state:%d' % (self.cmd, self.state))
        #self.drop_head()
        return self.state
    
    def clean(self, content, *args):
        self.drop_head()
    
    def _check_header(self):
        ''' 检查结果文件  '''
        if not os.path.isfile(self.f_save):
            logger.error('file unexist.')
            return None
        with open(self.f_save, 'rb') as f:
            # 检查文件头
            head = f.readline()
            if self.str_head in head:
                logger.info('head exist in file: %s' % self.f_save)
                return 'header'
        return None
    
    def drop_head(self):
        ''' 如果存在文件头则删除之 '''
        if self._check_header() is None:
            return False
        data = []
        with open(self.f_save, 'rb') as f:
            data = f.readlines()
        if len(data) <= 1:
            logger.warn('class::%s file header drop return empty files' % (self.__name__))
        with open(self.f_save, 'wb') as f:
            f.write(''.join(data[1:]))
        logger.info('drop file header part.')
        return True
    
    def backup(self, content, rdir=None, model='w', t_format='%Y%m%d', **kvargs):
        ''' 备份文件 且覆盖目标文件 '''
        if super(ChannelPull, self).backup(self.f_tmp, self.f_save, model, t_format,):
            with open(self.f_save, 'rb') as f_r:
                f_w = open(self.f_dst, 'wb')
                f_w.write(''.join(f_r.readlines()))
                f_w.close()
                logger.debug('copy file to path:%s' % (self.f_dst))
        return True
        
        
    
if __name__ == '__main__':
    t = ChannelPull(F_DIM_TMP, URL_DIM_DATA)
    stat = t.pull(None, 1)
    print 'finish get channel_dim file and clear.'
