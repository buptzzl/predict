#!/usr/bin/env python
#coding=utf-8
'''
/***************************************************************************
 * 
 * Copyright (c) 2014 Baidu.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file code/check_regress.py
 * @author qerd(com@baidu.com)
 * @date 2014/04/10 10:35:20
 * @brief 
 *  
 **/
'''

import sys
import os


class CBCDifferece(object):
    ''' CBC 预测模型的结果分析 ''' 
    def __init__(self, ): 
        #TODO
        pass

def diff_percent(fp, is_empty=True):
    # 统计 百分比 DIFF
    data, perc, n_perc, idx_per, diff, idx_nw0 = [], 0.0, 0, 5, 0.0, 1,  
    SEPA, perc_empty, = ',', '1', 
    f = open(fp, 'rb')
    for line in f:
        atom = line.strip().split(SEPA);
        #print "TEST::check_diff_percent() atom=", atom
        if len(atom) < (idx_per+1) or (is_empty and atom[idx_per]==perc_empty):
            continue
        if atom[idx_per] == "":
            atom[idx_per] = '0.0'
        #print "TEST::check_diff_percent()", atom[idx_per];
        percent = float(atom[idx_per])
        percent = percent > 0 and percent or (-1)*percent
        perc += percent
        diff += percent*float(atom[idx_nw0])
        n_perc += 1
    f.close()
    return (perc, n_perc, diff)

def diff_total():
    # 统计全量的DIFF
    dir_, = r'../exc/cid2/',
    #attrs = {'diff_percent':[True] } ;
    files = os.listdir(dir_)
    for fi in files:
        #for fun_, args_ in attrs.items():
        (perc, n_perc, diff) = diff_percent(dir_+fi, True)
        print perc, n_perc, perc/n_perc, diff, fi  
    return None







if __name__ == '__main__':
    diff_total()

    '''
    for fi in sys.argv[1:]:
    (perc, n_perc, diff) = diff_percent(fi, is_empty=True)
    print "total: ", n_perc, " sum(|percent|): ",perc, " percent average:", perc/n_perc, " total diff:",diff
    '''



#/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
