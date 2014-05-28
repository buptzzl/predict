#encoding utf8
import urllib2
import datetime
import os
import sys
import json
import commands
import logging
import time
import hashlib
import md5
import base64
reload(sys)
sys.setdefaultencoding('utf8')

HIVE = '/home/qerd/tools/qe/bin/queryengine'
HADOOP = '/home/qerd/tools/hadoop/bin/hadoop'
#HIVE ='/home/qerd/tools/queryengine-client-1.6-online/queryengine/bin/queryengine'
DATA_PATH = '/home/qerd/chenhao/jingpin/data'
LOG_FILE = '/home/qerd/chenhao/jingpin/log/jingpin.log'

API = 'baidudailydownapi$01$'
TABLE_NAME = 'insight_jingpin_download_day' 
DATA_RESULT = 'market_download'
DATA_URL = 'http://report.sj.91.com/Services/ReportAPI.ashx?act=getsummarydowncountdaily&stattime=yyyy-mm-dd&sign=md5_sign'

def load_data_to_hive(data_file,data_date):
    dfs_path = "/app/insight/release/%s/event_action=insight/event_day=%s/event_product=baiduapp"%(TABLE_NAME,data_date)
    partition = "(event_action='insight',event_day='%s',event_product='baiduapp')"%(data_date)
    logging.info("load 91 download data to hive table ...")
    if os.path.getsize(data_file) > 0:
        check_cmd = "%s dfs -ls %s|grep 'event_day=%s'|wc -l"%(HADOOP,dfs_path,data_date)
        path_num = commands.getstatusoutput(check_cmd)
        if len(path_num[1]) ==1 and path_num[1] != '0' :
            hql_cmd = "alter table %s drop partition %s"%(TABLE_NAME,partition)
            hive_cmd = HIVE + " -e \"" + hql_cmd + "\""
            logging.info("partition already exists,execute:%s"%(hive_cmd))
            os.system(hive_cmd)
            hadoop_cmd = "%s dfs -rmr %s"%(HADOOP,dfs_path)
            logging.info("partition path already exists,execute:%s"%(hadoop_cmd))
            os.system(hadoop_cmd)
        hadoop_cmd = "%s dfs -mkdir %s"%(HADOOP,dfs_path)
        logging.info("create partition path,execute:%s"%(hadoop_cmd))
        os.system(hadoop_cmd)
        put_cmd = "%s dfs -put %s %s"%(HADOOP,data_file,dfs_path)
        logging.info("put data to partition,execute:%s"%(put_cmd))
        os.system(put_cmd)
        hql_cmd = "alter table %s add partition %s location '%s'"%(TABLE_NAME,partition,dfs_path)
        hive_cmd = HIVE + " -e \"" + hql_cmd + "\""
        logging.info("add partition,execute:%s"%(hive_cmd))
        os.system(hive_cmd)
        logging.info("load 91 download to hive table success")
        return True
    else:
        logging.error("data file is empty,please check data source")
        return False

def parse_json_to_file(json_str,data_file):
    if json_str ==None or json_str == '':
        return False
    file_object = open(data_file, 'w')
    item = json.loads(json_str)
    success = str(item['success']).lower()
    if success == 'false':
        logging.error("request data is not ready")
        return False
    markets = item['datas']
    for market in markets:
        datatime = market['datatime'].replace('-','')
        total_count = str(market['downtotalcount'])
        from_client = str(market['downfromclient'])
        client_one = str(market['downfromclient_one'])
        client_two = str(market['downfromclient_two'])
        client_search = str(market['downfromclient_search'])
        from_pc = str(market['downfrompc'])
        pc_one = str(market['downfrompc_one'])
        pc_two = str(market['downfrompc_two'])
        pc_serch = str(market['downfrompc_search'])
        from_other = str(market['downfromother'])
        from_web = str(market['downfromweb'])
        from_api = str(market['downfromapi'])
        soft = market['soft']
        line = '\t'.join([datatime,total_count,from_client,client_one,client_two,client_search,from_pc,pc_one,pc_two,pc_serch,from_other,from_web,from_api,soft]) + '\n'
        file_object.write(line)
    file_object.close()
    if os.path.getsize(data_file) > 0:
        logging.info("parse interface data success")
        return True
    else:
        logging.error("parse interface data failed")
        return False

def generate_md5(prefix):
    if prefix == None or prefix == '':
        logging.error("request sign param is error")
        return False
    src_value = prefix + "&" + API
    hash = hashlib.md5()
    hash.update(src_value)
    value = hash.hexdigest()
    return value 

def handler_jingpin_interface(data_date):
    ready = False
    date_para = data_date[0:4]+ '-' + data_date[4:6] + '-' + data_date[6:]
    md5_sign= generate_md5(date_para)
    req_url = DATA_URL.replace('yyyy-mm-dd',date_para).replace("md5_sign",md5_sign)
    logging.info("request 91 download info:%s"%(req_url))
    data_file = DATA_PATH + "/%s.%s.txt"%(DATA_RESULT,data_date)
    while not ready:
        json_str = urllib2.urlopen(req_url).read()
        if json_str != None and json_str != '':
            ready = parse_json_to_file(json_str,data_file)
            if ready == True:
                logging.info("date:%s data source is ready"%(data_date))
                break
            else:
                logging.error("data:%s data source is not ready,waiting ..."%(data_date))
                time.sleep(60*5)
    logging.info("request 91 download info success:%s"%(data_file))
    return load_data_to_hive(data_file,data_date)

if __name__ == '__main__':
    date = ""
    if len(sys.argv) == 2:
        date = sys.argv[1]
    if date == None or date == "":
        today = datetime.datetime.now()
        dest = today - datetime.timedelta(days=1)
        date = dest.strftime("%Y%m%d")
    logging.basicConfig(filename=LOG_FILE,level=logging.DEBUG)
    logging.info("begin to handle interface data ...")
    succ = False
    succ = handler_jingpin_interface(date)
    if succ == True:
        logging.info("handle interface data success")
    else:
        logging.error("handle interface data failed")
