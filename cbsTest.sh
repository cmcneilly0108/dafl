#!/bin/bash 

# All Hitters
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/view' -H 'Host: dafl.baseball.cbssports.com' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:34.0) Gecko/20100101 Firefox/34.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main' -H 'Cookie: tempSessionId=Cg6hK1ME07zYcaiNI+c; apexLat=1401727508126; s_fid=0045FC8783BA25BC-0BCE5F47030267F0; s_getNewRepeat=1421172878386-Repeat; s_lv_cbssports=1403536712266; __gads=ID=0fea60ae6584d668:T=1392825277:S=ALNI_MYpC61JB9AomJ7hIh1ogdcZsJhoFA; XCLGFbrowser=EL46WFIstF+OATrKQfQ; apexLrps="1401722068657:1401660111973:1401640140076:1401596800378:1401555345597"; pid=L%3A13%3AK4vV6pgauFaN%252FklYj01qiA%253D%253D%3A1; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; __CT_Data=gpv=199&apv_4536_www09=3&apv_9595_www09=13; WRUID=0; utag_main=v_id:01465ee90c21003e1269d4e41ec809051003000f00fb8$_sn:76$_ss:0$_st:1421174678056$_pn:2%3Bexp-session$ses_id:1421172853560%3Bexp-session; s_lv_undefined=1421172878386; ALERT_sound=off; CBS_INTERNAL=0; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-2017484664%7CMCMID%7C74487302739133420394431413717016729041%7CMCAAMLH-1421777654%7C9%7CMCAAMB-1421777654%7CcIBAx_aQzFEHcPoEv0GwcQ%7CMCAID%7C2A52096405193149-60000604200028B7; s_vnum=1422630857805%26vn%3D2; aam_uuid=74243932077103711194406774148147744937; fantasy_cookie=ws1236%3A8000%3A1421172726%3Adafl.baseball.cbssports.com; last_access=1421172876; surround=f|3; s_invisit=true; s_lv_undefined_s=More%20than%207%20days; __CT_Data=gpv=200&apv_4536_www09=3&apv_9595_www09=12; __utma=133659419.571239040.1421172854.1421172854.1421172854.1; __utmb=133659419.2.10.1421172854; __utmc=133659419; __utmz=133659419.1421172854.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); s_cc=true; aam_uuid=74243932077103711194406774148147744937; LDCLGFbrowser=5e0a423d-5a00-47d8-9c67-a02e014a726a; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2F%3Fdo_fb_join%3D1~1421172854745%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main~1421172878886; s_sq=cbsicbssportssite%252Ccbsicbsiall%3D%2526pid%253Dcbssports%25253A%25252Fmlb%25252Fmgmt%25252Fgold%25252Fstats%25252Fstats-main%2526pidt%253D1%2526oid%253Dhttp%25253A%25252F%25252Fdafl.baseball.cbssports.com%25252Fprint%25252Fcsv%25252Fstats%25252Fview%2526ot%253DA' -H 'Connection: keep-alive' > cAllHitters.csv

# All Pitchers

# All Pitchers YTD
