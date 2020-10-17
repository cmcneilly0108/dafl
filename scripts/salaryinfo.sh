#!/bin/bash

#cd /home/cmcneilly/Dropbox/Personal/DAFL
#cd /mnt/c/Users/cmcne/Dropbox/Personal/DAFL
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/stats-main/team:all/y:p/salary%20info' -H 'Host: dafl.baseball.cbssports.com' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:40.0) Gecko/20100101 Firefox/40.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main/team:all/y:p/salary%20info/' -H 'Cookie: tempSessionId=Cg6hK1ME07zYcaiNI+c; s_fid=0045FC8783BA25BC-0BCE5F47030267F0; s_getNewRepeat=1440541016346-Repeat; s_lv_cbssports=1403536712266; __gads=ID=0fea60ae6584d668:T=1392825277:S=ALNI_MYpC61JB9AomJ7hIh1ogdcZsJhoFA; XCLGFbrowser=EL46WFIstF+OATrKQfQ; pid=L:38:K4vV6pgauFaN%252FklYj01qiA%253D%253D:1; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; __CT_Data=gpv=266&apv_4536_www09=3&apv_9595_www09=201; WRUID=0; utag_main=v_id:01465ee90c21003e1269d4e41ec809051003000f00fb8$_sn:237$_ss:0$_st:1440542814941$_pn:12%3Bexp-session$ses_id:1440540385443%3Bexp-session; s_lv_undefined=1440541016346; ALERT_sound=off; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-2017484664%7CMCMID%7C74487302739133420394431413717016729041%7CMCAAMLH-1440777263%7C9%7CMCAAMB-1440777263%7CcIBAx_aQzFEHcPoEv0GwcQ%7CMCAID%7C2A52096405193149-60000604200028B7; aam_uuid=74243932077103711194406774148147744937; __CT_Data=gpv=236&apv_4536_www09=3&apv_9595_www09=169; __utma=133659419.571239040.1421172854.1440537285.1440540390.132; __utmz=133659419.1421172854.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); aam_uuid=74243932077103711194406774148147744937; LDCLGFbrowser=5e0a423d-5a00-47d8-9c67-a02e014a726a; gebDnVVAmj=81111133915; chatMessage=1; optimizelySegments=%7B%222505680561%22%3A%22ff%22%2C%222508950642%22%3A%22false%22%2C%222529080292%22%3A%22direct%22%2C%222524630265%22%3A%22none%22%2C%22233752133%22%3A%22ff%22%2C%221762890450%22%3A%22none%22%2C%221633460559%22%3A%22true%22%2C%22233682199%22%3A%22direct%22%2C%22233620402%22%3A%22false%22%2C%222774430393%22%3A%22direct%22%2C%222760641268%22%3A%22false%22%2C%222782740429%22%3A%22none%22%2C%222767250350%22%3A%22ff%22%7D; optimizelyEndUserId=oeu1426090126985r0.7084489872710146; optimizelyBuckets=%7B%223226860680%22%3A%223210181298%22%7D; _dyus_8765636=607%7C3073%7C13%7C0%7C0%7C0.0.1426090129238.1440446257992.14356128.0%7C235%7C35%7C7%7C115%7C2%7C0%7C0%7C0%7C0%7C0%7C0%7C2%7C0%7C0%7C0%7C3%7C0%7C2%7C8%7C11%7C19%7C6%7C47; _dycst=m.f.ws.nc.; _dy_geo=US.NA.US_CA.US_CA_San%20Jose; omni_videoAutoPlayState=true; OX_nd=537074755_2_1438612757742; CBS_INTERNAL=0; _dy_toffset=-4; hycw4hSBtd=true; s_vnum=1442094818358%26vn%3D17; fantasy_cookie=ws1236%3A8000%3A1440540853%3Adafl.baseball.cbssports.com; last_access=1440541003; surround=f|3; JYaH5Y2vxL=true; s_cc=true; s_sq=%5B%5BB%5D%5D; __utmc=133659419; lpsTrackCount7=result; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2Fscoring%2Fstandard~1440537288216%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2F~1440537307744%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main~1440540408723%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats~1440540414951%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main%2Fteam%3Aall%2Fy%3Af%2Fsalary%2520info%2F~1440540455429%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats~1440540769143; s_invisit=true; s_lv_undefined_s=Less%20than%201%20day; __utmb=133659419.12.10.1440540390; sq4YFvJMK2=0; optimizelyPendingLogEvents=%5B%22n%3Dengagement%26u%3Doeu1426090126985r0.7084489872710146%26wxhr%3Dtrue%26time%3D1440541100.035%26f%3D3153990678%26g%3D233681392%22%5D' -H 'Connection: keep-alive' > salaryinfo.csv
