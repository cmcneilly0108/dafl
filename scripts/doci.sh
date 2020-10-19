#!/bin/bash

# Hitters - last 28 days, projected next 28 days
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/28d:p/standard/projections'   -H 'Connection: keep-alive'   -H 'Upgrade-Insecure-Requests: 1'   -H 'DNT: 1'   -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.106 Safari/537.36'   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9'   -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main'   -H 'Accept-Language: en-US,en;q=0.9'   -H 'Cookie: fly_device=desktop; AMCVS_10D31225525FF5790A490D4D%40AdobeOrg=1; _ga=GA1.2.345958452.1569188572; s_cc=true; AAMC_cbsi_0=REGION%7C9; i10cfd=1; sports_video_token=%257B%2522parameters%2522%253A%257B%2522subscription_client%2522%253Anull%252C%2522master_product_id%2522%253A%252225924%2522%257D%257D; surround=b|1; tpid=K:1PyAL1PGsMdTt4gzfrWCH%252BClGJ%252FLzsLYI94v%252BEKqxpM%253D:1; pid=L:50:K4vV6pgauFaN%252FklYj01qiA%253D%253D:1; ppid=ac7c6ecaaecc33b6f4b54a092036508a; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; fly_tracking=%7B%22userId%22%3A43879498%2C%22userState%22%3A%22authenticated%22%2C%22userType%22%3A%22registered%22%7D; cbssports_ad=%7B%22region%22%3A%22aw%22%2C%22session%22%3A%22c%22%2C%22subSession%22%3A%223%22%2C%22type%22%3A%22gpt%22%7D; XCLGFbrowser=PboCHl2H6t0RJfFYAdA; _cb_ls=1; _cb=D_IVjcDgw9B8BSt-Fz; m0r9h.salt=MOREPHEUS12$; trc_cookie_storage=taboola%2520global%253Auser-id%3Df01550f8-4e9a-40e6-a560-f27afad46f23-tuct47de5bd; fly_geo={"countryCode":"us","region":"ca"}; _chartbeat2=.1569192731626.1583960416269.0000000000000001.EuP_gBmtKI9CPiF61CnTGGZCHQWLs.2; fly-session=tHwyrnJ7vonIJLv7nJqJuv9F1o; initial_referrer=SNL-04-10aaa0e; CBS_INTERNAL=0; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2Fteams~1586185035873%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main~1586185097870%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2F2138665~1586483246954%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fteams%2Fscout-team~1586483278505; s_lv_undefined=1587422764855; fly_ab=3a60bc20-2193-4aeb-b516-cd314026aa66; _BB.bs=b|4; _pubcid=6fd01686-ec5e-4f05-a9fe-6f38c4e25a23; __gads=ID=a04a4a100d3b3454-227571e429c30047:T=1598900073:S=ALNI_MYeh71EhpOz-qIoSfbJwUsy1ljrMQ; _gcl_au=1.1.1386652081.1599098292; _fbp=fb.1.1599098292651.1898321276; aam_uuid=66382081200777632630003692687175684392; s_sq=%5B%5BB%5D%5D; __qca=P0-1575641368-1599495420903; cbsiaa=43879498; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-1712354808%7CMCIDTS%7C18524%7CMCMID%7C66496153417240104390025496076474930256%7CMCAAMLH-1601053553%7C9%7CMCAAMB-1601053553%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1600455953s%7CNONE%7CMCAID%7C2EC3F56F7FFF8000-4014DFFAE01297BD%7CvVersion%7C4.3.0%7CMCCIDH%7C2023274812; _gid=GA1.2.434313960.1600448753; RT="sl=1&ss=kf8hv49j&tt=4jm&dm=cbssports.com&si=6c227b9f-ea83-498b-999a-e0251ebb8dea&z=1&bcn=%2F%2F173e255e.akstat.io%2F&ld=4jo&ul=799&hd=7aq"; XFP_FIRSTPAGE=0; _BB.d=0|||2; OptanonAlertBoxClosed=2020-09-19T00:58:11.717Z; OptanonConsent=isIABGlobal=false&datestamp=Fri+Sep+18+2020+17%3A58%3A12+GMT-0700+(Pacific+Daylight+Time)&version=6.5.0&landingPath=NotLandingPage&groups=1%3A1%2C2%3A1%2C3%3A1%2C4%3A1%2C5%3A1&hosts=&consentId=570966ef-4e80-41e7-824d-876f5c249fbb&interactionCount=0&AwaitingReconsent=false&geolocation=US%3BCA&legInt=; utag_main=v_id:016d5aed67a90017f199a0e7c29302086001a07e00ac2$_sn:235$_ss:0$_st:1600478892383$vapi_domain:cbssports.com$dc_visit:73$_se:148$ses_id:1600477074102%3Bexp-session$_pn:2%3Bexp-session; fantasy_cookie=ws5978%3A8000%3A1600478167%3Adafl.baseball.cbssports.com; last_access=1600478317' --insecure > dociH.csv 
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/ytd:p/standard/stats' -H 'Host: dafl.baseball.cbssports.com' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:36.0) Gecko/20100101 Firefox/36.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main' -H 'Cookie: tempSessionId=Cg6hK1ME07zYcaiNI+c; apexLat=1401727508126; s_fid=0045FC8783BA25BC-0BCE5F47030267F0; s_getNewRepeat=1428852473713-Repeat; s_lv_cbssports=1403536712266; __gads=ID=0fea60ae6584d668:T=1392825277:S=ALNI_MYpC61JB9AomJ7hIh1ogdcZsJhoFA; XCLGFbrowser=EL46WFIstF+OATrKQfQ; apexLrps="1401722068657:1401660111973:1401640140076:1401596800378:1401555345597"; pid=L:14:K4vV6pgauFaN%252FklYj01qiA%253D%253D:1; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; __CT_Data=gpv=210&apv_4536_www09=3&apv_9595_www09=45; WRUID=0; utag_main=v_id:01465ee90c21003e1269d4e41ec809051003000f00fb8$_sn:105$_ss:0$_st:1428854273315$_pn:2%3Bexp-session$ses_id:1428852465376%3Bexp-session; s_lv_undefined=1428852473713; ALERT_sound=off; CBS_INTERNAL=0; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-2017484664%7CMCMID%7C74487302739133420394431413717016729041%7CMCAAMLH-1429418356%7C7%7CMCAAMB-1429418356%7CcIBAx_aQzFEHcPoEv0GwcQ%7CMCAID%7C2A52096405193149-60000604200028B7; aam_uuid=74243932077103711194406774148147744937; __CT_Data=gpv=211&apv_4536_www09=3&apv_9595_www09=44; __utma=133659419.571239040.1421172854.1428845262.1428852468.20; __utmz=133659419.1421172854.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); aam_uuid=74243932077103711194406774148147744937; LDCLGFbrowser=5e0a423d-5a00-47d8-9c67-a02e014a726a; gebDnVVAmj=81111133915; chatMessage=1; optimizelySegments=%7B%222505680561%22%3A%22ff%22%2C%222508950642%22%3A%22false%22%2C%222529080292%22%3A%22direct%22%2C%222524630265%22%3A%22none%22%7D; optimizelyEndUserId=oeu1426090126985r0.7084489872710146; optimizelyBuckets=%7B%7D; _dyus_8765636=390%7C0%7C3%7C275%7C0%7C0.0.1426090129238.1427476066545.1385937.0%7C85%7C13%7C2%7C115%7C4%7C4%7C0%7C0%7C0%7C0%7C0%7C8%7C37%7C2%7C0%7C0%7C0%7C47%7C0%7C0%7C0%7C0%7C0; _dycst=m.frv2.ms.tos.clk.f.; _dy_geo=US.NA.US_CA.US_CA_San%20Jose; omni_videoAutoPlayState=true; hycw4hSBtd=true; fantasy_cookie=ws1236%3A8000%3A1428852335%3Adafl.baseball.cbssports.com; last_access=1428852485; surround=a|2; s_vnum=1431405555257%26vn%3D3; __utmc=133659419; s_cc=true; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2F%3Fdo_fb_join%3D1~1428813556214%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2F1666825~1428813594653%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2Flastfive%2Foverall%2F1666825~1428813686390%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2F1945243~1428813725831%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2F1765815~1428813972312; s_sq=cbsicbssportssite%252Ccbsicbsiall%3D%2526pid%253Dcbssports%25253A%25252Fmlb%25252Fmgmt%25252Fgold%25252Fstats%25252Fstats-main%2526pidt%253D1%2526oid%253Dhttp%25253A%25252F%25252Fdafl.baseball.cbssports.com%25252Fall%2526ot%253DA; JYaH5Y2vxL=true; s_invisit=true; s_lv_undefined_s=Less%20than%201%20day; __utmb=133659419.2.10.1428852468; sq4YFvJMK2=0' -H 'Connection: keep-alive' > AllHYTD.csv 
# Pitchers - last 28 days, projected next 28 day
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/view/all:P/28d:p/scoring/projections?:sort_col=9&:sort_dir=1'   -H 'Connection: keep-alive'   -H 'Upgrade-Insecure-Requests: 1'   -H 'DNT: 1'   -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.106 Safari/537.36'   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9'   -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main'   -H 'Accept-Language: en-US,en;q=0.9'   -H 'Cookie: fly_device=desktop; AMCVS_10D31225525FF5790A490D4D%40AdobeOrg=1; _ga=GA1.2.345958452.1569188572; s_cc=true; AAMC_cbsi_0=REGION%7C9; i10cfd=1; sports_video_token=%257B%2522parameters%2522%253A%257B%2522subscription_client%2522%253Anull%252C%2522master_product_id%2522%253A%252225924%2522%257D%257D; surround=b|1; tpid=K:1PyAL1PGsMdTt4gzfrWCH%252BClGJ%252FLzsLYI94v%252BEKqxpM%253D:1; pid=L:50:K4vV6pgauFaN%252FklYj01qiA%253D%253D:1; ppid=ac7c6ecaaecc33b6f4b54a092036508a; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; fly_tracking=%7B%22userId%22%3A43879498%2C%22userState%22%3A%22authenticated%22%2C%22userType%22%3A%22registered%22%7D; cbssports_ad=%7B%22region%22%3A%22aw%22%2C%22session%22%3A%22c%22%2C%22subSession%22%3A%223%22%2C%22type%22%3A%22gpt%22%7D; XCLGFbrowser=PboCHl2H6t0RJfFYAdA; _cb_ls=1; _cb=D_IVjcDgw9B8BSt-Fz; m0r9h.salt=MOREPHEUS12$; trc_cookie_storage=taboola%2520global%253Auser-id%3Df01550f8-4e9a-40e6-a560-f27afad46f23-tuct47de5bd; fly_geo={"countryCode":"us","region":"ca"}; _chartbeat2=.1569192731626.1583960416269.0000000000000001.EuP_gBmtKI9CPiF61CnTGGZCHQWLs.2; fly-session=tHwyrnJ7vonIJLv7nJqJuv9F1o; initial_referrer=SNL-04-10aaa0e; CBS_INTERNAL=0; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2Fteams~1586185035873%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main~1586185097870%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fplayers%2Fplayerpage%2F2138665~1586483246954%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fteams%2Fscout-team~1586483278505; s_lv_undefined=1587422764855; fly_ab=3a60bc20-2193-4aeb-b516-cd314026aa66; _BB.bs=b|4; _pubcid=6fd01686-ec5e-4f05-a9fe-6f38c4e25a23; __gads=ID=a04a4a100d3b3454-227571e429c30047:T=1598900073:S=ALNI_MYeh71EhpOz-qIoSfbJwUsy1ljrMQ; _gcl_au=1.1.1386652081.1599098292; _fbp=fb.1.1599098292651.1898321276; aam_uuid=66382081200777632630003692687175684392; s_sq=%5B%5BB%5D%5D; __qca=P0-1575641368-1599495420903; cbsiaa=43879498; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-1712354808%7CMCIDTS%7C18524%7CMCMID%7C66496153417240104390025496076474930256%7CMCAAMLH-1601053553%7C9%7CMCAAMB-1601053553%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1600455953s%7CNONE%7CMCAID%7C2EC3F56F7FFF8000-4014DFFAE01297BD%7CvVersion%7C4.3.0%7CMCCIDH%7C2023274812; _gid=GA1.2.434313960.1600448753; RT="sl=1&ss=kf8hv49j&tt=4jm&dm=cbssports.com&si=6c227b9f-ea83-498b-999a-e0251ebb8dea&z=1&bcn=%2F%2F173e255e.akstat.io%2F&ld=4jo&ul=799&hd=7aq"; XFP_FIRSTPAGE=0; _BB.d=0|||2; OptanonAlertBoxClosed=2020-09-19T00:58:11.717Z; OptanonConsent=isIABGlobal=false&datestamp=Fri+Sep+18+2020+17%3A58%3A12+GMT-0700+(Pacific+Daylight+Time)&version=6.5.0&landingPath=NotLandingPage&groups=1%3A1%2C2%3A1%2C3%3A1%2C4%3A1%2C5%3A1&hosts=&consentId=570966ef-4e80-41e7-824d-876f5c249fbb&interactionCount=0&AwaitingReconsent=false&geolocation=US%3BCA&legInt=; utag_main=v_id:016d5aed67a90017f199a0e7c29302086001a07e00ac2$_sn:235$_ss:0$_st:1600478892383$vapi_domain:cbssports.com$dc_visit:73$_se:148$ses_id:1600477074102%3Bexp-session$_pn:2%3Bexp-session; fantasy_cookie=ws5978%3A8000%3A1600477222%3Adafl.baseball.cbssports.com; last_access=1600477372'  --insecure > dociP.csv 
curl 'http://dafl.baseball.cbssports.com/print/csv/stats/view/all:P/ytd:p/Hot%20Pitchers/stats' -H 'Host: dafl.baseball.cbssports.com' -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:37.0) Gecko/20100101 Firefox/37.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://dafl.baseball.cbssports.com/stats/stats-main/all:P/14d:p/Hot%20Pitchers/' -H 'Cookie: utag_main=v_id:014c89f6865900194c5826b9ee670504c004700900ac2$_sn:4$_ss:0$_st:1428888441080$_pn:4%3Bexp-session$ses_id:1428886594733%3Bexp-session; CBS_INTERNAL=0; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-2017484664%7CMCMID%7C65927062220778700982685519504085117931%7CMCAAMLH-1429491394%7C9%7CMCAAMB-1429491394%7CNRX38WO0n5BH8Th-nqAG_A%7CMCAID%7CNONE; s_vnum=1430835777387%26vn%3D4; s_getNewRepeat=1428886641482-Repeat; s_lv_undefined=1428886641482; _dyid=-6597773366770186944; _dyus_8765636=9%7C0%7C2%7C0%7C0%7C0.0.1428243777624.1428383411025.139633.0%7C95%7C15%7C3%7C115%7C6%7C6%7C0%7C0%7C0%7C0%7C0%7C12%7C0%7C0%7C0%7C0%7C0%7C12%7C0%7C0%7C0%7C0%7C0; _dycst=l.frv3.ms.clk.f.; _dy_geo=US.NA.US_CA.US_CA_Pleasanton; aam_uuid=65691922689728362022671596874779831955; pid=L:15:K4vV6pgauFaN%252FklYj01qiA%253D%253D:1; password_cache=a289c6a41d9c460e3aa5d4093e7312292b0d0ba5017b2a1f5f1b8ad18ba0043d5b75292c40baef23; anon=FALSE; SportsLine=lname&McNeilly&userid&22085bf8208e&fname&Chris; __gads=ID=7cd895097cad43d2:T=1428243786:S=ALNI_MZe9Q5EAf8xnvh1qJmwk6JGibnvCw; hycw4hSBtd=true; gebDnVVAmj=111410126191; __CT_Data=gpv=3; __CT_Data=gpv=3&apv_9595_www09=1; WRUID=0; LDCLGFbrowser=f78ef282-63f5-473e-888f-bfbb9c7d8d37; aam_uuid=65691922689728362022671596874779831955; XCLGFbrowser=N/nWLlUhRUGwjchBwfU; chatMessage=1; optimizelySegments=%7B%222505680561%22%3A%22ff%22%2C%222508950642%22%3A%22false%22%2C%222529080292%22%3A%22direct%22%2C%222524630265%22%3A%22none%22%7D; optimizelyEndUserId=oeu1428244885584r0.5119898541464656; optimizelyBuckets=%7B%7D; _dycmc=3; omni_videoAutoPlayState=true; _dyri_8765636=dycontent%7C54995%7C%7C395%7C%7C%7C; fantasy_cookie=ws1236%3A8000%3A1428886563%3Adafl.baseball.cbssports.com; last_access=1428886713; surround=d|3; s_invisit=true; s_lv_undefined_s=Less%20than%207%20days; s_cc=true; QSI_HistorySession=http%3A%2F%2Fdafl.baseball.cbssports.com%2F%3Fdo_fb_join%3D1~1428886595123%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main~1428886601113%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats~1428886606439%7Chttp%3A%2F%2Fdafl.baseball.cbssports.com%2Fstats%2Fstats-main%2Fall%3AP%2F14d%3Ap%2FHot%2520Pitchers%2F~1428886641769; JYaH5Y2vxL=true; s_sq=cbsicbssportssite%252Ccbsicbsiall%3D%2526pid%253Dcbssports%25253A%25252Fmlb%25252Fmgmt%25252Fgold%25252Fstats%25252Fstats-main%25252Fall%25253AP%25252F14d%25253Ap%25252FHot%252520Pitchers%2526pidt%253D1%2526oid%253DExport%2526oidt%253D3%2526ot%253DSUBMIT; sq4YFvJMK2=0' -H 'Connection: keep-alive' > AllPYTD.csv