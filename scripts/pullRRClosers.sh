#!/bin/bash

 curl 'https://www.fangraphs.com/api/roster-resource/closer-depth-charts/data' \
  -H 'authority: www.fangraphs.com' \
  -H 'accept: application/json, text/plain, */*' \
  -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.128 Safari/537.36' \
  -H 'dnt: 1' \
  -H 'sec-fetch-site: same-origin' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-dest: empty' \
  -H 'referer: https://www.fangraphs.com/roster-resource/closer-depth-chart' \
  -H 'accept-language: en-US,en;q=0.9' \
  -H 'cookie: _ga=GA1.2.951311186.1575169785; _jsuid=1285642928; wordpress_test_cookie=WP+Cookie+check; _cb=CHxDrjD20wJhDVdRAO; __qca=P0-705887623-1585836263306; wordpress_logged_in_0cae6f5cb929d209043cb97f8c2eee44=cmcneilly%7C1641776622%7CVMMju4Q4CfKJi1Eq20NglS7eMtIyQRCO8lk2dHelLCB%7C6419d0ce9a1c72ba0dad0cfa8b5ed87235870774462e76cb461c48e56fa38c25; _cb_ls=1; _cb_svref=null; _chartbeat2=.1575169791328.1619213108591.0110011001110101.DklyI7CFPMxWL_VmxCb0DkHD5K8T-.2; _chartbeat4=t=CgiRCSBVgQBYDXVE4lBgPWqRB26Ozh&E=25&x=266&c=11.71&y=6171&w=817' \
  --compressed > ../Closers.json