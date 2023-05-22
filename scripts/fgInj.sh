#!/bin/bash


curl 'https://cdn.fangraphs.com/api/roster-resource/injury-report/data?loaddate=1619181073&season=2023' -H 'authority: cdn.fangraphs.com' -H 'accept: application/json, text/plain, */*' -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.128 Safari/537.36' -H 'dnt: 1' -H 'origin: https://www.fangraphs.com' -H 'sec-fetch-site: same-site' -H 'sec-fetch-mode: cors' -H 'sec-fetch-dest: empty' -H 'referer: https://www.fangraphs.com/' -H 'accept-language: en-US,en;q=0.9' --compressed > ../injuryReport.json 

#curl 'https://www.fangraphs.com/api/prospects/board/prospects-list-combined?pos=all&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32,33&stats=bat&qual=0&type=0&team=&season=2021&seasonend=2021&draft=2022prospect&valueheader=prospect-new&quickleaderboard=2021all' -H 'authority: www.fangraphs.com' -H 'accept: application/json, text/plain, */*' -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.128 Safari/537.36' -H 'dnt: 1' -H 'origin: https://www.fangraphs.com' -H 'sec-fetch-site: same-site' -H 'sec-fetch-mode: cors' -H 'sec-fetch-dest: empty' -H 'referer: https://www.fangraphs.com/' -H 'accept-language: en-US,en;q=0.9' --compressed > ../fangraphs-the-board-dataH.csv 

curl 'https://www.fangraphs.com/api/prospects/board/prospects-list-combined?pos=all&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32,33&stats=bat&qual=0&type=0&team=&season=2023&seasonend=2023&draft=2023prospect&valueheader=prospect-new&quickleaderboard=2021all' -H 'authority: www.fangraphs.com' -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' -H 'accept-language: en-US,en;q=0.9' -H 'cache-control: max-age=0' -H 'cookie: wordpress_test_cookie=WP%20Cookie%20check; wordpress_logged_in_0cae6f5cb929d209043cb97f8c2eee44=cmcneilly%7C1680652592%7CKMq3phKSJTdwBmv6NKg680XDCJKXlcTSNKIZC0HqXhe%7C4d55c897c8719d69f67db50943937e51f5e6d4ab453c13469f0ac495aeb3d335; _omappvp=dzmCqW3WuxwYjQGRfPtvDu4eR4j0tTxumTTxRCTvcjWMfNRzHT250mHx6f06Or53MOaQ5UHd1aHqrNk4ACRIEOVWmrmyX3b3; _cb_ls=1; _cb=CHxDrjD20wJhDVdRAO; _chartbeat2=.1575169791328.1651244399350.0000000000100001.zJEdXCaPJK0BP2iLmDE61yMB7XR2C.1; _cb_svref=null' -H 'dnt: 1' -H 'referer: https://www.fangraphs.com/' -H 'sec-ch-ua: "Chromium";v="100", " Not A;Brand";v="99"' -H 'sec-ch-ua-mobile: ?0' -H 'sec-ch-ua-platform: "Windows"' -H 'sec-fetch-dest: document' -H 'sec-fetch-mode: navigate' -H 'sec-fetch-site: same-origin' -H 'sec-fetch-user: ?1' -H 'upgrade-insecure-requests: 1' -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.133 Safari/537.36' --compressed > ../fangraphs-the-board-dataH.json 

curl 'https://www.fangraphs.com/api/prospects/board/prospects-list-combined?pos=all&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32,33&stats=pit&qual=0&type=0&team=&season=2023&seasonend=2023&draft=2023prospect&valueheader=prospect-new&quickleaderboard=2021all' -H 'authority: www.fangraphs.com' -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' -H 'accept-language: en-US,en;q=0.9' -H 'cache-control: max-age=0' -H 'cookie: wordpress_test_cookie=WP%20Cookie%20check; wordpress_logged_in_0cae6f5cb929d209043cb97f8c2eee44=cmcneilly%7C1680652592%7CKMq3phKSJTdwBmv6NKg680XDCJKXlcTSNKIZC0HqXhe%7C4d55c897c8719d69f67db50943937e51f5e6d4ab453c13469f0ac495aeb3d335; _omappvp=dzmCqW3WuxwYjQGRfPtvDu4eR4j0tTxumTTxRCTvcjWMfNRzHT250mHx6f06Or53MOaQ5UHd1aHqrNk4ACRIEOVWmrmyX3b3; _cb_ls=1; _cb=CHxDrjD20wJhDVdRAO; _chartbeat2=.1575169791328.1651244399350.0000000000100001.zJEdXCaPJK0BP2iLmDE61yMB7XR2C.1; _cb_svref=null' -H 'dnt: 1' -H 'referer: https://www.fangraphs.com/' -H 'sec-ch-ua: "Chromium";v="100", " Not A;Brand";v="99"' -H 'sec-ch-ua-mobile: ?0' -H 'sec-ch-ua-platform: "Windows"' -H 'sec-fetch-dest: document' -H 'sec-fetch-mode: navigate' -H 'sec-fetch-site: same-origin' -H 'sec-fetch-user: ?1' -H 'upgrade-insecure-requests: 1' -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.133 Safari/537.36' --compressed > ../fangraphs-the-board-dataP.json 

