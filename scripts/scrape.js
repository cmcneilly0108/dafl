var page = require('webpage').create();
                     page.open('https://www.fangraphs.com/prospects/the-board-scouting-and-stats', function () {
                     console.log(page.content); //page source
                     phantom.exit();
                     });
