var TurndownService = require('turndown');
var fs = require('fs');

var home = '/Users/olli/src/lisp/cl-synthesizer/';

var htmlString = fs.readFileSync(home + 'makedoc/generated/readme.html', 'utf8');
var turndownService = new TurndownService();
var markdown = turndownService.turndown(htmlString);
fs.writeFileSync(home + 'makedoc/generated/readme.md', markdown);
