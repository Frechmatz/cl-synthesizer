var TurndownService = require('turndown');
var fs = require('fs');

var htmlString = fs.readFileSync('/Users/olli/src/lisp/cl-synthesizer/makedoc/doc.html', 'utf8');
var turndownService = new TurndownService();
var markdown = turndownService.turndown(htmlString);
fs.writeFileSync('/Users/olli/src/lisp/cl-synthesizer/makedoc/doc.md', markdown);
