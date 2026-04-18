;; -*- mode: lisp-data -*-
;; Tempel templates. Each section header is a major mode (or derived modes).
;; Elements: strings are literal text, p = placeholder, q = exit point,
;; n = newline, n> = newline+indent, > = indent line.


go-mode

(pf "fmt.Printf(\"" p "\")")
(pl "fmt.Println(" p ")")


;; js2-mode derives from js-mode, so js-mode templates apply to both.
js-mode

(after "after(function() {" n > p n "})")
(afterEach "afterEach(function() {" n > p n "})")
(assert "assert." p "(" q ");")
(before "before(function() {" n > p n "})")
(beforeEach "beforeEach(function() {" n > p n "})")
(cb "function(" p ") {" n > q n "})")
(com n "/**" n " * " q n " * @param {" p "} " p n " * @return {" p "}" n " */")
(describe "describe('" p "', function() {" n > p n "})")
(expect "expect(" p ")." p "(" q ");")
(exports "module.exports = {" n > q n "};")
(fn "function" p "(" p ") {" n > q n "}")
(for "for (" p "; " p "; " p ") {" n > p n "}")
(fori "for (var i = 0; i < " p "; i++) {" n > p n "}")
(id "document.getElementById('" q "');")
(if "if (" p ") {" n > q n "}")
(invoc "(function(" p "){" n > q n "})(" p ");")
(it "it('" p "', function(done) {" n > p n "})")
(method p ".prototype." p " = function(" p "){" n > q n "};")
(mod (p "mod" v) " = require('" v "')" n n "module.exports = {" n > p n "};")
(prop n "var " (p "name" v) " = (function(){" n n > "var value = undefined;" n n > "return function " v "(newValue){" n n > "if( " v ".arguments.length > 0 ){" n > "value = newValue;" n "}" n n > "return value;" n "};" n n "})();")
(proto p ".prototype." p " = " q)
(rq (p "name" v) " = require('" v "')" q)
(rtnerr "if (" (p "err" e) ") {" n > "callback(" e ");" n > "return;" n "}")
(slice "Array.prototype.slice." p "(" q ");")
(super p ".prototype." p "." p "(" q ");")
(switch "switch(" p "){" n > q n "}")
(throw "throw new Error(\"" p "\");")
(try "try {" n > p n "} catch(error) {" n > q n "}")
(var "var " p " = " q)


markdown-mode

(img "<p align=\"center\"><img src=\"" p "\" height=\"240px\" width=\"auto\"></p>")
(kep "## " p n n "- *Date: " p "*" n n "*References*" n n "- [" p " KEP link](" p ")")
(cb "<details><summary>" p "</summary><p>" n n p n n "</p></details></br>")


python-mode

(pdb "import pdb" n "pdb.set_trace()")
