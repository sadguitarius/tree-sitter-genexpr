(identifier) @variable

; ((identifier) @constant
;  (#match? @constant "^[_a-zA-Z][_a-zA-Z0-9]*$"))

"break" @keyword
"continue" @keyword
"else" @keyword
"for" @keyword
"if" @keyword
"return" @keyword
"while" @keyword

; "-" @operator
; "-=" @operator
; "->" @operator
; "=" @operator
; "!=" @operator
; "*" @operator
; "&" @operator
; "&&" @operator
; "+" @operator
; "+=" @operator
; "<" @operator
; "==" @operator
; ">" @operator
; "||" @operator
;
; "." @delimiter
; ";" @delimiter

(string_literal) @string

(number_literal) @number

(type_specifier) @keyword.type

(inlet_outlet) @constant.builtin

(postfix_expression
  object: (identifier) @function)
(function_declaration
  name: (identifier) @function)

(comment) @comment
