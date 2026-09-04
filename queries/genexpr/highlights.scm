(identifier) @variable

; ((identifier) @constant
;  (#match? @constant "^[_a-zA-Z][_a-zA-Z0-9]*$"))

"break" @keyword
"continue" @keyword
"do" @keyword
"else" @keyword
"for" @keyword
"if" @keyword
"require" @keyword
"return" @keyword
"while" @keyword

(assignment_operator) @operator
(binary_expression operator: _ @operator)
(unary_expression operator: _ @operator)

"." @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter

"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket

(string_literal) @string

(number_literal) @number

(type_specifier) @keyword.type

(inlet_outlet) @constant.builtin

(postfix_expression
  object: (identifier) @function
  field: (call_member_expression))
(literal_member_expression
  field: (identifier) @function
  parameters: (call_member_expression))
(function_declaration
  name: (identifier) @function)

(comment) @comment
