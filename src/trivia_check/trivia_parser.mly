%token EOF
%token IN
%token SEMICOLON

%start <unit> check

%%

check:
  | IN EOF {}
  | SEMICOLON* EOF {}
