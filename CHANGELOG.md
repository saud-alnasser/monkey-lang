## 0.0.13 (2024-09-23)

### Features

#### add support for colon token (#93)

### Fixes

#### update rust crate thiserror to v1.0.64 (#97)

## 0.0.12 (2024-09-19)

### Features

#### use better names of cursor pointers (#85)

## 0.0.11 (2024-09-18)

### Features

#### use single prefix for error messages (#83)

## 0.0.10 (2024-05-03)

### Features

#### replace null with undefined for expressions without value (#73)

## 0.0.9 (2024-05-03)

### Features

#### implement string data-type (#60)

#### implement string concatenation (#63)

#### implement builtin len function (#64)

#### implement array data-type (#65)

#### implement array indexing (#66)

#### implement builtin first function (#67)

#### implement builtin last function (#68)

#### implement builtin rest function (#69)

#### implement builtin push function (#70)

## 0.0.8 (2024-05-01)

### Features

#### increase stack limit from 2mb to 16mb (#58)

## 0.0.7 (2024-04-27)

### Features

#### implement bindings and the environment (#54)

#### implement functions & functions calls (#55)

## 0.0.6 (2024-04-26)

### Features

#### create a baseline for the evaluator (#44)

#### use evaluator in the repl (#45)

#### extend the evaluator to eval boolean literals (#46)

#### eval prefix expressions (#47)

#### eval infix expressions (#48)

#### eval lte and gte ops (#49)

#### eval if else expressions (#50)

#### eval return statements (#51)

#### use better error handling (#52)

## 0.0.5 (2024-04-24)

### Features

#### handle boolean expressions (#35)

#### handle grouped expressions (#36)

#### handle if expressions (#37)

#### handle function expressions (#39)

#### handle call expressions (#40)

#### change repl to reset on each loop (#41)

#### make better error messages (#42)

## 0.0.4 (2024-04-20)

### Features

#### create basic let statements parser (#23)

#### create basic return statements parser (#25)

#### create basic expression statments parser (#26)

#### handle basic prefix expressions (#28)

#### prepare for handling infix operators (#29)

#### handle infix expressions (#31)

#### use the parser in the repl (#32)

#### created a parser that can parse let, return and expression statments.

#### implemented prefix and infix pratt parsing for integers and identfiers for now.

## 0.0.3 (2024-02-27)

### Features

#### create basic symbol lexer (#1)

#### extend lexer to handle let statement (#2)

#### extend lexer to handle fn statements (#4)

#### extend lexer to handle more operators (#5)

#### extend lexer to handle conditional statement (#6)

#### extend lexer to handle longer opeartors (#7)

#### create basic repl (#8)

#### extend lexer to track line, column and length of a token (#9)

#### include illegal literal into the token (#12)

## 0.0.2 (2024-02-27)

### Features

#### create basic symbol lexer (#1)

#### extend lexer to handle let statement (#2)

#### extend lexer to handle fn statements (#4)

#### extend lexer to handle more operators (#5)

#### extend lexer to handle conditional statement (#6)

#### extend lexer to handle longer opeartors (#7)

#### create basic repl (#8)

#### extend lexer to track line, column and length of a token (#9)

#### include illegal literal into the token (#12)

## 0.0.1 (2024-02-27)

### Features

#### create basic symbol lexer (#1)

#### extend lexer to handle let statement (#2)

#### extend lexer to handle fn statements (#4)

#### extend lexer to handle more operators (#5)

#### extend lexer to handle conditional statement (#6)

#### extend lexer to handle longer opeartors (#7)

#### create basic repl (#8)

#### extend lexer to track line, column and length of a token (#9)

#### include illegal literal into the token (#12)

#### create basic repl to generate tokens

#### extended the lexer to tokenize `fn`

#### extended the lexer to tokenize `if else return true false`

#### extended the lexer to tokenize let statements and integers

#### extended the lexer to tokenize `= + - * / ! < > == != <= >= , ; ( ) { }`
