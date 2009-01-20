;;; wisent-java-malabar-wy.el --- Generated parser support file

;; Copyright (C) 2009 Espen Wiborg

;; Author: Espen Wiborg <espenhw@grumblesmurf.org>
;; Created: 2009-01-20 01:45:32+0100
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-java-malabar.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-java-malabar-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("abstract" . ABSTRACT)
     ("assert" . ASSERT)
     ("boolean" . BOOLEAN)
     ("break" . BREAK)
     ("byte" . BYTE)
     ("case" . CASE)
     ("catch" . CATCH)
     ("char" . CHAR)
     ("class" . CLASS)
     ("const" . CONST)
     ("continue" . CONTINUE)
     ("default" . DEFAULT)
     ("do" . DO)
     ("double" . DOUBLE)
     ("else" . ELSE)
     ("enum" . ENUM)
     ("extends" . EXTENDS)
     ("final" . FINAL)
     ("finally" . FINALLY)
     ("float" . FLOAT)
     ("for" . FOR)
     ("goto" . GOTO)
     ("if" . IF)
     ("implements" . IMPLEMENTS)
     ("import" . IMPORT)
     ("instanceof" . INSTANCEOF)
     ("int" . INT)
     ("interface" . INTERFACE)
     ("long" . LONG)
     ("native" . NATIVE)
     ("new" . NEW)
     ("package" . PACKAGE)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("public" . PUBLIC)
     ("return" . RETURN)
     ("short" . SHORT)
     ("static" . STATIC)
     ("strictfp" . STRICTFP)
     ("super" . SUPER)
     ("switch" . SWITCH)
     ("synchronized" . SYNCHRONIZED)
     ("this" . THIS)
     ("throw" . THROW)
     ("throws" . THROWS)
     ("transient" . TRANSIENT)
     ("try" . TRY)
     ("void" . VOID)
     ("volatile" . VOLATILE)
     ("while" . WHILE)
     ("true" . TRUE)
     ("false" . FALSE)
     ("null" . NULL))
   'nil)
  "Table of language keywords.")

(defconst wisent-java-malabar-wy--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (COMP . "~")
      (OROR . "||")
      (OREQ . "|=")
      (OR . "|")
      (XOREQ . "^=")
      (XOR . "^")
      (QUESTION . "?")
      (GTEQ . ">=")
      (EQEQ . "==")
      (EQ . "=")
      (LTEQ . "<=")
      (LSHIFTEQ . "<<=")
      (LSHIFT . "<<")
      (DIVEQ . "/=")
      (DIV . "/")
      (MINUSEQ . "-=")
      (MINUSMINUS . "--")
      (MINUS . "-")
      (PLUSEQ . "+=")
      (PLUSPLUS . "++")
      (PLUS . "+")
      (MULTEQ . "*=")
      (MULT . "*")
      (ANDEQ . "&=")
      (ANDAND . "&&")
      (AND . "&")
      (MODEQ . "%=")
      (MOD . "%")
      (NOTEQ . "!=")
      (NOT . "!")
      (GT . ">")
      (LT . "<")
      (AT . "@")
      (COMMA . ",")
      (COLON . ":")
      (SEMICOLON . ";")
      (DOT . "."))
     ("close-paren"
      (RBRACK . "]")
      (RPAREN . ")")
      (RBRACE . "}"))
     ("open-paren"
      (LBRACK . "[")
      (LPAREN . "(")
      (LBRACE . "{"))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (PAREN_BLOCK . "(LPAREN RPAREN)")
      (BRACE_BLOCK . "(LBRACE RBRACE)"))
     ("unicode"
      (unicodecharacter))
     ("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)))
   '(("punctuation" :declared t)
     ("block" :declared t)
     ("unicode" syntax "\\\\u[0-9a-f][0-9a-f][0-9a-f][0-9a-f]")
     ("unicode" :declared t)
     ("number" :declared t)
     ("string" :declared t)
     ("symbol" syntax "\\sw\\(\\sw\\|\\s_\\)*")
     ("symbol" :declared t)
     ("keyword" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-java-malabar-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE TRUE FALSE NULL IDENTIFIER STRING_LITERAL NUMBER_LITERAL unicodecharacter BRACE_BLOCK PAREN_BLOCK BRACK_BLOCK LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK DOT SEMICOLON COLON COMMA AT LT GT NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ MINUS MINUSMINUS MINUSEQ DIV DIVEQ LSHIFT LSHIFTEQ LTEQ EQ EQEQ GTEQ QUESTION XOR XOREQ OR OREQ OROR COMP)
       nil
       (compilationUnit
        ((annotations))
        ((packageDeclaration))
        ((importDeclaration))
        ((typeDeclaration)))
       (packageDeclaration
        ((PACKAGE qualifiedName SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-package $2 nil))))
       (importDeclaration
        ((IMPORT STATIC qualifiedName DOT MULT SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include
           (concat $3 $4 $5)
           nil :modifiers $2)))
        ((IMPORT STATIC qualifiedName SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include $3 nil :modifiers $2)))
        ((IMPORT qualifiedName DOT MULT SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include
           (concat $2 $3 $4)
           nil)))
        ((IMPORT qualifiedName SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include $2 nil))))
       (typeDeclaration
        ((classDeclaration))
        ((enumDeclaration))
        ((annotationTypeDeclaration))
        ((interfaceDeclaration)))
       (annotationTypeDeclaration
        ((annotations_opt modifiers_opt AT INTERFACE IDENTIFIER annotationTypeBody)
         (wisent-raw-tag
          (semantic-tag-new-type $5 'annotation $6 nil :typemodifiers $2 :typeannotations $1))))
       (annotationTypeBody
        ((BRACE_BLOCK)
         nil))
       (interfaceDeclaration
        ((annotations_opt modifiers_opt INTERFACE IDENTIFIER typeParameters_opt extends_if_opt interfaceBody)
         (wisent-raw-tag
          (semantic-tag-new-type $4 $3 $7 $6 :typemodifiers $2 :typeparameters $5 :typeannotations $1))))
       (extends_if_opt
        (nil)
        ((EXTENDS typeList)
         (nreverse $2)))
       (interfaceBody
        ((BRACE_BLOCK)
         nil))
       (enumDeclaration
        ((annotations_opt modifiers_opt ENUM IDENTIFIER implements_opt enumBody)
         (wisent-raw-tag
          (semantic-tag-new-type $4 $3 $6 nil :typemodifiers $2 :typeannotations $1))))
       (enumBody
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'enumMemberDeclarations 1)))
       (enumMemberDeclarations
        (nil)
        ((enumConstants_opt COMMA enumMembers_opt))
        ((enumConstants_opt enumMembers_opt)))
       (enumMembers_opt
        (nil)
        ((classBodyDeclarations)
         (nreverse $1)))
       (enumConstants_opt
        (nil)
        ((enumConstants)
         (nreverse $1)))
       (enumConstants
        ((enumConstants COMMA enumConstant)
         (cons $3 $1))
        ((enumConstant)
         (list $1)))
       (enumConstant
        ((annotations_opt IDENTIFIER argumentList_opt classBody_opt)
         (wisent-raw-tag
          (semantic-tag $2 'enum-constant :annotations $1 :argument-list $3 :body $4))))
       (argumentList_opt
        (nil)
        ((argumentList)))
       (argumentList
        ((PAREN_BLOCK)
         nil))
       (classDeclaration
        ((annotations_opt modifiers_opt CLASS IDENTIFIER typeParameters_opt extends_opt implements_opt classBody)
         (wisent-raw-tag
          (semantic-tag-new-type $4 $3 $8
                                 (if
                                     (or $6 $7)
                                     (cons $6 $7))
                                 :typemodifiers $2 :typeparameters $5 :typeannotations $1))))
       (extends_opt
        (nil)
        ((EXTENDS type)
         (identity $2)))
       (implements_opt
        (nil)
        ((IMPLEMENTS typeList)
         (nreverse $2)))
       (classBody_opt
        (nil)
        ((classBody)))
       (classBody
        ((BRACE_BLOCK)
         (nreverse
          (semantic-parse-region
           (car $region1)
           (cdr $region1)
           'classBodyDeclarations 1))))
       (classBodyDeclarations
        ((classBodyDeclarations classBodyDeclaration)
         (cons $2 $1))
        ((classBodyDeclaration)
         (list $1)))
       (classBodyDeclaration
        ((STATIC block))
        ((block))
        ((memberDeclaration)))
       (block
           ((BRACE_BLOCK)
            nil))
       (memberDeclaration
        ((methodDeclaration))
        ((fieldDeclaration)))
       (methodDeclaration
        ((annotations_opt modifiers_opt typeParameters_opt VOID IDENTIFIER argumentList throws_opt block)
         (wisent-raw-tag
          (semantic-tag-new-function $4 $3 $5 :typemodifiers $2 :annotations $1 :throws $6 :body $7)))
        ((annotations_opt modifiers_opt typeParameters_opt type IDENTIFIER argumentList throws_opt block)
         (wisent-raw-tag
          (semantic-tag-new-function $4 $3 $5 :typemodifiers $2 :annotations $1 :throws $6 :body $7))))
       (throws_opt
        (nil)
        ((THROWS qualifiedNameList)
         (nreverse $2)))
       (fieldDeclaration
        ((annotations_opt modifiers_opt type variableDeclarators SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-variable $4 $3 nil :typemodifiers $2 :annotations $1))))
       (variableDeclarators
        ((variableDeclarators COMMA variableDeclarator)
         (progn
           (setcdr
            (cdr
             (car $1))
            (cdr $region2))
           (cons $3 $1)))
        ((variableDeclarator)
         (list $1)))
       (variableDeclarator
        ((variableDeclaratorName EQ variableInitializer)
         (cons $1 $region))
        ((variableDeclaratorName)
         (cons $1 $region)))
       (variableInitializer
        ((arrayInitializer))
        ((expression)))
       (arrayInitializer
        ((BRACK_BLOCK)
         nil))
       (expression
        ((expression term))
        ((term)))
       (term
        ((literal))
        ((operator))
        ((primitiveType))
        ((IDENTIFIER))
        ((BRACK_BLOCK))
        ((PAREN_BLOCK))
        ((BRACE_BLOCK))
        ((NEW))
        ((CLASS))
        ((THIS))
        ((SUPER)))
       (literal
        ((STRING_LITERAL))
        ((TRUE))
        ((FALSE))
        ((NULL))
        ((NUMBER_LITERAL)))
       (operator
        ((NOT))
        ((PLUS))
        ((PLUSPLUS))
        ((MINUS))
        ((MINUSMINUS))
        ((NOTEQ))
        ((MOD))
        ((MODEQ))
        ((AND))
        ((ANDAND))
        ((ANDEQ))
        ((MULT))
        ((MULTEQ))
        ((PLUSEQ))
        ((MINUSEQ))
        ((DOT))
        ((DIV))
        ((DIVEQ))
        ((COLON))
        ((LT))
        ((LSHIFT))
        ((LSHIFTEQ))
        ((LTEQ))
        ((EQ))
        ((EQEQ))
        ((GT))
        ((GTEQ))
        ((QUESTION))
        ((XOR))
        ((XOREQ))
        ((OR))
        ((OREQ))
        ((OROR))
        ((COMP))
        ((INSTANCEOF)))
       (variableDeclaratorName
        ((IDENTIFIER dims_opt)
         (concat $1 $2)))
       (dims_opt
        (nil)
        ((dims)))
       (dims
        ((dims BRACK_BLOCK)
         (concat $1 "[]"))
        ((BRACK_BLOCK)
         (identity "[]")))
       (typeParameters_opt
        (nil)
        ((typeParameters)))
       (typeParameters
        ((LT typeParameter_list GT)
         (nreverse $2)))
       (typeParameter_list
        ((typeParameter_list COMMA typeParameter)
         (cons $3 $1))
        ((typeParameter)
         (list $1)))
       (typeParameter
        ((IDENTIFIER EXTENDS typeBound)
         (list $1 :extends
               (nreverse $3)))
        ((QUESTION EXTENDS typeBound)
         (list $1 :extends
               (nreverse $3)))
        ((IDENTIFIER SUPER typeBound)
         (list $1 :super
               (nreverse $3)))
        ((QUESTION SUPER typeBound)
         (list $1 :super
               (nreverse $3)))
        ((IDENTIFIER))
        ((QUESTION)))
       (typeBound
        ((typeBound AND type)
         (cons $3 $1))
        ((type)
         (list $1)))
       (typeList
        ((typeList COMMA type)
         (cons $3 $1))
        ((type)
         (list $1)))
       (type
        ((primitiveType dims_opt)
         (concat $1 $2))
        ((referenceType)))
       (referenceType
        ((type DOT IDENTIFIER typeParameters_opt dims_opt)
         (list
          (concat $1 $2 $4)
          :typeparameters $3))
        ((IDENTIFIER typeParameters_opt dims)
         (list
          (concat $1 $3)
          :typeparameters $2))
        ((IDENTIFIER typeParameters_opt)
         (list $1 :typeparameters $2)))
       (primitiveType
        ((BOOLEAN))
        ((CHAR))
        ((BYTE))
        ((SHORT))
        ((INT))
        ((LONG))
        ((FLOAT))
        ((DOUBLE)))
       (modifiers_opt
        (nil)
        ((modifiers)
         (nreverse $1)))
       (modifiers
        ((modifiers modifier)
         (cons $2 $1))
        ((modifier)
         (list $1)))
       (modifier
        ((PUBLIC))
        ((PROTECTED))
        ((PRIVATE))
        ((STATIC))
        ((ABSTRACT))
        ((FINAL))
        ((NATIVE))
        ((SYNCHRONIZED))
        ((TRANSIENT))
        ((VOLATILE))
        ((STRICTFP)))
       (annotations_opt
        (nil)
        ((annotations)
         (nreverse $1)))
       (annotations
        ((annotations annotation)
         (cons $2 $1))
        ((annotation)
         (list $1)))
       (annotation
        ((AT qualifiedName PAREN_BLOCK)
         (list $2))
        ((AT qualifiedName)
         (list $2)))
       (qualifiedNameList
        ((qualifiedNameList COMMA qualifiedName)
         (cons $3 $1))
        ((qualifiedName)
         (list $1)))
       (qualifiedName
        ((qualifiedName DOT IDENTIFIER)
         (concat $1 $2 $3))
        ((IDENTIFIER))))
     '(compilationUnit enumMemberDeclarations classBodyDeclarations classBodyDeclaration memberDeclaration fieldDeclaration methodDeclaration typeDeclaration annotations_opt modifiers_opt typeBound typeParameters typeParameter type)))
  "Parser table.")

(defun wisent-java-malabar-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table wisent-java-malabar-wy--parse-table
        semantic-debug-parser-source "wisent-java-malabar.wy"
        semantic-flex-keywords-obarray wisent-java-malabar-wy--keyword-table
        semantic-lex-types-obarray wisent-java-malabar-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)

(define-lex-keyword-type-analyzer wisent-java-malabar-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")

(define-lex-block-type-analyzer wisent-java-malabar-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("{" LBRACE BRACE_BLOCK)
     ("(" LPAREN PAREN_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    ("}" RBRACE)
    (")" RPAREN)
    ("]" RBRACK))
  )

(define-lex-regex-type-analyzer wisent-java-malabar-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\sw\\(\\sw\\|\\s_\\)*"
  nil
  'IDENTIFIER)

(define-lex-sexp-type-analyzer wisent-java-malabar-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING_LITERAL)

(define-lex-regex-type-analyzer wisent-java-malabar-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-regex-type-analyzer wisent-java-malabar-wy--<unicode>-regexp-analyzer
  "regexp analyzer for <unicode> tokens."
  "\\\\u[0-9a-f][0-9a-f][0-9a-f][0-9a-f]"
  nil
  'unicodecharacter)

(define-lex-string-type-analyzer wisent-java-malabar-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((COMP . "~")
    (OROR . "||")
    (OREQ . "|=")
    (OR . "|")
    (XOREQ . "^=")
    (XOR . "^")
    (QUESTION . "?")
    (GTEQ . ">=")
    (EQEQ . "==")
    (EQ . "=")
    (LTEQ . "<=")
    (LSHIFTEQ . "<<=")
    (LSHIFT . "<<")
    (DIVEQ . "/=")
    (DIV . "/")
    (MINUSEQ . "-=")
    (MINUSMINUS . "--")
    (MINUS . "-")
    (PLUSEQ . "+=")
    (PLUSPLUS . "++")
    (PLUS . "+")
    (MULTEQ . "*=")
    (MULT . "*")
    (ANDEQ . "&=")
    (ANDAND . "&&")
    (AND . "&")
    (MODEQ . "%=")
    (MOD . "%")
    (NOTEQ . "!=")
    (NOT . "!")
    (GT . ">")
    (LT . "<")
    (AT . "@")
    (COMMA . ",")
    (COLON . ":")
    (SEMICOLON . ";")
    (DOT . "."))
  'punctuation)


;;; Epilogue
;;
;; Define the lexer for this grammar
(define-lex wisent-java-malabar-lexer
  "Lexical analyzer that handles Java buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ;;;; Auto-generated analyzers.
  wisent-java-malabar-wy--<number>-regexp-analyzer
  wisent-java-malabar-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-java-malabar-wy--<keyword>-keyword-analyzer
  wisent-java-malabar-wy--<symbol>-regexp-analyzer
  wisent-java-malabar-wy--<punctuation>-string-analyzer
  wisent-java-malabar-wy--<block>-block-analyzer
  ;; In theory, unicode chars should be turned into normal chars
  ;; and then combined into regular ascii keywords and text.  This
  ;; analyzer just keeps these things from making the lexer go boom.
  wisent-java-malabar-wy--<unicode>-regexp-analyzer
  ;;;;
  semantic-lex-default-action)

(provide 'wisent-java-malabar-wy)

;;; wisent-java-malabar-wy.el ends here
