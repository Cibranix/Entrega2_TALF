%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "machina.tab.h"
int linea=1;
int i = 0;
char cadena[256] = "";
%}

%x COMENTARIO
%x CADENA

IDENTIFICADOR [A-Za-z]([A-Za-z]|_[0-9]*)*
DECIMAL ([0-9]+)
OCTAL ((\\O|\\o)[0-7]+)
HEXADECIMAL (("\\H"|"\\h")[0-9A-Fa-f]+)
REALES (({DECIMAL}|{OCTAL}|{HEXADECIMAL})\.({DECIMAL}|{OCTAL}|{HEXADECIMAL}))
EXPONENTES (({DECIMAL}|{OCTAL}|{HEXADECIMAL}|{REALES})^[+-]?({DECIMAL}|{OCTAL}|{HEXADECIMAL}))
DELIMITADORES ("{"|"}"|"("|")"|"["|"]"|"|"|":"|";"|",")
CTE_ESCAPADOS (\\'|\\\"|\\\\|\\a|\\b|\\f|\\n|\\r|\\t|\\v)
CARACTER_DECIMAL \\(([01]?[0-9]{1,2})|(2[0-4][0-9])|(25[0-5]))
CTE_CADENA ([^\\\n\"]*|{CTE_ESCAPADOS}|{CARACTER_DECIMAL}|(\\[Oo])[0-3]?[0-7]{1,2}|(\\[Hh])[0-9a-fA-f]{1,2})
CTE_CARACTER '([^\\'\n]|{CTE_ESCAPADOS}|{CARACTER_DECIMAL}|(\\[Oo])[0-3]?[0-7]{1,2}|(\\[Hh])[0-9a-fA-f]{1,2})'
ARITMETICOS ("+"|"-"|"*"|"%")
BITS (\@|\&)
RELACIONALES ([<>]|\=)
COMENTARIOS_UNA_LINEA ("--".*\n)
COMENTARIOS_MULTILINEA (\/\/)

%%

" "   ;
\t    ;
\n linea++;

(?i:abstract)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(ABSTRACT);
(?i:array)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(ARRAY);
(?i:boolean)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(BOOLEAN);
(?i:case)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(CASE);
(?i:character)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(CHARACTER);
(?i:class)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(CLASS);
(?i:constant)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(CONSTANT);
(?i:constructor)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(CONSTRUCTOR);
(?i:default)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(DEFAULT);
(?i:destructor)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(DESTRUCTOR);
(?i:else)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(ELSE);
(?i:enumeration)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(ENUMERATION);
(?i:especific)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(ESPECIFIC);
(?i:exception)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(EXCEPTION);
(?i:exit)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(EXIT);
(?i:false)  printf("Linea %d - Palabra Reservada/Constante Booleana: %s\n", linea, yytext);return(FALSE);
(?i:final)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FINAL);
(?i:finish)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FINISH);
(?i:float)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FLOAT);
(?i:for)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FOR);
(?i:foreach)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FOREACH);
(?i:function)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(FUNCTION);
(?i:hashtable)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(HASHTABLE);
(?i:if)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(IF);
(?i:in)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(IN);
(?i:integer)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(INTEGER);
(?i:is)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(IS);
(?i:loop)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(LOOP);
(?i:nil)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(NIL);
(?i:of)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(OF);
(?i:others)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(OTHERS);
(?i:out)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(OUT);
(?i:private)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(PRIVATE);
(?i:procedure)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(PROCEDURE);
(?i:protected)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(PROTECTED);
(?i:public)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(PUBLIC);
(?i:raise)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(RAISE);
(?i:record)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(RECORD);
(?i:return)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(RETURN);
(?i:reverse)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(REVERSE);
(?i:then)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(THEN);
(?i:true)  printf("Linea %d - Palabra Reservada/Constante Booleana: %s\n", linea, yytext);return(TRUE);
(?i:try)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(TRY);
(?i:type)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(TYPE);
(?i:start)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(START);
(?i:when)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(WHEN);
(?i:while)  printf("Linea %d - Palabra Reservada: %s\n", linea, yytext);return(WHILE);
(?i:not) printf("Linea %d - Operador logico: %s\n",linea,yytext);return(NOT);
(?i:and) printf("Linea %d - Operador logico: %s\n",linea,yytext);return(AND);
(?i:or) printf("Linea %d - Operador logico: %s\n",linea,yytext);return(OR);
(?i:mod) printf("Linea %d - Operador logico: %s\n",linea,yytext);return(MOD);

{DELIMITADORES} printf("Linea %d - Delimitador: %s\n",linea,yytext);return(yytext[0]);
{COMENTARIOS_UNA_LINEA} linea++;
{CTE_CARACTER} printf("Linea %d - Constante Caracter: %s\n", linea, yytext);return(CTC_CARACTER);
{IDENTIFICADOR} printf("Linea %d - Identificador: %s\n", linea, yytext);return(IDENTIFICADOR);
{DECIMAL} printf("Linea %d - Constante Entera Decimal: %s\n", linea, yytext);return(CTC_INT);
{OCTAL} printf("Linea %d - Constante Entera Octal: %s\n", linea, yytext);return(CTC_INT);
{HEXADECIMAL} printf("Linea %d - Constante Entera Hexadecimal: %s\n", linea, yytext);return(CTC_INT);
{REALES} printf("Linea %d - Constante Real: %s\n", linea, yytext);return(CTC_FLOAT);
{EXPONENTES} printf("Linea %d - Constante Real Exponente: %s\n", linea, yytext);return(CTC_FLOAT);
{BITS} printf("Linea %d - Operador de bits: %s\n",linea,yytext);return(yytext[0]);
{ARITMETICOS} printf("Linea %d - Operador aritmetico: %s\n",linea,yytext);return(yytext[0]);
{RELACIONALES} printf("Linea %d - Operador relacional: %s\n",linea,yytext);return(yytext[0]);

(\.) printf("Linea %d - Operador de acceso a memoria: %s\n",linea,yytext);return(yytext[0]);
(\-\>) printf("Linea %d - Delimitador: %s\n",linea,yytext);return(FLECHA);
(\.\.) printf("Linea %d - Delimitador: %s\n",linea,yytext);return(DOS_PTOS);
(:=) printf("Linea %d - Operador asignacion: %s\n",linea,yytext);return(ASIG);
(\*\*) printf("Linea %d - Operador aritmetico: %s\n",linea,yytext);return(EXP);
(\/=) printf("Linea %d - Operador relacional: %s\n",linea,yytext);return(DISTINTO);
(\>=) printf("Linea %d - Operador relacional: %s\n",linea,yytext);return(MAYOR_IGUAL);
(\<=) printf("Linea %d - Operador relacional: %s\n",linea,yytext);return(MENOR_IGUAL);

{COMENTARIOS_MULTILINEA}	BEGIN(COMENTARIO);
<COMENTARIO>\n			linea++;
<COMENTARIO>.			;
<COMENTARIO>\\\\		BEGIN 0;

\"		BEGIN(CADENA);strcat(cadena, yytext);
<CADENA>\\\n	i++;linea++;strcat(cadena, yytext);
<CADENA>\"	BEGIN 0;strcat(cadena, yytext); printf("Linea %d - Constante Cadena: %s\n", linea-i, cadena);i=0;strcpy(cadena,"");
<CADENA>{CTE_CADENA}		strcat(cadena,yytext);return(CTC_CADENA);

. printf ("Linea %d - Error: %s\n",linea,yytext);

%%
