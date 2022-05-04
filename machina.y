%{

  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();

  #define YYDEBUG 1

  int yyerror(char *);

%}

%token ABSTRACT AND ARRAY ASIG BOOLEAN CLASS CONSTRUCTOR CTC_CADENA CTC_CARACTER CTC_FLOAT CTC_INT
%token CASE CHARACTER CONSTANT DEFAULT DESTRUCTOR DISTINTO DOS_PTOS ELSE ENUMERATION ESPECIFIC
%token EXCEPTION EXIT EXP FALSE FINAL FINISH FLECHA FLOAT FOREACH FOR FUNCTION HASHTABLE IDENTIFICADOR
%token IF IN INTEGER IS LOOP MAYOR_IGUAL MENOR_IGUAL MOD NOT NIL OF OR OTHERS OUT PROCEDURE PRIVATE 
%token PROTECTED PUBLIC RAISE RECORD RETURN REVERSE START THEN TRUE TRY TYPE WHEN WHILE



%%
/*
%left OR AND '*' '%' MOD '+' '-' '&' '@'
%right EXP
%nonassoc '-' '=' DISTINTO '<' '>' MENOR_IGUAL' MAYOR_IGUAL NOT
*/
/* DECLARACIONES */

declaracion
      : declaracion_objeto
      | declaracion_tipo
      | declaracion_subprograma
      ;

declaracion_objeto
      : ( IDENTIFICADOR )+ ':' [ CONSTANT ]? tipo_escalar
      [ asignacion_escalar ]? ';'
      | ( IDENTIFICADOR )+ ':' [ CONSTANT ]? tipo_complejo
      [ asignacion_complejo ]? ';'
      ;

tipo_escalar
      : INTEGER
      | FLOAT
      | BOOLEAN
      | CHARACTER
      ;

asignacion_escalar
      : ASIG ( expresion )+
      ;

tipo_complejo
      : nombre_de_tipo
      | tipo_compuesto
      ;

nombre_de_tipo
      : IDENTIFICADOR
      ;

tipo_compuesto
      : tipo_tablero
      | tipo_registro
      | tipo_hashtable
      | tipo_clase
      | tipo_enumeracion
      ;

asignacion_compleja
      : ASIG objeto_complejo
      ;

objeto_complejo
      : '[' ( objeto_complejo )+ ']'
      | '{' ( elemento_hastable )+ '}'
      | '(' ( elemento_registro )+ ')'
      | literal
      ;

elemento_hastable
      : objeto_complejo FLECHA objeto_complejo
      ;

elemento_registro
      : IDENTIFICADOR ASIG objeto_complejo
      ;

/* TIPOS */

declaracion_tipo
      : TYPE IDENTIFICADOR IS especificacion_tipo ';'
      ;

especificacion_tipo
      : tipo_escalar
      | nombre_de_tipo
      | tipo_compuesto
      ;

tipo_tablero
      : ARRAY '(' expresion DOS_PTOS expresion ')' OF especificacion_tipo
      ;

tipo_registro
      : RECORD [ componente ]+ FINISH RECORD
      ;

componente
      : ( IDENTIFICADOR )+ ':' especificacion_tipo ';'
      ;

tipo_hashtable
      : HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>'
      ;

tipo_clase
      : CLASS [ '(' nombre_de_tipo ')' ]? [ componente_clase ]+ FINISH CLASS
      ;

componente_clase
      : [ visibilidad ]? declaracion_componente
      ;

declaracion_componente
      : declaracion_objeto
      | declaracion_tipo
      | declaracion_metodo
      ;

visibilidad
      : PUBLIC
      | PROTECTED
      | PRIVATE
      ;

declaracion_metodo
      : [ modificador ]* declaracion_subprograma
      ;

modificador
      : CONSTRUCTOR
      | DESTRUCTOR
      | ABSTRACT
      | ESPECIFIC
      | FINAL 
      ;

tipo_enumeracion
      : ENUMERATION OF tipo_escalar ( elemento )+ FINISH ENUMERATION
      ;

elemento
      : [ IDENTIFICADOR FLECHA ]? literal
      ;

/* SUBPROGRAMAS */

declaracion_subprograma
      : especificacion_subprograma [ cuerpo_subprograma ]? ';'
      ;

especificacion_subprograma
      : PROCEDURE IDENTIFICADOR [ '(' parte_formal ')' ]?
      | FUNCTION IDENTIFICADOR [ '(' parte_formal ')' ]?
      RETURN especificacion_tipo
      ;

parte_formal
      : [ declaracion_parametros ]?
      ;

declaracion_parametros
      : declaracion_parametro [ ';' declaracion_parametro ]*
      ;

declaracion_parametro
      : ( IDENTIFICADOR )+ ':' [ modo ]? especificacion_tipo
      ;

modo
      : IN [ OUT ]?
      ;

cuerpo_subprograma
      : IS [ declaracion ]* START [ instruccion ]+ FINISH [ IDENTIFICADOR ]?
      ;

/* INSTRUCCIONES */

instruccion
      : instruccion_vacia
      | instruccion_asignacion
      | instruccion_exit
      | instruccion_return
      | instruccion_if
      | instruccion_case
      | instruccion_loop
      | instruccion_rise
      | instruccion_try_catch
      | llamada_procedure
      ;

instruccion_vacia
      : NIL ';'
      ;

instruccion_asignacion
      : nombre ASIG expresion ';'
      ;

instruccion_return
      : RETURN expresion ';'
      ;

instruccion_exit
      : EXIT [ IDENTIFICADOR ]? [ WHEN expresion ]? ';'
      ;

instruccion_if
      : IF expresion THEN [ instruccion ]+
      [ ELSE [ instruccion ]+ ]? FINISH IF ';' 
      ;

instruccion_case
      : CASE expresion IS [ caso_when ]+ FINISH CASE ';'
      ;

caso_when
      : WHEN entrada [ '|' entrada ]* FLECHA [ instruccion ]+
      ;

entrada
      : expresion [ DOS_PTOS expresion ]?
      | OTHERS
      ;

instruccion_loop
      : [ IDENTIFICADOR ':' ]? clausula_iteracion bucle_base ';'
      ;

clausula_iteracion
      : FOR IDENTIFICADOR IN [ REVERSE ]? expresion DOS_PTOS expresion
      | FOREACH IDENTIFICADOR IN expresion
      | WHILE expresion
      ;

bucle_base
      : LOOP [ instruccion ]+ FINISH LOOP
      ;

instruccion_rise
      : RAISE IDENTIFICADOR ';'
      ;

instruccion_try_catch
      : TRY [ instruccion ]+ clausulas_excepcion FINISH TRY
      ;

clausulas_excepcion
      : [ clausula_especifica ]* clausula_defecto
      | [ clausula_especifica ]+
      ;

clausula_especifica
      : EXCEPTION '(' IDENTIFICADOR ')' [ instruccion ]+
      ;

clausula_defecto
      : DEFAULT '(' IDENTIFICADOR ')' [ instruccion ]+
      ;

llamada_procedure
      : llamada_suprograma ';'

llamada_suprograma
      : IDENTIFICADOR '(' ( expresion )* ')'
      ;

/* EXPRESIONES */

primario
      : literal
      | nombre
      | '(' expresion ')'
      ;

literal
      : CTC_CADENA
      | CTC_CARACTER
      | CTC_FLOAT
      | CTC_INT
      | TRUE
      | FALSE
      ;

operador_unario
      : NOT
      | '-'
      ;

nombre
      : componente_indexado
      | componente_hash
      | componente_compuesto
      | llamada_suprograma
      | IDENTIFICADOR
      ;

componente_indexado
      : nombre '[' expresion ']'
      ;

componente_hash
      : nombre '{' expresion '}'
      ;

componente_compuesto
      : nombre '.' IDENTIFICADOR
      | nombre '.' llamada_suprograma
      ;

expresion
      : expresion_logica [ IF expresion ELSE expresion ]?
      ;

%%


int yyerror(char *s) {
  fflush(stdout);
  printf("   *****************, %s\n",s);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./machina NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
