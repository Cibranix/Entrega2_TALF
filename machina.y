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

/* DECLARACIONES */

declaracion ::= declaracion_objeto
| declaracion_tipo
| declaracion_subprograma

declaracion_objeto ::= ( IDENTIFICADOR )+ ’:’ [ ’constant’ ]? tipo_escalar
[ asignacion_escalar ]? ’;’
| ( IDENTIFICADOR )+ ’:’ [ ’constant’ ]? tipo_complejo
[ asignacion_complejo ]? ’;’
tipo_escalar ::= ’integer’ | ’float’ | ’boolean’ | ’character’
asignacion_escalar ::= ’:=’ ( expresion )+
tipo_complejo ::= nombre_de_tipo | tipo_compuesto
nombre_de_tipo ::= IDENTIFICADOR
tipo_compuesto ::= tipo_tablero | tipo_registro | tipo_hashtable | tipo_clase | tipo_enumeracion
asignacion_compleja ::= ’:=’ objeto_complejo
objeto_complejo ::= ’[’ ( objeto_complejo )+ ’]’
| ’{’ ( elemento_hastable )+ ’}’
| ’(’ ( elemento_registro )+ ’)’
| literal
elemento_hastable ::= objeto_complejo ’->’ objeto_complejo
elemento_registro ::= IDENTIFICADOR ’:=’ objeto_complejo

/* TIPOS */

declaracion_tipo ::= ’type’ IDENTIFICADOR ’is’ especificacion_tipo ’;’

especificacion_tipo ::= tipo_escalar | nombre_de_tipo | tipo_compuesto

tipo_tablero ::= ’array’ ’(’ expresion ’..’ expresion ’)’ ’of’ especificacion_tipo

tipo_registro ::= ’record’ [ componente ]+ ’finish’ ’record’

componente ::= ( IDENTIFICADOR )+ ’:’ especificacion_tipo ’;’

tipo_hashtable ::= ’hastable’ ’of’ ’<’ especificacion_tipo ’,’ especificacion_tipo ’>’

tipo_clase ::= ’class’ [ ’(’ nombre_de_tipo ’)’ ]? [ componente_clase ]+ ’finish’ ’class’

componente_clase ::= [ visibilidad ]? declaracion_componente

declaracion_componente ::= declaracion_objeto | declaracion_tipo | declaracion_metodo

visibilidad ::= ’public’ | ’protected’ | ’private’

declaracion_metodo ::= [ modificador ]* declaracion_subprograma

modificador ::= ’constructor’ | ’destructor’ | ’abstract’ | ’especific’ | ’final’

tipo_enumeracion ::= ’enumeration’ ’of’ tipo_escalar ( elemento )+ ’finish’ ’enumeration’

elemento ::= [ IDENTIFICADOR ’->’ ]? literal

/* SUBPROGRAMAS */

declaracion_subprograma ::= especificacion_subprograma [ cuerpo_subprograma ]? ’;’

especificacion_subprograma ::= ’procedure’ IDENTIFICADOR [ ’(’ parte_formal ’)’ ]?
                            | ’function’ IDENTIFICADOR [ ’(’ parte_formal ’)’ ]?
                            ’return’ especificacion_tipo

parte_formal ::= [ declaracion_parametros ]?

declaracion_parametros ::= declaracion_parametro [ ’;’ declaracion_parametro ]*

declaracion_parametro ::= ( IDENTIFICADOR )+ ’:’ [ modo ]? especificacion_tipo

modo ::= IN [ OUT ]?

cuerpo_subprograma ::= ’is’ [ declaracion ]* ’start’ [ instruccion ]+ ’finish’ [ IDENTIFICADOR ]?

/* INSTRUCCIONES */

instruccion ::= instruccion_vacia
              | instruccion_asignacion
              | instruccion_exit
              | instruccion_return
              | instruccion_if
              | instruccion_case
              | instruccion_loop
              | instruccion_rise
              | instruccion_try_catch
              | llamada_procedure

instruccion_vacia: ’nil’ ’;’

instruccion_asignacion: nombre ’:=’ expresion ’;’

instruccion_return: ’return’ expresion ’;’

instruccion_exit ::= ’exit’ [ IDENTIFICADOR ]? [ ’when’ expresion ]? ’;’

instruccion_if ::= ’if’ expresion ’then’ [ instruccion ]+
          [ ’else’ [ instruccion ]+ ]? ’finish’ ’if’ ’;’

instruccion_case: ’case’ expresion ’is’ [ caso_when ]+ ’finish’ ’case’ ’;’

caso_when ::= ’when’ entrada [ ’|’ entrada ]* ’->’ [ instruccion ]+

entrada ::= expresion [ ’..’ expresion ]?
| OTHERS

instruccion_loop ::= [ IDENTIFICADOR ’:’ ]? clausula_iteracion bucle_base ’;’

clausula_iteracion ::= ’for’ IDENTIFICADOR ’in’ [ ’reverse’ ]? expresion ’..’ expresion
| ’foreach’ IDENTIFICADOR ’in’ expresion
| ’while’ expresion

bucle_base ::= ’loop’ [ instruccion ]+ ’finish’ ’loop’

instruccion_rise ::= ’raise’ IDENTIFICADOR ’;’

instruccion_try_catch ::= ’try’ [ instruccion ]+ clausulas_excepcion ’finish’ ’try’

clausulas_excepcion ::= [ clausula_especifica ]* clausula_defecto
| [ clausula_especifica ]+

clausula_especifica ::= ’exception’ ’(’ IDENTIFICADOR ’)’ [ instruccion ]+

clausula_defecto ::= ’default’ ’(’ IDENTIFICADOR ’)’ [ instruccion ]+

llamada_procedure ::= llamada_suprograma ’;

llamada_suprograma ::= IDENTIFICADOR ’(’ ( expresion )* ’)’

/* EXPRESIONES */

primario ::= literal | nombre | ’(’ expresion ’)’

literal ::= CTC_CADENA | CTC_CARACTER | CTC_FLOAT | CTC_INT | ’true’ | ’false’

nombre ::= componente_indexado
        | componente_hash
        | componente_compuesto
        | llamada_suprograma
        | IDENTIFICADOR

componente_indexado ::= nombre ’[’ expresion ’]’

componente_hash ::= nombre ’{’ expresion ’}’

componente_compuesto ::= nombre ’.’ IDENTIFICADOR
                      | nombre ’.’ llamada_suprograma

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
