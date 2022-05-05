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

declaracion
      : declaracion_objeto                {printf("declaracion->declaracion_objeto\n");}
      | declaracion_tipo                  {printf("declaracion->declaracion_tipo\n");}
      | declaracion_subprograma           {printf("declaracion->declaracion_subprograma\n");}
      ;

declaracion_objeto
      : lista_identificadores ':' tipo_escalar ';'                                  {printf("declaracion_objeto->lista_identificadores ':' tipo_escalar ';'\n");}
      | lista_identificadores ':' CONSTANT tipo_escalar ';'                         {printf("declaracion_objeto->lista_identificadores ':' CONSTANT tipo_escalar ';'\n");}
      | lista_identificadores ':' tipo_escalar asignacion_escalar ';'               {printf("declaracion_objeto->lista_identificadores ':' tipo_escalar asignacion_escalar ';'\n");}
      | lista_identificadores ':' CONSTANT tipo_escalar asignacion_escalar ';'      {printf("declaracion_objeto->lista_identificadores ':' CONSTANT tipo_escalar asignacion_escalar ';'\n");}
      | lista_identificadores ':' tipo_complejo ';'                                 {printf("declaracion_objeto->lista_identificadores ':' tipo_complejo ';'\n");}
      | lista_identificadores ':' CONSTANT tipo_complejo ';'                        {printf("declaracion_objeto->lista_identificadores ':' CONSTANT tipo_complejo ';'\n");}
      | lista_identificadores ':' tipo_complejo asignacion_complejo ';'             {printf("declaracion_objeto->lista_identificadores ':' tipo_complejo asignacion_complejo ';'\n");}
      | lista_identificadores ':' CONSTANT tipo_complejo asignacion_complejo ';'    {printf("declaracion_objeto->lista_identificadores ':' CONSTANT tipo_complejo asignacion_complejo ';'\n");}
      ;

lista_identificadores
      : IDENTIFICADOR                                 {printf("lista_identificadores->IDENTIFICADOR\n");}
      | lista_identificadores ',' IDENTIFICADOR       {printf("lista_identificadores->lista_identificadores ',' IDENTIFICADOR\n");}
      ;

tipo_escalar
      : INTEGER         {printf("tipo_escalar->INTEGER\n");}
      | FLOAT           {printf("tipo_escalar->FLOAT\n");}
      | BOOLEAN         {printf("tipo_escalar->BOOLEAN\n");}
      | CHARACTER       {printf("tipo_escalar->CHARACTER\n");}
      ;

asignacion_escalar
      : ASIG lista_expresion        {printf("asignacion_escalar->ASIG lista_expresion \n");}
      ;

lista_expresion
      : expresion                         {printf("lista_expresion->expresion\n");}
      | lista_expresion ',' expresion     {printf("lista_expresion->lista_expresion ',' expresion\n");}
      ;

tipo_complejo
      : nombre_de_tipo        {printf("tipo_complejo->nombre_de_tipo\n");}
      | tipo_compuesto        {printf("tipo_complejo->tipo_compuesto\n");}
      ;

nombre_de_tipo
      : IDENTIFICADOR         {printf("nombre_de_tipo->IDENTIFICADOR\n");}
      ;

tipo_compuesto
      : tipo_tablero          {printf("tipo_compuesto->tipo_tablero\n");}
      | tipo_registro         {printf("tipo_compuesto->tipo_registro\n");}
      | tipo_hashtable        {printf("tipo_compuesto->tipo_hashtable\n");}
      | tipo_clase            {printf("tipo_compuesto->tipo_clase\n");}
      | tipo_enumeracion      {printf("tipo_compuesto->tipo_enumeracion\n");}
      ;

asignacion_complejo
      : ASIG objeto_complejo  {printf("asignacion_complejo->ASIG objeto_complejo\n");}
      ;

objeto_complejo
      : '[' lista_objeto_complejo ']'     {printf("objeto_complejo->'[' lista_objeto_complejo ']'\n");}
      | '{' lista_elemento_hastable '}'   {printf("objeto_complejo->'{' lista_elemento_hastable '}'\n");}
      | '(' lista_elemento_registro ')'   {printf("objeto_complejo->'(' lista_elemento_registro ')'\n");}
      | literal                           {printf("objeto_complejo->literal\n");}
      ;

elemento_hastable
      : objeto_complejo FLECHA objeto_complejo  {printf("elemento_hastable->objeto_complejo FLECHA objeto_complejo\n");}
      ;

elemento_registro
      : IDENTIFICADOR ASIG objeto_complejo      {printf("elemento_hastable->IDENTIFICADOR ASIG objeto_complejo\n");}
      ;

lista_objeto_complejo
      : objeto_complejo                               {printf("lista_objeto_complejo->objeto_complejo\n");}
      | lista_objeto_complejo ',' objeto_complejo     {printf("lista_objeto_complejo->lista_objeto_complejo ',' objeto_complejo\n");}
      ;

lista_elemento_hastable
      : elemento_hastable                                   {printf("lista_elemento_hastable->elemento_hastable\n");}
      | lista_elemento_hastable ',' elemento_hastable       {printf("lista_elemento_hastable->lista_elemento_hastable ',' elemento_hastable\n");}
      ;

lista_elemento_registro
      : elemento_registro
      | lista_elemento_registro ',' elemento_registro
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
      : RECORD lista_componente FINISH RECORD
      ;

lista_componente
      : componente
      | lista_componente componente
      ;

componente
      : lista_identificadores ':' especificacion_tipo ';'
      ;

tipo_hashtable
      : HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>'
      ;

tipo_clase
      : CLASS lista_componente_clase FINISH CLASS
      | CLASS '(' nombre_de_tipo ')' lista_componente_clase FINISH CLASS
      ;

lista_componente_clase
      : componente_clase
      | lista_componente_clase componente_clase
      ;

componente_clase
      : declaracion_componente
      | visibilidad declaracion_componente
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
      : declaracion_subprograma
      | lista_modificador declaracion_subprograma
      ;

modificador
      : CONSTRUCTOR
      | DESTRUCTOR
      | ABSTRACT
      | ESPECIFIC
      | FINAL 
      ;

lista_modificador
      : modificador
      | lista_modificador modificador
      ;

tipo_enumeracion
      : ENUMERATION OF tipo_escalar lista_elemento FINISH ENUMERATION
      ;

elemento
      : literal
      | IDENTIFICADOR FLECHA literal
      ;

lista_elemento
      : elemento
      | lista_elemento ',' elemento
      ;

/* SUBPROGRAMAS */

declaracion_subprograma
      : especificacion_subprograma ';'
      | especificacion_subprograma cuerpo_subprograma ';'
      ;

especificacion_subprograma
      : PROCEDURE IDENTIFICADOR
      | PROCEDURE IDENTIFICADOR '(' parte_formal ')'
      | FUNCTION IDENTIFICADOR
      | FUNCTION IDENTIFICADOR '(' parte_formal ')'
      RETURN especificacion_tipo
      ;

parte_formal
      : /*vacio*/
      | declaracion_parametros
      ;

declaracion_parametros
      : declaracion_parametro
      | declaracion_parametro lista_declaracion_parametro
      ;

lista_declaracion_parametro
      : declaracion_parametro
      | declaracion_parametro ';' lista_declaracion_parametro
      ;

declaracion_parametro
      : lista_identificadores ':' especificacion_tipo
      | lista_identificadores ':' modo especificacion_tipo
      ;

modo
      : IN
      | IN OUT
      ;

cuerpo_subprograma
      : IS START lista_instruccion FINISH
      | IS START lista_instruccion FINISH IDENTIFICADOR
      | IS lista_declaracion START lista_instruccion FINISH
      | IS lista_declaracion START lista_instruccion FINISH IDENTIFICADOR
      ;

lista_instruccion
      : instruccion
      | lista_instruccion instruccion
      ;

lista_declaracion
      : declaracion
      | lista_declaracion declaracion
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
      : EXIT ';'
      | EXIT IDENTIFICADOR ';'
      | EXIT WHEN expresion ';'
      | EXIT IDENTIFICADOR WHEN expresion ';'
      ;

instruccion_if
      : IF expresion THEN lista_instruccion FINISH IF ';'
      | IF expresion THEN lista_instruccion ELSE lista_instruccion FINISH IF ';'
      ;

instruccion_case
      : CASE expresion IS lista_caso_when FINISH CASE ';'
      ;

caso_when
      : WHEN entrada FLECHA lista_instruccion
      | WHEN entrada lista_entrada FLECHA lista_instruccion
      ;

lista_caso_when
      : caso_when
      | lista_caso_when caso_when
      ;

entrada
      : expresion
      | expresion DOS_PTOS expresion
      | OTHERS
      ;

lista_entrada
      : entrada
      | entrada '|' lista_entrada
      ;

instruccion_loop
      : clausula_iteracion bucle_base ';'
      | IDENTIFICADOR ':' clausula_iteracion bucle_base ';'
      ;

clausula_iteracion
      : FOR IDENTIFICADOR IN expresion DOS_PTOS expresion
      | FOR IDENTIFICADOR IN REVERSE expresion DOS_PTOS expresion
      | FOREACH IDENTIFICADOR IN expresion
      | WHILE expresion
      ;

bucle_base
      : LOOP lista_instruccion FINISH LOOP
      ;

instruccion_rise
      : RAISE IDENTIFICADOR ';'
      ;

instruccion_try_catch
      : TRY lista_instruccion clausulas_excepcion FINISH TRY
      ;

clausulas_excepcion
      : clausula_defecto
      | lista_clausula_especifica clausula_defecto
      | lista_clausula_especifica
      ;

clausula_especifica
      : EXCEPTION '(' IDENTIFICADOR ')' lista_instruccion
      ;

lista_clausula_especifica
      : clausula_especifica
      | lista_clausula_especifica clausula_especifica
      ;

clausula_defecto
      : DEFAULT '(' IDENTIFICADOR ')' lista_instruccion
      ;

llamada_procedure
      : llamada_suprograma ';'

llamada_suprograma
      : IDENTIFICADOR '(' lista_expresion ')'
      ;

/* EXPRESIONES */

primario
      : literal               {printf("primario->literal\n");}
      | nombre                {printf("primario->nombre\n");}
      | '(' expresion ')'     {printf("primario->(expresion)\n");}
      ;

literal
      : CTC_CADENA            {printf("literal->CTC_CADENA\n");}
      | CTC_CARACTER          {printf("literal->CTC_CARACTER\n");}
      | CTC_FLOAT             {printf("literal->CTC_FLOAT\n");}
      | CTC_INT               {printf("literal->CTC_INT\n");}
      | TRUE                  {printf("literal->TRUE\n");}
      | FALSE                 {printf("literal->FALSE\n");}
      ;

nombre
      : componente_indexado   {printf("nombre->componente_indexado\n");}
      | componente_hash       {printf("nombre->componente_hash\n");}
      | componente_compuesto  {printf("nombre->componente_compuesto\n");}
      | llamada_suprograma    {printf("nombre->llamada_suprograma\n");}
      | IDENTIFICADOR         {printf("nombre->IDENTIFICADOR\n");}
      ;

componente_indexado
      : nombre '[' expresion ']'    {printf("componente_indexado->nombre[expresion]\n");}
      ;

componente_hash
      : nombre '{' expresion '}'    {printf("componente_hash->nombre{expresion}\n");}
      ;

componente_compuesto
      : nombre '.' IDENTIFICADOR          {printf("componente_compuesto->nombre . IDENTIFICADOR\n");}
      | nombre '.' llamada_suprograma     {printf("componente_compuesto->nombre . llamada_suprograma\n");}
      ;

expresion_logica 
    : expresion_logica OR expresion_logica1     {printf("expresion_logica->expresion_logica OR expresion_logica1\n");}
    | expresion_logica1                         {printf("expresion_logica->expresion_logica1\n");}
    ;

expresion_logica1
    : expresion_logica1 AND expresion_logica2   {printf("expresion_logica1->expresion_logica1 AND expresion_logica2\n");}
    | expresion_logica2                         {printf("expresion_logica1->expresion_logica2\n");}
    ;

expresion_logica2
    : NOT expresion_logica3                     {printf("expresion_logica2->NOT expresion_logica3\n");}
    | expresion_logica3                         {printf("expresion_logica2->expresion_logica3\n");}
    ;

expresion_logica3
    : expresion_logica3 '=' expresion_logica4         {printf("expresion_logica3->expresion_logica3 = expresion_logica4\n");}
    | expresion_logica3 DISTINTO expresion_logica4    {printf("expresion_logica3->expresion_logica3 DISTINTO expresion_logica4\n");}
    | expresion_logica3 '<' expresion_logica4         {printf("expresion_logica3->expresion_logica3 < expresion_logica4\n");}
    | expresion_logica3 '>' expresion_logica4         {printf("expresion_logica3->expresion_logica3 > expresion_logica4\n");}
    | expresion_logica3 MENOR_IGUAL expresion_logica4 {printf("expresion_logica3->expresion_logica3 MENOR_IGUAL expresion_logica4\n");}
    | expresion_logica3 MAYOR_IGUAL expresion_logica4 {printf("expresion_logica3->expresion_logica3 MAYOR_IGUAL expresion_logica4\n");}
    | expresion_logica4                               {printf("expresion_logica3->expresion_logica4\n");}
    ;

expresion_logica4
    : expresion_logica4 '@' expresion_logica5         {printf("expresion_logica4->expresion_logica4 @ expresion_logica5\n");}
    | expresion_logica5                               {printf("expresion_logica4->expresion_logica5\n");}
    ;

expresion_logica5
    : expresion_logica5 '&' expresion_logica6         {printf("expresion_logica5->expresion_logica5 & expresion_logica6\n");}
    | expresion_logica6                               {printf("expresion_logica5->expresion_logica6\n");}
    ;

expresion_logica6
    : expresion_logica6 '+' expresion_logica7         {printf("expresion_logica6->expresion_logica6 '+' expresion_logica7\n");}
    //| expresion_logica6 '-' expresion_logica7         {printf("expresion_logica6->expresion_logica6 '-' expresion_logica7\n");}
    //| expresion_logica6 '-' expresion_logica9 //sobrecargar para resta y menos unitario
    | expresion_logica7                               {printf("expresion_logica6->expresion_logica7\n");}
    ;

expresion_logica7
    : expresion_logica7 '*' expresion_logica8         {printf("expresion_logica7->expresion_logica7 * expresion_logica8\n");}
    | expresion_logica7 '%' expresion_logica8         //{printf("expresion_logica7->expresion_logica7 '%' expresion_logica8\n");}
    | expresion_logica7 MOD expresion_logica8         {printf("expresion_logica7->expresion_logica7 MOD expresion_logica8\n");}
    | expresion_logica8                               {printf("expresion_logica7->expresion_logica8\n");}
    ;

expresion_logica8
    : expresion_logica9 EXP expresion_logica8         {printf("expresion_logica8->expresion_logica9 EXP expresion_logica8\n");}
    | expresion_logica9                               {printf("expresion_logica8->expresion_logica9\n");}
    ;

expresion_logica9
    : '-' primario                                    {printf("expresion_logica9-> - primario\n");}
    | primario                                        {printf("expresion_logica9-> primario\n");}
    ;

expresion
      : expresion_logica
      | expresion_logica IF expresion ELSE expresion
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
