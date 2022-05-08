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

%right EXP
%left OR AND '@' '&' '+' '-' '*' '%' MOD
%nonassoc NOT '=' DISTINTO '<' '>' MENOR_IGUAL MAYOR_IGUAL

%%

/* DECLARACIONES */

declaracion
      : declaracion_objeto                {printf("declaracion->declaracion_objeto\n");}
      | declaracion_tipo                  {printf("declaracion->declaracion_tipo\n");}
      | declaracion_subprograma           {printf("declaracion->declaracion_subprograma\n");}
      //| error ';'
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
      //| '[' error ']'
      | '{' lista_elemento_hastable '}'   {printf("objeto_complejo->'{' lista_elemento_hastable '}'\n");}
      //| '{' error '}'
      | '(' lista_elemento_registro ')'   {printf("objeto_complejo->'(' lista_elemento_registro ')'\n");}
      //| '(' error ')'
      | literal                           {printf("objeto_complejo->literal\n");}
      ;

elemento_hastable
      : objeto_complejo FLECHA objeto_complejo  {printf("elemento_hastable->objeto_complejo FLECHA objeto_complejo\n");}
      ;

elemento_registro
      : IDENTIFICADOR ASIG objeto_complejo      {printf("elemento_registro->IDENTIFICADOR ASIG objeto_complejo\n");}
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
      : elemento_registro                                   {printf("lista_elemento_registro->elemento_registro\n");}
      | lista_elemento_registro ',' elemento_registro       {printf("lista_elemento_registro->lista_elemento_registro ',' elemento_registro\n");}
      ;

/* TIPOS */

declaracion_tipo
      : TYPE IDENTIFICADOR IS especificacion_tipo ';'       {printf("declaracion_tipo->TYPE IDENTIFICADOR IS especificacion_tipo ';'\n");}
      ;

especificacion_tipo
      : tipo_escalar                      {printf("especificacion_tipo->tipo_escalar\n");}
      | nombre_de_tipo                    {printf("especificacion_tipo->nombre_de_tipo\n");}
      | tipo_compuesto                    {printf("especificacion_tipo->tipo_compuesto\n");}
      ;

tipo_tablero
      : ARRAY '(' expresion DOS_PTOS expresion ')' OF especificacion_tipo     {printf("tipo_tablero->ARRAY '(' expresion DOS_PTOS expresion ')' OF especificacion_tipo\n");}
      ;

tipo_registro
      : RECORD lista_componente FINISH RECORD   {printf("tipo_registro->RECORD lista_componente FINISH RECORD\n");}
      ;

lista_componente
      : componente                        {printf("lista_componente->componente\n");}
      | lista_componente componente       {printf("lista_componente->lista_componente componente\n");}
      ;

componente
      : lista_identificadores ':' especificacion_tipo ';'   {printf("componente->lista_identificadores ':' especificacion_tipo ';'\n");}
      ;

tipo_hashtable
      : HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>'      {printf("tipo_hashtable->HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>'\n");}
      ;

tipo_clase
      : CLASS lista_componente_clase FINISH CLASS                             {printf("tipo_clase->CLASS lista_componente_clase FINISH CLASS\n");}
      | CLASS '(' nombre_de_tipo ')' lista_componente_clase FINISH CLASS      {printf("tipo_clase->CLASS '(' nombre_de_tipo ')' lista_componente_clase FINISH CLASS\n");}
      ;

lista_componente_clase
      : componente_clase                              {printf("lista_componente_clase->componente_clase\n");}
      | lista_componente_clase componente_clase       {printf("lista_componente_clase->lista_componente_clase componente_clase\n");}
      ;

componente_clase
      : declaracion_componente                        {printf("componente_clase->declaracion_componente\n");}
      | visibilidad declaracion_componente            {printf("componente_clase->visibilidad declaracion_componente\n");}
      ;

declaracion_componente
      : declaracion_objeto          {printf("declaracion_componente->declaracion_objeto\n");}
      | declaracion_tipo            {printf("declaracion_componente->declaracion_tipo\n");}
      | declaracion_metodo          {printf("declaracion_componente->declaracion_metodo\n");}
      ;

visibilidad
      : PUBLIC          {printf("visibilidad->PUBLIC\n");}
      | PROTECTED       {printf("visibilidad->PROTECTED\n");}
      | PRIVATE         {printf("visibilidad->PRIVATE\n");}
      ;

declaracion_metodo
      : declaracion_subprograma                       {printf("declaracion_metodo->declaracion_subprograma\n");}
      | lista_modificador declaracion_subprograma     {printf("declaracion_metodo->lista_modificador declaracion_subprograma\n");}
      ;

modificador
      : CONSTRUCTOR     {printf("modificador->CONSTRUCTOR\n");}
      | DESTRUCTOR      {printf("modificador->DESTRUCTOR\n");}
      | ABSTRACT        {printf("modificador->ABSTRACT\n");}
      | ESPECIFIC       {printf("modificador->ESPECIFIC\n");}
      | FINAL           {printf("modificador->FINAL\n");}
      ;

lista_modificador
      : modificador                       {printf("lista_modificador->modificador\n");}
      | lista_modificador modificador     {printf("lista_modificador->lista_modificador modificador\n");}
      ;

tipo_enumeracion
      : ENUMERATION OF tipo_escalar lista_elemento FINISH ENUMERATION   {printf("tipo_enumeracion->ENUMERATION OF tipo_escalar lista_elemento FINISH ENUMERATION\n");}
      ;

elemento
      : literal                           {printf("elemento->literal\n");}
      | IDENTIFICADOR FLECHA literal      {printf("elemento->IDENTIFICADOR FLECHA literal\n");}
      ;

lista_elemento
      : elemento                          {printf("lista_elemento->elemento\n");}
      | lista_elemento ',' elemento       {printf("lista_elemento->lista_elemento ',' elemento\n");}
      ;

/* SUBPROGRAMAS */

declaracion_subprograma
      : especificacion_subprograma ';'                      {printf("declaracion_subprograma->especificacion_subprograma ';'\n");}
      | especificacion_subprograma cuerpo_subprograma ';'   {printf("declaracion_subprograma->especificacion_subprograma cuerpo_subprograma ';'\n");}
      ;

especificacion_subprograma
      : PROCEDURE IDENTIFICADOR                             {printf("especificacion_subprograma->PROCEDURE IDENTIFICADOR\n");}
      | PROCEDURE IDENTIFICADOR '(' parte_formal ')'        {printf("especificacion_subprograma->PROCEDURE IDENTIFICADOR '(' parte_formal ')'\n");}
      | FUNCTION IDENTIFICADOR                              {printf("especificacion_subprograma->FUNCTION IDENTIFICADOR\n");}
      | FUNCTION IDENTIFICADOR '(' parte_formal ')'         {printf("especificacion_subprograma->FUNCTION IDENTIFICADOR '(' parte_formal ')'\n");}
      RETURN especificacion_tipo                            {printf("especificacion_subprograma->RETURN especificacion_tipo\n");}
      ;

parte_formal
      : /*vacio*/                   {printf("parte_formal->vacio\n");}
      | declaracion_parametros      {printf("parte_formal->declaracion_parametros\n");}
      //| error
      ;

declaracion_parametros
      : declaracion_parametro                               {printf("declaracion_parametros->declaracion_parametro\n");}
      | declaracion_parametro lista_declaracion_parametro   {printf("declaracion_parametros->declaracion_parametro lista_declaracion_parametro\n");}
      ;

lista_declaracion_parametro
      : declaracion_parametro                                     {printf("lista_declaracion_parametro->declaracion_parametro\n");}
      | declaracion_parametro ';' lista_declaracion_parametro     {printf("lista_declaracion_parametro->declaracion_parametro ';' lista_declaracion_parametro\n");}
      ;

declaracion_parametro
      : lista_identificadores ':' especificacion_tipo             {printf("declaracion_parametro->lista_identificadores ':' especificacion_tipo\n");}
      | lista_identificadores ':' modo especificacion_tipo        {printf("declaracion_parametro->lista_identificadores ':' modo especificacion_tipo\n");}
      ;

modo
      : IN              {printf("modo->IN\n");}
      | IN OUT          {printf("modo->IN OUT\n");}
      ;

cuerpo_subprograma
      : IS START lista_instruccion FINISH                                     {printf("cuerpo_subprograma->IS START lista_instruccion FINISH\n");}
      | IS START lista_instruccion FINISH IDENTIFICADOR                       {printf("cuerpo_subprograma->IS START lista_instruccion FINISH IDENTIFICADOR\n");}
      | IS lista_declaracion START lista_instruccion FINISH                   {printf("cuerpo_subprograma->IS lista_declaracion START lista_instruccion FINISH\n");}
      | IS lista_declaracion START lista_instruccion FINISH IDENTIFICADOR     {printf("cuerpo_subprograma->IS lista_declaracion START lista_instruccion FINISH IDENTIFICADOR\n");}
      ;

lista_instruccion
      : instruccion                       {printf("lista_instruccion->instruccion\n");}
      | lista_instruccion instruccion     {printf("lista_instruccion->lista_instruccion instruccion\n");}
      ;

lista_declaracion
      : declaracion                       {printf("lista_declaracion->declaracion\n");}
      | lista_declaracion declaracion     {printf("lista_declaracion->lista_declaracion declaracion\n");}
      ;

/* INSTRUCCIONES */

instruccion
      : instruccion_vacia           {printf("instruccion->instruccion_vacia\n");}
      | instruccion_asignacion      {printf("instruccion->instruccion_asignacion\n");}
      | instruccion_exit            {printf("instruccion->instruccion_exit\n");}
      | instruccion_return          {printf("instruccion->instruccion_return\n");}
      | instruccion_if              {printf("instruccion->instruccion_if\n");}
      | instruccion_case            {printf("instruccion->instruccion_case\n");}
      | instruccion_loop            {printf("instruccion->instruccion_loop\n");}
      | instruccion_rise            {printf("instruccion->instruccion_rise\n");}
      | instruccion_try_catch       {printf("instruccion->instruccion_try_catch\n");}
      | llamada_procedure           {printf("instruccion->llamada_procedure\n");}
      ;

instruccion_vacia
      : NIL ';'   {printf("instruccion_vacia->NIL ';'\n");}
      ;

instruccion_asignacion
      : nombre ASIG expresion ';'   {printf("instruccion_asignacion->nombre ASIG expresion ';'\n");}
      ;

instruccion_return
      : RETURN expresion ';'        {printf("instruccion_return->RETURN expresion ';'\n");}
      ;

instruccion_exit
      : EXIT ';'                                {printf("instruccion_exit->EXIT ';'\n");}
      | EXIT IDENTIFICADOR ';'                  {printf("instruccion_exit->EXIT IDENTIFICADOR ';'\n");}
      | EXIT WHEN expresion ';'                 {printf("instruccion_exit->EXIT WHEN expresion ';'\n");}
      | EXIT IDENTIFICADOR WHEN expresion ';'   {printf("instruccion_exit->EXIT IDENTIFICADOR WHEN expresion ';'\n");}
      ;

instruccion_if
      : IF expresion THEN lista_instruccion FINISH IF ';'                           {printf("instruccion_if->IF expresion THEN lista_instruccion FINISH IF ';'\n");}
      | IF expresion THEN lista_instruccion ELSE lista_instruccion FINISH IF ';'    {printf("instruccion_if->IF expresion THEN lista_instruccion ELSE lista_instruccion FINISH IF ';'\n");}
      ;

instruccion_case
      : CASE expresion IS lista_caso_when FINISH CASE ';'   {printf("instruccion_case->CASE expresion IS lista_caso_when FINISH CASE ';'\n");}
      ;

caso_when
      : WHEN entrada FLECHA lista_instruccion                     {printf("caso_when->WHEN entrada FLECHA lista_instruccion\n");}
      | WHEN entrada lista_entrada FLECHA lista_instruccion       {printf("caso_when->WHEN entrada lista_entrada FLECHA lista_instruccion\n");}
      ;

lista_caso_when
      : caso_when                         {printf("lista_caso_when->caso_when\n");}
      | lista_caso_when caso_when         {printf("lista_caso_when->lista_caso_when caso_when\n");}
      ;

entrada
      : expresion                         {printf("entrada->expresion\n");}
      | expresion DOS_PTOS expresion      {printf("entrada->expresion DOS_PTOS expresion\n");}
      | OTHERS                            {printf("entrada->OTHERS\n");}
      ;

lista_entrada
      : entrada                           {printf("lista_entrada->entrada\n");}
      | entrada '|' lista_entrada         {printf("lista_entrada->entrada '|' lista_entrada\n");}
      ;

instruccion_loop
      : clausula_iteracion bucle_base ';'                         {printf("instruccion_loop->clausula_iteracion bucle_base ';'\n");}
      | IDENTIFICADOR ':' clausula_iteracion bucle_base ';'       {printf("instruccion_loop->IDENTIFICADOR ':' clausula_iteracion bucle_base ';'\n");}
      ;

clausula_iteracion
      : FOR IDENTIFICADOR IN expresion DOS_PTOS expresion               {printf("clausula_iteracion->FOR IDENTIFICADOR IN expresion DOS_PTOS expresion\n");}
      | FOR IDENTIFICADOR IN REVERSE expresion DOS_PTOS expresion       {printf("clausula_iteracion->FOR IDENTIFICADOR IN REVERSE expresion DOS_PTOS expresion\n");}
      | FOREACH IDENTIFICADOR IN expresion                              {printf("clausula_iteracion->FOREACH IDENTIFICADOR IN expresion\n");}
      | WHILE expresion                                                 {printf("clausula_iteracion->WHILE expresion\n");}
      ;

bucle_base
      : LOOP lista_instruccion FINISH LOOP      {printf("bucle_base->LOOP lista_instruccion FINISH LOOP\n");}
      ;

instruccion_rise
      : RAISE IDENTIFICADOR ';'     {printf("instruccion_rise->RAISE IDENTIFICADOR ';'\n");}
      ;

instruccion_try_catch
      : TRY lista_instruccion clausulas_excepcion FINISH TRY      {printf("instruccion_try_catch->TRY lista_instruccion clausulas_excepcion FINISH TRY\n");}
      ;

clausulas_excepcion
      : clausula_defecto                                    {printf("clausulas_excepcion->clausula_defecto\n");}
      | lista_clausula_especifica clausula_defecto          {printf("clausulas_excepcion->lista_clausula_especifica clausula_defecto\n");}
      | lista_clausula_especifica                           {printf("clausulas_excepcion->lista_clausula_especifica\n");}
      ;

clausula_especifica
      : EXCEPTION '(' IDENTIFICADOR ')' lista_instruccion   {printf("clausula_especifica->EXCEPTION '(' IDENTIFICADOR ')' lista_instruccion\n");}
      ;

lista_clausula_especifica
      : clausula_especifica                                 {printf("lista_clausula_especifica->clausula_especifica\n");}
      | lista_clausula_especifica clausula_especifica       {printf("lista_clausula_especifica->lista_clausula_especifica clausula_especifica\n");}
      ;

clausula_defecto
      : DEFAULT '(' IDENTIFICADOR ')' lista_instruccion     {printf("clausula_defecto->DEFAULT '(' IDENTIFICADOR ')' lista_instruccion\n");}
      ;

llamada_procedure
      : llamada_suprograma ';'      {printf("llamada_procedure->llamada_suprograma ';'\n");}
      ;

llamada_suprograma
      : IDENTIFICADOR '('')'                    {printf("llamada_suprograma->IDENTIFICADOR '('')'\n");}
      | IDENTIFICADOR '(' lista_expresion ')'   {printf("llamada_suprograma->IDENTIFICADOR '(' lista_expresion ')'\n");}
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
/*
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
    : expresion_logica4 '@' expresion_logica5         {printf("expresion_logica4->expresion_logica4 '@' expresion_logica5\n");}
    | expresion_logica5                               {printf("expresion_logica4->expresion_logica5\n");}
    ;

expresion_logica5
    : expresion_logica5 '&' expresion_logica6         {printf("expresion_logica5->expresion_logica5 '&' expresion_logica6\n");}
    | expresion_logica6                               {printf("expresion_logica5->expresion_logica6\n");}
    ;

expresion_logica6
    : expresion_logica6 '+' expresion_logica7         {printf("expresion_logica6->expresion_logica6 '+' expresion_logica7\n");}
    | expresion_logica6 '-' expresion_logica7         {printf("expresion_logica6->expresion_logica6 '-' expresion_logica7\n");}
    | expresion_logica7                               {printf("expresion_logica6->expresion_logica7\n");}
    ;

expresion_logica7
    : expresion_logica7 '*' expresion_logica8         {printf("expresion_logica7->expresion_logica7 '*' expresion_logica8\n");}
    | expresion_logica7 '%' expresion_logica8         {printf("exp_log7->exp_log7 (porcentaje) exp_log8\n");}
    | expresion_logica7 MOD expresion_logica8         {printf("expresion_logica7->expresion_logica7 MOD expresion_logica8\n");}
    | expresion_logica8                               {printf("expresion_logica7->expresion_logica8\n");}
    ;

expresion_logica8
    : expresion_logica9 EXP expresion_logica8         {printf("expresion_logica8->expresion_logica9 EXP expresion_logica8\n");}
    | expresion_logica9                               {printf("expresion_logica8->expresion_logica9\n");}
    ;

expresion_logica9
    : '-' primario                                    {printf("expresion_logica9->- primario\n");}
    | primario                                        {printf("expresion_logica9->primario\n");}
    ;
*/

expresion_logica 
    : CTC_FLOAT
    | CTC_INT
    | expresion_logica OR expresion_logica            {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica AND expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | NOT expresion_logica                            {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica MAYOR_IGUAL expresion_logica   {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica MENOR_IGUAL expresion_logica   {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '>' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '<' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica DISTINTO expresion_logica      {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '=' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '@' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '&' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '+' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '-' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica MOD expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '%' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica '*' expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | expresion_logica EXP expresion_logica           {printf("expresion_logica->expresion_logica\n");}
    | '-' expresion_logica                            {printf("expresion_logica->expresion_logica\n");}
    ;

expresion
      : expresion_logica                                    {printf("expresion->expresion_logica\n");}
      | expresion_logica IF expresion ELSE expresion        {printf("expresion->expresion_logica IF expresion ELSE expresion\n");}
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
