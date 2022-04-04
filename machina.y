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


/* TIPOS */


/* SUBPROGRAMAS */


/* INSTRUCCIONES */


/* EXPRESIONES */


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
