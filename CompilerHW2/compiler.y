/* Definition section */
%{
    #include "compiler_common.h"
    #include "compiler_util.h"
    #include "main.h"

    int yydebug = 1;

    typedef struct Node
    {
        char name[50];
        char type[20];
        int address;
        int lineno;
	char func_sig[50];
    } NODE;
    
    void pushScopes();
    void dumpScopes();
    void stdoutPrints();
    void pushFunParms(ObjectType variableType, char* variableName, int parmFlag);
    void createFunctions(ObjectType variableType, char* funcName);
    void pushFunInParms(int type_flag);
    void functionArgPush(Object *variable);
    void insert_symbol(char* name, char* type);
    void pushAssignParms(ObjectType variableType, char* variableName, int parmFlag);
    void updateFunSig(ObjectType variableType, char* funcName);

    int scope_level=-1;
    int variable_address = 0;
    bool has_func = 0;
    int type_flag = -1;
    int cout_idx = 0;
    int cout_list[50] = {};
    int table_len[50] = {};
    NODE table[50][50];
    ObjectType tmp;
    char func_sig_tmp[50];
    int array_count = 0;
%}

/* Variable or self-defined structure */
%union {
    ObjectType var_type;

    bool b_var;
    int i_var;
    float f_var;
    char *s_var;

    Object object_val;
}

/* Token without return */
%token COUT
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <b_var> BOOL_LIT
%token <i_var> INT_LIT
%token <f_var> FLOAT_LIT
%token <s_var> STR_LIT
%token <s_var> IDENT '(' ')'

/* Nonterminal with return, which need to sepcify type */
//%type <object_val> Expression
//%type <object_val> term
//%type <object_val> factor

%left ADD SUB
%left MUL DIV REM
%left LOR LAN
%left GTR LES EQL NEQ LEQ GEQ

/* Yacc will start at this nonterminal */
%start Program

%%
/* Grammar section */

Program
    : { pushScopes(); } GlobalStmtList { dumpScopes(); }
    | /* Empty file */
;

GlobalStmtList 
    : GlobalStmtList GlobalStmt
    | GlobalStmt
;

GlobalStmt
    : DefineVariableStmt
    | FunctionDefStmt
;
DefineVariableStmt
    : VARIABLE_T { tmp = $<var_type>1; } IDENTList
;
IDENTList
    : IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ',' IDENTList
    | IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ';'
    | IDENT VAL_ASSIGN Expression { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ',' IDENTList
    | IDENT VAL_ASSIGN Expression { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ';'
    | IDENT '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } IDENTList
    | IDENT '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ';'
    | IDENT '[' ValueStmt ']' '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } IDENTList
    | IDENT '[' ValueStmt ']' '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ';'
    | IDENT '[' ValueStmt ']' VAL_ASSIGN '{' ArrayParms '}' { printf("create array: %d\n", array_count); array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } IDENTList
    | IDENT '[' ValueStmt ']' VAL_ASSIGN '{' ArrayParms '}' { printf("create array: %d\n", array_count); array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ';'
    | IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT); } ':' Expression
;
/* Function */
FunctionDefStmt
    : VARIABLE_T IDENT { createFunctions($<var_type>1, $<s_var>2); } '(' FunctionParameterStmtList ')' { updateFunSig($<var_type>1, $<s_var>2); } '{' StmtList '}' { dumpScopes(); }
;
FunctionParameterStmtList 
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
    | /* Empty function parameter */
;
FunctionParameterStmt
    : VARIABLE_T IDENT { pushFunParms($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT); }
    | VARIABLE_T IDENT '[' ']' { strcat(func_sig_tmp, "["); pushFunParms($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT); }
;
/* Scope */
StmtList 
    : StmtList Stmt
    | Stmt
;
Stmt
    : ';'
    | COUT CoutParmListStmt ';' { stdoutPrints(); }
    | RETURN Expression ';' { printf("RETURN\n"); }
    | DefineVariableStmt
    | IDENT { 
	for(int j = 0; j < scope_level + 1; j++){
		for(int i = 0; i < table_len[j]; i++){
                	if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                        	printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                        	break;
                	}
        	}
	}
      } AssignStmt ';'
    | IDENT '[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                break;
                        }
                }
        }
      } AssignStmt ';'
    | IDENT '[' Parm ']' '[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                break;
                        }
                }
        }
      } AssignStmt ';'
    | IF_Stmt
    | WHILE_Stmt
    | FOR_Stmt
    | FuncCall
    | RETURN ';' { printf("RETURN\n"); }
;
IF_Stmt
    : IF Expression { printf("IF\n"); pushScopes(); } '{' StmtList '}' { dumpScopes(); }
    | IF Expression { printf("IF\n"); }
    | ELSE { printf("ELSE\n"); pushScopes(); } '{' StmtList '}' { dumpScopes(); }
;
WHILE_Stmt
    : WHILE { printf("WHILE\n"); } Expression { pushScopes(); } '{' StmtList '}' { dumpScopes(); }
;
FOR_Stmt
    : FOR { printf("FOR\n"); pushScopes(); } '(' FOR_Block ')' '{' StmtList '}' { dumpScopes(); }
    | BREAK ';' { printf("BREAK\n"); }
;
FOR_Block
    : DefineVariableStmt FOR_Block
    | Expression FOR_Block
    | Expression ';' FOR_Block
    | ';' FOR_Block
    | IDENT {
		for(int j = 0; j < scope_level + 1; j ++){
                	for(int i = 0; i < table_len[j]; i++){
                        	if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                	printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                        break;
                                }
                        }
                }
	} AssignStmt
    | /*Empty parameter*/
;
CoutParmListStmt
    : CoutParmListStmt SHL Expression { pushFunInParms(type_flag); }
    | SHL Expression { pushFunInParms(type_flag); }
;
Expression
    : Expression LOR Expression1 { type_flag = 0; printf("LOR\n"); }
    | Expression1
;
Expression1
    : Expression1 LAN Expression2 { type_flag = 0; printf("LAN\n"); }
    | Expression2
;
Expression2
    : Expression2 BOR Expression3 { printf("BOR\n"); }
    | Expression3
;
Expression3
    : Expression3 BXO Expression4 { printf("BXO\n"); }
    | Expression4
;
Expression4
    : Expression4 BAN Expression5 { printf("BAN\n"); }
    | Expression5
;
Expression5
    : Expression5 EQL Expression6 { printf("EQL\n"); }
    | Expression5 NEQ Expression6 { printf("NEQ\n"); }
    | Expression6
;
Expression6
    : Expression6 GEQ Expression7 { printf("GEQ\n"); }
    | Expression6 LEQ Expression7 { printf("LEQ\n"); }
    | Expression6 GTR Expression7 { printf("GTR\n"); }
    | Expression6 LES Expression7 { printf("LES\n"); }
    | Expression7
;
Expression7
    : Expression7 SHR Expression8 { printf("SHR\n"); }
    | Expression8
;
Expression8
    : Expression8 ADD term { printf("ADD\n"); }
    | Expression8 SUB term { printf("SUB\n"); }
    | term
;
term
    : term MUL factor { printf("MUL\n"); }
    | term DIV factor { printf("DIV\n"); }
    | term REM factor { printf("REM\n"); }
    | factor
;
factor
    : '(' Expression ')'
    | IDENT { 
		if(strcmp($<s_var>1, "endl") == 0){
			type_flag = 3;
		        printf("IDENT (name=%s, address=-1)\n", $<s_var>1);
		}
		else {  
			bool exist = false;
			for(int j = 0; j < scope_level + 1; j ++){
				for(int i = 0; i < table_len[j]; i++){
					if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
						exist = true;
						printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
						if(strcmp(table[j][i].type, "bool") == 0) type_flag = 0;
                                		else if(strcmp(table[j][i].type, "int") == 0) type_flag = 1;
                                		else if(strcmp(table[j][i].type, "float") == 0) type_flag = 2;
                                		else if(strcmp(table[j][i].type, "string") == 0) type_flag = 3;
                                		else printf("Error in type define!\n");
						break;
			    		}
				}
			}
			if(exist == false){
				printf("IDENT (name=%s, address=%d)\n", $<s_var>1, variable_address);
				variable_address++;
			}
		}
	    }
    | SUB factor { printf("NEG\n"); }
    | NOT factor { printf("NOT\n"); }
    | BNT factor { printf("BNT\n"); }
    | INC_ASSIGN factor { printf("INC_ASSIGN\n"); }
    | factor INC_ASSIGN { printf("INC_ASSIGN\n"); }
    | DEC_ASSIGN factor { printf("DEC_ASSIGN\n"); }
    | factor DEC_ASSIGN { printf("DEC_ASSIGN\n"); }
    | '(' VARIABLE_T ')' factor {
	int num = $<var_type>2;
        switch(num){
                case 1:
                        printf("Cast to auto\n");
                        break;
                case 2:
                        printf("Cast to void\n");
                        break;
                case 3:
                        printf("Cast to char\n");
                        break;
                case 4:
                        printf("Cast to int\n");
                        break;
                case 5:
                        printf("Cast to long\n");
                        break;
                case 6:
                        printf("Cast to float\n");
                        break;
                case 7:
                        printf("Cast to double\n");
                        break;
                case 8:
                        printf("Cast to bool\n");
                        break;
                case 9:
                        printf("Cast to string\n");
                        break;
        } 
    }
    | ValueStmt
    | FuncCall
    | ArrayCall
;
ValueStmt
    : BOOL_LIT {
		type_flag = 0;
		if($<b_var>1 == 0) printf("BOOL_LIT FALSE\n");
		else printf("BOOL_LIT TRUE\n");
 	}
    | INT_LIT { type_flag = 1; printf("INT_LIT %d\n", $<i_var>1); }
    | FLOAT_LIT { type_flag = 2; printf("FLOAT_LIT %f\n", $<f_var>1); }
    | STR_LIT { type_flag = 3; printf("STR_LIT \"%s\"\n", $<s_var>1); }
;
AssignStmt
    : VAL_ASSIGN Expression { printf("EQL_ASSIGN\n"); }
    | ADD_ASSIGN Expression { printf("ADD_ASSIGN\n"); }
    | SUB_ASSIGN Expression { printf("SUB_ASSIGN\n"); }
    | MUL_ASSIGN Expression { printf("MUL_ASSIGN\n"); }
    | DIV_ASSIGN Expression { printf("DIV_ASSIGN\n"); }
    | REM_ASSIGN Expression { printf("REM_ASSIGN\n"); }
    | SHR_ASSIGN Expression { printf("SHR_ASSIGN\n"); }
    | SHL_ASSIGN Expression { printf("SHL_ASSIGN\n"); }
    | BAN_ASSIGN Expression { printf("BAN_ASSIGN\n"); }
    | BOR_ASSIGN Expression { printf("BOR_ASSIGN\n"); }
    | BXO_ASSIGN Expression { printf("BXO_ASSIGN\n"); }
;
FuncCall
    : IDENT '(' FuncOpt ')' {
        for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") == 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
				printf("call: %s%s\n", table[j][i].name, table[j][i].func_sig);
                                break;
                        }
                }
        }
    }
;
FuncOpt
    : factor ',' FuncOpt
    | factor
;
ArrayParms
    : ValueStmt ',' ArrayParms { array_count++; }
    | ValueStmt { array_count++; }
    | /*Empty parameter*/ 
;
ArrayCall
    : IDENT '[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                break;
                        }
                }
        }
      }
    | IDENT '[' Parm ']' '[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                break;
                        }
                }
        }
      }
;
Parm
    : ValueStmt
    | IDENT { 
	for(int j = 0; j < scope_level + 1; j++){
        	for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                break;
                        }
                }
        }
    }
;
%%
/* C code section */
void pushScopes(){
	scope_level++;
	printf("> Create symbol table (scope level %d)\n", scope_level);
}
void dumpScopes(){
	printf("\n> Dump symbol table (scope level: %d)\n", scope_level);
	printf("%-10s%-20s%-10s%-10s%-10s%-10s\n","Index", "Name", "Type", "Addr", "Lineno", "Func_sig");
	for(int i=0; i<table_len[scope_level]; ++i){
		printf("%-10d%-20s%-10s%-10d%-10d%-10s\n",i, table[scope_level][i].name, table[scope_level][i].type, table[scope_level][i].address, table[scope_level][i].lineno, table[scope_level][i].func_sig);
	}
	table_len[scope_level] = 0;
	scope_level--;
}
void stdoutPrints(){
	printf("cout ");
	for(int i = 0; i < cout_idx; i++){
		switch(cout_list[i]){
			case 0:
				printf("bool");
				break;
			case 1:
				printf("int");
				break;
			case 2:
				printf("float");
				break;
			case 3:
				printf("string");
				break;
			default:
				printf("Error in cout phase!");
				break;
		}
		cout_list[i] = -1;
		if(i != cout_idx - 1) printf(" ");
	}
	printf("\n");
	cout_idx = 0;	
}
void createFunctions(ObjectType variableType, char* funcName){
        printf("func: %s\n",funcName); 
        printf("> Insert `%s` (addr: %d) to scope level %d\n", funcName, -1, scope_level);
	insert_symbol(funcName, "function");
	pushScopes();
}
void pushFunParms(ObjectType variableType, char* variableName, int parmFlag){
	printf("> Insert `%s` (addr: %d) to scope level %d\n", variableName, variable_address, scope_level);
	char var_type[50];
	int num = variableType;
	switch(num){
		case 1:
			strcpy(var_type, "auto");
			break;
		case 2:
			strcpy(var_type, "void");
			strcat(func_sig_tmp, "V");
                        break;
		case 3:
			strcpy(var_type, "char");
			strcat(func_sig_tmp, "C");
                        break;
		case 4:
			strcpy(var_type, "int");
			strcat(func_sig_tmp, "I");
                        break;
		case 5:
			strcpy(var_type, "long");
			strcat(func_sig_tmp, "J");
                        break;
		case 6:
			strcpy(var_type, "float");
			strcat(func_sig_tmp, "F");
                        break;
		case 7:
			strcpy(var_type, "double");
			strcat(func_sig_tmp, "D");
                        break;
		case 8:
			strcpy(var_type, "bool");
			strcat(func_sig_tmp, "B");
                        break;
		case 9:
			strcpy(var_type, "string");
			strcat(func_sig_tmp, "Ljava/lang/String;");
                        break;
	}
	insert_symbol(variableName, var_type);
}
void pushAssignParms(ObjectType variableType, char* variableName, int parmFlag){
	printf("> Insert `%s` (addr: %d) to scope level %d\n", variableName, variable_address, scope_level);
        char var_type[50];
        int num = variableType;
        switch(num){
               	case 1:
                       	switch(type_flag){
                        	case 0:
                                	strcpy(var_type, "bool");
                                	break;
                        	case 1:
                                	strcpy(var_type, "int");
                                	break;
                        	case 2:
                                	strcpy(var_type, "float");
                                	break;
                        	case 3:
                                	strcpy(var_type, "string");
                                	break;
                        	default:
                                	printf("Error in pushAssignParms!\n");
                                	break;
                	}
        		break;
               	case 2:
                       	strcpy(var_type, "void");
                       	break;
               	case 3:
                       	strcpy(var_type, "char");
                       	break;
               	case 4:
                       	strcpy(var_type, "int");
                       	break;
               	case 5:
                       	strcpy(var_type, "long");
                       	break;
               	case 6:
                       	strcpy(var_type, "float");
                       	break;
               	case 7:
                       	strcpy(var_type, "double");
                       	break;
               	case 8:
                       	strcpy(var_type, "bool");
                       	break;
               	case 9:
                       	strcpy(var_type, "string");
                       	break;
       	}
       	insert_symbol(variableName, var_type);
}
void pushFunInParms(int type_flag){
	cout_list[cout_idx] = type_flag;
	cout_idx++;
}
void insert_symbol(char* name, char* type) {
	table[scope_level][table_len[scope_level]].address = variable_address;
	table[scope_level][table_len[scope_level]].lineno = yylineno;
	strcpy(table[scope_level][table_len[scope_level]].name, name);
	strcpy(table[scope_level][table_len[scope_level]].type, type);
	strcpy(table[scope_level][table_len[scope_level]].func_sig, "-");
	table_len[scope_level]++;
	if(strcmp(type,"function") == 0) table[scope_level][table_len[scope_level] - 1].address = -1;
	else variable_address++;
}
void updateFunSig(ObjectType variableType, char* funcName){
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, funcName) == 0 && strcmp(table[j][i].type, "function") == 0){
                                int num = variableType;
				char FunSig[50] = "";
        			switch(num){
                			case 2:
                        			strcat(FunSig, "(");
						strcat(FunSig, func_sig_tmp);
						strcat(FunSig, ")V");
                        			break;
					case 3:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")C");
                        			break;
                			case 4:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")I");
                        			break;
                			case 5:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")J");
                        			break;
                			case 6:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")F");
                        			break;
                			case 7:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")D");
                        			break;
                			case 8:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")B");
                        			break;
                			case 9:
						strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")Ljava/lang/String;");
                        			break;
        			}
				strcpy(table[j][i].func_sig, FunSig);
				memset(func_sig_tmp, 0, strlen(func_sig_tmp));
                                break;
                        }
                }
        }
}
