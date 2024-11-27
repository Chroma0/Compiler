/* Definition section */
%{
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
	bool has_val;
    } NODE;
    
    void pushScopes();
    void dumpScopes();
    void stdoutPrints();
    void pushFunParms(ObjectType variableType, char* variableName, int parmFlag, bool val);
    void createFunctions(ObjectType variableType, char* funcName, bool val);
    void pushFunInParms(int type_flag);
    void functionArgPush(Object *variable);
    void insert_symbol(char* name, char* type, bool val);
    void pushAssignParms(ObjectType variableType, char* variableName, int parmFlag, bool val);
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

    bool endl_mode = false;
    int add_ct = 0;
    int assign_address = 0;
    int if_ct = -1;
    int while_ct = -1;
    int for_ct = -1;
    int array_assign = 0;
    bool has_br = false;
    int array_dim = 0;
    bool for3_block = false;
    int for3_tmp = -1;
    bool for2_block = false;
    int type = 0;
    int loop_ct = -1;
    int for3_tmp_2 = -1;
    int trick = 0;
%}
/* Variable or self-defined structure */
%union {
    ObjectType var_type;
    bool b_var;
    char c_var;
    int32_t i_var;
    int64_t l_var;
    float f_var;
    double d_var;
    char *s_var;
    Object obj_val;
}
/* Token without return */
%token COUT
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <b_var> BOOL_LIT
%token <c_var> CHAR_LIT
%token <i_var> INT_LIT
%token <f_var> FLOAT_LIT
%token <s_var> STR_LIT
%token <s_var> IDENT

/* Nonterminal with return, which need to sepcify type */
//%type <obj_val> Expression
//%type <array_subscript> ArraySubscriptStmtList

%left ADD SUB
%left MUL DIV REM

%nonassoc THEN
%nonassoc ELSE

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
    : IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } ',' IDENTList
    | IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } ';'
    | IDENT VAL_ASSIGN Expression { 
	pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, true);
	if(tmp == 3) code("\tistore %d", assign_address);
        else if(tmp == 7) code("\tistore %d", assign_address);
        else if(tmp == 9) code("\tfstore %d", assign_address);
        else if(tmp == 11) code("\tastore %d", assign_address);
        else if(tmp == 5) code("\tcstore %d", assign_address); 
    } ',' IDENTList
    | IDENT VAL_ASSIGN Expression { 
	pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, true);
	if(tmp == 3) code("\tistore %d", assign_address);
        else if(tmp == 7) code("\tistore %d", assign_address);
        else if(tmp == 9) code("\tfstore %d", assign_address);
        else if(tmp == 11) code("\tastore %d", assign_address);
        else if(tmp == 5) code("\tcstore %d", assign_address);
	else if(tmp == 1) {
		if(type_flag == 0) code("\tistore %d", assign_address);
		else if(type_flag == 1) code("\tistore %d", assign_address);
		else if(type_flag == 2) code("\tfstore %d", assign_address);
		else codeRaw("\terror in IDENTList-4");
	}
    } ';'
    | IDENT '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } IDENTList
    | IDENT '[' ValueStmt ']' { codeRaw("\tnewarray int"); array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } ';'{
                for(int j = 0; j < scope_level + 1; j++){
                        for(int i = 0; i < table_len[j]; i++){
                                if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                        code("\tastore %d", table[j][i].address);
                                        break;
                                }
                        }
                }
        }
    | IDENT '[' ValueStmt ']' '[' ValueStmt ']' { array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } IDENTList
    | IDENT '[' ValueStmt ']' '[' ValueStmt ']' { codeRaw("\tmultianewarray [[I 2"); array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, false); } ';' {
                for(int j = 0; j < scope_level + 1; j++){
                        for(int i = 0; i < table_len[j]; i++){
                                if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                        code("\tastore %d", table[j][i].address);
                                        break;
                                }
                        }
                }
        }
    | IDENT '[' ValueStmt ']'{codeRaw("\tnewarray int");} VAL_ASSIGN '{' { codeRaw("\tdup"); code("\tldc %d",array_count);} ArrayParms '}' 
	{ printf("create array: %d\n", array_count); array_count = 0; pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, true); } 
	';' {
		for(int j = 0; j < scope_level + 1; j++){
                	for(int i = 0; i < table_len[j]; i++){
                        	if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                	code("\tastore %d", table[j][i].address);
                                	break;
                        	}
                	}
        	}
	}
    | IDENT { pushAssignParms(tmp, $<s_var>1, VAR_FLAG_DEFAULT, true); } ':' Expression
;

/* Function */
FunctionDefStmt
    : VARIABLE_T IDENT { createFunctions($<var_type>1, $<s_var>2, false); } '(' FunctionParameterStmtList ')' { updateFunSig($<var_type>1, $<s_var>2); } '{' StmtList '}' { 
	if(strcmp($<s_var>2, "main")==0) codeRaw("\treturn");
	else {
		if(type_flag == 0) codeRaw("\tireturn");
                else if(type_flag == 1) codeRaw("\tireturn");
                else if(type_flag == 2) codeRaw("\tfreturn");
                else codeRaw("\treturn");
	}
	codeRaw(".end method"); dumpScopes(); variable_address = 0; }
;
FunctionParameterStmtList 
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
    | /* Empty function parameter */
;
FunctionParameterStmt
    : VARIABLE_T IDENT { pushFunParms($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT, false); }
    | VARIABLE_T IDENT '[' ']' { strcat(func_sig_tmp, "["); pushFunParms($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT, false); }
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
				assign_address = table[j][i].address;
				if(strcmp(table[j][i].type, "int") == 0 && table[j][i].has_val == true) code("\tiload %d", table[j][i].address);
				else if(strcmp(table[j][i].type, "float") == 0 && table[j][i].has_val == true) code("\tfload %d", table[j][i].address);
				//else if(strcmp(table[j][i].type, "bool") == 0 && table[j][i].has_val == true) code("\tiload %d", table[j][i].address);
				else if(strcmp(table[j][i].type, "string") == 0 && table[j][i].has_val == true) code("\taload %d", table[j][i].address);
                        	break;
                	}
        	}
	}
      } AssignStmt ';'
    | IDENT{
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
				code("\taload %d", table[j][i].address);
                                break;
                        }
                }
        }
      } ARR_ASS { array_assign = 1; } AssignStmt ';' { array_assign = 0; }
    | IF_Stmt
    | WHILE_Stmt
    | FOR_Stmt
    | FuncCall
    | RETURN ';' { printf("RETURN\n"); }
;
ARR_ASS
    : '[' Parm ']' { array_dim = 1;}
    | '[' Parm ']' { codeRaw("\taaload");} '[' Parm ']' { array_dim = 2;}
;
IF_Stmt
    : IF Expression { 
	if_ct++;
	code("\tifeq IF_FALSE_%d", if_ct); 
	printf("IF\n"); 
	pushScopes(); } IF_Continue
    | IF Expression { if_ct++; code("\tifeq IF_EXIT_%d", if_ct); printf("IF\n"); } RETURN Expression { 
	codeRaw("\tireturn"); code("IF_EXIT_%d:", if_ct); } ';'
;
IF_Continue
    : '{' StmtList '}' { code("IF_FALSE_%d:", if_ct); dumpScopes(); if(scope_level>1 && has_br == false) if_ct--; }
    | '{' StmtList '}' { code("\tgoto IF_EXIT_%d", if_ct); dumpScopes(); } ELSE {
	code("IF_FALSE_%d:", if_ct);
        printf("ELSE\n");
        pushScopes(); } '{' StmtList {code("\tgoto IF_EXIT_%d", if_ct);} '}' { code("IF_EXIT_%d:", if_ct); dumpScopes(); if(scope_level>1) if_ct--; }
;
WHILE_Stmt
    : WHILE { 
	while_ct++;
	has_br = true;
	code("WHILE_START_%d:", while_ct);
	printf("WHILE\n"); } Expression { 
				code("\tifeq WHILE_END_%d", while_ct); 
				pushScopes(); } '{' StmtList {code("\tgoto WHILE_START_%d", while_ct);} '}' { 
						code("WHILE_END_%d:", while_ct); 
						dumpScopes(); 
						if(scope_level>1 && for_ct>-1) ;
						else if(scope_level>1) while_ct--; }
;
FOR_Stmt
    : FOR { loop_ct++; for_ct=loop_ct; printf("FOR\n"); pushScopes(); for2_block=true; for3_block=true; } '(' FOR_COND ')' 
	'{' StmtList {
		if(type == 1){
			 if(for3_tmp_2 != -1){
				code("\tiinc %d 1", for3_tmp_2);
				for3_tmp_2 = -1;
			}
			else{
				if(trick == 1) codeRaw("\tiinc 4 -1");
				else code("\tiinc %d 1", for3_tmp);
				for3_tmp = -1;
				trick = 0;
			}
		}
		code("\tgoto FOR_START_%d", for_ct);} '}' { 
		code("FOR_END_%d:", for_ct);
		dumpScopes();
		if(scope_level>1 && while_ct>-1) ;
		else if(scope_level>1) for_ct--;
	}
    | BREAK ';' { code("\tgoto WHILE_END_%d", while_ct); printf("BREAK\n"); }
;
FOR_COND
    : FOR_Block1 {code("FOR_START_%d:", for_ct);}
    | FOR_Block1 {code("FOR_START_%d:", for_ct);} FOR_Block2 { code("\tifeq FOR_END_%d", for_ct); } ';' FOR_Block3
;
FOR_Block1
    : ';'
    | DefineVariableStmt
;
FOR_Block2
    : /*empty*/ { for2_block = false; }
    | Expression { for2_block = false; }
;
FOR_Block3
    : Expression { for3_block = false; }
    | /*empty*/ { for3_block = false; }
    | IDENT {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
                                assign_address = table[j][i].address;
                                if(strcmp(table[j][i].type, "int") == 0 && table[j][i].has_val == true) code("\tiload %d", table[j][i].address);
                                else if(strcmp(table[j][i].type, "float") == 0 && table[j][i].has_val == true) code("\tfload %d", table[j][i].address);
                                else if(strcmp(table[j][i].type, "bool") == 0 && table[j][i].has_val == true) code("\tiload %d", table[j][i].address);
                                else if(strcmp(table[j][i].type, "string") == 0 && table[j][i].has_val == true) code("\taload %d", table[j][i].address);
                                break;
                        }
                }
        }
      } AssignStmt { for3_block = false; }
;
CoutParmListStmt
    : CoutParmListStmt SHL { codeRaw("\tgetstatic java/lang/System/out Ljava/io/PrintStream;"); }Expression { 
	switch(type_flag){
		case 0:
			codeRaw("\tinvokevirtual java/io/PrintStream/print(Z)V");
                        break;
                case 1:
                        codeRaw("\tinvokevirtual java/io/PrintStream/print(I)V");
                        break;
                case 2:
                        codeRaw("\tinvokevirtual java/io/PrintStream/print(F)V");
                        break;
                case 3:
			if(endl_mode == true){ 
				codeRaw("\tinvokevirtual java/io/PrintStream/println()V");
				endl_mode = false;
			}
                        else codeRaw("\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
                        break;
		case 4:
			codeRaw("\tinvokevirtual java/io/PrintStream/print(C)V");
			break;
                default:
                        codeRaw("\tError in cout phase!");
                        break;
	}
	pushFunInParms(type_flag); }
    | SHL { codeRaw("\tgetstatic java/lang/System/out Ljava/io/PrintStream;"); } Expression { 
	switch(type_flag){
                case 0:
                        codeRaw("\tinvokevirtual java/io/PrintStream/print(Z)V");
                        break;
                case 1:
                        codeRaw("\tinvokevirtual java/io/PrintStream/print(I)V");
                        break;
                case 2:
                        codeRaw("\tinvokevirtual java/io/PrintStream/print(F)V");
                        break;
                case 3:
			if(endl_mode == true){
				codeRaw("\tinvokevirtual java/io/PrintStream/println()V");
				endl_mode = false;
			}
                        else codeRaw("\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
                        break;
                case 4:
			codeRaw("\tinvokevirtual java/io/PrintStream/print(C)V");
                        break;
                default:
                        codeRaw("\tError in cout phase!");
                        break;
	}
	pushFunInParms(type_flag); }
;
Expression
    : Expression LOR Expression1 { type_flag = 0; codeRaw("\tior"); printf("LOR\n"); }
    | Expression1
;
Expression1
    : Expression1 LAN Expression2 { type_flag = 0; codeRaw("\tiand"); printf("LAN\n"); }
    | Expression2
;
Expression2
    : Expression2 BOR Expression3 { 
	printf("BOR\n");
	if(type_flag == 1) codeRaw("\tior");
	else if(type_flag == 2) codeRaw("\tfor");
	else codeRaw("\terror in BOR"); }
    | Expression3
;
Expression3
    : Expression3 BXO Expression4 { 
	printf("BXO\n"); 
	if(type_flag == 1) codeRaw("\tixor");
	else if(type_flag == 2) codeRaw("\tfxor");
	else codeRaw("\terror in BXO"); }
    | Expression4
;
Expression4
    : Expression4 BAN Expression5 { 
	printf("BAN\n");
	if(type_flag == 1) codeRaw("\tiand");
	else if(type_flag == 2) codeRaw("\tfand");
	else codeRaw("\terror in BAN"); }
    | Expression5
;
Expression5
    : Expression5 EQL Expression6 { 
	if(type_flag == 1 || type_flag == 0){
		code("\tif_icmpeq gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("\tfsub");
		codeRaw("\tf2i");
		code("\tif_icmpeq gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else codeRaw("\tiastore");
	printf("EQL\n"); }
    | Expression5 NEQ Expression6 {
	if(type_flag == 1){
		code("\tif_icmpne gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("\tfsub");
		codeRaw("\tf2i");
		code("\tif_icmpne gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else codeRaw("\terror in NEQ");
	printf("NEQ\n");
	}
    | Expression6
;
Expression6
    : Expression6 GEQ Expression7 { 
	if(type_flag == 1){
		code("\tif_icmpge gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("fsub");
		codeRaw("f2i");
		code("\tif_icmpge gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else codeRaw("\terror in GEQ");
	printf("GEQ\n"); }
    | Expression6 LEQ Expression7 { 
	if(type_flag == 1){
		code("\tif_icmple gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("\tfsub");
		codeRaw("\tf2i");
		code("\tif_icmple gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else codeRaw("\terror in LEQ");
	printf("LEQ\n"); }
    | Expression6 GTR Expression7 { 
	if(type_flag == 1){
		code("\tif_icmpgt gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("\tfsub");
		codeRaw("\tf2i");
		code("\tifgt gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else codeRaw("\terror in GTR");
	printf("GTR\n"); }
    | Expression6 LES Expression7 { 
	if(type_flag == 1){
		code("\tif_icmplt gtr_true_%d", add_ct);
		code("gtr_false_%d:", add_ct);
		codeRaw("\ticonst_0");
		code("\tgoto gtr_end_%d", add_ct);
		code("gtr_true_%d:", add_ct);
		codeRaw("\ticonst_1");
		code("gtr_end_%d:", add_ct);
		add_ct++;
	}
	else if(type_flag == 2){
		codeRaw("\tfsub");
		codeRaw("\tf2i");
		code("\tif_icmplt gtr_true_%d", add_ct);
                code("gtr_false_%d:", add_ct);
                codeRaw("\ticonst_0");
                code("\tgoto gtr_end_%d", add_ct);
                code("gtr_true_%d:", add_ct);
                codeRaw("\ticonst_1");
                code("gtr_end_%d:", add_ct);
                add_ct++;
	}
	else codeRaw("\terror in LES");
	printf("LES\n"); }
    | Expression7
;
Expression7
    : Expression7 SHR Expression8 { printf("SHR\n"); codeRaw("\tiushr"); }
    | Expression8
;
Expression8
    : Expression8 ADD term { 
	if(type_flag == 1) codeRaw("\tiadd");
	else if(type_flag == 2) codeRaw("\tfadd");
	else codeRaw("\terror in ADD");
	printf("ADD\n"); }
    | Expression8 SUB term { 
	if(type_flag == 1) codeRaw("\tisub");
        else if(type_flag == 2) codeRaw("\tfsub");
        else codeRaw("\terror in SUB");
	printf("SUB\n"); }
    | term
;
term
    : term MUL factor { 
	if(type_flag == 1) codeRaw("\timul");
        else if(type_flag == 2) codeRaw("\tfmul");
        else codeRaw("\terror in MUL");
	printf("MUL\n"); }
    | term DIV factor { 
	if(type_flag == 1) codeRaw("\tidiv");
        else if(type_flag == 2) codeRaw("\tfdiv");
        else codeRaw("\terror in DIV");
	printf("DIV\n"); }
    | term REM factor {
	if(type_flag == 1) codeRaw("\tirem");
        else codeRaw("\terror in REM"); 
	printf("REM\n"); }
    | factor
;
factor
    : '(' Expression ')'
    | IDENT {
		if(strcmp($<s_var>1, "endl") == 0){
			type_flag = 3;
		        printf("IDENT (name=%s, address=-1)\n", $<s_var>1);
			endl_mode = true;
		}
		else {
			if(for3_block==false || for2_block==true){
			bool exist = false;
			for(int j = 0; j < scope_level + 1; j ++){
				for(int i = 0; i < table_len[j]; i++){
					if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
						exist = true;
						printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
						if(strcmp(table[j][i].type, "bool") == 0){ 
							type_flag = 0;
							code("\tiload %d", table[j][i].address);
						}
                                		else if(strcmp(table[j][i].type, "int") == 0){
							type_flag = 1;
							code("\tiload %d", table[j][i].address);
						}
                                		else if(strcmp(table[j][i].type, "float") == 0){ 
							type_flag = 2;
							code("\tfload %d", table[j][i].address);
						}	
                                		else if(strcmp(table[j][i].type, "string") == 0){
							type_flag = 3;
							code("\taload %d", table[j][i].address);
						}
                                		else codeRaw("Error in type define!");
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
	    }
    | SUB factor {
	if(type_flag == 1) codeRaw("\tineg");
	else if(type_flag == 2) codeRaw("\tfneg");
	else codeRaw("\terror in NEG"); 
	printf("NEG\n"); }
    | NOT factor { 
	codeRaw("\ticonst_m1");
	codeRaw("\tixor");
	printf("NOT\n"); }
    | BNT factor { 
	codeRaw("\tldc -1");
	codeRaw("\tixor");
	printf("BNT\n"); }
    | INC_ASSIGN factor { 
	if(type_flag == 1){
		codeRaw("\tldc 1");
		codeRaw("\tiadd");
	}
	else if(type_flag == 2){
		codeRaw("\tldc 1.0");
		codeRaw("\tfadd");
	}
	else codeRaw("\terror in INC");
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
                                if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
                                if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                else codeRaw("\terror in val_assign");
                                break;
                        }
                }
        }
	printf("INC_ASSIGN\n"); }
    | factor INC_ASSIGN {
	if(for3_block == false){
	if(type_flag == 1){
                codeRaw("\tldc 1");
                codeRaw("\tiadd");
        }
        else if(type_flag == 2){
                codeRaw("\tldc 1.0");
                codeRaw("\tfadd");
        }
        else codeRaw("\terror in INC");
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
                                if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
                                if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                else codeRaw("\terror in val_assign");
                                break;
                        }
                }
        }
	}
	else {
		if(for3_tmp == -1) for3_tmp = assign_address;
		else for3_tmp_2 = assign_address;
		type = 1;
	}
	printf("INC_ASSIGN\n"); }
    | DEC_ASSIGN factor { 
	if(type_flag == 1){
                codeRaw("\tldc -1");
                codeRaw("\tiadd");
        }
        else if(type_flag == 2){
                codeRaw("\tldc -1.0");
                codeRaw("\tfadd");
        }
        else codeRaw("\terror in DEC");
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
                                if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
                                if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                else codeRaw("\terror in val_assign");
                                break;
                        }
                }
        }
	printf("DEC_ASSIGN\n"); }
    | factor DEC_ASSIGN {
	if(for3_block == false){ 
	if(type_flag == 1){
                codeRaw("\tldc -1");
                codeRaw("\tiadd");
        }
        else if(type_flag == 2){
                codeRaw("\tldc -1.0");
                codeRaw("\tfadd");
        }
        else codeRaw("\terror in DEC");
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
                                if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
                                if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                else codeRaw("\terror in val_assign");
                                break;
                        }
                }
        }
	}
	else{
		for3_tmp = assign_address;
		type = 2;
		trick = 1;
	}
	printf("DEC_ASSIGN\n"); }
    | '(' VARIABLE_T ')' factor {
	int num = $<var_type>2;
        switch(num){
                case 1:
                        printf("Cast to auto\n");
                        break;
                case 2:
                        printf("Cast to void\n");
                        break;
                case 5:
                        printf("Cast to char\n");
                        break;
                case 7:
                        printf("Cast to int\n");
			type_flag = 1;
			codeRaw("\tf2i");
                        break;
                case 8:
                        printf("Cast to long\n");
                        break;
                case 9:
                        printf("Cast to float\n");
			type_flag = 2;
			codeRaw("\ti2f");
                        break;
                case 10:
                        printf("Cast to double\n");
                        break;
                case 3:
                        printf("Cast to bool\n");
                        break;
                case 11:
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
		if($<b_var>1 == 0) {
			printf("BOOL_LIT FALSE\n");
			codeRaw("\ticonst_0");
		}
		else {
			 printf("BOOL_LIT TRUE\n");
			codeRaw("\ticonst_1");
		}
 	}
    | INT_LIT { type_flag = 1;
		code("\tldc %d", $<i_var>1);
		printf("INT_LIT %d\n", $<i_var>1); }
    | FLOAT_LIT { type_flag = 2; 
		code("\tldc %f", $<f_var>1);
		printf("FLOAT_LIT %f\n", $<f_var>1); }
    | STR_LIT { type_flag = 3; 
		code("\tldc \"%s\"", $<s_var>1);
		printf("STR_LIT \"%s\"\n", $<s_var>1); }
    | CHAR_LIT { type_flag = 4;
		code("\tbipush %d", $<c_var>1); }
;
AssignStmt
    : VAL_ASSIGN Expression {
	if( array_assign == 0){
	for(int j = 0; j < scope_level + 1; j ++){
        	for(int i = 0; i < table_len[j]; i++){
                	if(table[j][i].address == assign_address){
                        	table[j][i].has_val = true;
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
				else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
        			else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
        			else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
        			else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
        			else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
        			else codeRaw("\terror in val_assign");
                                break;
                        }
                }
        }
	}
	else codeRaw("\tiastore");		      
	printf("EQL_ASSIGN\n"); }
    | ADD_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tiadd");
        			else if(type_flag == 2) codeRaw("\tfadd");
        			else codeRaw("\terror in add_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("ADD_ASSIGN\n"); }
    | SUB_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tisub");
        			else if(type_flag == 2) codeRaw("\tfsub");
        			else codeRaw("\terror in sub_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	if(for3_block == true) type = 0;
	printf("SUB_ASSIGN\n"); }
    | MUL_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\timul");
        			else if(type_flag == 2) codeRaw("\tfmul");
        			else codeRaw("\terror in mul_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("MUL_ASSIGN\n"); }
    | DIV_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tidiv");
        			else if(type_flag == 2) codeRaw("\tfdiv");
        			else codeRaw("\terror in div_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("DIV_ASSIGN\n"); }
    | REM_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tirem");
        			else codeRaw("\terror in rem_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("REM_ASSIGN\n"); }
    | SHR_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				codeRaw("\tiushr");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("SHR_ASSIGN\n"); }
    | SHL_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				codeRaw("\tishl");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("SHL_ASSIGN\n"); }
    | BAN_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tiand");
        			else if(type_flag == 2) codeRaw("\tfand");
        			else codeRaw("\terror in ban_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("BAN_ASSIGN\n"); }
    | BOR_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tior");
        			else if(type_flag == 2) codeRaw("\tfor");
        			else codeRaw("\terror in bor_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("BOR_ASSIGN\n"); }
    | BXO_ASSIGN Expression { 
	for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(table[j][i].address == assign_address){
                                table[j][i].has_val = true;
				if(type_flag == 1) codeRaw("\tixor");
        			else if(type_flag == 2) codeRaw("\tfxor");
        			else codeRaw("\terror in bxo_assign");
				if(strcmp(table[j][i].type, "int") == 0 && type_flag == 2) codeRaw("\tf2i");
                                else if(strcmp(table[j][i].type, "float") == 0 && type_flag == 1) codeRaw("\ti2f");
				if(strcmp(table[j][i].type, "bool") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "int") == 0) code("\tistore %d", assign_address);
                                else if(strcmp(table[j][i].type, "float") == 0) code("\tfstore %d", assign_address);
                                else if(strcmp(table[j][i].type, "string") == 0) code("\tastore %d", assign_address);
                                else if(strcmp(table[j][i].type, "char") == 0) code("\tcstore %d", assign_address);
                                break;
                        }
                }
        }
	printf("BXO_ASSIGN\n"); }
;
FuncCall
    : IDENT '(' FuncOpt ')' {
        for(int j = 0; j < scope_level + 1; j ++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") == 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
				printf("call: %s%s\n", table[j][i].name, table[j][i].func_sig);
				code("\tinvokestatic Main/%s%s", table[j][i].name, table[j][i].func_sig);
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
    : ValueStmt { codeRaw("\tiastore"); array_count++; codeRaw("\tdup"); code("\tldc %d", array_count);} ',' ArrayParms
    | ValueStmt { codeRaw("\tiastore"); array_count++; }
    | /*Empty parameter*/
;
ArrayCall
    : IDENT{
        for(int j = 0; j < scope_level + 1; j++){
                        for(int i = 0; i < table_len[j]; i++){
                                if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                        code("\taload %d", table[j][i].address);
                                        break;
                                }
                        }
                }
	type_flag = 1;
        } ARR_IND
;
ARR_IND
    :'[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
				codeRaw("\tiaload");
                                break;
                        }
                }
        }
      }
    |'[' Parm ']' { codeRaw("\taaload"); } '[' Parm ']' {
        for(int j = 0; j < scope_level + 1; j++){
                for(int i = 0; i < table_len[j]; i++){
                        if(strcmp(table[j][i].name, $<s_var>1) == 0 && strcmp(table[j][i].type, "function") != 0){
                                printf("IDENT (name=%s, address=%d)\n", table[j][i].name, table[j][i].address);
				codeRaw("\tiaload");
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
				code("\tiload %d", table[j][i].address);
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
void createFunctions(ObjectType variableType, char* funcName, bool val){
        printf("func: %s\n",funcName);
        printf("> Insert `%s` (addr: %d) to scope level %d\n", funcName, -1, scope_level);
	insert_symbol(funcName, "function", val);
	pushScopes();
}
void pushFunParms(ObjectType variableType, char* variableName, int parmFlag, bool val){
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
		case 5:
			strcpy(var_type, "char");
			strcat(func_sig_tmp, "C");
                        break;
		case 7:
			strcpy(var_type, "int");
			strcat(func_sig_tmp, "I");
                        break;
		case 8:
			strcpy(var_type, "long");
			strcat(func_sig_tmp, "J");
                        break;
		case 9:
			strcpy(var_type, "float");
			strcat(func_sig_tmp, "F");
                        break;
		case 10:
			strcpy(var_type, "double");
			strcat(func_sig_tmp, "D");
                        break;
		case 3:
			strcpy(var_type, "bool");
			strcat(func_sig_tmp, "Z");
                        break;
		case 11:
			strcpy(var_type, "string");
			strcat(func_sig_tmp, "Ljava/lang/String;");
                        break;
	}
	insert_symbol(variableName, var_type, val);
}
void pushAssignParms(ObjectType variableType, char* variableName, int parmFlag, bool val){
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
               	case 5:
                       	strcpy(var_type, "char");
                       	break;
               	case 7:
                       	strcpy(var_type, "int");
                       	break;
               	case 8:
                       	strcpy(var_type, "long");
                       	break;
               	case 9:
                       	strcpy(var_type, "float");
                       	break;
               	case 10:
                       	strcpy(var_type, "double");
                       	break;
               	case 3:
                       	strcpy(var_type, "bool");
                       	break;
               	case 11:
                       	strcpy(var_type, "string");
                       	break;
       	}
       	insert_symbol(variableName, var_type, val);
}
void pushFunInParms(int type_flag){
	cout_list[cout_idx] = type_flag;
	cout_idx++;
}
void insert_symbol(char* name, char* type, bool val) {
	table[scope_level][table_len[scope_level]].address = variable_address;
	table[scope_level][table_len[scope_level]].lineno = yylineno;
	strcpy(table[scope_level][table_len[scope_level]].name, name);
	strcpy(table[scope_level][table_len[scope_level]].type, type);
	strcpy(table[scope_level][table_len[scope_level]].func_sig, "-");
	table[scope_level][table_len[scope_level]].has_val = val;
	table_len[scope_level]++;
	assign_address = variable_address;
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
					case 5:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")C");
                        			break;
                			case 7:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")I");
                        			break;
                			case 8:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")J");
                        			break;
                			case 9:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")F");
                        			break;
                			case 10:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")D");
                        			break;
                			case 3:
                        			strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")Z");
                        			break;
                			case 11:
						strcat(FunSig, "(");
                                                strcat(FunSig, func_sig_tmp);
                                                strcat(FunSig, ")Ljava/lang/String;");
                        			break;
        			}
				strcpy(table[j][i].func_sig, FunSig);
				if(strcmp(table[j][i].name, "main") == 0) codeRaw(".method public static main([Ljava/lang/String;)V");
				else code(".method public static %s%s", table[j][i].name, table[j][i].func_sig);
				codeRaw(".limit stack 100");
				codeRaw(".limit locals 100");
				memset(func_sig_tmp, 0, strlen(func_sig_tmp));
                                break;
                        }
                }
        }
}