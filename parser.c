//2015117662 ±Ç¹ÎÁö
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define BUFLEN 256						
#define MAXCHILDREN 3	
#define MAXTOKENLEN 40
#define FALSE 0
#define TRUE 1
			

typedef enum { StmtK, ExpK, DecK } NodeKind;
typedef enum { IfK, WhileK, ReturnK, CallK, CompoundK } StmtKind;
typedef enum { OpK, IdK, ConstK, AssignK } ExpKind;
typedef enum { ScalarK, ArrK, FuncK } DecKind;

typedef enum { Void, Integer } ExpType;		

typedef enum
{
	ENDFILE, ERROR,
	ELSE, IF, INT, RETURN, VOID, WHILE,
	ID, NUM,
	PLUS, MINUS, TIMES, OVER, LT, LTEQ, BG, BGEQ, EQ, NEQ, ASSIGN, LPAREN, RPAREN, SEMICOL, COL, LBRACE, RBRACE, LBRACKET, RBRACKET
} TokenType;

typedef enum
{
	START, INCOMPAREL, INCOMPAREB, INCOMPAREE, INNEQ, INCOMMENTA, INCOMMENTB, INCOMMENTC, INID, INNUM, DONE
}
StateType;

typedef struct treeNode {
	struct treeNode *child[MAXCHILDREN];
	struct treeNode *sibling;
	int lineno;
	NodeKind nodekind;
	union {
		StmtKind stmt;
		ExpKind exp;
		DecKind dec;
	} kind;

	TokenType op;
	int val;
	char *name;

	ExpType funcType;
	ExpType varType;
	ExpType expType;
} TreeNode;			

static struct
{
	char * str;
	TokenType tok;
}reservedWords[6] =
{
	{ "else", ELSE }, { "if", IF }, { "int", INT }, { "return", RETURN }, { "void", VOID }, { "while", WHILE }
};

static char lineBuf[BUFLEN];			
static int linepos = 0;				
static int bufsize = 0;					
static int indentno = 0;

#define INDENT indentno+=4
#define UNINDENT indentno-=4

int lineno = 0;						
int Error = FALSE;
char tokenString[MAXTOKENLEN + 1];

FILE * source;						
FILE * output;

static char getNextChar(void); 
static void ungetNextChar(void); 
static TokenType reservedLookup(char *s);
void printToken(TokenType token, const char* tokenString); 
TreeNode *newStmtNode(StmtKind kind); 
TreeNode *newExpNode(ExpKind kind); 
TreeNode *newDecNode(DecKind kind); 
char * copyString(char * s);
static void printSpace(void);
char *typeName(ExpType type);
void printTree(TreeNode * tree);
TokenType getToken(void);

static TokenType token;
static ExpType type_specifier(void);
static TreeNode *declaration_list(void);
static TreeNode *declaration(void);
static TreeNode *var_declaration(void);
static TreeNode *fun_declaration(ExpType type, char * id);
static TreeNode *params(void);
static TreeNode *param_list(void);
static TreeNode *param(void);
static TreeNode *compound_stmt(void);
static TreeNode *local_declarations(void);
static TreeNode *statement_list(void);
static TreeNode *statement(void);
static TreeNode *expression_stmt(void);
static TreeNode *selection_stmt(void);
static TreeNode *iteration_stmt(void);
static TreeNode *return_stmt(void);
static TreeNode *expression(void);
static TreeNode *simple_expression(TreeNode *next);
static TreeNode *additive_expression(TreeNode *next);
static TreeNode *term(TreeNode *next);
static TreeNode *factor(TreeNode *next);
static TreeNode *call(void);
static TreeNode *args(void);
static TreeNode *arg_list(void);

TreeNode *parse(void);
static void syntaxError(char *message);
static void match(TokenType expected);


static char getNextChar(void)
{
	if (!(linepos<bufsize))
	{
		lineno++;
		if (fgets(lineBuf, BUFLEN - 1, source))
		{
			bufsize = strlen(lineBuf);
			linepos = 0;
			return lineBuf[linepos++];
		}
		else return EOF;
	}
	else return lineBuf[linepos++];
}

static void ungetNextChar(void)
{
	linepos--;
}

static TokenType reservedLookup(char *s)
{
	int i;

	for (i = 0; i<6; i++)
		if (!strcmp(s, reservedWords[i].str))
			return reservedWords[i].tok;
	return ID;
}

void printToken(TokenType token, const char* tokenString)
{
	switch (token)
	{
	case ELSE:
	case IF:
	case INT:
	case RETURN:
	case VOID:
	case WHILE:
		break;
	case LT: 
		fprintf(output, "<\n");
		break;
	case LTEQ: 
		fprintf(output, "<=\n"); 
		break;
	case BG: 
		fprintf(output, ">\n");
		break;
	case BGEQ:
		fprintf(output, ">=\n"); 
		break;
	case ASSIGN: 
		fprintf(output, "=\n"); 
		break;
	case EQ: 
		fprintf(output, "==\n"); 
		break;
	case NEQ: 
		fprintf(output, "!=\n");
		break;
	case PLUS:
		fprintf(output, "+\n"); 
		break;
	case MINUS: 
		fprintf(output, "-\n"); 
		break;
	case TIMES:
		fprintf(output, "*\n"); 
		break;
	case OVER: 
		fprintf(output, "/\n"); 
		break;
	case LPAREN: 
		fprintf(output, "(\n");
		break;
	case RPAREN:
		fprintf(output, ")\n"); 
		break;
	case SEMICOL: 
		fprintf(output, ";\n"); 
		break;
	case COL: 
		fprintf(output, ",\n"); 
		break;
	case LBRACE: 
		fprintf(output, "{\n"); 
		break;
	case RBRACE: 
		fprintf(output, "}\n"); 
		break;
	case LBRACKET: 
		fprintf(output, "[\n"); 
		break;
	case RBRACKET: 
		fprintf(output, "]\n");
		break;
	case ENDFILE: 
		fprintf(output, "EOF\n");
		break;
	case NUM:
		fprintf(output, "NUM = %s\n", tokenString);
		break;
	case ID:
		fprintf(output, "ID = %s\n", tokenString);
		break;
	case ERROR:
		fprintf(output, "ERROR: %s\n", tokenString);
		break;
	}
}

TreeNode *newStmtNode(StmtKind kind)
{
	TreeNode *t = (TreeNode*)malloc(sizeof(TreeNode));

	int i;

	if (t == NULL)
		fprintf(output, "Out of memory at line %d.\n", lineno);
	else
	{
		for (i = 0; i < MAXCHILDREN; ++i) t->child[i] = NULL;
		t->sibling = NULL;
		t->lineno = lineno;
		t->nodekind = StmtK;
		t->kind.stmt = kind;
	}
	return t;
}

TreeNode *newExpNode(ExpKind kind)
{
	TreeNode *t = (TreeNode*)malloc(sizeof(TreeNode));
	int i;

	if (t == NULL)
		fprintf(output, "Out of memory at line %d.\n", lineno);
	else
	{
		for (i = 0; i < MAXCHILDREN; ++i) t->child[i] = NULL;
		t->sibling = NULL;
		t->lineno = lineno;
		t->nodekind = ExpK;
		t->kind.exp = kind;
	}
	return t;
}

TreeNode *newDecNode(DecKind kind)
{
	TreeNode *t = (TreeNode*)malloc(sizeof(TreeNode));
	int i;

	if (t == NULL)
		fprintf(output, "*** Out of memory at line %d.\n", lineno);
	else
	{
		for (i = 0; i < MAXCHILDREN; ++i) t->child[i] = NULL;
		t->sibling = NULL;
		t->lineno = lineno;
		t->nodekind = DecK;
		t->kind.dec = kind;
	}
	return t;
}

char* copyString(char * s)
{
	int n;
	char * t;

	if (s == NULL) return NULL;
	n = strlen(s) + 1;
	t = (char*)malloc(n*sizeof(char));
	if (t == NULL)
		fprintf(output, "Out of memory error at line %d\n", lineno);
	else strcpy(t, s);
	return t;
}

static void printSpace(void)
{
	int i;

	for (i = 0; i<indentno; i++)
		fprintf(output, " ");
}

char *typeName(ExpType type)
{
	static char i[] = "int";
	static char v[] = "void";
	static char unknown[] = "unknown type";

	switch (type)
	{
	case Integer: return i; break;
	case Void:    return v; break;
	default:      return unknown;
	}
}

void printTree(TreeNode * tree)
{
	int i;

	INDENT;

	while (tree != NULL)
	{
		printSpace();

		if (tree->nodekind == DecK)
		{
			switch (tree->kind.dec)
			{
			case ScalarK:
				fprintf(output, "scalar: %s type: %s\n", tree->name, typeName(tree->varType));
				break;
			case ArrK:
				fprintf(output, "array: %s size: %d type: %s\n", tree->name, tree->val, typeName(tree->varType));
				break;
			case FuncK:
				fprintf(output, "function: %s() return type: %s \n", tree->name, typeName(tree->funcType));
				break;
			default:
				fprintf(output, "unknown ExpNode kind\n");
				break;
			}
		}
		else if (tree->nodekind == ExpK)
		{
			switch (tree->kind.exp)
			{
			case OpK:
				fprintf(output, "Op: ");
				printToken(tree->op, "\0");
				break;
			case IdK:
				fprintf(output, "Id: %s", tree->name);
				fprintf(output, "\n");
				break;
			case ConstK:
				fprintf(output, "const: %d\n", tree->val);
				break;
			case AssignK:
				fprintf(output, "assign: \n");
				break;
			default:
				fprintf(output, "unknown ExpNode kind\n");
				break;
			}
		}
		else if (tree->nodekind == StmtK)
		{
			switch (tree->kind.stmt)
			{
			case CompoundK:
				fprintf(output, "Compound\n");
				break;
			case IfK:
				fprintf(output, "If\n");
				break;
			case WhileK:
				fprintf(output, "While\n");
				break;
			case ReturnK:
				fprintf(output, "Return\n");
				break;
			case CallK:
				fprintf(output, "Call %s\n", tree->name);
				break;
			default:
				fprintf(output, "unknown ExpNode kind\n");
				break;
			}
		}
		else
			fprintf(output, "Unknown node kind>\n");
		for (i = 0; i<MAXCHILDREN; ++i)
			printTree(tree->child[i]);
		tree = tree->sibling;
	}
	UNINDENT;
}

TokenType getToken(void)
{

	int tokenStringIndex = 0;		
	TokenType currentToken;		
	StateType state = START;		

	int save;

	while (state != DONE)
	{
		char c = getNextChar();
		save = TRUE;
		switch (state)
		{
		case START:
			if (isdigit(c))
				state = INNUM;
			else if (isalpha(c))
				state = INID;
			else if (c == '/')
			{
				save = FALSE;
				state = INCOMMENTA;
			}
			else if ((c == ' ') || (c == '\t') || (c == '\n'))
				save = FALSE;
			else if (c == '!')
			{
				state = INNEQ;
			}
			else if (c == '<')
			{
				save = FALSE;
				state = INCOMPAREL;
			}
			else if (c == '>')
			{
				save = FALSE;
				state = INCOMPAREB;
			}
			else if (c == '=')
			{
				save = FALSE;
				state = INCOMPAREE;
			}
			else
			{
				state = DONE;
				switch (c)
				{
				case EOF:
					save = FALSE;
					currentToken = ENDFILE;
					break;
				case '+':
					currentToken = PLUS;
					break;
				case '-':
					currentToken = MINUS;
					break;
				case '*':
					currentToken = TIMES;
					break;
				case '(':
					currentToken = LPAREN;
					break;
				case ')':
					currentToken = RPAREN;
					break;
				case '{':
					currentToken = LBRACE;
					break;
				case '}':
					currentToken = RBRACE;
					break;
				case '[':
					currentToken = LBRACKET;
					break;
				case ']':
					currentToken = RBRACKET;
					break;
				case ';':
					currentToken = SEMICOL;
					break;
				case ',':
					currentToken = COL;
					break;
				default:
					currentToken = ERROR;
					break;
				}
			}
			break;
		case INCOMMENTA:
			if (c == '*') { state = INCOMMENTB; save = FALSE; }
			else
			{
				ungetNextChar();
				save = TRUE;
				state = DONE;
				currentToken = OVER;
			}
			break;
		case INCOMMENTB:
			save = FALSE;
			if (c == '*') state = INCOMMENTC;
			else
				state = INCOMMENTB;
			break;
		case INCOMMENTC:
			save = FALSE;
			if (c == '*') state = INCOMMENTC;
			else if (c == '/')
				state = START;
			else
				state = INCOMMENTB;
			break;
		case INNEQ:
			state = DONE;
			if (c == '=')
				currentToken = NEQ;
			else
			{
				ungetNextChar();
				save = FALSE;
				currentToken = ERROR;
			}
			break;
		case INCOMPAREL:
			state = DONE;
			if (c == '=')
				currentToken = LTEQ;
			else
			{
				ungetNextChar();
				currentToken = LT;
			}
			break;
		case INCOMPAREB:
			state = DONE;
			if (c == '=')
				currentToken = BGEQ;
			else
			{
				ungetNextChar();
				currentToken = BG;
			}
			break;
		case INCOMPAREE:
			state = DONE;
			if (c == '=')
				currentToken = EQ;
			else
			{
				ungetNextChar();
				currentToken = ASSIGN;
			}
			break;
		case INNUM:
			if (!isdigit(c))
			{
				ungetNextChar();
				save = FALSE;
				state = DONE;
				currentToken = NUM;
			}
			break;
		case INID:
			if (!isalpha(c))
			{
				ungetNextChar();
				save = FALSE;
				state = DONE;
				currentToken = ID;
			}
			break;
		case DONE:
		default:
			fprintf(output, "Scanner Bug: state= %d\n", state);
			state = DONE;
			currentToken = ERROR;
			break;
		}
		if ((save) && (tokenStringIndex <= MAXTOKENLEN))
			tokenString[tokenStringIndex++] = c;
		if (state == DONE)
		{
			tokenString[tokenStringIndex] = '\0';
			if (currentToken == ID)
				currentToken = reservedLookup(tokenString);
		}
	}
	return currentToken;
}

static void syntaxError(char *message)
{
	fprintf(output, "\n>>>");
	fprintf(output, "Syntax error at line %d : %s", lineno, message);
	Error = TRUE;
}

static void match(TokenType expected)
{
	if (token == expected)
		token = getToken();
	else
	{
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
	}
}

static ExpType type_specifier(void)
{
	ExpType type;

	if (token == INT)
	{
		type = Integer;
		token = getToken();
	}
	else if (token == VOID)
	{
		type = Void;
		token = getToken();
	}
	else
	{
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
	}

	return type;
}

TreeNode *declaration_list(void)
{
	TreeNode *t = declaration();
	TreeNode *p = t;

	while (token != ENDFILE)
	{
		TreeNode *q;
		q = declaration();

		if ((q != NULL) && (p != NULL))
		{
			p->sibling = q;
			p = q;
		}
	}
	return t;
}

TreeNode *declaration(void)
{
	TreeNode *t = NULL;
	ExpType declare;
	char *idName;

	declare = type_specifier();
	idName = copyString(tokenString);

	match(ID);

	switch (token)
	{
	case SEMICOL:		
		t = newDecNode(ScalarK);
		if (t != NULL)
		{
			t->varType = declare;
			t->name = idName;
		}
		match(SEMICOL);
		break;
	case LBRACKET:		
		t = newDecNode(ArrK);
		if (t != NULL)
		{
			t->varType = declare;
			t->name = idName;
		}
		match(LBRACKET);
		if (t != NULL)
			t->val = atoi(tokenString);
		match(NUM);
		match(RBRACKET);
		match(SEMICOL);
		break;
	case LPAREN: t = fun_declaration(declare, idName); break;
	default:
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
		break;
	}
	return t;
}

TreeNode *var_declaration(void)
{
	TreeNode  *t = NULL;
	ExpType declare;
	char *idName;

	declare = type_specifier();

	idName = copyString(tokenString);
	match(ID);

	switch (token)
	{
	case SEMICOL:	
		t = newDecNode(ScalarK);
		if (t != NULL)
		{
			t->varType = declare;
			t->name = idName;
		}
		match(SEMICOL);
		break;
	case LBRACKET:		
		t = newDecNode(ArrK);
		if (t != NULL)
		{
			t->varType = declare;
			t->name = idName;
		}
		match(LBRACKET);
		if (t != NULL) t->val = atoi(tokenString);
		match(NUM);
		match(RBRACKET);
		match(SEMICOL);
		break;
	default:
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
		break;
	}
	return t;
}

TreeNode *fun_declaration(ExpType type, char * id)
{
	TreeNode  *t = NULL;
	ExpType declare;
	char *idName;

	declare = type;
	idName = id;

	switch (token)
	{
	case LPAREN:
		t = newDecNode(FuncK);
		if (t != NULL)
		{
			t->funcType = declare;
			t->name = idName;
		}
		match(LPAREN);
		if (t != NULL) t->child[0] = params();
		match(RPAREN);
		if (t != NULL) t->child[1] = compound_stmt();
		break;
	default:
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
		break;
	}
	return t;
}

TreeNode *params(void)
{
	TreeNode *t = NULL;

	if (token == VOID)
	{
		match(VOID);
		return NULL;
	}
	else
		t = param_list();
	return t;
}

TreeNode *param_list(void)
{
	TreeNode *t = param();
	TreeNode *p = t;
	TreeNode *q;

	while ((t != NULL) && (token == COL))
	{
		match(COL);
		q = param();
		if (q != NULL)
		{
			p->sibling = q;
			p = q;
		}
	}
	return t;
}

TreeNode *param(void)
{
	TreeNode *t;
	ExpType param;
	char *idName;

	param = type_specifier();
	idName = copyString(tokenString);
	match(ID);

	if (token == LBRACKET)
	{
		match(LBRACKET);
		match(RBRACKET);
		t = newDecNode(ArrK);
	}
	else
		t = newDecNode(ScalarK);

	if (t != NULL)
	{
		t->name = idName;
		t->val = 0;
		t->varType = param;
	}
	return t;
}

TreeNode *compound_stmt(void)
{
	TreeNode *t = NULL;

	match(LBRACE);

	if ((token != RBRACE) && (t = newStmtNode(CompoundK)))
	{
		if ((token == INT) || (token == VOID))
			t->child[0] = local_declarations();
		if (token != RBRACE)
			t->child[1] = statement_list();
	}

	match(RBRACE);
	return t;
}

TreeNode *local_declarations(void)
{
	TreeNode *t;
	TreeNode *p;
	TreeNode *q;

	if ((token == INT) || (token == VOID))
		t = var_declaration();

	if (t != NULL)
	{
		p = t;

		while ((token == INT) || (token == VOID))
		{
			q = var_declaration();
			if (q != NULL)
			{
				p->sibling = q;
				p = q;
			}
		}
	}
	return t;
}

TreeNode *statement_list(void)
{
	TreeNode *t = statement();
	TreeNode *p = t;
	TreeNode *q;

	while (token != RBRACE)
	{
		q = statement();
		if ((p != NULL) && (q != NULL))
		{
			p->sibling = q;
			p = q;
		}
	}
	return t;
}

TreeNode *statement(void)
{
	TreeNode *t = NULL;

	switch (token)
	{
	case ID:
	case SEMICOL:
	case LPAREN:
	case NUM: t = expression_stmt(); break;
	case LBRACE: t = compound_stmt(); break;
	case IF: t = selection_stmt(); break;
	case WHILE: t = iteration_stmt(); break;
	case RETURN: t = return_stmt(); break;
	default:
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
		break;
	}
	return t;
}

TreeNode *expression_stmt(void)
{
	TreeNode *t = NULL;

	if (token == SEMICOL)
		match(SEMICOL);
	else if (token != RBRACE)
	{
		t = expression();
		match(SEMICOL);
	}
	return t;
}

TreeNode *selection_stmt(void)
{
	TreeNode *t = newStmtNode(IfK);

	match(IF);
	match(LPAREN);
	if (t != NULL) t->child[0] = expression();
	match(RPAREN);
	if (t != NULL) t->child[1] = statement();

	if (token == ELSE)
	{
		match(ELSE);
		if (t != NULL)
			t->child[2] = statement();
	}
	return t;
}

TreeNode *iteration_stmt(void)
{
	TreeNode *t = newStmtNode(WhileK);

	match(WHILE);
	match(LPAREN);
	t->child[0] = expression();
	match(RPAREN);
	t->child[1] = statement();

	return t;
}

TreeNode *return_stmt(void)
{
	TreeNode *t = newStmtNode(ReturnK);

	match(RETURN);
	if (token == SEMICOL)
		match(SEMICOL);
	else if (t != NULL)
	{
		t->child[0] = expression();
		match(SEMICOL);
	}
	return t;
}

TreeNode *expression(void)
{
	TreeNode *t = NULL;
	TreeNode *right = NULL;
	TreeNode *left = NULL;

	if (token == ID)
	{
		left = call();
	}

	if (token == ASSIGN)
	{
		if (left != NULL)
		{
			match(ASSIGN);
			right = expression();
			t = newExpNode(AssignK);
			if (t != NULL)
			{
				t->child[0] = left;
				t->child[1] = right;
			}
		}
		else
		{
			syntaxError("unexpected token ");
			token = getToken();
		}
	}
	else
		t = simple_expression(left);

	return t;
}

TreeNode *simple_expression(TreeNode *next)
{
	TreeNode *t = additive_expression(next);

	if ((token == LT) || (token == LTEQ) || (token == BG) || (token == BGEQ) || (token == EQ) || (token == NEQ))
	{
		TreeNode *p = newExpNode(OpK);

		if (p != NULL)
		{
			p->child[0] = t;
			p->op = token;
			t = p;
		}
		match(token);
		if (t != NULL)
			t->child[1] = additive_expression(NULL);

	}
	return t;
}

TreeNode *additive_expression(TreeNode *next)
{
	TreeNode *t = term(next);
	while ((token == PLUS) || (token == MINUS))
	{
		TreeNode *p = newExpNode(OpK);
		if (p != NULL)
		{
			p->child[0] = t;
			p->op = token;
			t = p;
			match(token);
			t->child[1] = term(NULL);
		}
	}
	return t;
}

TreeNode *term(TreeNode *next)
{
	TreeNode *t = factor(next);

	while ((token == TIMES) || (token == OVER))
	{
		TreeNode *p = newExpNode(OpK);
		if (p != NULL)
		{
			p->child[0] = t;
			p->op = token;
			t = p;
			match(token);
			p->child[1] = factor(NULL);
		}
	}
	return t;
}

TreeNode *factor(TreeNode *next)
{
	TreeNode *t = NULL;

	if (next != NULL)
		return next;

	switch (token)
	{
	case NUM:
		t = newExpNode(ConstK);
		if ((t != NULL) && (token == NUM))
			t->val = atoi(tokenString);
		match(NUM);
		break;

	case ID:
		t = call();
		break;

	case LPAREN:
		match(LPAREN);
		t = expression();
		match(RPAREN);
		break;

	default:
		syntaxError("unexpected token ");
		printToken(token, tokenString);
		fprintf(output, "\n");
		token = getToken();
		break;
	}
	return t;
}

TreeNode *call(void)
{
	TreeNode *t = NULL;
	TreeNode *p = NULL;
	TreeNode *q = NULL;
	char *idName;

	if (token == ID)
		idName = copyString(tokenString);
	match(ID);

	if (token == LPAREN)
	{
		match(LPAREN);
		q = args();
		match(RPAREN);

		t = newStmtNode(CallK);
		if (t != NULL)
		{
			t->child[0] = q;
			t->name = idName;
		}
	}

	else
	{
		if (token == LBRACKET)
		{
			match(LBRACKET);
			p = expression();
			match(RBRACKET);
		}

		t = newExpNode(IdK);
		if (t != NULL)
		{
			t->child[0] = p;
			t->name = idName;
		}
	}

	return t;
}

TreeNode *args(void)
{
	TreeNode *t = NULL;

	if (token != RPAREN)
		t = arg_list();
	return t;
}

TreeNode *arg_list(void)
{
	TreeNode *t = expression();
	TreeNode *p = t;
	TreeNode *q;

	while (token == COL)
	{
		match(COL);
		q = expression();
		if ((p != NULL) && (t != NULL))
		{
			p->sibling = q;
			p = q;
		}
	}
	return t;
}

TreeNode *parse(void)
{
	TreeNode *t;
	token = getToken();
	t = declaration_list();
	if (token != ENDFILE)
		syntaxError("Code ends before file\n");
	return t;
}

int main(int argc, char *argv[])
{
	char sourceFileName[20];
	TreeNode *syntaxTree;
	if (argc != 3)
	{
		fprintf(stderr, "usage : %s <C source> <Text File>", argv[0]);
		exit(1);
	}
	strcpy(sourceFileName, argv[1]);

	source = fopen(sourceFileName, "r");
	if (source == NULL)
	{
		fprintf(stderr, "File %s not found\n", sourceFileName);
		exit(1);
	}
	output = fopen(argv[2], "w");

	syntaxTree = parse();

	printTree(syntaxTree);

	return 0;
}
