#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#pragma warning(disable : 4996)

#define buf 1024
#define RESERVEDWORD_NUM 6

#define ST 1
#define COM 2
#define EQ 3
#define NOT 4
#define ID 5
#define NUM 6
#define C1 7
#define C2 8
#define C3 9
#define FIN 10
#define ERR 11

int accept_state[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,0};

char* reserve[6] = { "else", "if", "int", "return", "void", "while" };
char* tokenType[5] = { "reserved word", "ID, name", "NUM, val", "ERROR", "" };

int trans_state[12][10] =
{
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ FIN, NUM, ID, C1, FIN, COM, COM, NOT, ERR, ST }, //start
	{ FIN, FIN, FIN, FIN, FIN, FIN, EQ, FIN, FIN, FIN,}, //comp
	{ FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN}, //equal
	{ ERR, ERR, ERR, ERR, ERR, ERR, FIN, ERR, ERR, ERR }, //diffent_comp
	{ FIN, FIN, ID, FIN, FIN, FIN, FIN, FIN, FIN, FIN}, //Identifier
	{ FIN, NUM, FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN}, //Number
	{ FIN, FIN, FIN, FIN, C3, FIN, FIN, FIN, FIN, FIN }, //Comment1
	{ C2, C2, C2, C2, C3, C2, C2, C2, C2, C2 }, //Comment2
	{ C2, C2, C2, ST, C3, C2, C2, C2, C2, C2 }, //CommentEQUAL
	{ FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN, FIN }, //finish
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }  //error
};

int look_ahead_T[12][10] =
{
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, //start
	{ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 }, //comp1
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, //comp2
	{ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 }, //comp3
	{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 }, //Identifier
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, //Number
	{ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 }, //Comment1
	{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, //Comment2
	{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, //Comment3
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, //finish
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }  //error
};

int check_reserved_word(char token[buf]);
int find_symbol_type(char);
int check_token_type(char token[buf]);
void get_token(int *i);

char line[buf];
char token[buf][buf] = { 0, };
int type[buf] = { 0, };
int line_cnt, buffer_length, cnt;

int find_symbol_type(char ch)
{
	if (ch == '+' || ch == '-' || ch == ';' || ch == ',' || ch == '(' || ch == ')' 
		|| ch == '[' || ch == ']' || ch == '{' || ch == '}')
		return 0;
	else if (ch >= '0' && ch <= '9')
		return 1;
	else if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
		return 2;
	else if (ch == '/')
		return 3;
	else if (ch == '*')
		return 4;
	else if (ch == '<' || ch == '>')
		return 5;
	else if (ch == '=')
		return 6;
	else if (ch == '!')
		return 7;
	else if (ch == ' ' || ch == '\t' || ch == '\n')
		return 9;
	else
		return 8;
}

int check_token_type(char token[buf])
{
	int one_state; 

	one_state = trans_state[1][find_symbol_type(token[0])];

	if (one_state == 5 && check_reserved_word(token)) //reserve
		return 0;
	else if (one_state == 5) //id
		return 1;
	else if (one_state == 6) //num
		return 2;
	else if (one_state == 11 || (one_state == 5 && strlen(token)<2))
		return 3;
	else
		return 4; //nothing
}

int check_reserved_word(char token[buf])
{
	int i = 0;
	for (i = 0; i<RESERVEDWORD_NUM; i++){
		if (strcmp(reserve[i], token) == 0)
			return 1;
	}
	return 0;
}
void get_token(int *i){

	int t = 0;
	int ts; //symbol type저장
	static int state = 1, new_state = 10, state_temp;
	static char temp[buf] = { 0, };

	if (new_state == 10 || new_state == 11)
		state = 1;
	else 
		state = state_temp;

	while ((state != 11 && !accept_state[state]) && buffer_length >= *i){

		if (line[*i] == 0)//주석이 처리부분 -> 여러문장에 걸쳐 있을 경우!
			state_temp = new_state;

		ts = find_symbol_type(line[*i]);//symbol type저장
		new_state = trans_state[state][ts];//옮겨갈 state지정

		if (look_ahead_T[state][ts])  // look_ahead로 보기
		{
			temp[t++] = line[*i];
			*i +=  1;
			
			if (new_state == 1){
				t = 0; 
			}
		}
		state = new_state;
	}

	temp[t] = 0;

	if (accept_state[state] || state == 11){ 		
		strcpy(&token[cnt][0], temp); //token을 저장하면서 type을 정해준다
		
		if (state == 11)
			type[cnt] = 3; //error 
		else
			type[cnt] = check_token_type(token[cnt]);
		cnt++;
	}
}


int main(int argc, char *argv[])
{
	FILE *source_file, *target_file;
	int len, j, k = 0;

	if (argc != 3){
		fprintf(stderr, "실행형식이 잘못되었습니다\n");
		return -1;
	}
	
	if (!(source_file = fopen(argv[1], "r"))){
		fprintf(stderr, "source file open fail!");
		return -1;
	}
	if (!(target_file = fopen(argv[2], "w"))){
		fprintf(stderr, "target file open fail");
		return -1;
	}
	
	while ( fgets(line, buf, source_file) ){

		line_cnt++; 
		len = 0;
		cnt = 0; 

		fprintf(target_file, "%d : %s\n", line_cnt, line);
		buffer_length = strlen(line);

		line[buffer_length] = 0;

		while (buffer_length > len)
			get_token(&len);

		for (len = 0; len<cnt; len++)
		{
			if (token[len][0] != 0){

				fprintf(target_file, "      %d :%-15s:\t%s\n", line_cnt, tokenType[type[len]], token[len]);
			}
		}
	}
	return 0;
}