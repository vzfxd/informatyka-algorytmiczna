%{
#include<iostream>
#include <string>
#define P_BASE 1234577

using namespace std;

string onp = "";
int err = 0;
string err_msg = "";

int yylex();
int yyparse();

int yyerror(string s) {
    return 0;
}

int euklides(int a, int P, int &x, int &y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return P;
    }

    int x1, y1;
    int gcd = euklides(P % a, a, x1, y1);

    x = y1 - (P / a) * x1;
    y = x1;

    return gcd;
}

int sub_p(int a, int b, int P){
	b = P - b;
	return (a + b) % P;
}

int div_p(int a, int b, int P){
	int x,y;
	int gcd = euklides(b,P,x,y);
	if(gcd!=1){
        err = 1;
        err_msg = "Brak odwrotności liczby " + to_string(b);
	}
	if(x<0){
		x += P;
	}
	return (a * x) % P;
}

int pow_p(int base, int exponent, int P) {
    if (exponent == 0) {
        return 1;
    }

    long long result = 1;
    long long base_mod = base % P;

    while (exponent > 0){

        if (exponent % 2 == 1){
            result = (result * base_mod) % P;
        }

        exponent /= 2;

        base_mod = (base_mod * base_mod) % P;
    }

    return static_cast<int>(result);
}


%}

%token 		NUM ERR
%left 		'+' '-'
%left 		'*' '/'
%precedence 	NEG
%nonassoc 	'^'


%%

input:
    %empty
    | line input

line: 
    exp '\n' {
        if(err == 0){
            cout << onp << endl;
            cout << "Wynik: " << $$ << endl;
        }else{
            cout << "Error: " << err_msg << endl;
        }
        err = 0;
        onp = "";
        err_msg = "";
    }
    | error '\n' {
        cout << "Error: Syntax Erorr" << endl;
        err = 0;
        onp = "";
        err_msg = "";
    }

exp: 
    NUM         	        { 	onp += to_string($1) + " "; $$ = $1 % P_BASE;	        }
    |'-' exp %prec NEG      {   onp += "~ ";$$ = (P_BASE - $2) % P_BASE;                }
    | exp '/' exp 	        { 	onp += "/ ";$$ = div_p($1,$3,P_BASE);	                }
    | exp '-' exp 	        { 	onp += "- ";$$ = sub_p($1,$3,P_BASE);	                }
    | exp '+' exp 	        { 	onp += "+ ";$$ = ($1 + $3) % P_BASE; 	                }
    | exp '*' exp 	        { 	onp += "* ";$$ = ($1 * $3) % P_BASE;	                }
    | '(' exp ')' 	        {	$$ = $2;				                                } 
    | exp '^' exp_pow       {   onp += "^ ";$$ = pow_p($1,$3,P_BASE);                   }

exp_pow:
    NUM         	            { 	onp += to_string($1) + " "; $$ = $1 % (P_BASE-1);	        }
    |'-' exp_pow %prec NEG      {   onp += "~ ";$$ = (P_BASE-1) - $2 % (P_BASE-1);              }
    | exp_pow '/' exp_pow 	    { 	onp += "/ ";$$ = div_p($1,$3,P_BASE-1);	                    }
    | exp_pow '-' exp_pow 	    { 	onp += "- ";$$ = sub_p($1,$3,P_BASE-1);	                    }
    | exp_pow '+' exp_pow 	    { 	onp += "+ ";$$ = ($1 + $3) % (P_BASE-1); 	                }
    | exp_pow '*' exp_pow 	    { 	onp += "* ";$$ = ($1 * $3) % (P_BASE-1);	                }
    | '(' exp_pow ')' 	        {	$$ = $2;				                                    } 
%%

int main(){
    return yyparse();
}
