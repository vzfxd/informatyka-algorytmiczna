from sly import Lexer, Parser
from calc_functions import *

class CalcLexer(Lexer):
    tokens = {NUM,ERROR}
    literals = {'+', '-', '*', '/', '(', ')','^'}
    
    ignore_spaces = r' '
    ignore_slash = r'\\\n'
    ignore_new_line = r'\n'
    ignore_comment = '^\#(.|\\\n)*\n'
    

 
    @_(r'\d+')
    def NUM(self, t):
        t.value = int(t.value)
        return t
    
    def error(self ,t):
        return t

class CalcParser(Parser):
    P_BASE = 1234577
    onp = ""
    err = 0

    tokens = CalcLexer.tokens

    precedence = (
        ('left', '+', '-'),
        ('left', '*', '/'),
        ('nonassoc', '^'),
        ('left', 'NEG')
    )
    
    def error(self, p):
        pass
    
    def error_p(self, msg="Syntax Error"):
        print(f"Error: {msg}")
        self.onp = ""
        raise RuntimeError

    @_('ERROR')
    def statement(self, p):
        self.error_p()
        
    @_('exp')
    def statement(self, p):
        print(self.onp)
        print(f"Wynik: {p.exp}")
        self.onp = ""
    
    #OPERACJE STANDARDOWE########################################
    @_('exp "^" exp_pow')
    def exp(self, p):
        self.onp += "^ "
        return pow_p(p.exp,p.exp_pow)
    
    @_('exp "/" exp')
    def exp(self, p):
        self.onp += "/ "
        result = div_p(p.exp0,p.exp1,self.P_BASE)
        if(result == -1):
            self.error_p(f"Brak liczby odwrotnej dla {p.exp1}")
        return result
    
    @_('exp "-" exp')
    def exp(self, p):
        self.onp += "- "
        return sub_p(p.exp0,p.exp1,self.P_BASE)
    
    @_('"(" exp ")"')
    def exp(self, p):
        return p.exp
    
    @_('"-" exp %prec NEG')
    def exp(self, p):
        self.onp += "~ "
        return -p.exp % self.P_BASE
    
    @_('exp "*" exp')
    def exp(self, p):
        self.onp += "* "
        return (p.exp0 * p.exp1) % self.P_BASE
    
    @_('exp "+" exp')
    def exp(self, p):
        self.onp += "+ "
        return (p.exp0 + p.exp1) % self.P_BASE
    
    @_('NUM')
    def exp(self, p):
        num = int(p.NUM)
        self.onp += f"{num} "
        return num % self.P_BASE
    ############################################## 

    #OPERACJE W WYKŁADNIKU
    @_('exp_pow "/" exp_pow')
    def exp_pow(self, p):
        self.onp += "/ "
        result = div_p(p.exp_pow0,p.exp_pow1,self.P_BASE-1)
        if(result == -1):
            self.error_p(f"Brak liczby odwrotnej dla {p.exp_pow1}")
        return result
    
    @_('exp_pow "-" exp_pow')
    def exp_pow(self, p):
        self.onp += "- "
        return sub_p(p.exp_pow0,p.exp_pow1,self.P_BASE-1)
    
    @_('"(" exp_pow ")"')
    def exp_pow(self, p):
        return p.exp_pow
    
    @_('"-" exp_pow %prec NEG')
    def exp_pow(self, p):
        self.onp += "~ "
        return -p.exp_pow % (self.P_BASE-1)
    
    @_('exp_pow "*" exp_pow')
    def exp_pow(self, p):
        self.onp += "* "
        return (p.exp_pow0 * p.exp_pow1) % (self.P_BASE-1)
    
    @_('exp_pow "+" exp_pow')
    def exp_pow(self, p):
        self.onp += "+ "
        return (p.exp_pow0 + p.exp_pow1) % (self.P_BASE-1)
    
    @_('NUM')
    def exp_pow(self, p):
        num = int(p.NUM)
        self.onp += f"{num} "
        return num % (self.P_BASE-1)
    ###############################################

if __name__ == '__main__':
    lexer = CalcLexer()
    parser = CalcParser()
    while True:
        text = ""
        try:
            while True:
                text += input()
                text += "\n"
                if not (text.endswith("\\\n")):
                    break
        except EOFError:
            break
        try:
            parser.parse(lexer.tokenize(text))
        except RuntimeError:
            continue