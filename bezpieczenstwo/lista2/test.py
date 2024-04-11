import subprocess
import os
import random

MIN_LEN = 4
MAX_LEN = 15
TIME_LIMIT = 60

D_CHARSET = '123456789'
L_CHARSET = 'qwertyuiopasdfghjklzxcvbnm'
U_CHARSET = 'QWERTYUIOPASDFGHJKLZXCVBNM'
DL_CHARSET = D_CHARSET + L_CHARSET
S_CHARSET = "!#$%&()*+,-./:;<=>?@[\]^_{|}~"

CHARSET_LIST = [D_CHARSET, DL_CHARSET]
REPS = len(CHARSET_LIST)

COMMAND = [
        "hashcat",
        f"--runtime={TIME_LIMIT}",
        "--quiet",
        "-m", "100",
        "-a", "ATTACK_TYPE",
        "haslo.sha1",
    ]

def print_answer(a):
    if(len(a) == 0):
        print(f"{a} timeout")
        exit()
    else:
        print(f"cracked: {a}")

def generate_password(len,charset):
    pas = ""
    mask = ""
    for _ in range(len):
        c = random.choice(charset)
        pas += c

        if(c in D_CHARSET):
            mask += "?d"
        elif(c in S_CHARSET):
            mask += "?s"
        elif(c in L_CHARSET):
            mask += "?l"
        elif(c in U_CHARSET):
            mask += "?u"
    
    return pas,mask

def bruteforce():
    global COMMAND
    COMMAND[COMMAND.index("ATTACK_TYPE")] = "3"
    for l in range(MIN_LEN,MAX_LEN):
        for r in range(REPS):
            pas,mask = generate_password(l, CHARSET_LIST[r])
            print(f"{pas} cracking in progress")
            os.system(f"echo -n '{pas}'" + "| sha1sum | awk '{print $1}' > haslo.sha1")
            COMMAND += [mask]
            answer = subprocess.run(COMMAND,capture_output=True, text=True).stdout
            COMMAND.pop()
            
            print_answer(answer)


def dict_attack():
    global COMMAND
    COMMAND[COMMAND.index("ATTACK_TYPE")] = "6"
    with open('rockyou.txt', 'r', encoding='latin-1') as f:
        content = f.readlines()

    suff = random.choice(S_CHARSET) + random.choice(D_CHARSET)
    mask = "?s?d"
    pas = ''.join(list(random.choice(content).rstrip())) + suff
    os.system(f"echo -n '{pas}'" + "| sha1sum | awk '{print $1}' > haslo.sha1")

    print(f"{pas} cracking in progress")
    COMMAND += ["rockyou.txt",mask]
    answer = subprocess.run(COMMAND,capture_output=True, text=True).stdout
    COMMAND.pop()
      
    print_answer(answer)
    

if __name__ == "__main__":
    #bruteforce()
    dict_attack()
