extern void print_string(char c[]);
void indirect(char c[]);
void indirect2(char c[]);

void main(void){

    char c[3];

    c[0] = 'h';
    c[1] = 'i';
    c[2] = '\0';

    print_string(c);
    print_string("\n");
    indirect(c);
    print_string("\n");
    print_string(c);
    print_string("\n");
}

void indirect(char c[]){
    c[0] = 'f';
    print_string(c);
    indirect2(c);
}

void indirect2(char c[]){
    c[0] = 'p';
    print_string(c);
}
