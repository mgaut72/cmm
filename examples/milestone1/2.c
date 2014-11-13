extern void print_string(char c[]);

void indirect2(char c[]);

void indirect1(char c[]){
    indirect2(c);
}

void main(void){
    indirect1("hello");
}

void indirect2(char c[]){
    print_string(c);
}

