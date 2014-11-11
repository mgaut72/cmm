extern void print_string(char c[]);
char c[5];

void main(void){

    c[0] = 'h';
    c[1] = 'e';
    c[2] = 'y';
    c[3] = '\n';
    c[4] = '\0';

    print_string("hey hey ");
    print_string(c);
}

