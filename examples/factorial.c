int fact(int x);
extern void print_int(int x);

int main(void){
    int i,x;

    x = 5;
    i = 0;
    print_int(fact(x));
    return i;
}

int fact(int x){
    if (x == 0)
        return 1;
    else
        return x * fact(x - 1);
}
