extern void print_int(int a);

int list[10];
int main(void){
    int i,sum;

    for(i = 0; i<10; i=i+1){
        list[i] = i;
        print_int(list[i]);
    }

    i = 0;
    while(i <= 9){
        sum = sum + list[i];
    }

    return sum;
}
