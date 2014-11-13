extern void print_int(int a);

int getInt(void){
    return 5;
}

void main(void){
    int elephant, i;
    for(elephant = 0; elephant < getInt(); elephant = elephant + 2){
        for(i = 0; i<elephant; i = i + 1){
            print_int(i);
        }
    }
}
