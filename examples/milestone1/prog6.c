extern void print_int(int x);
extern void print_string(char x[]);

void main(void)
{
  int num;

  num=11111111111;
  print_string("Should get some negative number\n");
  print_int(num);
  print_string("\n");
}
