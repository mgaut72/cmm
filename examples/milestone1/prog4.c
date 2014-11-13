extern void print_int(int x);
extern void print_string(char x[]);

void main(void)
{
  char c;
  int x;
  
  x = 999999999;
  c = x;
  print_string("Should get -1 due to overflow in char\n");
  print_int(c);
  print_string("\n");
}
