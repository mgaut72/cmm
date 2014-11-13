extern void print_int(int x);
extern void print_string(char x[]);

void main(void)
{
  int x;
  
  x = 999999999;
  print_string("Should get 999999999\n");
  print_int(x);
  print_string("\n");
}
