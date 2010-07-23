/* { dg-do run } */
/* { dg-require-effective-target tls } */
/* { dg-add-options tls } */
/* { dg-skip-if "PR44140" { *-*-* } { "-flto" "-fwhopr" } { "" } } */

extern void _exit(int);

static __thread int fstat ;

static __thread int fstat = 1;

static __thread int fstat ;

int test_code(int b)
{
  fstat += b ;
  return fstat;
}

int main (int ac, char *av[])
{
  int a = test_code(1);
  
  if ( a != 2 || fstat != 2 ) _exit (-(__LINE__)) ;
  
  return 0;
}
