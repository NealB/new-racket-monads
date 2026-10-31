#include <stdio.h>
/*#include <string.h>*/
#include <stdlib.h>

typedef  int zuo_t;

typedef struct {
  zuo_t obj;
  zuo_t *car;
  zuo_t *cdr;
} zuo_pair_t;

#define ZUO_CAR(p) (((zuo_pair_t *)(p))->car)
#define ZUO_CDR(p) (((zuo_pair_t *)(p))->cdr)

#define PAIR_MAGIC 42

int main(int argc, char **argv)
{
	long num = atoi(argv[1]);
	printf("num -> %d\n", num);

	zuo_pair_t *newpr = (zuo_pair_t*)malloc(sizeof(zuo_pair_t));
	newpr->obj = PAIR_MAGIC;
	ZUO_CAR(newpr) = (zuo_t *)1;
	ZUO_CDR(newpr) = (zuo_t *)2;
	if(ZUO_CAR(newpr) == (zuo_t *)num) {
		printf("#t\n");
	}
	else {
		printf("#f\n");
	}

	return 0;
}

