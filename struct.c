#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct sample {
    float f;
    int i;
    char s[80];
    void *p;
};

struct sample *get_sample() {
    struct sample *example = malloc(sizeof(*example));
    if(example == NULL) {
	printf("Cannot create sample.\n");
    } else {
	example->f = 3.14159;
	example->i = 1024;
	strcpy(example->s, "Hello, CFFI.\n");
	example->p = example;
    }
    return example;
}

void free_sample(struct sample *example) {
    free(example);
}
