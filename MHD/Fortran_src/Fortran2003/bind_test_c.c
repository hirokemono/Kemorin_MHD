#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int lengthchara_f();
int num_base_fields_f();
void set_base_field_names_f(char *names);

int main(int argc, char **argv)
{
	int i;
	char **names;
	
	int len_f = lengthchara_f();
	int nword = num_base_fields_f();
	char *names1 = (char *)calloc(len_f*nword, sizeof(char));
	
	if ((names = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for names\n");
		exit(0);
	}
	
	set_base_field_names_f(names1);
	
	printf("nword %d %d \n", len_f, nword);
	for(i=0;i<nword;i++){
		names[i] = (char *)calloc(strlen(&names1[len_f*i])+1, sizeof(char));
		strcpy(names[i], &names1[len_f*i]);
		printf("name: %d: %d: %s\n", i, (int) strlen(names[i]), names[i]);
	}
	
	return 0;
};