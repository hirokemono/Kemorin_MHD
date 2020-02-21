#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int lengthchara_f();
int num_diff_filtered_forces_f();
void diff_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);

int main(int argc, char **argv)
{
	int i;
	char **names;
	char **maths;
	
	int len_f = lengthchara_f();
	int nword = num_diff_filtered_forces_f();
	
	int *ncomp = (int *)calloc(nword, sizeof(int));
	char *name1 = (char *)calloc(len_f*nword, sizeof(char));
	char *math1 = (char *)calloc(len_f*nword, sizeof(char));
	
	if ((names = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for names\n");
		exit(0);
	}
	if ((maths = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for maths\n");
		exit(0);
	}
	
	diff_filtered_force_labels_f(ncomp, name1, math1);
	
	printf("nword %d %d \n", len_f, nword);
	for(i=0;i<nword;i++){
		names[i] = (char *)calloc(strlen(&name1[len_f*i])+1, sizeof(char));
		maths[i] = (char *)calloc(strlen(&math1[len_f*i])+1, sizeof(char));
		strcpy(names[i], &name1[len_f*i]);
		strcpy(maths[i], &math1[len_f*i]);
		printf("name: %d: %d: %s: %s\n", i, ncomp[i], names[i], maths[i]);
	}
	
	return 0;
};