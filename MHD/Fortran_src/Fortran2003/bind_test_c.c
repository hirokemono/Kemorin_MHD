#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "m_field_name_from_f.h"

int main(int argc, char **argv)
{
	int i;
	struct field_names_f *fnames = init_field_name_f();
	
	printf("nword %d %d \n", fnames->len_f, fnames->ntot_fields);
	for(i=0;i<fnames->ntot_fields;i++){
		printf("name: %d: %d: %s: %s\n", i, fnames->num_comp[i],
			   fnames->field_name[i], fnames->field_math[i]);
	}
	printf("\n");
	
	dealloc_field_name_f(fnames);
	
	/*
	nword = num_advection_controls_f();
	
	ncomp = (int *)calloc(nword, sizeof(int));
	name1 = (char *)calloc(len_f*nword, sizeof(char));
	math1 = (char *)calloc(len_f*nword, sizeof(char));
	
	if ((names = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for names\n");
		exit(0);
	}
	if ((maths = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for maths\n");
		exit(0);
	}
	set_advection_control_labels_f(&ncomp[0], 
						   &name1[len_f*0],
						   &math1[len_f*0]);
	
	printf("nword %d %d \n", len_f, nword);
	for(i=0;i<nword;i++){
		names[i] = (char *)calloc(strlen(&name1[len_f*i])+1, sizeof(char));
		maths[i] = (char *)calloc(strlen(&math1[len_f*i])+1, sizeof(char));
		strcpy(names[i], &name1[len_f*i]);
		strcpy(maths[i], &math1[len_f*i]);
		printf("Advection name: %d: %d: %s: %s\n", i, ncomp[i], names[i], maths[i]);
	}
	printf("\n");
	
	for(i=0;i<nword;i++){
		free(names[i]);
		free(maths[i]);
	}
	free(names);
	free(maths);
	free(ncomp);
	free(name1);
	free(math1);
	
	*/
	/*
	nword = num_force_controls_f();
	
	ncomp = (int *)calloc(nword, sizeof(int));
	name1 = (char *)calloc(len_f*nword, sizeof(char));
	math1 = (char *)calloc(len_f*nword, sizeof(char));
	
	if ((names = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for names\n");
		exit(0);
	}
	if ((maths = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for maths\n");
		exit(0);
	}
	set_force_control_labels_f(&ncomp[0], 
						   &name1[len_f*0],
						   &math1[len_f*0]);
	
	printf("nword %d %d \n", len_f, nword);
	for(i=0;i<nword;i++){
		names[i] = (char *)calloc(strlen(&name1[len_f*i])+1, sizeof(char));
		maths[i] = (char *)calloc(strlen(&math1[len_f*i])+1, sizeof(char));
		strcpy(names[i], &name1[len_f*i]);
		strcpy(maths[i], &math1[len_f*i]);
		printf("Force name: %d: %d: %s: %s\n", i, ncomp[i], names[i], maths[i]);
	}
	printf("\n");
	
	for(i=0;i<nword;i++){
		free(names[i]);
		free(maths[i]);
	}
	free(names);
	free(maths);
	free(ncomp);
	free(name1);
	free(math1);
	*/
	return 0;
};