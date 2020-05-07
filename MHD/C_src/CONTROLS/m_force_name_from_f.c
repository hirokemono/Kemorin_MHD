/*
//  m_force_name_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "m_force_name_from_f.h"

int lengthchara_f();

int num_advection_controls_f();
int num_force_controls_f();
void set_advection_control_labels_f(int *ncomp1, char *name1, char *math1);
void set_force_control_labels_f(int *ncomp1, char *name1, char *math1);

struct advection_names_f * init_advection_name_f(){
	int i;
	char *packed_name;
	char *packed_math;
	
	struct advection_names_f *fnames;
	if((fnames = (struct advection_names_f *) malloc(sizeof(struct advection_names_f))) == NULL){
		printf("malloc error for advection_names_f\n");
		exit(0);
	};
	
	fnames->len_f = lengthchara_f();
	
	fnames->istack_advection_controls[0] = 0;
	fnames->istack_advection_controls[1] = fnames->istack_advection_controls[0]
			+ num_advection_controls_f();
	
	fnames->ntot_advection = fnames->istack_advection_controls[1];
	
	if((fnames->num_comp = (int *)calloc(fnames->ntot_advection, sizeof(int))) == NULL) {
		printf("malloc error for num_comp\n");
		exit(0);
	}
	
	int ntot_chara = fnames->len_f * fnames->ntot_advection;
	if((packed_name = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_name\n");
		exit(0);
	}
	if((packed_math = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_math\n");
		exit(0);
	}
	
	set_advection_control_labels_f(&fnames->num_comp[fnames->istack_advection_controls[0]], 
								&packed_name[fnames->len_f * fnames->istack_advection_controls[0]],
								&packed_math[fnames->len_f * fnames->istack_advection_controls[0]]);
	
	
	if ((fnames->term_name = (char **) malloc(fnames->ntot_advection*sizeof(char *))) == NULL) {
		printf("malloc error for term_name\n");
		exit(0);
	}
	if ((fnames->term_math = (char **) malloc(fnames->ntot_advection*sizeof(char *))) == NULL) {
		printf("malloc error for term_math\n");
		exit(0);
	}
	
	int len;
	for(i=0;i<fnames->ntot_advection;i++){
		len = strlen(&packed_name[fnames->len_f * i])+1;
		if((fnames->term_name[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for term_name[%d]\n", i);
			exit(0);
		};
		
		len = strlen(&packed_math[fnames->len_f * i])+1;
		if((fnames->term_math[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for term_math[%d]\n", i);
			exit(0);
		};
		
		strcpy(fnames->term_name[i], &packed_name[fnames->len_f * i]);
		strcpy(fnames->term_math[i], &packed_math[fnames->len_f * i]);
	}
	printf("\n");
	
	free(packed_name);
	free(packed_math);
	
	return fnames;
};

void dealloc_advection_name_f(struct advection_names_f *fnames){
	int i;
	for(i=0;i<fnames->ntot_advection;i++){
		free(fnames->term_name[i]);
		free(fnames->term_math[i]);
	}
	free(fnames->term_name);
	free(fnames->term_math);
	free(fnames->num_comp);
	return;
}



struct force_names_f * init_force_name_f(){
	int i;
	char *packed_name;
	char *packed_math;
	
	struct force_names_f *fnames;
	if((fnames = (struct force_names_f *) malloc(sizeof(struct force_names_f))) == NULL){
		printf("malloc error for force_names_f\n");
		exit(0);
	};
	
	fnames->len_f = lengthchara_f();
	
	fnames->istack_force_controls[0] = 0;
	fnames->istack_force_controls[1] = fnames->istack_force_controls[0] + num_force_controls_f();
	
	fnames->ntot_forcrs = fnames->istack_force_controls[1];
	
	if((fnames->num_comp = (int *)calloc(fnames->ntot_forcrs, sizeof(int))) == NULL) {
		printf("malloc error for num_comp\n");
		exit(0);
	}
	
	int ntot_chara = fnames->len_f * fnames->ntot_forcrs;
	if((packed_name = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_name\n");
		exit(0);
	}
	if((packed_math = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_math\n");
		exit(0);
	}
	
	set_force_control_labels_f(&fnames->num_comp[fnames->istack_force_controls[0]], 
								&packed_name[fnames->len_f * fnames->istack_force_controls[0]],
								&packed_math[fnames->len_f * fnames->istack_force_controls[0]]);
	
	
	if ((fnames->force_name = (char **) malloc(fnames->ntot_forcrs*sizeof(char *))) == NULL) {
		printf("malloc error for force_name\n");
		exit(0);
	}
	if ((fnames->forth_math = (char **) malloc(fnames->ntot_forcrs*sizeof(char *))) == NULL) {
		printf("malloc error for forth_math\n");
		exit(0);
	}
	
	int len;
	for(i=0;i<fnames->ntot_forcrs;i++){
		len = strlen(&packed_name[fnames->len_f * i])+1;
		if((fnames->force_name[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for force_name[%d]\n", i);
			exit(0);
		};
		
		len = strlen(&packed_math[fnames->len_f * i])+1;
		if((fnames->forth_math[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for forth_math[%d]\n", i);
			exit(0);
		};
		
		strcpy(fnames->force_name[i], &packed_name[fnames->len_f * i]);
		strcpy(fnames->forth_math[i], &packed_math[fnames->len_f * i]);
	}
	printf("\n");
	
	free(packed_name);
	free(packed_math);
	
	return fnames;
};

void dealloc_force_name_f(struct force_names_f *fnames){
	int i;
	for(i=0;i<fnames->ntot_forcrs;i++){
		free(fnames->force_name[i]);
		free(fnames->forth_math[i]);
	}
	free(fnames->force_name);
	free(fnames->forth_math);
	free(fnames->num_comp);
	return;
}
