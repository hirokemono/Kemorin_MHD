/*
//  t_control_label_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "t_control_label_from_f.h"

int lengthchara_f();

struct control_labels_f * init_control_labels_f(int (*num_list_func)(void),
                                                void (*name_list_func)(char *)){
	int i;
	long ntot_chara, len;
	char *packed_name;
	
	struct control_labels_f *ctl_list;
	if((ctl_list = (struct control_labels_f *) malloc(sizeof(struct control_labels_f))) == NULL){
		printf("malloc error for control_labels_f\n");
		exit(0);
	};
	ctl_list->len_f = lengthchara_f();
	ctl_list->num_labels = (*num_list_func)();
	ntot_chara = (long) ctl_list->len_f * (long) ctl_list->num_labels;
	
	packed_name = alloc_string(ntot_chara);
	(*name_list_func)(packed_name);
	
	if ((ctl_list->label = (char **) malloc(ctl_list->num_labels*sizeof(char *))) == NULL) {
		printf("malloc error for label\n");
		exit(0);
	}
	
	ctl_list->maxlen = 0;
	for(i=0;i<ctl_list->num_labels;i++){
		len = strlen(&packed_name[ctl_list->len_f * i]);
		if(len > ctl_list->maxlen){ctl_list->maxlen = len;};
		ctl_list->label[i] = alloc_string(len);
		strcpy(ctl_list->label[i], &packed_name[ctl_list->len_f * i]);
	}
	free(packed_name);
	return ctl_list;
};
void dealloc_control_labels_f(struct control_labels_f *ctl_list){
	int i;
	for(i=0;i<ctl_list->num_labels;i++){free(ctl_list->label[i]);};
	free(ctl_list);
	return;
};

void check_control_labels_f(struct control_labels_f *ctl_list){
	int i;
	printf("ctl_list->len_f %d \n", ctl_list->len_f);
	printf("ctl_list->num_labels %d \n", ctl_list->num_labels);
	for(i=0;i<ctl_list->num_labels;i++){
		printf("label[%d]   %s \n", i, ctl_list->label[i]);
	};
    printf("\n");
	return;
};


struct flag_with_math_f * init_flag_with_math_f(int (*num_list_func)(void),
												void (*name_list_func)(int *, char *, char *)){
	
	struct flag_with_math_f *flag_w_math;
	if((flag_w_math = (struct flag_with_math_f *) malloc(sizeof(struct flag_with_math_f))) == NULL){
		printf("malloc error for flag_with_math_f\n");
		exit(0);
	};
	flag_w_math->len_f = lengthchara_f();
	flag_w_math->num_flags = (*num_list_func)();
	
	long ntot_chara, len;
	int i;
	ntot_chara = (long) flag_w_math->len_f * (long) flag_w_math->num_flags;
	char *packed_name = alloc_string(ntot_chara);;
	char *packed_math = alloc_string(ntot_chara);;
	
	if((flag_w_math->num_comp = (int *)calloc(flag_w_math->num_flags, sizeof(int))) == NULL) 
	{
		printf("malloc error for num_comp\n");
		exit(0);
	}
	
	(*name_list_func) (flag_w_math->num_comp, packed_name, packed_math);
	
	if ((flag_w_math->component_name = (char **) malloc(flag_w_math->num_flags*sizeof(char *))) == NULL) {
		printf("malloc error for component_name\n");
		exit(0);
	}
	if ((flag_w_math->component_math = (char **) malloc(flag_w_math->num_flags*sizeof(char *))) == NULL) {
		printf("malloc error for component_math\n");
		exit(0);
	}
	
	flag_w_math->maxlen = 0;
	for(i=0;i<flag_w_math->num_flags;i++){
		len = strlen(&packed_name[flag_w_math->len_f * i]);
		if(len > flag_w_math->maxlen){flag_w_math->maxlen = len;};
		flag_w_math->component_name[i] = alloc_string(len);
		
		len = strlen(&packed_math[flag_w_math->len_f * i]);
		flag_w_math->component_math[i] = alloc_string(len);
		
		strcpy(flag_w_math->component_name[i], &packed_name[flag_w_math->len_f * i]);
		strcpy(flag_w_math->component_math[i], &packed_math[flag_w_math->len_f * i]);
	}
	
	free(packed_name);
	free(packed_math);
	return flag_w_math;
};

void dealloc_flag_with_math_f(struct flag_with_math_f *flag_w_math){
	int i;
	for(i=0;i<flag_w_math->num_flags;i++){
		free(flag_w_math->component_name[i]);
		free(flag_w_math->component_math[i]);
	};
	free(flag_w_math->component_name);
	free(flag_w_math->component_math);
	free(flag_w_math->num_comp);
	free(flag_w_math);
	return;
};

void check_flag_with_math_f(struct flag_with_math_f *flag_w_math){
	int i;
	printf("flag_w_math->len_f %d \n", flag_w_math->len_f);
	printf("flag_w_math->num_labels %d \n", flag_w_math->num_flags);
	for(i=0;i<flag_w_math->num_flags;i++){
		printf("%d: %d %s, %s \n", i, flag_w_math->num_comp[i],
            flag_w_math->component_name[i], flag_w_math->component_math[i]);
	};
	printf("\n");
	return;
};


