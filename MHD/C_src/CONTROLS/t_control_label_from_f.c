/*
//  t_control_label_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "t_control_label_from_f.h"

int lengthchara_f(void);

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

int maxlen_flag_with_math_f(struct chara2_int_clist *flag_w_math){
    struct chara2_int_ctl_item *tmp_item;
    int maxlen = 0;
    int i, len;
    for(i=0;i<count_chara2_int_clist(flag_w_math);i++){
        tmp_item = chara2_int_clist_at_index(i, flag_w_math);
        len = strlen(tmp_item->c1_tbl);
        if(len > maxlen){maxlen = len;};
    }
    return maxlen;
}

void check_flag_with_math_f(struct chara2_int_clist *flag_w_math){
    struct chara2_int_ctl_item *tmp_item;
	int i;
    int num = count_chara2_int_clist(flag_w_math);
	printf("flag_w_math->num_labels %d \n", num);
	for(i=0;i<num;i++){
        tmp_item = chara2_int_clist_at_index(i, flag_w_math);
		printf("%d: %d %s, %s \n", i, tmp_item->i_data,
               tmp_item->c1_tbl, tmp_item->c2_tbl);
	};
	printf("\n");
	return;
};


