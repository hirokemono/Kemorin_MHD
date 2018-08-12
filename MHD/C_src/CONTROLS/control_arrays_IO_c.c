/*
//  control_arrays_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/24.
*/

#include "control_arrays_IO_c.h"


void init_ctl_int_array(struct int_ctl_array *i_array){
	i_array->num = 0;
	i_array->icou = 0;
	return;
	return;
}
void alloc_ctl_int_array(struct int_ctl_array *i_array){
	int i;
	
	i_array->i_array_item = (struct int_ctl_item **) malloc(i_array->num*sizeof(struct int_ctl_item *));
	for(i=0;i<i_array->num;i++){
		i_array->i_array_item[i] = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
		init_ctl_int_item(i_array->i_array_item[i]);
	};
	return;
}
void dealloc_ctl_int_array(struct int_ctl_array *i_array){
	int i;
	
	if(i_array->num == 0) return;
	for(i=0;i<i_array->num;i++){
		free(i_array->i_array_item[i]);
	};
	free(i_array->i_array_item);
	init_ctl_int_array(i_array);
	return;
}
void read_integer_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_ctl_array *i_array){
	if(i_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &i_array->num) == 0) return;
    if(i_array->num == 0) return;
	
	if(i_array->num > 0) alloc_ctl_int_array(i_array);
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, i_array->num, i_array->icou) == 0){
		if(i_array->icou >= i_array->num){
			printf("Number of int item is larger than defined \n");
			return;
		}
		
		read_integer_ctl_item_c(buf, label, i_array->i_array_item[i_array->icou]);
		i_array->icou = i_array->icou + i_array->i_array_item[i_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_integer_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_array *i_array){
	int i;
	
	if(i_array->icou == 0) return;
	
	level = write_array_flag_for_ctl_c(fp, level, label, i_array->num);
	for(i=0;i<i_array->num;i++){
		write_integer_ctl_item_c(fp, level, (int) strlen(label),
					label, i_array->i_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}

