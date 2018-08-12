/*
//  control_arrays_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/24.
*/

#include "control_arrays_IO_c.h"


void init_ctl_chara_array(struct chara_ctl_array *c_array){
	c_array->num = 0;
	c_array->icou = 0;
	return;
};
void alloc_ctl_chara_array(struct chara_ctl_array *c_array){
	int i;
	
	c_array->c_array_item = (struct chara_ctl_item **) malloc(c_array->num*sizeof(struct chara_ctl_item *));
	for(i=0;i<c_array->num;i++){
		c_array->c_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		alloc_ctl_chara_item(c_array->c_array_item[i]);
	};
	return;
}
void dealloc_ctl_chara_array(struct chara_ctl_array *c_array){
	int i;
	
	if(c_array->num == 0) return;
	for(i=0;i<c_array->num;i++){
		dealloc_ctl_chara_item(c_array->c_array_item[i]);
		free(c_array->c_array_item[i]);
	};
	free(c_array->c_array_item);
	init_ctl_chara_array(c_array);
	return;
}
void read_character_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_ctl_array *c_array){
	if(c_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &c_array->num) == 0) return;
	
	if(c_array->num > 0) alloc_ctl_chara_array(c_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, c_array->num, c_array->icou) == 0){
		read_character_ctl_item_c(buf, label, c_array->c_array_item[c_array->icou]);
		c_array->icou = c_array->icou + c_array->c_array_item[c_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_character_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_ctl_array *c_array){
	int i;
	
	if(c_array->icou == 0) return;
	level = write_array_flag_for_ctl_c(fp, level, label, c_array->num);
	for(i=0;i<c_array->num;i++){
		write_character_ctl_item_c(fp, level, (int) strlen(label),
					label, c_array->c_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


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


void init_ctl_chara2_array(struct chara2_ctl_array *c2_array){
	c2_array->num = 0;
	c2_array->icou = 0;
	return;
};
void alloc_ctl_chara2_array(struct chara2_ctl_array *c2_array){
	int i;
	
	c2_array->c1_array_item = (struct chara_ctl_item **) malloc(c2_array->num*sizeof(struct chara_ctl_item *));
	c2_array->c2_array_item = (struct chara_ctl_item **) malloc(c2_array->num*sizeof(struct chara_ctl_item *));
	for(i=0;i<c2_array->num;i++){
		c2_array->c1_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		c2_array->c2_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		alloc_ctl_chara_item(c2_array->c1_array_item[i]);
		alloc_ctl_chara_item(c2_array->c2_array_item[i]);
	};
	c2_array->icou = 0;
	return;
}
void dealloc_ctl_chara2_array(struct chara2_ctl_array *c2_array){
	int i;
	
	if(c2_array->num == 0) return;
	for(i=0;i<c2_array->num;i++){
		dealloc_ctl_chara_item(c2_array->c1_array_item[i]);
		dealloc_ctl_chara_item(c2_array->c2_array_item[i]);
		free(c2_array->c1_array_item[i]);
		free(c2_array->c2_array_item[i]);
	};
	free(c2_array->c1_array_item);
	free(c2_array->c2_array_item);
	init_ctl_chara2_array(c2_array);
	return;
}
void read_chara2_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara2_ctl_array *c2_array){
	
	if(c2_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &c2_array->num) == 0) return;
    if(c2_array->num == 0) return;
	
	if(c2_array->num > 0) alloc_ctl_chara2_array(c2_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, c2_array->num, c2_array->icou) == 0){
		if(c2_array->icou >= c2_array->num){
			printf("Number of char2 item is larger than defined, %d %d \n",
						c2_array->icou, c2_array->num);
			return;
		}
		
		read_chara2_ctl_item_c(buf, label, c2_array->c1_array_item[c2_array->icou],
					c2_array->c2_array_item[c2_array->icou]);
		c2_array->icou = c2_array->icou + c2_array->c1_array_item[c2_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_chara2_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara2_ctl_array *c2_array){
	int i;
	
	if(c2_array->icou == 0) return;
	
	c2_array->maxlen[0] = (int) strlen(label);
	c2_array->maxlen[1] = (int) strlen(c2_array->c1_array_item[0]->c_tbl);
	for(i=0;i<c2_array->num;i++){
		if((int) strlen(c2_array->c1_array_item[i]->c_tbl) > c2_array->maxlen[1]){
			c2_array->maxlen[1] = (int) strlen(c2_array->c1_array_item[i]->c_tbl);
		};
	};
	
	level = write_array_flag_for_ctl_c(fp, level, label, c2_array->num);
	for(i=0;i<c2_array->num;i++){
		write_chara2_ctl_item_c(fp, level, c2_array->maxlen,
					label, c2_array->c1_array_item[i], c2_array->c2_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_int2_array(struct int2_ctl_array *i2_array){
	i2_array->num = 0;
	i2_array->icou = 0;
	return;
};
void alloc_ctl_int2_array(struct int2_ctl_array *i2_array){
	int i;
	
	i2_array->i1_array_item = (struct int_ctl_item **) malloc(i2_array->num*sizeof(struct int_ctl_item *));
	i2_array->i2_array_item = (struct int_ctl_item **) malloc(i2_array->num*sizeof(struct int_ctl_item *));
	for(i=0;i<i2_array->num;i++){
		i2_array->i1_array_item[i] = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
		i2_array->i2_array_item[i] = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
		init_ctl_int_item(i2_array->i1_array_item[i]);
		init_ctl_int_item(i2_array->i2_array_item[i]);
	};
	i2_array->icou = 0;
	return;
}
void dealloc_ctl_int2_array(struct int2_ctl_array *i2_array){
	int i;
	
	if(i2_array->num == 0) return;
	for(i=0;i<i2_array->num;i++){
		free(i2_array->i1_array_item[i]);
		free(i2_array->i2_array_item[i]);
	};
	free(i2_array->i1_array_item);
	free(i2_array->i2_array_item);
	init_ctl_int2_array(i2_array);
	return;
}
void read_int2_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int2_ctl_array *i2_array){
	
	if(i2_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &i2_array->num) == 0) return;
    if(i2_array->num == 0) return;
	
	if(i2_array->num > 0) alloc_ctl_int2_array(i2_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, i2_array->num, i2_array->icou) == 0){
		if(i2_array->icou >= i2_array->num){
			printf("Number of int2 item is larger than defined \n");
			return;
		}
		
		read_int2_ctl_item_c(buf, label, i2_array->i1_array_item[i2_array->icou],
					i2_array->i2_array_item[i2_array->icou]);
		i2_array->icou = i2_array->icou + i2_array->i1_array_item[i2_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_int2_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int2_ctl_array *i2_array){
	int i;
	
	if(i2_array->icou == 0) return;
	
	level = write_array_flag_for_ctl_c(fp, level, label, i2_array->num);
	for(i=0;i<i2_array->num;i++){
		write_int2_ctl_item_c(fp, level, (int) strlen(label),
					label, i2_array->i1_array_item[i], i2_array->i2_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_real2_array(struct real2_ctl_array *r2_array){
	r2_array->num = 0;
	r2_array->icou = 0;
	return;
};
void alloc_ctl_real2_array(struct real2_ctl_array *r2_array){
	int i;
	
	r2_array->r1_array_item = (struct real_ctl_item **) malloc(r2_array->num*sizeof(struct real_ctl_item *));
	r2_array->r2_array_item = (struct real_ctl_item **) malloc(r2_array->num*sizeof(struct real_ctl_item *));
	for(i=0;i<r2_array->num;i++){
		r2_array->r1_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		r2_array->r2_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		init_ctl_real_item(r2_array->r1_array_item[i]);
		init_ctl_real_item(r2_array->r2_array_item[i]);
	};
	r2_array->icou = 0;
	return;
}
void dealloc_ctl_real2_array(struct real2_ctl_array *r2_array){
	int i;
	
	if(r2_array->num == 0) return;
	for(i=0;i<r2_array->num;i++){
		free(r2_array->r1_array_item[i]);
		free(r2_array->r2_array_item[i]);
	};
	free(r2_array->r1_array_item);
	free(r2_array->r2_array_item);
	init_ctl_real2_array(r2_array);
	return;
}
void read_real2_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct real2_ctl_array *r2_array){
	
	if(r2_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &r2_array->num) == 0) return;
    if(r2_array->num == 0) return;
	
	if(r2_array->num > 0) alloc_ctl_real2_array(r2_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, r2_array->num, r2_array->icou) == 0){
		if(r2_array->icou >= r2_array->num){
			printf("Number of int2 item is larger than defined \n");
			return;
		}
		
		read_real2_ctl_item_c(buf, label, r2_array->r1_array_item[r2_array->icou],
					r2_array->r2_array_item[r2_array->icou]);
		r2_array->icou = r2_array->icou + r2_array->r1_array_item[r2_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_real2_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct real2_ctl_array *r2_array){
	int i;
	
	if(r2_array->icou == 0) return;
	
	level = write_array_flag_for_ctl_c(fp, level, label, r2_array->num);
	for(i=0;i<r2_array->num;i++){
		write_real2_ctl_item_c(fp, level, (int) strlen(label),
					label, r2_array->r1_array_item[i], r2_array->r2_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_ci_array(struct chara_int_ctl_array *ci_array){
	ci_array->num = 0;
	ci_array->icou = 0;
	return;
};
void alloc_ctl_ci_array(struct chara_int_ctl_array *ci_array){
	int i;
	
	ci_array->c_array_item = (struct chara_ctl_item **) malloc(ci_array->num*sizeof(struct chara_ctl_item *));
	ci_array->i_array_item = (struct int_ctl_item **) malloc(ci_array->num*sizeof(struct int_ctl_item *));
	for(i=0;i<ci_array->num;i++){
		ci_array->c_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		ci_array->i_array_item[i] = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
		alloc_ctl_chara_item(ci_array->c_array_item[i]);
		init_ctl_int_item(ci_array->i_array_item[i]);
	};
	ci_array->icou = 0;
	return;
}
void dealloc_ctl_ci_array(struct chara_int_ctl_array *ci_array){
	int i;
	
	if(ci_array->num == 0) return;
	for(i=0;i<ci_array->num;i++){
		dealloc_ctl_chara_item(ci_array->c_array_item[i]);
		free(ci_array->c_array_item[i]);
		free(ci_array->i_array_item[i]);
	};
	free(ci_array->c_array_item);
	free(ci_array->i_array_item);
	init_ctl_ci_array(ci_array);
	return;
}
void read_ci_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_int_ctl_array *ci_array){
	
	if(ci_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &ci_array->num) == 0) return;
    if(ci_array->num == 0) return;
	
	if(ci_array->num > 0) alloc_ctl_ci_array(ci_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, ci_array->num, ci_array->icou) == 0){
		if(ci_array->icou >= ci_array->num){
			printf("Number of char_real item is larger than defined \n");
			return;
		}
		
		read_ci_ctl_item_c(buf, label, ci_array->c_array_item[ci_array->icou],
					ci_array->i_array_item[ci_array->icou]);
		ci_array->icou = ci_array->icou + ci_array->c_array_item[ci_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_ci_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_int_ctl_array *ci_array){
	int i;
	
	if(ci_array->icou == 0) return;
	
	ci_array->maxlen[0] = (int) strlen(label);
	ci_array->maxlen[1] = (int) strlen(ci_array->c_array_item[0]->c_tbl);
	for(i=0;i<ci_array->num;i++){
		if(strlen(ci_array->c_array_item[i]->c_tbl) > ci_array->maxlen[1]){
			ci_array->maxlen[1] = (int) strlen(ci_array->c_array_item[i]->c_tbl);
		};
	};
	
	level = write_array_flag_for_ctl_c(fp, level, label, ci_array->num);
	for(i=0;i<ci_array->num;i++){
		write_ci_ctl_item_c(fp, level, ci_array->maxlen,
					label, ci_array->c_array_item[i], ci_array->i_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_cr_array(struct chara_real_ctl_array *cr_array){
	cr_array->num = 0;
	cr_array->icou = 0;
	return;
};
void alloc_ctl_cr_array(struct chara_real_ctl_array *cr_array){
	int i;
	
	cr_array->c_array_item = (struct chara_ctl_item **) malloc(cr_array->num*sizeof(struct chara_ctl_item *));
	cr_array->r_array_item = (struct real_ctl_item **) malloc(cr_array->num*sizeof(struct real_ctl_item *));
	for(i=0;i<cr_array->num;i++){
		cr_array->c_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		cr_array->r_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		alloc_ctl_chara_item(cr_array->c_array_item[i]);
		init_ctl_real_item(cr_array->r_array_item[i]);
	};
	cr_array->icou = 0;
	return;
}
void dealloc_ctl_cr_array(struct chara_real_ctl_array *cr_array){
	int i;
	
	if(cr_array->num == 0) return;
	for(i=0;i<cr_array->num;i++){
		dealloc_ctl_chara_item(cr_array->c_array_item[i]);
		free(cr_array->c_array_item[i]);
		free(cr_array->r_array_item[i]);
	};
	free(cr_array->c_array_item);
	free(cr_array->r_array_item);
	init_ctl_cr_array(cr_array);
	return;
}
void read_cr_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_real_ctl_array *cr_array){
	
	if(cr_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &cr_array->num) == 0) return;
    if(cr_array->num == 0) return;
	
	if(cr_array->num > 0) alloc_ctl_cr_array(cr_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, cr_array->num, cr_array->icou) == 0){
		if(cr_array->icou >= cr_array->num){
			printf("Number of char_real item is larger than defined \n");
			return;
		}
		
		read_cr_ctl_item_c(buf, label, cr_array->c_array_item[cr_array->icou],
					cr_array->r_array_item[cr_array->icou]);
		cr_array->icou = cr_array->icou + cr_array->c_array_item[cr_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_cr_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_real_ctl_array *cr_array){
	int i;
	
	if(cr_array->icou == 0) return;
	
	cr_array->maxlen[0] = (int) strlen(label);
	cr_array->maxlen[1] = (int) strlen(cr_array->c_array_item[0]->c_tbl);
	for(i=0;i<cr_array->num;i++){
		if(strlen(cr_array->c_array_item[i]->c_tbl) > cr_array->maxlen[1]){
			cr_array->maxlen[1] = (int) strlen(cr_array->c_array_item[i]->c_tbl);
		};
	};
	
	level = write_array_flag_for_ctl_c(fp, level, label, cr_array->num);
	for(i=0;i<cr_array->num;i++){
		write_cr_ctl_item_c(fp, level, cr_array->maxlen,
					label, cr_array->c_array_item[i], cr_array->r_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_ir_array(struct int_real_ctl_array *ir_array){
	ir_array->num = 0;
	ir_array->icou = 0;
	return;
};
void alloc_ctl_ir_array(struct int_real_ctl_array *ir_array){
	int i;
	
	ir_array->i_array_item = (struct int_ctl_item **) malloc(ir_array->num*sizeof(struct int_ctl_item *));
	ir_array->r_array_item = (struct real_ctl_item **) malloc(ir_array->num*sizeof(struct real_ctl_item *));
	for(i=0;i<ir_array->num;i++){
		ir_array->i_array_item[i] = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
		ir_array->r_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		init_ctl_int_item(ir_array->i_array_item[i]);
		init_ctl_real_item(ir_array->r_array_item[i]);
	};
	ir_array->icou = 0;
	return;
}
void dealloc_ctl_ir_array(struct int_real_ctl_array *ir_array){
	int i;
	
	if(ir_array->num == 0) return;
	for(i=0;i<ir_array->num;i++){
		free(ir_array->i_array_item[i]);
		free(ir_array->r_array_item[i]);
	};
	free(ir_array->i_array_item);
	free(ir_array->r_array_item);
	init_ctl_ir_array(ir_array);
	return;
}
void read_ir_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_real_ctl_array *ir_array){
	
	if(ir_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &ir_array->num) == 0) return;
    if(ir_array->num == 0) return;
	
	if(ir_array->num > 0) alloc_ctl_ir_array(ir_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, ir_array->num, ir_array->icou) == 0){
		if(ir_array->icou >= ir_array->num){
			printf("Number of int_real item is larger than defined \n");
			return;
		}
		
		read_ir_ctl_item_c(buf, label, ir_array->i_array_item[ir_array->icou],
					ir_array->r_array_item[ir_array->icou]);
		ir_array->icou = ir_array->icou + ir_array->i_array_item[ir_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_ir_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_real_ctl_array *ir_array){
	int i;
	
	if(ir_array->icou == 0) return;
	
	level = write_array_flag_for_ctl_c(fp, level, label, ir_array->num);
	for(i=0;i<ir_array->num;i++){
		write_ir_ctl_item_c(fp, level, (int) strlen(label),
					label, ir_array->i_array_item[i], ir_array->r_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_chara3_array(struct chara3_ctl_array *c3_array){
	c3_array->num = 0;
	c3_array->icou = 0;
	return;
};
void alloc_ctl_chara3_array(struct chara3_ctl_array *c3_array){
	int i;
	
	c3_array->c1_array_item = (struct chara_ctl_item **) malloc(c3_array->num*sizeof(struct chara_ctl_item *));
	c3_array->c2_array_item = (struct chara_ctl_item **) malloc(c3_array->num*sizeof(struct chara_ctl_item *));
	c3_array->c3_array_item = (struct chara_ctl_item **) malloc(c3_array->num*sizeof(struct chara_ctl_item *));
	for(i=0;i<c3_array->num;i++){
		c3_array->c1_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		c3_array->c2_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		c3_array->c3_array_item[i] = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
		alloc_ctl_chara_item(c3_array->c1_array_item[i]);
		alloc_ctl_chara_item(c3_array->c2_array_item[i]);
		alloc_ctl_chara_item(c3_array->c3_array_item[i]);
	};
	c3_array->icou = 0;
	return;
}
void dealloc_ctl_chara3_array(struct chara3_ctl_array *c3_array){
	int i;
	
	if(c3_array->num == 0) return;
	for(i=0;i<c3_array->num;i++){
		dealloc_ctl_chara_item(c3_array->c1_array_item[i]);
		dealloc_ctl_chara_item(c3_array->c2_array_item[i]);
		dealloc_ctl_chara_item(c3_array->c3_array_item[i]);
		free(c3_array->c1_array_item[i]);
		free(c3_array->c2_array_item[i]);
		free(c3_array->c3_array_item[i]);
	};
	free(c3_array->c1_array_item);
	free(c3_array->c2_array_item);
	free(c3_array->c3_array_item);
	init_ctl_chara3_array(c3_array);
	return;
}
void read_chara3_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara3_ctl_array *c3_array){
	
	if(c3_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &c3_array->num) == 0) return;
    if(c3_array->num == 0) return;
	
	if(c3_array->num > 0) alloc_ctl_chara3_array(c3_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, c3_array->num, c3_array->icou) == 0){
		if(c3_array->icou >= c3_array->num){
			printf("Number of char3 item is larger than defined, %d %d \n",
						c3_array->icou, c3_array->num);
			return;
		}
		
		read_chara3_ctl_item_c(buf, label, c3_array->c1_array_item[c3_array->icou],
					c3_array->c2_array_item[c3_array->icou],
					c3_array->c3_array_item[c3_array->icou]);
		c3_array->icou = c3_array->icou + c3_array->c1_array_item[c3_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_chara3_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara3_ctl_array *c3_array){
	int i;
	
	if(c3_array->icou == 0) return;
	
	c3_array->maxlen[0] = (int) strlen(label);
	c3_array->maxlen[1] = (int) strlen(c3_array->c1_array_item[0]->c_tbl);
	c3_array->maxlen[2] = (int) strlen(c3_array->c2_array_item[0]->c_tbl);
	for(i=0;i<c3_array->num;i++){
		if(strlen(c3_array->c1_array_item[i]->c_tbl) > c3_array->maxlen[1]){
			c3_array->maxlen[1] = (int) strlen(c3_array->c1_array_item[i]->c_tbl);
		};
		if(strlen(c3_array->c2_array_item[i]->c_tbl) > c3_array->maxlen[2]){
			c3_array->maxlen[2] = (int) strlen(c3_array->c2_array_item[i]->c_tbl);
		};
	};
	
	level = write_array_flag_for_ctl_c(fp, level, label, c3_array->num);
	for(i=0;i<c3_array->num;i++){
		write_chara3_ctl_item_c(fp, level, c3_array->maxlen,
					label, c3_array->c1_array_item[i], c3_array->c2_array_item[i], 
					c3_array->c3_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}


void init_ctl_real3_array(struct real3_ctl_array *r3_array){
	r3_array->num = 0;
	r3_array->icou = 0;
	return;
};
void alloc_ctl_real3_array(struct real3_ctl_array *r3_array){
	int i;
	
	r3_array->r1_array_item = (struct real_ctl_item **) malloc(r3_array->num*sizeof(struct real_ctl_item *));
	r3_array->r2_array_item = (struct real_ctl_item **) malloc(r3_array->num*sizeof(struct real_ctl_item *));
	r3_array->r3_array_item = (struct real_ctl_item **) malloc(r3_array->num*sizeof(struct real_ctl_item *));
	for(i=0;i<r3_array->num;i++){
		r3_array->r1_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		r3_array->r2_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		r3_array->r3_array_item[i] = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
		init_ctl_real_item(r3_array->r1_array_item[i]);
		init_ctl_real_item(r3_array->r2_array_item[i]);
		init_ctl_real_item(r3_array->r3_array_item[i]);
	};
	r3_array->icou = 0;
	return;
}
void dealloc_ctl_real3_array(struct real3_ctl_array *r3_array){
	int i;
	
	if(r3_array->num == 0) return;
	for(i=0;i<r3_array->num;i++){
		free(r3_array->r1_array_item[i]);
		free(r3_array->r2_array_item[i]);
		free(r3_array->r3_array_item[i]);
	};
	free(r3_array->r1_array_item);
	free(r3_array->r2_array_item);
	free(r3_array->r3_array_item);
	init_ctl_real3_array(r3_array);
	return;
}
void read_real3_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct real3_ctl_array *r3_array){
	
	if(r3_array->icou > 0) return;
	if(find_control_array_flag_c(buf, label, &r3_array->num) == 0) return;
    if(r3_array->num == 0) return;
	
	if(r3_array->num > 0) alloc_ctl_real3_array(r3_array);
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, r3_array->num, r3_array->icou) == 0){
		if(r3_array->icou >= r3_array->num){
			printf("Number of real3 item is larger than defined, %d %d \n",
						r3_array->icou, r3_array->num);
			return;
		}
		
		read_real3_ctl_item_c(buf, label, r3_array->r1_array_item[r3_array->icou],
					r3_array->r2_array_item[r3_array->icou],
					r3_array->r3_array_item[r3_array->icou]);
		r3_array->icou = r3_array->icou + r3_array->r1_array_item[r3_array->icou]->iflag;
		
		skip_comment_read_line(fp, buf);
	};
	
	return;
}
void write_real3_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct real3_ctl_array *r3_array){
	int i;
	
	if(r3_array->icou == 0) return;
	
	level = write_array_flag_for_ctl_c(fp, level, label, r3_array->num);
	for(i=0;i<r3_array->num;i++){
		write_real3_ctl_item_c(fp, level, (int) strlen(label),
					label, r3_array->r1_array_item[i], r3_array->r2_array_item[i], 
					r3_array->r3_array_item[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return;
}
