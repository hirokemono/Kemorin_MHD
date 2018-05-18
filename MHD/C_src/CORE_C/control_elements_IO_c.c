
/* control_elements_IO_c.c */

#include "control_elements_IO_c.h"


const char *label_begin = "begin";
const char *label_end =   "end";
const char *label_array = "array";
const char *label_file  = "file";

void write_spaces_c(FILE *fp, int num_space){
	int l;
	for(l=0;l<num_space;l++){fprintf(fp, " ");}
	return;
}

void write_space_4_parse_c(FILE *fp, int level){
	write_spaces_c(fp, (2*level));
	return;
}

void adjust_text_output(FILE *fp, int maxlen,  const char *label){
	int len;
	len = maxlen - strlen(label) - 2*check_cautation_require(label);
	write_spaces_c(fp, len);
	return;
}

void write_one_label_cont_c(FILE *fp, int maxlen, const char *label){
	if(check_cautation_require(label) != 0){
		fprintf(fp, "%s", label);
	} else {
		fprintf(fp, "%s", label);
	};
	adjust_text_output(fp, (maxlen+2), label);
	return;
}

void write_one_label_w_lf_c(FILE *fp, const char *label){
	if(check_cautation_require(label) != 0){
		fprintf(fp, "'%s'\n", label);
	} else {
		fprintf(fp, "%s\n", label);
	};
	return;
}


void write_file_flag_for_ctl_c(FILE *fp, int level, const char *label, const char *file_name){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "%s  %s  %s\n", label_file, label, file_name);
	return;
}

int write_begin_flag_for_ctl_c(FILE *fp, int level, const char *label){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "%s  %s\n", label_begin, label);
	return (level + 1);
}

int write_end_flag_for_ctl_c(FILE *fp, int level, const char *label){
	write_space_4_parse_c(fp, level-1);
	fprintf(fp, "end  %s\n", label);
	return (level - 1);
}

int write_array_flag_for_ctl_c(FILE *fp, int level, char *label, int num){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "array  %s    %d\n", label, num);
	return (level + 1);
}

int write_end_array_flag_for_ctl_c(FILE *fp, int level, char *label){
	write_space_4_parse_c(fp, (level - 1));
	fprintf(fp, "end  array  %s\n", label);
	return (level - 1);
}

int right_file_flag_c(const char buf[LENGTHBUF], const char *label, char *file_name){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_file) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		if(cmp_no_case_c(label, item_name) > 0){
			sscanf(buf, "%s %s %s", header_chara, item_name, file_name);
			strip_cautation_marks(file_name);
			iflag = 1;
		};
	};
	return iflag;
}

int right_begin_flag_c(const char buf[LENGTHBUF], const char *label){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_begin) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = cmp_no_case_c(label, item_name);
	};
	return iflag;
}

int find_control_end_flag_c(const char buf[LENGTHBUF], const char *label){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_end) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = cmp_no_case_c(label, item_name);
	};
	return iflag;
}

int find_control_array_flag_c(const char buf[LENGTHBUF], const char *label, int *num){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C];
	
	if(*num > 0) return iflag;
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_array) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		if(cmp_no_case_c(label, item_name) == 0){return 0;};
		sscanf(buf, "%s %s %d", header_chara, item_name, num);
		iflag = 1;
	};
	return iflag;
}

int find_control_end_array_flag_c(const char buf[LENGTHBUF], const char *label, int num, int icou){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C], array_head[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_end) > 0){
		sscanf(buf, "%s %s", header_chara, array_head);
		if(cmp_no_case_c(array_head, label_array) > 0){
			sscanf(buf, "%s %s %s", header_chara, array_head, item_name);
			iflag = cmp_no_case_c(label, item_name);
		};
	};
	if(iflag !=0){
		if(icou < num){
			printf("number of array is not enough!\n");
			return -1;
		};
	} else {
		if(icou > num){
			printf("array should be finished!\n");
			return -1;
		};
	};
	
	return iflag;
}


void alloc_ctl_chara_item(struct chara_ctl_item *c_item){
	
	c_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c_item->iflag = 0;
	return;
}
void dealloc_ctl_chara_item(struct chara_ctl_item *c_item){
	
	free(c_item->c_tbl);
	return;
}
void init_ctl_int_item(struct int_ctl_item *i_item){
	i_item->iflag = 0;
	return;
}
void init_ctl_real_item(struct real_ctl_item *r_item){
	r_item->iflag = 0;
	return;
}

void read_character_ctl_item_c(const char *buf, const char *label,
			struct chara_ctl_item *c_item){
	char header_chara[KCHARA_C];
	
		printf("tako %s \n", c_item->c_tbl);
	if(c_item->iflag == 0){
		sscanf(buf, "%s", header_chara);
		if(cmp_no_case_c(header_chara, label) > 0){
			sscanf(buf, "%s %s", header_chara, c_item->c_tbl);
			strip_cautation_marks(c_item->c_tbl);
			c_item->iflag = 1;
		};
	};
	
	return;
}

void write_character_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_ctl_item *c_item){
	
	if(c_item->iflag != 0){
		write_space_4_parse_c(fp, level);
		write_one_label_cont_c(fp, maxlen, label);
		write_one_label_w_lf_c(fp, c_item->c_tbl);
	};
	return;
}

void read_integer_ctl_item_c(const char *buf, const char *label,
			struct int_ctl_item *i_item){
	char header_chara[KCHARA_C];
	
	if(i_item->iflag == 0){
		sscanf(buf, "%s", header_chara);
		if(cmp_no_case_c(header_chara, label) > 0){
			sscanf(buf, "%s %d", header_chara, &i_item->i_data);
			i_item->iflag = 1;
		};
	};
	
	return;
}

void write_ineger_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_item *i_item){
	
	if(i_item->iflag != 0){
		write_space_4_parse_c(fp, level);
		write_one_label_cont_c(fp, maxlen, label);
		fprintf(fp, "%d\n", i_item->i_data);
	};
	return;
}

void read_real_ctl_item_c(const char *buf, const char *label,
			struct real_ctl_item *r_item){
	char header_chara[KCHARA_C];
	
	if(r_item->iflag == 0){
		sscanf(buf, "%s", header_chara);
		if(cmp_no_case_c(header_chara, label) > 0){
			sscanf(buf, "%s %lf", header_chara, &r_item->r_data);
			r_item->iflag = 1;
		};
	};
	
	return;
}

void write_real_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct real_ctl_item *r_item){
	
	if(r_item->iflag != 0){
		write_space_4_parse_c(fp, level);
		write_one_label_cont_c(fp, maxlen, label);
				fprintf(fp, "%.12e\n", r_item->r_data);
	};
	return;
}


void find_max_length_c2r(const char *label, struct ctl_c2r_item *c2r_ctl, int *maxlen){
	maxlen[0] = strlen(label);
	if(strlen(c2r_ctl->c1_tbl) > maxlen[1]) {maxlen[1] = strlen(c2r_ctl->c1_tbl);};
	if(strlen(c2r_ctl->c2_tbl) > maxlen[2]) {maxlen[2] = strlen(c2r_ctl->c2_tbl);};
	return;
};


void write_control_c2_r_list_c(FILE *fp, int level, int maxlen[3],
			const char *label, struct ctl_c2r_item *c2r_ctl){
	
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2r_ctl->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c2r_ctl->c2_tbl);
	fprintf(fp, "%.12e\n", *c2r_ctl->vect);
	
	return;
}

void write_control_array_c2_r_list_c(FILE *fp, int level, int num,
			const char *label, struct ctl_c2r_array *array_c2r){
	int i;
	
	array_c2r->maxlen[0] = strlen(label);
	array_c2r->maxlen[1] = 0;
	array_c2r->maxlen[2] = 0;
	for(i=0;i<array_c2r->num;i++){
		find_max_length_c2r(label, &array_c2r->c2r_ctl[i], array_c2r->maxlen);
	}
	
	
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, strlen(label_begin), label_begin);
	write_one_label_cont_c(fp, strlen(label_begin), label_array);
	write_one_label_cont_c(fp, strlen(label), label);
	fprintf(fp, "%d \n", array_c2r->num);
	for(i=0;i<array_c2r->num;i++){
		write_control_c2_r_list_c(fp, (level+1), array_c2r->maxlen, label, &array_c2r->c2r_ctl[i]);
	}
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, strlen(label_end), label_end);
	write_one_label_w_lf_c(fp, label_array);
	return;
}

int alloc_ctl_c2r_item(struct ctl_c2r_item *c2r_ctl){
	
	c2r_ctl->c1_tbl = (char *)calloc(KCHARA_C+1, sizeof(char));
	c2r_ctl->c2_tbl = (char *)calloc(KCHARA_C+1, sizeof(char));
	c2r_ctl->vect = (double *)calloc(1, sizeof(double));
	*c2r_ctl->vect = 0.0;
	c2r_ctl->iflag = 0;
	return 0;
}

void dealloc_ctl_c2r_item(struct ctl_c2r_item *c2r_ctl){
	free(c2r_ctl->c2_tbl);
	free(c2r_ctl->c1_tbl);
	return;
}

void read_control_c2_r_list_c(const char buf[LENGTHBUF], const char *label, struct ctl_c2r_item *c2r_ctl){
	char header_chara[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		alloc_ctl_c2r_item(c2r_ctl);
		sscanf(buf, "%s %s %s %lf", header_chara, 
					c2r_ctl->c1_tbl, c2r_ctl->c2_tbl, c2r_ctl->vect);
		c2r_ctl->iflag = 1;
	};
	return;
}


int alloc_ctl_c2r_array(int num, struct ctl_c2r_array *array_c2r){
	int i;
	
	array_c2r->num = num;
	array_c2r->maxlen = (int *)calloc(3, sizeof(int));
	array_c2r->c2r_ctl = (struct ctl_c2r_item *)malloc(array_c2r->num * sizeof(struct ctl_c2r_item));
	for (i = 0; i < array_c2r->num; i++) {
		alloc_ctl_c2r_item(&array_c2r->c2r_ctl[i]);
	};
	
	return array_c2r->num;
}

void dealloc_ctl_c2r_array(struct ctl_c2r_array *array_c2r){
	int i;
	
	for (i = 0; i < array_c2r->num; i++) {
		dealloc_ctl_c2r_item(&array_c2r->c2r_ctl[i]);
	};
	free(array_c2r->c2r_ctl);
	return;
}

int read_control_array_c2_r_list_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct ctl_c2r_array *array_c2r){
	int num, iflag;
	
	iflag = find_control_array_flag_c(buf, label, &num);
	if(iflag > 0){
		alloc_ctl_c2r_array(num, array_c2r);
		
		while(array_c2r->icou <= num){
			fgets(buf, LENGTHBUF, fp);
			array_c2r->icou = find_control_end_array_flag_c(buf, label, num, array_c2r->icou);
			read_control_c2_r_list_c(buf, label, &array_c2r->c2r_ctl[array_c2r->icou]);
			array_c2r->icou = array_c2r->icou + array_c2r->c2r_ctl[array_c2r->icou].iflag;
		};
	};
	
	return num;
}


