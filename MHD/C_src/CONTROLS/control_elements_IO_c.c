
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
	len = maxlen - (int) strlen(label) - 2*check_cautation_require(label);
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
    write_one_label_cont_c(fp, (int) strlen(label_file), label_file);
    write_one_label_cont_c(fp, (int) strlen(label), label);
    write_one_label_w_lf_c(fp, file_name);
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

int write_array_flag_for_ctl_c(FILE *fp, int level, const char *label, int num){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "array  %s    %d\n", label, num);
	return (level + 1);
}

int write_end_array_flag_for_ctl_c(FILE *fp, int level, const char *label){
	write_space_4_parse_c(fp, (level - 1));
	fprintf(fp, "end array  %s\n", label);
	return (level - 1);
}

void skip_comment_read_line(FILE *fp, char buf[LENGTHBUF]){
    
    long offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    return;
};


int right_file_flag_c(const char buf[LENGTHBUF], const char *label){
	int iflag = 0;
	char header_chara[KCHARA_C], item_name[KCHARA_C];
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label_file) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = cmp_no_case_c(label, item_name);
	};
	return iflag;
}

int read_file_flag_c(const char buf[LENGTHBUF], char *file_name){
    char header_chara[KCHARA_C], item_name[KCHARA_C];
    
    sscanf(buf, "%s %s %s", header_chara, item_name, file_name);
    strip_cautation_marks(file_name);
    return -1;
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
};

int count_max_length_of_label(int num, const char *label[KCHARA_C]){
	int i, maxlen;
	
	maxlen = 0;
	for (i=0;i<num;i++){
		if(strlen(label[i]) > maxlen){
			maxlen = (int) strlen(label[i]);
		};
	};
	
	return maxlen;
};

int find_direction_from_ctl(const char *c_tbl) {
	if(cmp_no_case_c(c_tbl, "x") + cmp_no_case_c(c_tbl, "1") > 0){
		return 0;
	}else if(cmp_no_case_c(c_tbl, "y") + cmp_no_case_c(c_tbl, "2") > 0){
		return 1;
	}else if(cmp_no_case_c(c_tbl, "z") + cmp_no_case_c(c_tbl, "3") > 0){
		return 2;
	}else if(cmp_no_case_c(c_tbl, "w") + cmp_no_case_c(c_tbl, "4") > 0){
		return 3;
	};
	return -1;
};
void set_direction_from_ctl(int i, char *c_tbl){
	if(i == 0){
        sprintf(c_tbl, "%s", "x");
	}else if(i == 1){
        sprintf(c_tbl, "%s", "y");
	}else if(i == 2){
        sprintf(c_tbl, "%s", "z");
	}else if(i == 3){
        sprintf(c_tbl, "%s", "w");
	};
	return;
};

