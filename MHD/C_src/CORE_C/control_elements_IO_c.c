
/* control_elements_IO_c.c */

#include "control_elements_IO_c.h"


const char *label_begin = "begin";
const char *label_end =   "end";
const char *label_array = "array";
const char *label_file  = "file";

void write_space_4_parse_c(FILE *fp, int level){
	int l;
	for(l=0;l<level;l++){fprintf(fp, "  ");}
	return;
}

void write_one_label_cont_c(FILE *fp, char *label){
	fprintf(fp, "%s  ", label);
	return;
}


void write_begin_flag_for_ctl_c(FILE *fp, int level, const char *label){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "%s  %s\n", label_begin, label);
	return;
}

void write_end_flag_for_ctl_c(FILE *fp, int level, char *label){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "end  %s\n", label);
	return;
}

void write_array_flag_for_ctl_c(FILE *fp, int level, char *label, int num){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "array  %s    %d\n", label, num);
	return;
}

void write_end_array_flag_for_ctl_c(FILE *fp, int level, char *label){
	write_space_4_parse_c(fp, level);
	fprintf(fp, "end  array  %s\n", label);
	return;
}

int right_file_flag_c(FILE *fp, const char *label){
	int iflag = 0;
	char *header_chara, *item_name;
	char buf[LENGTHBUF];
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s", header_chara);
	if(compare_string(5, header_chara, label_file) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = compare_string(strlen(label), label, item_name);
	};
	return iflag;
}

int right_begin_flag_c(FILE *fp, const char *label){
	int iflag = 0;
	char *header_chara, *item_name;
	char buf[LENGTHBUF];
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s", header_chara);
	if(compare_string(5, header_chara, label_begin) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = compare_string(strlen(label), label, item_name);
	};
	return iflag;
}

int find_control_end_flag_c(FILE *fp, const char *label){
	int iflag = 0;
	char *header_chara, *item_name;
	char buf[LENGTHBUF];
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s", header_chara);
	if(compare_string(3, header_chara, label_end) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		iflag = compare_string(strlen(label), label, item_name);
	};
	return iflag;
}

int find_control_array_flag_c(FILE *fp, const char *label, int num){
	int num_array = 0;
	char *header_chara, *item_name;
	char buf[LENGTHBUF];
	
	if(num >= 0) return num;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s", header_chara);
	if(compare_string(5, header_chara, label_array) > 0){
		sscanf(buf, "%s %s", header_chara, item_name);
		if(compare_string(strlen(label), label, item_name) > 0){return 0;};
		sscanf(buf, "%s %s %d", header_chara, item_name, &num_array);
	};
	return num_array;
}

int find_control_end_array_flag_c(FILE *fp, const char *label){
	int iflag = 0;
	char *header_chara, *item_name, *array_head;
	char buf[LENGTHBUF];
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s", header_chara);
	if(compare_string(3, header_chara, label_end) > 0){
		sscanf(buf, "%s %s", header_chara, array_head);
		if(compare_string(5, array_head, label_array) > 0){
			sscanf(buf, "%s %s %s", header_chara, array_head, item_name);
			iflag = compare_string(strlen(label), label, item_name);
		};
	};
	return iflag;
}


void read_control_array_c2_r_list(FILE *fp, int len_c1, int len_c2,
			const char *entity, char *c1_tbl, char*c2_tbl, double *vect){
	char buf[LENGTHBUF];            /* character buffer for reading line */
	char ctmp0[60], ctmp3[60];   /* character buffer for reading line */
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s %s %s %s ", ctmp0, c1_tbl, c2_tbl, ctmp3);
	*vect= atof(ctmp3);
	
	return;
}

void write_control_c2_r_list(FILE *fp, int level, int len_c1, int len_c2,
			const char *entity, const char *c1_tbl, const char*c2_tbl, double vect){
	char *fmt;
	
	sprintf(fmt, "%%s %ds %ds \\n", len_c1, len_c2);
	
	write_space_4_parse_c(fp, level);
	fprintf(fp, fmt, entity, c1_tbl, c2_tbl, vect);
	
	return;
}

void write_control_array_c2_r_list(FILE *fp, int level, int num,
			const char *entity, const char **c1_tbl, const char **c2_tbl, double *vect){
	int i;
	int len_c1 = 16;
	int len_c2 = 16;
	
	write_space_4_parse_c(fp, level);
	fprintf(fp, "begin array %s \n", entity);
	for(i=0;i<num;i++){
		write_control_c2_r_list(fp, (level+2), len_c1, len_c2, 
					entity, c1_tbl[i], c2_tbl[i], vect[i]);
	}
	write_space_4_parse_c(fp, level);
	fprintf(fp, "end array %s \n", entity);
	return;
}
