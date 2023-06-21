
/* skip_comment_c.c */

#include "skip_comment_c.h"

/* Allocate string with lengthchara */
char* alloc_string(long lengthchara){
    char *string;
    if((string = (char *)calloc(lengthchara+1, sizeof(char))) == NULL){
        printf("malloc error for string\n");
        exit(0);
    };
    return string;
};

/* trim tail */
char *ltrim(char *s)
{
    if (s && *s) {
        size_t len = strlen(s);
        size_t lws = strspn(s, " \n\r\t\v");
        memmove(s, s + lws, len -= lws);
        s[len] = '\0';
    }
    return s;
}

/* trim head spaces */
char *rtrim(char *s)
{
    if (s && *s) {
        size_t len = strlen(s);
        while (len && isspace(s[len-1])) --len;
        s[len] = '\0';
    }
    return s;
}

/* trim both sides spaces */
char *trim(char *s)
{
  return ltrim(rtrim(s));
}

long skip_comment_c(FILE *fp){
    char buf[LENGTHBUF];    /* array for reading line */
	long offset = ftell(fp);
	
	while ((fgets(buf, LENGTHBUF, fp)) != NULL) {
		if ((buf[0] != '!') && (buf[0] != '#') && (buf[0] != '\n')) break;
		offset = ftell(fp);
	};
	fseek(fp, offset, SEEK_SET);
	return offset;
};

static int toCapital(char c) {
  if (c >= 'a' && c <= 'z') {
   c = c - ('a' - 'A');
  };
  return c;
}

int count_comps_by_comma_c(FILE *fp){
	int num_comps = 0;
	long offset;
	int j;
	char buf[LENGTHBUF];    /* array for reading line */
	
	offset = skip_comment_c(fp);
	while ((fgets(buf, LENGTHBUF, fp)) != NULL) {
		/*printf("buf: %s \n",buf);*/
		if ((buf[0] == '0') || (buf[0] == '1') || (buf[0] == '2')
					|| (buf[0] == '3') || (buf[0] == '4') || (buf[0] == '5')
					|| (buf[0] == '6') || (buf[0] == '7') || (buf[0] == '8')
					|| (buf[0] == '9') || (buf[0] == '+') || (buf[0] == '-')
					) break;
		offset = ftell(fp);
		
		for (j = 0; j < LENGTHBUF; j++) {
			if (buf[j] == ',') num_comps = num_comps + 1;
		};
	};
	fseek(fp, offset, SEEK_SET);
	
	return num_comps;
};

int set_field_coordinate_flag(const char *data_name){
	int iflag_coord;
	int len;
	
    len = (int) strlen(data_name);
/*	printf("%d %s \n",len, data_name);*/
	
	if (data_name[len-4] == '_' && data_name[len-3] == 's'
		&& data_name[len-2] == 'p' && data_name[len-1] == 'h') {
		iflag_coord = 1;
	} else if (data_name[len-4] == '_' && data_name[len-3] == 'c'
			   && data_name[len-2] == 'y' && data_name[len-1] == 'l') {
		iflag_coord = 2;
	} else {
		iflag_coord = 0;
	}
	
	return iflag_coord;
};

int read_field_name_from_buffer(int len_buf, char *buf, char *data_name){
	int iflag_coord;
	int j, k;
	
	k = 0;
	for (j = 0; j < LENGTHBUF; j++) {
		/*printf("%d, %d, %c \n",j,k, buf[j]);*/
		if (buf[j] == ',') {
			k = j;
			break;
		}
		else if(buf[j] == ' '){
			k = 0;
		}
		else{
			data_name[k] = buf[j];
			k = k + 1;
		}
	};
	data_name[k] = '\0';
	/*printf("%s \n",data_name);*/
	iflag_coord = set_field_coordinate_flag(data_name);
	return iflag_coord;
};

void read_field_names(FILE *fp, int num, char **data_name, int *id_coord){
	int i, iflag;
	char buf[LENGTHBUF];    /* array for reading line */
	
	for (i = 0; i < num; i++) {
		fgets(buf, LENGTHBUF, fp);
		iflag = read_field_name_from_buffer(LENGTHBUF, buf, data_name[i]);
		id_coord[i] = iflag;
		/*printf("%d, %s \n", i, data_name[i]);*/
	};
	
	return;
};


void read_multi_field_name(FILE *fp, char **data_name){
	int i, j;
    long offset;
	int k = 0;
	char buf[LENGTHBUF];    /* array for reading line */
	
	offset = skip_comment_c(fp);
	
	i = 0;
	while ((fgets(buf, LENGTHBUF, fp)) != NULL) {
		/*printf("buf: %s \n",buf);*/
		if ((buf[0] == '0') || (buf[0] == '1') || (buf[0] == '2')
					|| (buf[0] == '3') || (buf[0] == '4') || (buf[0] == '5')
					|| (buf[0] == '6') || (buf[0] == '7') || (buf[0] == '8')
					|| (buf[0] == '9') || (buf[0] == '+') || (buf[0] == '-')
					) break;
		offset = ftell(fp);
		
		k = 0;
		for (j = 0; j < LENGTHBUF; j++) {
			if (buf[j] == ',') {
				data_name[i][k] = '\0';
				i = i + 1;
				k = 0;
			}
			else if(buf[j] == ' '){
				k = 0;
			}
			else{
				data_name[i][k] = buf[j];
				k = k + 1;
			};
		};
	};
	fseek(fp, offset, SEEK_SET);
	
	return;
};


int strngcopy_w_length(char *chara_out, const int len_chara, const char *chara_in){
    int j;
    
    for (j = 0; j < len_chara; j++) {
        if (chara_in[j] == ' ' || chara_in[j] == '\0') {
            
            return j;
        }
        else{
            chara_out[j] = chara_in[j];
        };
    };
    chara_out[len_chara] = '\0';
    /*printf("output %s \n",chara_out);*/
    return len_chara;
}

int strngcopy(char *chara_out, const char *chara_in){
    int j;
    
    int len_chara = (int) strlen(chara_in);
    j = strngcopy_w_length(chara_out, len_chara, chara_in);
    return j;
}

int compare_string(int length, const char *string1, const char *string2){
	int i, iflag;
	iflag = 1;
	for(i = 0; i<length;i++){
		if(string1[i] != string2[i]) {
			iflag= 0;
			break;};
	};
	return iflag;
}

int cmp_no_case_c(const char *string1, const char *string2) {
  int iflag = 1;
  while (*string1 != '\0' || *string2 != '\0')
    { if (toCapital(*string1) != toCapital(*string2)) {
        iflag = 0;
        break;
      }
      else {string1++; string2++;}
    };
  return iflag;
}

int check_cautation_require(const char *string){
	int iflag = 0;
	while (*string != '\0'){
		if (*string == '/' || *string == ';' || *string == ',') {
			iflag = 1;
			break;
		} else {
			string++;
		};
	}
	return iflag;
}

void strip_cautation_marks(char *string){
	int i, length;
	length = (int) strlen(string);
	if (string[0] == '\x27' || *string == '\x22'){
		for(i=0;i<length-2;i++){
			string[i] = string[i+1];
		}
		string[length-2] = '\0';
	};
	trim(string);
	return;
}

char * duplicate_underscore(const char *string){
	char *tmpchara;
	int i = 0;
	int icou = 1;
	int len = strlen(string);
	for(i=0;i<len;i++){
		if(string[i] == '_') icou = icou + 1;
	}
	if((tmpchara = (char *)calloc(len+icou, sizeof(char))) == NULL){
		printf("malloc error for tmpchara\n");
		exit(0);
	};
	icou = 0;
	for(i=0;i<len;i++){
		if(string[i] == '_'){
			tmpchara[icou] = '_';
			tmpchara[icou+1] = '_';
			icou = icou + 2;
		}else {
			tmpchara[icou] = string[i];
			icou = icou + 1;
		}
	}
	tmpchara[icou] = '\0';
	return tmpchara;
}

int get_index_from_file_head(const char *file_head, char *stripped_fhead){
	int int_stripped;
	int len_fhead, len_fhead_stripped;
	int j, num;
    char *buf = alloc_string(strlen(file_head));
	
	/*	printf("file_head: %s \n", file_head);*/
	len_fhead = (int) strlen(file_head);
	len_fhead_stripped = 0;
	j = len_fhead;
	while (len_fhead_stripped == 0 && j > 0) {
		j = j-1;
		if (file_head[j] == '.') {
			len_fhead_stripped = j;
		}
	}
	/*	printf("len_fhead_stripped %d \n", len_fhead_stripped);*/
	for(j=0;j<len_fhead_stripped;j++){
		stripped_fhead[j] = file_head[j];
	}
	stripped_fhead[len_fhead_stripped] = '\0';
	/*	printf("stripped_fhead: %s\n", stripped_fhead);*/
	
	num = len_fhead - len_fhead_stripped - 1;
	for(j=0;j<num;j++){
		buf[j] = file_head[j+len_fhead_stripped+1];
	}
	buf[num] = '\0';
	sscanf(buf, "%d", &int_stripped);
    free(buf);
	return int_stripped;
}

void get_ext_from_file_name_c(const char *file_name
							, char *stripped_fhead, char *stripped_ext){
	int len_fhead, len_fhead_stripped;
	int j, num;
	
	/*	printf("file_name: %s \n", file_name);*/
	len_fhead = (int) strlen(file_name);
	len_fhead_stripped = 0;
	j = len_fhead;
	while (len_fhead_stripped == 0 && j > 0) {
		j = j-1;
		if (file_name[j] == '.') {
			len_fhead_stripped = j;
		}
	}
	if (len_fhead_stripped == 0){
		len_fhead_stripped = len_fhead;
		num = 0;
	} else {
		num = len_fhead - len_fhead_stripped - 1;
	};
	/*	printf("len_fhead_stripped %d \n", len_fhead_stripped);*/
	for(j=0;j<len_fhead_stripped;j++){
		stripped_fhead[j] = file_name[j];
	}
	stripped_fhead[len_fhead_stripped] = '\0';

	/*	printf("stripped_fhead: %s\n", stripped_fhead);
	printf("len_fhead_stripped: %d %d %d \n", len_fhead, len_fhead_stripped, num); */
	
	for(j=0;j<num;j++){
		stripped_ext[j] = file_name[j+len_fhead_stripped+1];
	}
	stripped_ext[num] = '\0';
	
	return;
}

void split_dir_and_file_name_c(const char *file_name,
                                char *stripped_dir, char *stripped_fname){
	int len_fhead, len_fhead_stripped;
	int j, num;
	
	/*	printf("file_name: %s \n", file_name);*/
	len_fhead = (int) strlen(file_name);
	len_fhead_stripped = 0;
	j = len_fhead;
	while (len_fhead_stripped == 0 && j > 0) {
		j = j-1;
		if (file_name[j] == '/') {
			len_fhead_stripped = j;
		}
	}
	if (len_fhead_stripped == 0){
		len_fhead_stripped = len_fhead;
		num = 0;
	} else {
		num = len_fhead - len_fhead_stripped - 1;
	};
	/*	printf("len_fhead_stripped %d \n", len_fhead_stripped);*/
	for(j=0;j<len_fhead_stripped;j++){
		stripped_dir[j] = file_name[j];
	}
	stripped_dir[len_fhead_stripped] = '\0';
    
	/*	printf("stripped_dir: %s\n", stripped_dir);
     printf("len_fhead_stripped: %d %d %d \n", len_fhead, len_fhead_stripped, num); */
	
	for(j=0;j<num;j++){
		stripped_fname[j] = file_name[j+len_fhead_stripped+1];
	}
	stripped_fname[num] = '\0';
	
	return;
}

void add_ext_to_file_name_c(const char *file_head,
							const char *added_ext, char *file_name){
	strcpy(file_name, file_head);
	strcat(file_name, ".");
	strcat(file_name, added_ext);
	return;
};

int toggle_value_c(int current){
	int toggle = (current+1)%2;
	return toggle;
}

