/*
//  m_read_text_to_character.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/07/19.
*/

#include "m_read_text_to_character.h"

FILE *fp_txt;

struct file_text * init_file_text(const char *file_name){
	struct file_text *contents;
	
	if ((contents = (struct file_text *) malloc(sizeof(struct file_text))) == NULL){
		printf("malloc error for init_file_text\n");
		exit(0);
	}
	
	contents->file_name = alloc_string(strlen(file_name)+1);
	strcpy(contents->file_name, file_name);
	
	contents->len_line = 0;
	contents->len_chara = 1;
	contents->text = alloc_string(contents->len_chara);
	contents->text[0] = '\0';
	
	return contents;
};

void dealloc_file_text(struct file_text *contents){
	free(contents->text);
	free(contents);
};

void read_text_to_carray(struct file_text *contents){
	char *tmpchara;
	char buf[LENGTHBUF]; /* Text buffer */
	long len_buf;
	
	if ((fp_txt = fopen(contents->file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", contents->file_name);
		exit (2);                    /* terminate with error message */
	};
	
	while ((fgets(buf, LENGTHBUF-1, fp_txt)) != NULL){
		tmpchara = alloc_string(contents->len_chara);
		strcpy(tmpchara, contents->text);
		free(contents->text);
		
		len_buf = strlen(buf);
		contents->len_line = contents->len_line + 1;
		contents->len_chara = contents->len_chara + len_buf;
		contents->text = alloc_string(contents->len_chara);
		strcpy(contents->text, tmpchara);
		strcat(contents->text, buf);
		free(tmpchara);
	}
    fclose(fp_txt);                                /* Close file */
	
	return;
};

void check_file_contents(struct file_text *contents){
	printf("%s", contents->text);
	printf("file name:  %s\n", contents->file_name);
	printf("Number of line: %d\n", (int) contents->len_line);
	printf("Size of file:  %d\n", (int) contents->len_chara);
};

