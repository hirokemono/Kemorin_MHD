/*
//  m_read_text_to_character.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/07/19.
*/

#ifndef m_read_text_to_character_h__
#define m_read_text_to_character_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "skip_comment_c.h"

struct file_text{
	long len_chara;
	long len_line;
	char *file_name;
	
	char *text;
};

/* prototype */

struct file_text * init_file_text(const char *file_name);
void dealloc_file_text(struct file_text *contents);
void read_text_to_carray(struct file_text *contents);
void check_file_contents(struct file_text *contents);

#endif /* m_read_text_to_character_h__ */
