/*
// kemoviewer_base.h
*/
#ifndef KEMOVIEWER_BASE_
#define KEMOVIEWER_BASE_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemoviewer.h"

/* Prototypes */

void alloc_kvstringitem(unsigned long length, struct kv_string *ucd_m);
struct kv_string* alloc_kvstring(void);
struct kv_string* init_kvstring_by_string(const char *org_string);
void dealloc_kvstring(struct kv_string *kvstring);

void alloc_copy_string(const char *org_string, struct kv_string *ucd_copied);
void alloc_set_ucd_field_file_name(int iformat_ucd_file, int istep, const char *ucd_header,
			struct kv_string *ucd_m);
void alloc_set_grd_field_file_name(int iformat_ucd_file, const char *ucd_header, 
			struct kv_string *ucd_m);

#endif
