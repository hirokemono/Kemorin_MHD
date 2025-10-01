/*
// kemoviewer_base.c
*/

#include "kemoviewer_base.h"
#include "skip_comment_c.h"

void alloc_kvstringitem(unsigned long length, struct kv_string *ucd_m){
/*	ucd_m->string = alloc_string(length);*/
    if((ucd_m->string = (char *)calloc(length+1, sizeof(char))) == NULL){
        printf("malloc error for string\n");
        exit(0);
    };
	return;
};
void alloc_copy_string(const char *org_string, struct kv_string *ucd_copied){
	alloc_kvstringitem(strlen(org_string), ucd_copied);
	strngcopy(ucd_copied->string, org_string);
	return;
};

struct kv_string* alloc_kvstring(void){
	struct kv_string *kvstring;
	
    if ((kvstring = (struct kv_string *) malloc(sizeof(struct kv_string))) == NULL) {
        printf("malloc error for kvstring\n");
        exit(0);
    }
	return kvstring;
};

struct kv_string* init_kvstring_by_string(const char *org_string){
	struct kv_string *kvstring = alloc_kvstring();
	alloc_copy_string(org_string, kvstring);
	return kvstring;
};
void dealloc_kvstring(struct kv_string *kvstring){
	kvstring->string = NULL;
	free(kvstring);
	return;
};


void alloc_set_ucd_field_file_name(int iformat_ucd_file, int istep, const char *ucd_header,
			struct kv_string *ucd_m){
	alloc_kvstringitem(strlen(ucd_header)+25, ucd_m);
	
	if      (iformat_ucd_file == IFLAG_SURF_VTK_GZ){
		sprintf(ucd_m->string, "%s.%d.vtk.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_VTD_GZ){
		sprintf(ucd_m->string, "%s.%d.vtd.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_SDT_GZ) {
		sprintf(ucd_m->string, "%s.%d.sdt.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_PSF_BIN_GZ) {
		sprintf(ucd_m->string, "%s.%d.sfm.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
		sprintf(ucd_m->string, "%s.%d.udt.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_UCD_GZ) {
		sprintf(ucd_m->string, "%s.%d.inp.gz",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_VTK){
		sprintf(ucd_m->string, "%s.%d.vtk",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_VTD){
		sprintf(ucd_m->string, "%s.%d.vtd",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_SDT) {
		sprintf(ucd_m->string, "%s.%d.sdt",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_PSF_BIN) {
		sprintf(ucd_m->string, "%s.%d.sfm",ucd_header, istep);
	}else if(iformat_ucd_file == IFLAG_SURF_UDT){
		sprintf(ucd_m->string, "%s.%d.udt",ucd_header, istep);
	}else{
		sprintf(ucd_m->string, "%s.%d.inp",ucd_header, istep);
	};
	return;
};

void alloc_set_grd_field_file_name(int iformat_ucd_file, const char *ucd_header, 
			struct kv_string *ucd_m){
	alloc_kvstringitem(strlen(ucd_header)+25, ucd_m);
	
	if (iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
		sprintf(ucd_m->string, "%s.0.grd.gz",ucd_header);
	}else if(iformat_ucd_file == IFLAG_SURF_VTD_GZ){
		sprintf(ucd_m->string, "%s.0.vtg.gz",ucd_header);
    }else if(iformat_ucd_file == IFLAG_SURF_SDT_GZ) {
        sprintf(ucd_m->string, "%s.0.sgd.gz",ucd_header);
    }else if(iformat_ucd_file == IFLAG_SURF_SDT) {
        sprintf(ucd_m->string, "%s.0.sgd",ucd_header);
	}else if(iformat_ucd_file == IFLAG_SURF_UDT){
		sprintf(ucd_m->string, "%s.0.grd",ucd_header);
	}else if(iformat_ucd_file == IFLAG_SURF_VTD){
		sprintf(ucd_m->string, "%s.0.vtg",ucd_header);
	}else{
		sprintf(ucd_m->string, "%s.0.inp",ucd_header);
	};
	return;
};


