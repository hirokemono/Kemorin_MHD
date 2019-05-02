/*
//
//  select_read_psf_viewer_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2013/09/21.
//
//
*/

#include "select_read_psf_viewer_c.h"

int check_gzip_kemoview_ucd_first(int iformat_ucd_file, int istep, const char *ucd_header,
			struct psf_data *viz_s){
	struct kv_string *ucd_m;
	int iflag_datatype;
	char step_file_head[LENGTHBUF];
	
    if ((ucd_m = (struct kv_string *) malloc(sizeof(struct kv_string))) == NULL) {
        printf("malloc error for ucd_m\n");
        exit(0);
    }
	alloc_set_ucd_field_file_name(iformat_ucd_file, istep, ucd_header, ucd_m);
	
    if (iformat_ucd_file == IFLAG_SURF_UCD_GZ) {
        iflag_datatype = read_kemoview_ucd_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTK_GZ){
        iflag_datatype = read_kemoview_vtk_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTK){
        iflag_datatype = read_kemoview_vtk(ucd_m->string, viz_s);
    } else {
        iflag_datatype = read_kemoview_ucd(ucd_m->string, viz_s);
	};
	
	dealloc_ucd_m_file_name(ucd_m);
	free(ucd_m);
	return iflag_datatype;
}

int check_gzip_psf_grd_first(int iformat_ucd_file, const char *ucd_header, 
			struct psf_data *viz_s){
	struct kv_string *ucd_m;
	int iflag_datatype;
    
    if ((ucd_m = (struct kv_string *) malloc(sizeof(struct kv_string))) == NULL) {
        printf("malloc error for ucd_m\n");
        exit(0);
    }
	alloc_set_grd_field_file_name(iformat_ucd_file, ucd_header, ucd_m);
	
    if (iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
        iflag_datatype = read_psf_grd_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTD_GZ){
        iflag_datatype = read_psf_vtg_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTD){
        iflag_datatype = read_psf_vtg(ucd_m->string, viz_s);
    } else {
        iflag_datatype = read_psf_grd(ucd_m->string, viz_s);
    };
    
    if (iflag_datatype < 0) printf("Read error for grid data %s \n", ucd_m->string);
	
	dealloc_ucd_m_file_name(ucd_m);
	free(ucd_m);
	return iflag_datatype;
}

void check_gzip_psf_udt_first(int iformat_ucd_file, int istep, const char *ucd_header,
			struct psf_data *viz_s){
	struct kv_string *ucd_m;
	int ierr;
	
    if ((ucd_m = (struct kv_string *) malloc(sizeof(struct kv_string))) == NULL) {
        printf("malloc error for ucd_m\n");
        exit(0);
    }
	alloc_set_ucd_field_file_name(iformat_ucd_file, istep, ucd_header, ucd_m);
	
    if (iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
        ierr = read_psf_udt_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTD_GZ){
        ierr = read_psf_vtd_gz(ucd_m->string, viz_s);
    } else if(iformat_ucd_file == IFLAG_SURF_VTD){
        ierr = read_psf_vtd(ucd_m->string, viz_s);
    } else {
        ierr = read_psf_udt(ucd_m->string, viz_s);
    };
    
    if (ierr != 0) {
        printf("Read error for %s\n", ucd_m->string);
        exit(1);
    };
    
	dealloc_ucd_m_file_name(ucd_m);
	free(ucd_m);
	return;
}
