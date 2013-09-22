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

int check_gzip_kemoview_ucd_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s){
	int iflag_datatype;
	char step_file_head[LENGTHBUF];
	
	sprintf(step_file_head, "%s.%d",ucd_m->ucd_header, ucd_m->ucd_step);
    
    if (ucd_m->iformat_ucd_file == IFLAG_SURF_UCD_GZ) {
        iflag_datatype = read_kemoview_ucd_gz(step_file_head, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTK_GZ){
        iflag_datatype = read_kemoview_vtk_gz(step_file_head, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTK){
        iflag_datatype = read_kemoview_vtk(step_file_head, viz_s);
    } else {
        iflag_datatype = read_kemoview_ucd(step_file_head, viz_s);
    };

	return iflag_datatype;
}

int check_gzip_psf_grd_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s){
	int iflag_datatype;
    
    if (ucd_m->iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
        iflag_datatype = read_psf_grd_gz(ucd_m->ucd_header, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTD_GZ){
        iflag_datatype = read_psf_vtg_gz(ucd_m->ucd_header, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTD){
        iflag_datatype = read_psf_vtg(ucd_m->ucd_header, viz_s);
    } else {
        iflag_datatype = read_psf_grd(ucd_m->ucd_header, viz_s);
    };
    
    if (iflag_datatype < 0) printf("Read error for grid data %s \n", ucd_m->ucd_header);

	return iflag_datatype;
}

void check_gzip_psf_udt_first(struct ucd_file_menu_val *ucd_m, struct psf_data *viz_s){
	int ierr;
    
    if (ucd_m->iformat_ucd_file == IFLAG_SURF_UDT_GZ) {
        ierr = read_psf_udt_gz(ucd_m->ucd_header, ucd_m->ucd_step, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTD_GZ){
        ierr = read_psf_vtd_gz(ucd_m->ucd_header, ucd_m->ucd_step, viz_s);
    } else if(ucd_m->iformat_ucd_file == IFLAG_SURF_VTD){
        ierr = read_psf_vtd(ucd_m->ucd_header, ucd_m->ucd_step, viz_s);
    } else {
        ierr = read_psf_udt(ucd_m->ucd_header, ucd_m->ucd_step, viz_s);
    };
    
    if (ierr != 0) {
        printf("Read error for %s at %d\n", ucd_m->ucd_header, ucd_m->ucd_step);
        exit(1);
    };
    
	return;
}
