/*
 *  set_kemoviewer_ucd_data.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/02.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_kemoviewer_ucd_data.h"

static int base_viewer_data_format_flag(char *file_ext){
	int ifile_type;
	
	if((file_ext[0] == 'k' && file_ext[1] == 's' && file_ext[2] == 'm')
	   || (file_ext[0] == 'K' && file_ext[1] == 'S' && file_ext[2] == 'M') ){
		ifile_type = IFLAG_SURF_MESH;
	}else if((file_ext[0] == 'u' && file_ext[1] == 'd' && file_ext[2] == 't')
			 || (file_ext[0] == 'U' && file_ext[1] == 'D' && file_ext[2] == 'T') ){
		ifile_type = IFLAG_SURF_UDT;
	}else if((file_ext[0] == 'i' && file_ext[1] == 'n' && file_ext[2] == 'p')
			 || (file_ext[0] == 'I' && file_ext[1] == 'N' && file_ext[2] == 'P') ){
		ifile_type = IFLAG_SURF_UCD;
	}else if((file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'd')
			 || (file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'D') ){
		ifile_type = IFLAG_SURF_VTD;
	}else if((file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'k')
			 ||	(file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'K') ){
		ifile_type = IFLAG_SURF_VTK;
	}else if((file_ext[0] == 's' && file_ext[1] == 'd' && file_ext[2] == 't')
			 || (file_ext[0] == 'S' && file_ext[1] == 'D' && file_ext[2] == 'T') ){
		ifile_type = IFLAG_SURF_SDT;
	}else if((file_ext[0] == 's' && file_ext[1] == 'f' && file_ext[2] == 'm')
			 ||	(file_ext[0] == 'S' && file_ext[1] == 'F' && file_ext[2] == 'M') ){
		ifile_type = IFLAG_PSF_BIN;
	} else {
		ifile_type = 99;
	}; 
	return ifile_type;
};

int set_data_format_flag(const char *file_name, char *file_head, char *file_ext){
	int ifile_type;
	
	char *file_head2 = alloc_string(strlen(file_name));
	
	get_ext_from_file_name_c(file_name, file_head, file_ext);
	
	if(   (file_ext[0] == 'g' && file_ext[1] == 'z') 
       || (file_ext[0] == 'G' && file_ext[1] == 'Z') ){
		get_ext_from_file_name_c(file_head, file_head2, file_ext);
		ifile_type = 100 + base_viewer_data_format_flag(file_ext);
		strngcopy(file_head, file_head2);
	}else{
		ifile_type = base_viewer_data_format_flag(file_ext);
	};
	free(file_head2);
	return ifile_type;
}

int kemoviewer_open_data(struct kv_string *filename, struct kemoview_mesh *kemo_mesh, 
			struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline, 
			struct psf_data *ucd_tmp, struct view_element *view){
	int iflag_datatype;
	int iflag_fileformat;
	int istep;
	
	struct kv_string *file_head_w_step = alloc_kvstring();
	struct kv_string *ucd_header = alloc_kvstring();
    struct kv_string *file_ext = alloc_kvstring();
	
    alloc_kvstringitem(strlen(filename->string), file_head_w_step);
    alloc_kvstringitem(strlen(filename->string), ucd_header);
    alloc_kvstringitem(strlen(filename->string), file_ext);
	
	iflag_fileformat = set_data_format_flag(filename->string, file_head_w_step->string, file_ext->string);
	printf("iflag_fileformat %d\n", iflag_fileformat);
	printf("file_name %s\n", filename->string);
    dealloc_kvstring(file_ext);
    
	if(   iflag_fileformat == IFLAG_SURF_MESH 
		  || iflag_fileformat == IFLAG_SURF_MESH_GZ){
        kemo_mesh->mesh_m->iformat_surface_mesh = iflag_fileformat;
		
		dealloc_kvstring(kemo_mesh->mesh_m->mesh_file_name);
		kemo_mesh->mesh_m->mesh_file_name = init_kvstring_by_string(filename->string);
        
		reset_draw_mesh(kemo_mesh);
		set_kemoview_mesh_data(kemo_mesh->mesh_d, kemo_mesh->mesh_m, view);
		iflag_datatype = IFLAG_MESH;
        
	} else if(   iflag_fileformat == IFLAG_SURF_UDT
				 || iflag_fileformat == IFLAG_SURF_UDT_GZ
				 || iflag_fileformat == IFLAG_SURF_SDT
				 || iflag_fileformat == IFLAG_SURF_SDT_GZ
				 || iflag_fileformat == IFLAG_SURF_VTD
				 || iflag_fileformat == IFLAG_SURF_VTD_GZ){
		istep = get_index_from_file_head(file_head_w_step->string, ucd_header->string);
		iflag_datatype = check_gzip_psf_grd_first(iflag_fileformat, ucd_header->string, ucd_tmp);
		if(iflag_datatype != 0){
			check_gzip_psf_udt_first(iflag_fileformat, istep, ucd_header->string, ucd_tmp);
			init_draw_psf(kemo_psf, ucd_tmp, iflag_fileformat, istep, ucd_header->string);
			view->iflag_view_type = VIEW_3D;
		} else{
			dealloc_psf_mesh_c(ucd_tmp);
		};
        
	} else if( iflag_fileformat == IFLAG_SURF_UCD
			   || iflag_fileformat == IFLAG_SURF_UCD_GZ
			   || iflag_fileformat == IFLAG_PSF_BIN
			   || iflag_fileformat == IFLAG_PSF_BIN_GZ
			   || iflag_fileformat == IFLAG_SURF_VTK
			   || iflag_fileformat == IFLAG_SURF_VTK_GZ){
		istep = get_index_from_file_head(file_head_w_step->string, ucd_header->string);
		
		iflag_datatype = check_gzip_kemoview_ucd_first(iflag_fileformat, istep, ucd_header->string, ucd_tmp);

        if(iflag_datatype == IFLAG_SURFACES){
			init_draw_psf(kemo_psf, ucd_tmp, iflag_fileformat, istep, ucd_header->string);
			view->iflag_view_type =   VIEW_3D;
		} else if(iflag_datatype == IFLAG_LINES){
			init_draw_fline(kemo_fline, ucd_tmp, iflag_fileformat, istep, ucd_header->string);
			view->iflag_view_type =   VIEW_3D;
		} else {
			dealloc_psf_data_s(ucd_tmp);
			dealloc_psf_mesh_c(ucd_tmp);
		}
	} else {
		iflag_datatype = 0;
	};
    
    if (kemo_mesh->mesh_m->iflag_draw_mesh == IZERO ) {
		cal_psf_viewer_range(kemo_psf->psf_d, kemo_psf->psf_a, 
					kemo_fline->fline_d, kemo_fline->fline_m, view);
        reset_to_init_angle(view);
    };
	
	dealloc_kvstring(ucd_header);
	dealloc_kvstring(file_head_w_step);
	
	return iflag_datatype;
}
