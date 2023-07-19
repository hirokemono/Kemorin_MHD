/*
//  c_ctl_data_PSF_ISOs.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_PSF_ISOs.h"

extern void * c_VIZ_PSF_ctl_block_name(void *f_psf_ctl);
extern void * c_VIZ_PSF_ctl_iflag(void *f_psf_ctl);
extern void * c_VIZ_PSF_fname_section_ctl(void *f_psf_ctl);
extern void * c_VIZ_PSF_psf_def_c(void *f_psf_ctl);
extern void * c_VIZ_PSF_fname_fld_on_psf(void *f_psf_ctl);
extern void * c_VIZ_PSF_fld_on_psf_c(void *f_psf_ctl);
extern void * c_VIZ_PSF_file_head_ctl(void *f_psf_ctl);
extern void * c_VIZ_PSF_output_type_ctl(void *f_psf_ctl);

extern void * c_VIZ_psf_define_ctl_block_name(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_ctl_iflag(void *f_psf_def_ctl);
extern void * c_VIZ_psf_def_sect_method_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_coefs_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_center_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_normal_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_axis_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_radius_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_grp_name_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_area_ctl(void *f_psf_def_ctl);

extern void * c_VIZ_fld_on_psf_ctl_block_name(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_ctl_iflag(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_field_out_ctl(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_out_value_ctl(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_out_type_ctl(void *f_psf_fld_ctl);

extern void * c_VIZ_ISO_ctl_block_name(void *f_iso_ctl);
extern void * c_VIZ_ISO_ctl_iflag(void *f_iso_ctl);
extern void * c_VIZ_ISO_iso_def_ctl(void *f_iso_ctl);
extern void * c_VIZ_ISO_fname_fld_on_iso(void *f_iso_ctl);
extern void * c_VIZ_ISO_fld_on_iso_c(void *f_iso_ctl);
extern void * c_VIZ_ISO_file_head_ctl(void *f_iso_ctl);
extern void * c_VIZ_ISO_output_type_ctl(void *f_iso_ctl);

extern void * c_VIZ_iso_define_ctl_block_name(void *f_iso_def_ctl);
extern void * c_VIZ_iso_define_ctl_iflag(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_data_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_comp_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_value_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_area_ctl(void *f_iso_def_ctl);


extern void * c_section_ctls_block_name(void *f_psf_ctls);
extern int    c_section_ctls_num_psf_ctl(void *f_psf_ctls);
extern char * c_section_ctls_fname(int idx, void *f_psf_ctls);
extern void * c_section_ctls_psf_ctl(int idx, void *f_psf_ctls);

extern void * c_isosurf_ctls_block_name(void *f_iso_ctls);
extern int    c_isosurf_ctls_num_iso_ctl(void *f_iso_ctls);
extern char * c_isosurf_ctls_fname(int idx, void *f_iso_ctls);
extern void * c_isosurf_ctls_iso_ctl(int idx, void *f_iso_ctls);



static struct f_VIZ_PSF_def_ctl * init_f_VIZ_PSF_def_ctl(char *file_name,
                                                  void *(*c_load_self)(void *f_parent), 
                                                  void *f_parent)
{
	struct f_VIZ_PSF_def_ctl *f_psf_def_c 
			= (struct f_VIZ_PSF_def_ctl *) malloc(sizeof(struct f_VIZ_PSF_def_ctl));
	if(f_psf_def_c == NULL){
		printf("malloc error for f_VIZ_PSF_def_ctl\n");
		exit(0);
	};
	
	f_psf_def_c->f_self =  c_load_self(f_parent);
    f_psf_def_c->psf_def_file_name = file_name;
	f_psf_def_c->f_iflag =   (int *) c_VIZ_psf_define_ctl_iflag(f_psf_def_c->f_self);
	char *f_block_name =   (char *) c_VIZ_psf_define_ctl_block_name(f_psf_def_c->f_self);
	f_psf_def_c->c_block_name = strngcopy_from_f(f_block_name);
    
    
    f_psf_def_c->f_section_method_ctl = init_f_ctl_chara_item(c_VIZ_psf_def_sect_method_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_coefs_ctl = init_f_ctl_cr_array(c_VIZ_psf_define_coefs_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_center_ctl = init_f_ctl_cr_array(c_VIZ_psf_define_center_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_normal_ctl = init_f_ctl_cr_array(c_VIZ_psf_define_normal_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_axis_ctl = init_f_ctl_cr_array(c_VIZ_psf_define_axis_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_radius_psf_ctl = init_f_ctl_real_item(c_VIZ_psf_define_radius_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_group_name_ctl = init_f_ctl_chara_item(c_VIZ_psf_define_grp_name_ctl,
                                                             f_psf_def_c->f_self);
    f_psf_def_c->f_psf_area_ctl = init_f_ctl_chara_array(c_VIZ_psf_define_area_ctl,
                                                             f_psf_def_c->f_self);
    return f_psf_def_c;
};

static void dealloc_f_VIZ_PSF_def_ctl(void *void_in)
{
    struct f_VIZ_PSF_def_ctl *f_psf_def_c = (struct f_VIZ_PSF_def_ctl *) void_in;
	
	f_psf_def_c->f_self =  NULL;
	
	free(f_psf_def_c->c_block_name);
    
    dealloc_chara_ctl_item_c(f_psf_def_c->f_section_method_ctl);
    dealloc_chara_real_clist(f_psf_def_c->f_psf_coefs_ctl);
    dealloc_chara_real_clist(f_psf_def_c->f_psf_center_ctl);
    dealloc_chara_real_clist(f_psf_def_c->f_psf_normal_ctl);
    dealloc_chara_real_clist(f_psf_def_c->f_psf_axis_ctl);
    dealloc_real_ctl_item_c(f_psf_def_c->f_radius_psf_ctl);
    dealloc_chara_ctl_item_c(f_psf_def_c->f_psf_group_name_ctl);
    dealloc_chara_clist(f_psf_def_c->f_psf_area_ctl);
    free(f_psf_def_c);
    return;
};

static struct f_VIZ_fld_on_PSF_ctl * init_f_VIZ_fld_on_PSF_ctl(char *file_name,
                                                               void *(*c_load_self)(void *f_parent), 
                                                               void *f_parent)
{
	struct f_VIZ_fld_on_PSF_ctl *f_fld_on_psf_c 
			= (struct f_VIZ_fld_on_PSF_ctl *) malloc(sizeof(struct f_VIZ_fld_on_PSF_ctl));
	if(f_fld_on_psf_c == NULL){
		printf("malloc error for f_VIZ_fld_on_PSF_ctl\n");
		exit(0);
	};
	
	f_fld_on_psf_c->f_self =  c_load_self(f_parent);
	
	f_fld_on_psf_c->f_iflag =   (int *) c_VIZ_fld_on_psf_ctl_iflag(f_fld_on_psf_c->f_self);
	char *f_block_name =   (char *) c_VIZ_fld_on_psf_ctl_block_name(f_fld_on_psf_c->f_self);
	f_fld_on_psf_c->c_block_name = strngcopy_from_f(f_block_name);
    f_fld_on_psf_c->fname_fld_on_psf = file_name;
    
    f_fld_on_psf_c->f_field_output_ctl = init_f_ctl_c2_array(c_VIZ_fld_on_psf_field_out_ctl, 
                                                             f_fld_on_psf_c->f_self);
    f_fld_on_psf_c->f_output_value_ctl = init_f_ctl_real_item(c_VIZ_fld_on_psf_out_value_ctl,
                                                             f_fld_on_psf_c->f_self);
    f_fld_on_psf_c->f_output_type_ctl = init_f_ctl_chara_item(c_VIZ_fld_on_psf_out_type_ctl,
                                                             f_fld_on_psf_c->f_self);
    return f_fld_on_psf_c;
};

static void dealloc_f_VIZ_fld_on_PSF_ctl(void *void_in)
{
    struct f_VIZ_fld_on_PSF_ctl *f_fld_on_psf_c = (struct f_VIZ_fld_on_PSF_ctl *) void_in;
	f_fld_on_psf_c->f_self = NULL;
	
	free(f_fld_on_psf_c->c_block_name);
    free(f_fld_on_psf_c->fname_fld_on_psf);
    
    dealloc_chara2_clist(f_fld_on_psf_c->f_field_output_ctl);
    dealloc_real_ctl_item_c(f_fld_on_psf_c->f_output_value_ctl);
    dealloc_chara_ctl_item_c(f_fld_on_psf_c->f_output_type_ctl);
    free(f_fld_on_psf_c);
    return;
};


struct f_VIZ_PSF_ctl * init_f_VIZ_PSF_ctl(int idx, void *f_parent)
{
	struct f_VIZ_PSF_ctl *f_psf_ctl 
			= (struct f_VIZ_PSF_ctl *) malloc(sizeof(struct f_VIZ_PSF_ctl));
	if(f_psf_ctl == NULL){
		printf("malloc error for f_VIZ_PSF_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_section_ctls_fname(idx, f_parent);
    f_psf_ctl->psf_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_psf_ctl->f_self =  c_section_ctls_psf_ctl(idx, f_parent);
	
	f_psf_ctl->f_iflag =   (int *) c_VIZ_PSF_ctl_iflag(f_psf_ctl->f_self);
	f_block_name =   (char *) c_VIZ_PSF_ctl_block_name(f_psf_ctl->f_self);
	f_psf_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    
	char *fname_tmp = c_VIZ_PSF_fname_section_ctl(f_psf_ctl->f_self);
    f_psf_ctl->f_psf_def_c = init_f_VIZ_PSF_def_ctl(strngcopy_from_f(fname_tmp),
                                                    c_VIZ_PSF_psf_def_c, 
                                                    f_psf_ctl->f_self);
    
    fname_tmp =  c_VIZ_PSF_fname_fld_on_psf(f_psf_ctl->f_self);
    f_psf_ctl->f_fld_on_psf_c = init_f_VIZ_fld_on_PSF_ctl(strngcopy_from_f(fname_tmp),
                                                          c_VIZ_PSF_fld_on_psf_c, f_psf_ctl->f_self);
    
    f_psf_ctl->f_psf_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_PSF_file_head_ctl,
                                                             f_psf_ctl->f_self);
    f_psf_ctl->f_psf_output_type_ctl = init_f_ctl_chara_item(c_VIZ_PSF_output_type_ctl,
                                                             f_psf_ctl->f_self);
    return f_psf_ctl;
};

void dealloc_f_VIZ_PSF_ctl(struct f_VIZ_PSF_ctl *f_psf_ctl)
{
    free(f_psf_ctl->psf_ctl_file_name);
	f_psf_ctl->f_self = NULL;
	
    free(f_psf_ctl->c_block_name);
    
    dealloc_f_VIZ_PSF_def_ctl(f_psf_ctl->f_psf_def_c);
    dealloc_f_VIZ_fld_on_PSF_ctl(f_psf_ctl->f_fld_on_psf_c);
    
    dealloc_chara_ctl_item_c(f_psf_ctl->f_psf_file_head_ctl);
    dealloc_chara_ctl_item_c(f_psf_ctl->f_psf_output_type_ctl);
    free(f_psf_ctl);
    return;
};


struct void_clist * init_f_VIZ_psf_ctls(void *f_parent, int *f_num_psf_ctl)
{
    char *f_block_name =   (char *) c_section_ctls_block_name(f_parent);
	struct void_clist *f_psf_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_psf_ctls->f_parent = f_parent;
	*f_num_psf_ctl = c_section_ctls_num_psf_ctl(f_psf_ctls->f_parent);
    
    if(*f_num_psf_ctl == 0){};
    struct f_VIZ_PSF_ctl *f_ctl_tmp;
    int i;
	for(i=0;i<*f_num_psf_ctl;i++){
        f_ctl_tmp = init_f_VIZ_PSF_ctl(i, f_psf_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_psf_ctls);
	}
	return f_psf_ctls;
}

static struct f_VIZ_ISO_def_ctl * init_f_VIZ_ISO_def_ctl(void *f_parent)
{
	struct f_VIZ_ISO_def_ctl *f_iso_def_c 
			= (struct f_VIZ_ISO_def_ctl *) malloc(sizeof(struct f_VIZ_ISO_def_ctl));
	if(f_iso_def_c == NULL){
		printf("malloc error for f_VIZ_ISO_def_ctl\n");
		exit(0);
    };
	f_iso_def_c->f_self =  c_VIZ_ISO_iso_def_ctl(f_parent);
	
	f_iso_def_c->f_iflag = (int *) c_VIZ_iso_define_ctl_iflag(f_iso_def_c->f_self);
	char *f_block_name =   (char *) c_VIZ_iso_define_ctl_block_name(f_iso_def_c->f_self);
	f_iso_def_c->c_block_name = strngcopy_from_f(f_block_name);
    
    f_iso_def_c->f_isosurf_data_ctl = init_f_ctl_chara_item(c_VIZ_isosurf_data_ctl,
                                                             f_iso_def_c->f_self);
    f_iso_def_c->f_isosurf_comp_ctl = init_f_ctl_chara_item(c_VIZ_isosurf_comp_ctl,
                                                             f_iso_def_c->f_self);
    f_iso_def_c->f_isosurf_value_ctl = init_f_ctl_real_item(c_VIZ_isosurf_value_ctl,
                                                            f_iso_def_c->f_self);
    f_iso_def_c->f_iso_area_ctl = init_f_ctl_chara_array(c_VIZ_isosurf_area_ctl,
                                                         f_iso_def_c->f_self);
    return f_iso_def_c;
};

static void dealloc_f_VIZ_ISO_def_ctl(struct f_VIZ_ISO_def_ctl *f_iso_def_c)
{
	f_iso_def_c->f_self =  NULL;
	free(f_iso_def_c->c_block_name);
    
    dealloc_chara_ctl_item_c(f_iso_def_c->f_isosurf_data_ctl);
    dealloc_chara_ctl_item_c(f_iso_def_c->f_isosurf_comp_ctl);
    dealloc_real_ctl_item_c(f_iso_def_c->f_isosurf_value_ctl);
    dealloc_chara_clist(f_iso_def_c->f_iso_area_ctl);
    free(f_iso_def_c);
    return;
};

struct f_VIZ_ISO_ctl * init_f_VIZ_ISO_ctl(int idx, void *f_parent)
{
	struct f_VIZ_ISO_ctl *f_iso_ctl 
			= (struct f_VIZ_ISO_ctl *) malloc(sizeof(struct f_VIZ_ISO_ctl));
	if(f_iso_ctl == NULL){
		printf("malloc error for f_VIZ_ISO_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_isosurf_ctls_fname(idx, f_parent);
    f_iso_ctl->iso_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_iso_ctl->f_self =  c_isosurf_ctls_iso_ctl(idx, f_parent);
	
	f_iso_ctl->f_iflag =   (int *) c_VIZ_ISO_ctl_iflag(f_iso_ctl->f_self);
	f_block_name =   (char *) c_VIZ_ISO_ctl_block_name(f_iso_ctl->f_self);
	f_iso_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    char *fname_tmp = c_VIZ_ISO_fname_fld_on_iso(f_iso_ctl->f_self);
    f_iso_ctl->f_fld_on_iso_c =   init_f_VIZ_fld_on_PSF_ctl(strngcopy_from_f(fname_tmp),
                                                            c_VIZ_ISO_fld_on_iso_c, f_iso_ctl->f_self);
    f_iso_ctl->f_iso_def_c =      init_f_VIZ_ISO_def_ctl(f_iso_ctl->f_self);
    
    f_iso_ctl->f_iso_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_ISO_file_head_ctl,
                                                             f_iso_ctl->f_self);
    f_iso_ctl->f_iso_output_type_ctl = init_f_ctl_chara_item(c_VIZ_ISO_output_type_ctl,
                                                             f_iso_ctl->f_self);
    return f_iso_ctl;
}

void dealloc_f_VIZ_ISO_ctl(struct f_VIZ_ISO_ctl *f_iso_ctl)
{
    free(f_iso_ctl->iso_ctl_file_name);
    f_iso_ctl->f_self = NULL;
    free(f_iso_ctl->c_block_name);
    
    dealloc_f_VIZ_fld_on_PSF_ctl(f_iso_ctl->f_fld_on_iso_c);
    dealloc_f_VIZ_ISO_def_ctl(f_iso_ctl->f_iso_def_c);
    
    dealloc_chara_ctl_item_c(f_iso_ctl->f_iso_file_head_ctl);
    dealloc_chara_ctl_item_c(f_iso_ctl->f_iso_output_type_ctl);
    free(f_iso_ctl);
    return;
}


struct void_clist * init_f_VIZ_iso_ctls(void *f_parent, int *f_num_iso_ctl)
{
    char *f_block_name =   (char *) c_isosurf_ctls_block_name(f_parent);
	struct void_clist *f_iso_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_iso_ctls->f_parent = f_parent;
	*f_num_iso_ctl = c_isosurf_ctls_num_iso_ctl(f_iso_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_iso_ctl;i++){
        struct f_VIZ_ISO_ctl *f_ctl_tmp = init_f_VIZ_ISO_ctl(i, f_iso_ctls->f_parent);
        append_void_clist((void *) f_ctl_tmp, f_iso_ctls);
	}
	return f_iso_ctls;
}

