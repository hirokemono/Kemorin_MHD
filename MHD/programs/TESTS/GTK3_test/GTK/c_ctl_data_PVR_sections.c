/*
//  c_ctl_data_PVR_sections.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_PVR_sections.h"

extern void * c_PVR_section_ctl_block_name(void *f_pvr_sect_ctl);
extern void * c_PVR_section_ctl_iflag(void *f_pvr_sect_ctl);
extern void * c_PVR_section_fname_sect_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_psf_def_c(void *f_pvr_sect_ctl);
extern void * c_PVR_section_opacity_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_zeroline_ctl(void *f_pvr_sect_ctl);

extern void * c_VIZ_PVR_sects_block_name(void *f_pvr_sect_ctl);
extern int    c_VIZ_PVR_num_pvr_sect_ctl(void *f_pvr_scts_c);
extern void * c_VIZ_PVR_section_ctl(int idx, void *f_pvr_scts_c);

extern void * c_VIZ_PVR_isos_block_name(void *f_pvr_isos_c);
extern int    c_VIZ_PVR_num_pvr_iso_ctl(void *f_pvr_isos_c);
extern void * c_VIZ_PVR_isosurface_ctl(int idx, void *f_pvr_isos_c);

extern void * c_PVR_isosurf_ctl_block_name(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_ctl_iflag(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_type_ctl(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_iso_value_ctl(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_opacity_ctl(void *f_pvr_iso_ctl);


static struct f_PVR_section_ctl * init_f_PVR_section_ctl(void *(*c_load_self)(int idx, void *f_parent),
                                                         int idx, void *f_parent)
{
    struct f_PVR_section_ctl *f_pvr_sect_ctl
            = (struct f_PVR_section_ctl *) malloc(sizeof(struct f_PVR_section_ctl));
    if(f_pvr_sect_ctl == NULL){
        printf("malloc error for f_PVR_section_ctl\n");
        exit(0);
    };
    f_pvr_sect_ctl->f_self =  c_load_self(idx, f_parent);
    f_pvr_sect_ctl->f_iflag =   (int *) c_PVR_section_ctl_iflag(f_pvr_sect_ctl->f_self);
    char *f_block_name =   (char *) c_PVR_section_ctl_block_name(f_pvr_sect_ctl->f_self);
    f_pvr_sect_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_block_name = (char *) c_PVR_section_fname_sect_ctl(f_pvr_sect_ctl->f_self);
    f_pvr_sect_ctl->f_psf_def_c = init_f_VIZ_PSF_def_ctl(strngcopy_from_f(f_block_name), 
                                                         c_PVR_section_psf_def_c, f_pvr_sect_ctl->f_self);
    
    f_pvr_sect_ctl->f_opacity_ctl =         init_f_ctl_real_item(c_PVR_section_opacity_ctl,
                                                                 f_pvr_sect_ctl->f_self);
    f_pvr_sect_ctl->f_zeroline_switch_ctl = init_f_ctl_chara_item(c_PVR_section_zeroline_ctl,
                                                                  f_pvr_sect_ctl->f_self);
    return f_pvr_sect_ctl;
};


static void dealloc_f_PVR_section_ctl(struct f_PVR_section_ctl *f_pvr_sect_ctl){
    
	dealloc_f_VIZ_PSF_def_ctl((void *) f_pvr_sect_ctl->f_psf_def_c);
	dealloc_chara_ctl_item_c(f_pvr_sect_ctl->f_zeroline_switch_ctl);
    dealloc_real_ctl_item_c(f_pvr_sect_ctl->f_opacity_ctl);
    
    free(f_pvr_sect_ctl->c_block_name);
    f_pvr_sect_ctl->f_iflag = NULL;
    f_pvr_sect_ctl->f_self = NULL;
    free(f_pvr_sect_ctl);
    return;
};


struct void_clist * init_f_PVR_sections_ctl(void *f_parent)
{
    char *f_block_name = c_VIZ_PVR_sects_block_name(f_parent);
	struct void_clist *f_pvr_scts_c = init_void_clist(strngcopy_from_f(f_block_name));
    f_pvr_scts_c->f_parent = f_parent;
    int num = c_VIZ_PVR_num_pvr_sect_ctl(f_pvr_scts_c->f_parent);
	int i;
	for(i=0;i<num;i++){
        struct f_PVR_section_ctl *f_ctl_tmp = init_f_PVR_section_ctl(c_VIZ_PVR_section_ctl,
                                                                     i, f_pvr_scts_c->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_pvr_scts_c);
	}
	return f_pvr_scts_c;
}


void dealloc_f_PVR_sections_ctl(struct void_clist *f_pvr_scts_c)
{
	int i;
	for(i=0;i<count_void_clist(f_pvr_scts_c);i++){
        struct f_PVR_section_ctl *f_ctl_tmp = void_clist_at_index(i, f_pvr_scts_c);
		dealloc_f_PVR_section_ctl(f_ctl_tmp);
    }
    
    f_pvr_scts_c->f_parent = NULL;
    free(f_pvr_scts_c->clist_name);
    free(f_pvr_scts_c);
	return;
}

static struct f_PVR_isosurface_ctl * init_f_PVR_isosurface_ctl(void *(*c_load_self)(int idx, void *f_parent),
                                                               int idx, void *f_parent)
{
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl
            = (struct f_PVR_isosurface_ctl *) malloc(sizeof(struct f_PVR_isosurface_ctl));
    if(f_pvr_iso_ctl == NULL){
        printf("malloc error for f_PVR_isosurface_ctl\n");
        exit(0);
    };
    f_pvr_iso_ctl->f_self =  c_load_self(idx, f_parent);
    f_pvr_iso_ctl->f_iflag =   (int *) c_PVR_isosurf_ctl_iflag(f_pvr_iso_ctl->f_self);
    char *f_block_name =   (char *) c_PVR_isosurf_ctl_block_name(f_pvr_iso_ctl->f_self);
    f_pvr_iso_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_pvr_iso_ctl->f_isosurf_type_ctl = init_f_ctl_chara_item(c_PVR_isosurf_type_ctl,
                                                              f_pvr_iso_ctl->f_self);
    f_pvr_iso_ctl->f_iso_value_ctl =    init_f_ctl_real_item(c_PVR_isosurf_iso_value_ctl,
                                                             f_pvr_iso_ctl->f_self);
    f_pvr_iso_ctl->f_opacity_ctl =      init_f_ctl_real_item(c_PVR_isosurf_opacity_ctl,
                                                             f_pvr_iso_ctl->f_self);
    return f_pvr_iso_ctl;
};

static void dealloc_f_PVR_isosurface_ctl(struct f_PVR_isosurface_ctl *f_pvr_iso_ctl){
    
	dealloc_chara_ctl_item_c(f_pvr_iso_ctl->f_isosurf_type_ctl);
	dealloc_real_ctl_item_c(f_pvr_iso_ctl->f_iso_value_ctl);
    dealloc_real_ctl_item_c(f_pvr_iso_ctl->f_opacity_ctl);
    
    free(f_pvr_iso_ctl->c_block_name);
    f_pvr_iso_ctl->f_iflag = NULL;
    f_pvr_iso_ctl->f_self = NULL;
    free(f_pvr_iso_ctl);
    return;
};

struct void_clist * init_f_PVR_isosurfs_ctl(void *f_parent)
{
    char *f_block_name = c_VIZ_PVR_isos_block_name(f_parent);
	struct void_clist *f_pvr_isos_c = init_void_clist(strngcopy_from_f(f_block_name));
    f_pvr_isos_c->f_parent = f_parent;
    
	int i;
	for(i=0;i<c_VIZ_PVR_num_pvr_iso_ctl(f_pvr_isos_c->f_parent);i++){
        struct f_PVR_isosurface_ctl *f_ctl_tmp = init_f_PVR_isosurface_ctl(c_VIZ_PVR_isosurface_ctl,
                                                                           i, f_pvr_isos_c->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_pvr_isos_c);
	}
	return f_pvr_isos_c;
}

void dealloc_f_PVR_isosurfs_ctl(struct void_clist *f_pvr_isos_c)
{
	int i;
	for(i=0;i<count_void_clist(f_pvr_isos_c);i++){
        struct f_PVR_isosurface_ctl *f_ctl_tmp = void_clist_at_index(i, f_pvr_isos_c);
		dealloc_f_PVR_isosurface_ctl(f_ctl_tmp);
    }
    
    f_pvr_isos_c->f_parent = NULL;
    free(f_pvr_isos_c->clist_name);
    free(f_pvr_isos_c);
	return;
}
