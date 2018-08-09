/*
//  t_ctl_data_4_FEM_mesh_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/06/15.
*/

#include "t_ctl_data_4_FEM_mesh_c.h"

#define NLBL_FEM_MESH_CTL  6

const char label_FEM_mesh_ctl[NLBL_FEM_MESH_CTL][KCHARA_C] = {
    /*[0]*/    {"memory_conservation_ctl"},
    /*[1]*/    {"FEM_mesh_output_switch"},
    /*[2]*/    {"FEM_surface_output_switch"},
    /*[3]*/    {"FEM_viewer_mesh_output_switch"},
    
    /*[4]*/    {"delete_original_data_flag"},
    /*[5]*/    {"excluding_FEM_mesh_ctl"},
};

void alloc_FEM_mesh_control_c(struct FEM_mesh_control_c *Fmesh){
    int i;
    
    Fmesh->maxlen = 0;
    for (i=0;i<NLBL_FEM_MESH_CTL;i++){
        if(strlen(label_FEM_mesh_ctl[i]) > Fmesh->maxlen){
            Fmesh->maxlen = strlen(label_FEM_mesh_ctl[i]);
        };
    };
    
    Fmesh->memory_conservation_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(Fmesh->memory_conservation_c);
    Fmesh->FEM_mesh_output_switch_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(Fmesh->FEM_mesh_output_switch_c);
    Fmesh->FEM_surface_output_switch_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(Fmesh->FEM_surface_output_switch_c);
    Fmesh->FEM_viewer_output_switch_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(Fmesh->FEM_viewer_output_switch_c);
    
    Fmesh->FEM_sleeve_level_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
    init_ctl_int_item(Fmesh->FEM_sleeve_level_c);
    Fmesh->FEM_element_overlap_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(Fmesh->FEM_element_overlap_c);
    
    return;
};

void dealloc_FEM_mesh_control_c(struct FEM_mesh_control_c *Fmesh){
    free(Fmesh->FEM_sleeve_level_c);
    
    dealloc_ctl_chara_item(Fmesh->memory_conservation_c);
    dealloc_ctl_chara_item(Fmesh->FEM_mesh_output_switch_c);
    dealloc_ctl_chara_item(Fmesh->FEM_surface_output_switch_c);
    dealloc_ctl_chara_item(Fmesh->FEM_viewer_output_switch_c);

    dealloc_ctl_chara_item(Fmesh->FEM_element_overlap_c);
    return;
};

int read_FEM_mesh_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
                                 struct FEM_mesh_control_c *Fmesh){
    while(find_control_end_flag_c(buf, label) == 0){
        
        skip_comment_read_line(fp, buf);
        
        read_character_ctl_item_c(buf, label_FEM_mesh_ctl[0], Fmesh->memory_conservation_c);
        read_character_ctl_item_c(buf, label_FEM_mesh_ctl[1], Fmesh->FEM_mesh_output_switch_c);
        read_character_ctl_item_c(buf, label_FEM_mesh_ctl[2], Fmesh->FEM_surface_output_switch_c);
        read_character_ctl_item_c(buf, label_FEM_mesh_ctl[3], Fmesh->FEM_viewer_output_switch_c);
        
        read_integer_ctl_item_c(buf, label_FEM_mesh_ctl[4], Fmesh->FEM_sleeve_level_c);
        read_character_ctl_item_c(buf, label_FEM_mesh_ctl[5], Fmesh->FEM_element_overlap_c);
    };
    return 1;
}

int write_FEM_mesh_control_c(FILE *fp, int level, const char *label, struct FEM_mesh_control_c *Fmesh){
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
    write_character_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[0], Fmesh->memory_conservation_c);
    write_character_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[1], Fmesh->FEM_mesh_output_switch_c);
    write_character_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[2], Fmesh->FEM_surface_output_switch_c);
    write_character_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[3], Fmesh->FEM_viewer_output_switch_c);
    
    write_integer_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[4], Fmesh->FEM_sleeve_level_c);
    write_character_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[5], Fmesh->FEM_element_overlap_c);
    
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}

