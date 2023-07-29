/*
//  t_ctl_data_4_FEM_mesh_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/06/15.
*/

#include "t_ctl_data_4_FEM_mesh_c.h"

const char label_FEM_mesh_ctl[NLBL_FEM_MESH_CTL][KCHARA_C] = {
    /*[0]*/    {"memory_conservation_ctl"},
    /*[1]*/    {"FEM_mesh_output_switch"},
    /*[2]*/    {"FEM_surface_output_switch"},
    /*[3]*/    {"FEM_viewer_mesh_output_switch"},
    
    /*[4]*/    {"sleeve_level_ctl"},
    /*[5]*/    {"element_overlap_ctl"}
};

void get_label_FEM_mesh_ctl(int index, char *label){
    if(index < NLBL_FEM_MESH_CTL) strngcopy(label, label_FEM_mesh_ctl[index]);
    return;
};

struct FEM_mesh_control_c * init_FEM_mesh_control_c(){
    int i;
    struct FEM_mesh_control_c *Fmesh;
    if((Fmesh = (struct FEM_mesh_control_c *) malloc(sizeof(struct FEM_mesh_control_c))) == NULL) {
        printf("malloc error for FEM_mesh_control_c \n");
        exit(0);
    }
    
    Fmesh->iflag_use = 0;
    Fmesh->maxlen = 0;
    for (i=0;i<NLBL_FEM_MESH_CTL;i++){
        if(strlen(label_FEM_mesh_ctl[i]) > Fmesh->maxlen){
            Fmesh->maxlen = (int) strlen(label_FEM_mesh_ctl[i]);
        };
    };
    
    Fmesh->memory_conservation_c = init_chara_ctl_item_c();
    Fmesh->FEM_mesh_output_switch_c = init_chara_ctl_item_c();
    Fmesh->FEM_surface_output_switch_c = init_chara_ctl_item_c();
    Fmesh->FEM_viewer_output_switch_c = init_chara_ctl_item_c();
    
    Fmesh->FEM_sleeve_level_c = init_int_ctl_item_c();
    Fmesh->FEM_element_overlap_c = init_chara_ctl_item_c();
    
    return Fmesh;
};

void dealloc_FEM_mesh_control_c(struct FEM_mesh_control_c *Fmesh){
    free(Fmesh->FEM_sleeve_level_c);
    
    dealloc_chara_ctl_item_c(Fmesh->memory_conservation_c);
    dealloc_chara_ctl_item_c(Fmesh->FEM_mesh_output_switch_c);
    dealloc_chara_ctl_item_c(Fmesh->FEM_surface_output_switch_c);
    dealloc_chara_ctl_item_c(Fmesh->FEM_viewer_output_switch_c);

    dealloc_chara_ctl_item_c(Fmesh->FEM_element_overlap_c);
    free(Fmesh);
    return;
};

void read_FEM_mesh_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
                                 struct FEM_mesh_control_c *Fmesh){
    while(find_control_end_flag_c(buf, label) == 0){
        
        skip_comment_read_line(fp, buf);
        
        read_chara_ctl_item_c(buf, label_FEM_mesh_ctl[0], Fmesh->memory_conservation_c);
        read_chara_ctl_item_c(buf, label_FEM_mesh_ctl[1], Fmesh->FEM_mesh_output_switch_c);
        read_chara_ctl_item_c(buf, label_FEM_mesh_ctl[2], Fmesh->FEM_surface_output_switch_c);
        read_chara_ctl_item_c(buf, label_FEM_mesh_ctl[3], Fmesh->FEM_viewer_output_switch_c);
        
        read_integer_ctl_item_c(buf, label_FEM_mesh_ctl[4], Fmesh->FEM_sleeve_level_c);
        read_chara_ctl_item_c(buf, label_FEM_mesh_ctl[5], Fmesh->FEM_element_overlap_c);
    };
    Fmesh->iflag_use = 1;
    return;
}

int write_FEM_mesh_control_c(FILE *fp, int level, const char *label, struct FEM_mesh_control_c *Fmesh){
    if(Fmesh->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
    write_chara_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[0], Fmesh->memory_conservation_c);
    write_chara_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[1], Fmesh->FEM_mesh_output_switch_c);
    write_chara_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[2], Fmesh->FEM_surface_output_switch_c);
    write_chara_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[3], Fmesh->FEM_viewer_output_switch_c);
    
    write_integer_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[4], Fmesh->FEM_sleeve_level_c);
    write_chara_ctl_item_c(fp, level, Fmesh->maxlen, label_FEM_mesh_ctl[5], Fmesh->FEM_element_overlap_c);
    
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}

