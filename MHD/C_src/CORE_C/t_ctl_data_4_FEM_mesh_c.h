/*
//  t_ctl_data_4_FEM_mesh_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/06/15.
*/

#ifndef t_ctl_data_4_FEM_mesh_c_h__
#define t_ctl_data_4_FEM_mesh_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"

struct FEM_mesh_control_c{
    int maxlen;

    struct chara_ctl_item *memory_conservation_c;
    struct chara_ctl_item *FEM_mesh_output_switch_c;
    struct chara_ctl_item *FEM_surface_output_switch_c;
    struct chara_ctl_item *FEM_viewer_output_switch_c;
    
    struct int_ctl_item *FEM_sleeve_level_c;
    struct chara_ctl_item *FEM_element_overlap_c;
};


/* prototypes */

void alloc_FEM_mesh_control_c(struct FEM_mesh_control_c *Fmesh);
void dealloc_FEM_mesh_control_c(struct FEM_mesh_control_c *Fmesh);

int read_FEM_mesh_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
                                 struct FEM_mesh_control_c *Fmesh);
int write_FEM_mesh_control_c(FILE *fp, int level, int *iflag, 
                                  const char *label, struct FEM_mesh_control_c *Fmesh);


#endif /* t_ctl_data_4_FEM_mesh_c_h */
