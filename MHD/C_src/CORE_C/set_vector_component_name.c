/***********************************************************************
 !!  set_vector_component_name.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "set_vector_component_name.h"

int find_comp_id_from_by_sph_vector(const char *selected){
    if     (cmp_no_case_c(selected, hd_r) != 0){return  SPH_to_R;}
    else if(cmp_no_case_c(selected, hd_t) != 0){return  SPH_to_T;}
    else if(cmp_no_case_c(selected, hd_p) != 0){return  SPH_to_P;}
    
    else if(cmp_no_case_c(selected, hd_s) != 0){return  SPH_to_S;}
    else if(cmp_no_case_c(selected, hd_z) != 0){return  SPH_to_Z;}
    
    else if(cmp_no_case_c(selected, hd_x) != 0){return  SPH_to_X;}
    else if(cmp_no_case_c(selected, hd_y) != 0){return  SPH_to_Y;}
    
    else if(cmp_no_case_c(selected, hd_magitude) != 0){return SPH_to_MAG;};
    return -1;
}

void set_component_name_by_sph_vector(int icomp, char *comp_name){
    if     (icomp == SPH_to_R){sprintf(comp_name, hd_r);}
    else if(icomp == SPH_to_T){sprintf(comp_name, hd_t);}
    else if(icomp == SPH_to_P){sprintf(comp_name, hd_p);}
           
    else if(icomp == SPH_to_S){sprintf(comp_name, hd_s);}
    else if(icomp == SPH_to_Z){sprintf(comp_name, hd_z);}
            
    else if(icomp == SPH_to_X){sprintf(comp_name, hd_x);}
    else if(icomp == SPH_to_Y){sprintf(comp_name, hd_x);}
    
    else if(icomp == SPH_to_MAG){sprintf(comp_name, hd_magitude);};
    return;
};


int find_comp_id_from_by_cyl_vector(const char *selected){
    if     (cmp_no_case_c(selected, hd_s) != 0){return CYL_to_S;}
    else if(cmp_no_case_c(selected, hd_p) != 0){return CYL_to_P;}
    else if(cmp_no_case_c(selected, hd_z) != 0){return CYL_to_Z;}
    
    else if(cmp_no_case_c(selected, hd_r) != 0){return CYL_to_R;}
    else if(cmp_no_case_c(selected, hd_t) != 0){return CYL_to_T;}
    
    else if(cmp_no_case_c(selected, hd_x) != 0){return CYL_to_X;}
    else if(cmp_no_case_c(selected, hd_y) != 0){return CYL_to_Y;}
    
    else if(cmp_no_case_c(selected, hd_magitude) != 0){return CYL_to_MAG;};
    return -1;
}

void set_component_name_by_cyl_vector(int icomp, char *comp_name){
    if     (icomp == CYL_to_S){sprintf(comp_name, hd_s);}
    else if(icomp == CYL_to_P){sprintf(comp_name, hd_p);}
    else if(icomp == CYL_to_Z){sprintf(comp_name, hd_z);}
            
    else if(icomp == CYL_to_R){sprintf(comp_name, hd_r);}
    else if(icomp == CYL_to_T){sprintf(comp_name, hd_t);}
            
    else if(icomp == CYL_to_X){sprintf(comp_name, hd_x);}
    else if(icomp == CYL_to_Y){sprintf(comp_name, hd_y);}
    else if(icomp == CYL_to_Z){sprintf(comp_name, hd_z);}
    
    else if(icomp == CYL_to_MAG){sprintf(comp_name, hd_magitude);};
    return;
};


int find_comp_id_from_by_xyz_vector(const char *selected){
    if     (cmp_no_case_c(selected, hd_x) != 0){return XYZ_to_X;}
    else if(cmp_no_case_c(selected, hd_y) != 0){return XYZ_to_X;}
    else if(cmp_no_case_c(selected, hd_z) != 0){return XYZ_to_X;}
    
    else if(cmp_no_case_c(selected, hd_r) != 0){return XYZ_to_R;}
    else if(cmp_no_case_c(selected, hd_t) != 0){return XYZ_to_T;}
    else if(cmp_no_case_c(selected, hd_p) != 0){return XYZ_to_P;}
    
    else if(cmp_no_case_c(selected, hd_s) != 0){return XYZ_to_S;}
    
    else if(cmp_no_case_c(selected, hd_magitude) != 0){return XYZ_to_MAG;};
    return -1;
}

void set_component_name_by_xyz_vector(int icomp, char *comp_name){
    if     (icomp == XYZ_to_X){sprintf(comp_name, hd_x);}
    else if(icomp == XYZ_to_Y){sprintf(comp_name, hd_y);}
    else if(icomp == XYZ_to_Z){sprintf(comp_name, hd_z);}
            
    else if(icomp == XYZ_to_R){sprintf(comp_name, hd_r);}
    else if(icomp == XYZ_to_T){sprintf(comp_name, hd_t);}
    else if(icomp == XYZ_to_P){sprintf(comp_name, hd_p);}
            
    else if(icomp == XYZ_to_S){sprintf(comp_name, hd_s);}
    
    else if(icomp == XYZ_to_MAG){sprintf(comp_name, hd_magitude);};
    return;
};


int find_vector_component_id(const int id_coord, const char *selected){
    if(id_coord == SPH_COORDINATE){
        return find_comp_id_from_by_sph_vector(selected);
    }else if(id_coord == CYL_COORDINATE){
        return find_comp_id_from_by_cyl_vector(selected);
    } else {
        return find_comp_id_from_by_xyz_vector(selected);
    };
    return -1;
}

void set_vector_component_name(const int id_coord, const int icomp, 
                               char *comp_name){
    if(id_coord == SPH_COORDINATE){
        set_component_name_by_sph_vector(icomp, comp_name);
    }else if(id_coord == CYL_COORDINATE){
        set_component_name_by_cyl_vector(icomp, comp_name);
    } else {
        set_component_name_by_xyz_vector(icomp, comp_name);
    };
    return;
};
