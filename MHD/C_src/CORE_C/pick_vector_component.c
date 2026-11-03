/***********************************************************************
 !!  pick_vector_component.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "pick_vector_component.h"

void pick_vector_comp_c(int icomp, int nnod,
                        double *vector, float *component){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){component[i] = (float) vector[i+icomp*nnod];};
    return;
};

void magnitude_of_vector_c(int nnod, double *vector, float *magnitude){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        magnitude[i] = (float) sqrt(  vector[i       ]*vector[i       ]
                                    + vector[i+1*nnod]*vector[i+1*nnod]
                                    + vector[i+2*nnod]*vector[i+2*nnod]);
    };
    return;
};

void pick_vector_component_by_sph(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component){
    if      (icomp == SPH_to_R){
        pick_vector_comp_c(SPH_to_R, nnod, vector, component);
    }else if(icomp == SPH_to_T){
        pick_vector_comp_c(SPH_to_T, nnod, vector, component);
    }else if(icomp == SPH_to_P){
        pick_vector_comp_c(SPH_to_P, nnod, vector, component);
           
    }else if(icomp == SPH_to_S){
        s_vector_field_from_sph(nnod, theta, vector, component);
    }else if(icomp == SPH_to_Z){
        z_vector_field_from_sph(nnod, theta, vector, component);
            
    }else if(icomp == SPH_to_X){
        x_vector_field_from_sph(nnod, theta, phi, vector, component);
    }else if(icomp == SPH_to_Y){
        y_vector_field_from_sph(nnod, theta, phi, vector, component);
    
    }else if(icomp == SPH_to_MAG){
        magnitude_of_vector_c(nnod, vector, component);
    };
    return;
};

void pick_vector_component_by_cyl(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component){
    if      (icomp == CYL_to_S){
        pick_vector_comp_c(CYL_to_S, nnod, vector, component);
    }else if(icomp == CYL_to_P){
        pick_vector_comp_c(CYL_to_P, nnod, vector, component);
    }else if(icomp == CYL_to_Z){
        pick_vector_comp_c(CYL_to_Z, nnod, vector, component);
            
    }else if(icomp == CYL_to_R){
        r_vector_field_from_cyl(nnod, theta, vector, component);
    }else if(icomp == CYL_to_T){
        theta_vector_field_from_cyl(nnod, theta, vector, component);
            
    }else if(icomp == CYL_to_X){
        x_vector_field_from_cyl(nnod, phi, vector, component);
    }else if(icomp == CYL_to_Y){
        y_vector_field_from_cyl(nnod, phi, vector, component);
    
    }else if(icomp == CYL_to_MAG){
        magnitude_of_vector_c(nnod, vector, component);
    };
    return;
};

void pick_vector_component_by_xyz(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component){
    if     (icomp == XYZ_to_X){
        pick_vector_comp_c(XYZ_to_X, nnod, vector, component);
    }else if(icomp == XYZ_to_Y){
        pick_vector_comp_c(XYZ_to_Y, nnod, vector, component);
    }else if(icomp == XYZ_to_Z){
        pick_vector_comp_c(XYZ_to_Z, nnod, vector, component);
            
    }else if(icomp == XYZ_to_R){
        r_vector_field_from_xyz(nnod, theta, phi, vector, component);
    }else if(icomp == XYZ_to_T){
        theta_vector_field_from_xyz(nnod, theta, phi, vector, component);
    }else if(icomp == XYZ_to_P){
        phi_vector_field_from_xyz(nnod, phi, vector, component);
            
    }else if(icomp == XYZ_to_S){
        s_vector_field_from_xyz(nnod, phi, vector, component);
    
    }else if(icomp == XYZ_to_MAG){
        magnitude_of_vector_c(nnod, vector, component);
    };
    return;
};


void pick_vector_component(const int id_coord, const int icomp, 
                           const int nnod, double *theta, double *phi,
                           double *vector, float *component){
    if(id_coord == SPH_COORDINATE){
        pick_vector_component_by_sph(icomp, nnod, theta, phi,
                                     vector, component);
    }else if(id_coord == CYL_COORDINATE){
        pick_vector_component_by_cyl(icomp, nnod, theta, phi,
                                     vector, component);
    } else {
        pick_vector_component_by_xyz(icomp, nnod, theta, phi,
                                     vector, component);
    };
    return;
};

void convert_to_cartesian_vector(const int id_coord, const int nnod, 
                                 double *theta, double *phi,
                                 double *vector, float *v_xyz){
    int i;
    if      (id_coord == SPH_COORDINATE){
        xyz_vector_field_by_sph_c(nnod, theta, phi, vector, v_xyz);
    }else if(id_coord == CYL_COORDINATE){
        xyz_vector_field_by_cyl_c(nnod, phi, vector, v_xyz);
    }else{
        for(i=0;i<3*nnod;i++){v_xyz[i] = (float) vector[i];}
    };
    return;
};
