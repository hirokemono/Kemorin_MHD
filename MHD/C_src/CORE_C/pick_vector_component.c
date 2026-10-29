/***********************************************************************
 !!  pick_vector_component.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "pick_vector_component.h"

void pick_vector_comp_c(int icomp, int numnod, 
                        const double *vector, double *component){
    int i;
    for(i=0;i<numnod;i++){component[i] = vector[i+icomp*numnod];};
    return;
};

void magnitude_of_vector_c(int numnod, const double *vector,
                           double *magnitude){
    int i;
    for(i=0;i<numnod;i++){
        magnitude[i] = sqrt(  vector[i         ]*vector[i         ]
                            + vector[i+1*numnod]*vector[i+1*numnod]
                            + vector[i+2*numnod]*vector[i+2*numnod]);
    };
    return;
};

void pick_vector_component_by_sph(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component){
    if      (icomp == SPH_to_R){
        pick_vector_comp_c(SPH_to_R, numnod, vector, component);
    }else if(icomp == SPH_to_T){
        pick_vector_comp_c(SPH_to_T, numnod, vector, component);
    }else if(icomp == SPH_to_P){
        pick_vector_comp_c(SPH_to_P, numnod, vector, component);
           
    }else if(icomp == SPH_to_S){
        s_comp_by_sph_vector_c(numnod, theta, vector, component);
    }else if(icomp == SPH_to_Z){
        z_comp_by_sph_vector_c(numnod, theta, vector, component);
            
    }else if(icomp == SPH_to_X){
        x_comp_by_sph_vector_c(numnod, theta, phi, vector, component);
    }else if(icomp == SPH_to_Y){
        y_comp_by_sph_vector_c(numnod, theta, phi, vector, component);
    
    }else if(icomp == SPH_to_MAG){
        magnitude_of_vector_c(numnod, vector, component);
    };
    return;
};

void pick_vector_component_by_cyl(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component){
    if      (icomp == CYL_to_S){
        pick_vector_comp_c(CYL_to_S, numnod, vector, component);
    }else if(icomp == CYL_to_P){
        pick_vector_comp_c(CYL_to_P, numnod, vector, component);
    }else if(icomp == CYL_to_Z){
        pick_vector_comp_c(CYL_to_Z, numnod, vector, component);
            
    }else if(icomp == CYL_to_R){
        r_comp_by_cyl_vector_c(numnod, theta, vector, component);
    }else if(icomp == CYL_to_T){
        theta_comp_by_cyl_vector_c(numnod, theta, vector, component);
            
    }else if(icomp == CYL_to_X){
        x_comp_by_cyl_vector_c(numnod, phi, vector, component);
    }else if(icomp == CYL_to_Y){
        y_comp_by_cyl_vector_c(numnod, phi, vector, component);
    
    }else if(icomp == CYL_to_MAG){
        magnitude_of_vector_c(numnod, vector, component);
    };
    return;
};

void pick_vector_component_by_xyz(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component){
    if     (icomp == XYZ_to_X){
        pick_vector_comp_c(XYZ_to_X, numnod, vector, component);
    }else if(icomp == XYZ_to_Y){
        pick_vector_comp_c(XYZ_to_Y, numnod, vector, component);
    }else if(icomp == XYZ_to_Z){
        pick_vector_comp_c(XYZ_to_Z, numnod, vector, component);
            
    }else if(icomp == XYZ_to_R){
        r_comp_by_xyz_vector_c(numnod, theta, phi, vector, component);
    }else if(icomp == XYZ_to_T){
        theta_comp_by_xyz_vector_c(numnod, theta, phi, vector, component);
    }else if(icomp == XYZ_to_P){
        phi_comp_by_xyz_vector_c(numnod, phi, vector, component);
            
    }else if(icomp == XYZ_to_S){
        s_comp_by_xyz_vector_c(numnod, phi, vector, component);
    
    }else if(icomp == XYZ_to_MAG){
        magnitude_of_vector_c(numnod, vector, component);
    };
    return;
};


void pick_vector_component(const int id_coord, const int icomp, 
                           const int numnod, const double *theta, const double *phi,
                           const double *vector, double *component){
    if(id_coord == SPH_COORDINATE){
        pick_vector_component_by_sph(icomp, numnod, theta, phi,
                                     vector, component);
    }else if(id_coord == CYL_COORDINATE){
        pick_vector_component_by_cyl(icomp, numnod, theta, phi,
                                     vector, component);
    } else {
        pick_vector_component_by_xyz(icomp, numnod, theta, phi,
                                     vector, component);
    };
    return;
};

void convert_to_cartesian_vector(const int id_coord, const int numnod, 
                                 const double *theta, const double *phi,
                                 const double *vector, double *v_xyz){
    if(id_coord == SPH_COORDINATE){
        xyz_vector_by_sph_c(numnod, theta, phi, vector, v_xyz);
    }else if(id_coord == CYL_COORDINATE){
        cyl_vector_by_xyz_c(numnod, theta, phi, vector, v_xyz);
    }else{
        pick_vector_comp_c(IZERO, numnod, vector, &v_xyz[0]);
    };
    return;
};
