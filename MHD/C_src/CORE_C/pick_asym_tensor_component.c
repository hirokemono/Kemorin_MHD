/***********************************************************************
 !!  pick_asym_tensor_component.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "pick_asym_tensor_component.h"

void pick_asym_tensor_comp_by_sph(const int icomp, const int numnod,
                                 double *theta, double *phi,
                                 double *tensor, float *component){
    if      (icomp == SPH_to_RT){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == SPH_to_RP){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == SPH_to_TP){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == SPH_to_SP){
        sp_asym_tensor_field_from_sph(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_SZ){
        zs_asym_tensor_field_from_sph(numnod, tensor, component);
    }else if(icomp == SPH_to_PZ){
        pz_asym_tensor_field_from_sph(numnod, theta, tensor, component);
        
    }else if(icomp == SPH_to_XY){
        xy_asym_tensor_field_from_sph(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_XZ){
        zx_asym_tensor_field_from_sph(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_YZ){
        yz_asym_tensor_field_from_sph(numnod, theta, phi, tensor, component);
    };
    return;
};

void pick_asym_tensor_comp_by_cyl(const int icomp, const int numnod,
                                  double *theta, double *phi,
                                  double *tensor, float *component){
    if      (icomp == CYL_to_SP){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == CYL_to_SZ){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == CYL_to_PZ){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == CYL_to_RT){
        rt_asym_tensor_field_from_cyl(numnod, tensor, component);
    }else if(icomp == CYL_to_RP){
        pr_asym_tensor_field_from_cyl(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_TP){
        tp_asym_tensor_field_from_cyl(numnod, theta, tensor, component);
        
    }else if(icomp == CYL_to_XY){
        xy_asym_tensor_field_from_cyl(numnod, tensor, component);
    }else if(icomp == CYL_to_XZ){
        zx_asym_tensor_field_from_cyl(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_YZ){
        yz_asym_tensor_field_from_cyl(numnod, phi, tensor, component);
    };
    return;
};

void pick_asym_tensor_comp_by_xyz(const int icomp, const int numnod,
                                  double *theta, double *phi,
                                  double *tensor, float *component){
    if      (icomp == XYZ_to_XY){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == XYZ_to_XZ){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == XYZ_to_YZ){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == XYZ_to_RT){
        rt_asym_tensor_field_from_xyz(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_RP){
        pr_asym_tensor_field_from_xyz(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_TP){
        tp_asym_tensor_field_from_xyz(numnod, theta, phi, tensor, component);
        
    }else if(icomp == XYZ_to_SP){
        sp_asym_tensor_field_from_xyz(numnod, tensor, component);
    }else if(icomp == XYZ_to_SZ){
        zs_asym_tensor_field_from_xyz(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_PZ){
        pz_asym_tensor_field_from_xyz(numnod, phi, tensor, component);
    };
    return;
};

void pick_asym_tensor_component(const int id_coord, const int icomp, const int numnod,
                                double *theta, double *phi,
                                double *tensor, float *component){
    if(id_coord == SPH_COORDINATE){
        pick_asym_tensor_comp_by_sph(icomp, numnod, theta, phi,
                                     tensor, component);
    }else if(id_coord == CYL_COORDINATE){
        pick_asym_tensor_comp_by_cyl(icomp, numnod, theta, phi,
                                     tensor, component);
    } else {
        pick_asym_tensor_comp_by_xyz(icomp, numnod, theta, phi,
                                     tensor, component);
    };
    return;
};
