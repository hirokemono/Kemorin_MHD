/***********************************************************************
 !!  pick_asym_tensor_component.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "calypso_param_c.h"
#include "convert_xyz_asym_tsr_2_sph_c.h"
#include "convert_sph_asym_tsr_2_xyz_c.h"
#include "convert_cyl_asym_tsr_2_sph_c.h"
#include "pick_sym_tensor_component.h"
#include "pick_asym_tensor_component.h"

void pick_sph_sym_tensor_by_sph(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if      (icomp == SPH_to_RT){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == SPH_to_RP){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == SPH_to_TP){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == SPH_to_SP){
        sph_sp_asym_tensor_by_sph_c(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_SZ){
        sph_zs_asym_tensor_by_sph_c(numnod, tensor, component);
    }else if(icomp == SPH_to_PZ){
        sph_pz_asym_tensor_by_sph_c(numnod, theta, tensor, component);
        
    }else if(icomp == SPH_to_XY){
        sph_xy_asym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_XZ){
        sph_zx_asym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_YZ){
        sph_yz_asym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
        
    }else if(icomp == SPH_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_sph_sym_tensor_by_cyl(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if      (icomp == CYL_to_SP){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == CYL_to_SZ){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == CYL_to_PZ){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == CYL_to_RT){
        sph_rt_asym_tensor_by_cyl_c(numnod, tensor, component);
    }else if(icomp == CYL_to_RP){
        sph_pr_asym_tensor_by_cyl_c(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_TP){
        sph_tp_asym_tensor_by_cyl_c(numnod, theta, tensor, component);
        
    }else if(icomp == CYL_to_XY){
        sph_xy_asym_tensor_by_cyl_c(numnod, tensor, component);
    }else if(icomp == CYL_to_XZ){
        sph_zx_asym_tensor_by_cyl_c(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_YZ){
        sph_yz_asym_tensor_by_cyl_c(numnod, phi, tensor, component);
        
    }else if(icomp == CYL_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_sph_asym_tensor_by_xyz(const int icomp, const int numnod,
                                 const double *theta, const double *phi,
                                 const double *tensor, double *component){
    if      (icomp == XYZ_to_XY){
        pick_tensor_comp_c(IZERO, numnod, tensor, component);
    }else if(icomp == XYZ_to_XZ){
        pick_tensor_comp_c(IONE, numnod, tensor, component);
    }else if(icomp == XYZ_to_YZ){
        pick_tensor_comp_c(ITWO, numnod, tensor, component);
        
    }else if(icomp == XYZ_to_RT){
        sph_rt_asym_tensor_by_xyz_c(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_RP){
        sph_pr_asym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_TP){
        sph_tp_asym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
        
    }else if(icomp == XYZ_to_SP){
        sph_sp_asym_tensor_by_xyz_c(numnod, tensor, component);
    }else if(icomp == XYZ_to_SZ){
        sph_zs_asym_tensor_by_xyz_c(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_PZ){
        sph_pz_asym_tensor_by_xyz_c(numnod, phi, tensor, component);
        
    }else if(icomp == XYZ_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_asym_tensor_component(const int id_coord, const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if(id_coord == SPH_COORDINATE){
        pick_sph_asym_tensor_by_sph(icomp, numnod, theta, phi,
                                    tensor, component);
    }else if(id_coord == CYL_COORDINATE){
        pick_sph_asym_tensor_by_cyl(icomp, numnod, theta, phi,
                                    tensor, component);
    } else {
        pick_sph_asym_tensor_by_cyl(icomp, numnod, theta, phi,
                                    tensor, component);
    };
    return;
};
