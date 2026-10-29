/***********************************************************************
 !!  pick_sym_tensor_component.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "pick_sym_tensor_component.h"

void pick_tensor_comp_c(int icomp, int numnod, 
                        const double *tensor, double *component){
    int i;
    for(i=0;i<numnod;i++){component[i] = tensor[i+icomp*numnod];};
    return;
};

void pick_sph_sym_tensor_by_sph(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if      (icomp == SPH_to_RR){
        pick_tensor_comp_c(SPH_to_RR, numnod, tensor, component);
    }else if(icomp == SPH_to_RT){
        pick_tensor_comp_c(SPH_to_RT, numnod, tensor, component);
    }else if(icomp == SPH_to_RP){
        pick_tensor_comp_c(SPH_to_RP, numnod, tensor, component);
    }else if(icomp == SPH_to_TT){
        pick_tensor_comp_c(SPH_to_TT, numnod, tensor, component);
    }else if(icomp == SPH_to_TP){
        pick_tensor_comp_c(SPH_to_TP, numnod, tensor, component);
    }else if(icomp == SPH_to_PP){
        pick_tensor_comp_c(SPH_to_PP, numnod, tensor, component);
        
    }else if(icomp == SPH_to_SS){
        sph_ss_sym_tensor_by_sph_c(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_SP){
        sph_sp_sym_tensor_by_sph_c(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_SZ){
        sph_sz_sym_tensor_by_sph_c(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_PZ){
        sph_pz_sym_tensor_by_sph_c(numnod, theta, tensor, component);
    }else if(icomp == SPH_to_ZZ){
        sph_zz_sym_tensor_by_sph_c(numnod, theta, tensor, component);
        
    }else if(icomp == SPH_to_XX){
        sph_xx_sym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_XY){
        sph_xy_sym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_XZ){
        sph_xz_sym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_YY){
        sph_yy_sym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
    }else if(icomp == SPH_to_YZ){
        sph_yz_sym_tensor_by_sph_c(numnod, theta, phi, tensor, component);
        
    }else if(icomp == SPH_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_sph_sym_tensor_by_cyl(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if      (icomp == CYL_to_SS){
        pick_tensor_comp_c(CYL_to_SS, numnod, tensor, component);
    }else if(icomp == CYL_to_SP){
        pick_tensor_comp_c(CYL_to_SP, numnod, tensor, component);
    }else if(icomp == CYL_to_SZ){
        pick_tensor_comp_c(CYL_to_SZ, numnod, tensor, component);
    }else if(icomp == CYL_to_PP){
        pick_tensor_comp_c(CYL_to_PP, numnod, tensor, component);
    }else if(icomp == CYL_to_PZ){
        pick_tensor_comp_c(CYL_to_PZ, numnod, tensor, component);
    }else if(icomp == CYL_to_ZZ){
        pick_tensor_comp_c(CYL_to_ZZ, numnod, tensor, component);
        
    }else if(icomp == CYL_to_RR){
        sph_rr_sym_tensor_by_cyl_c(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_RT){
        sph_rt_sym_tensor_by_cyl_c(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_RP){
        sph_rp_sym_tensor_by_cyl_c(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_TT){
        sph_tt_sym_tensor_by_cyl_c(numnod, theta, tensor, component);
    }else if(icomp == CYL_to_TP){
        sph_tp_sym_tensor_by_cyl_c(numnod, theta, tensor, component);
        
    }else if(icomp == CYL_to_XX){
        sph_xx_sym_tensor_by_cyl_c(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_XY){
        sph_xy_sym_tensor_by_cyl_c(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_XZ){
        sph_yz_sym_tensor_by_cyl_c(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_YY){
        sph_yy_sym_tensor_by_cyl_c(numnod, phi, tensor, component);
    }else if(icomp == CYL_to_YZ){
        sph_yz_sym_tensor_by_cyl_c(numnod, phi, tensor, component);
        
    }else if(icomp == CYL_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_sph_sym_tensor_by_xyz(const int icomp, const int numnod,
                                const double *theta, const double *phi,
                                const double *tensor, double *component){
    if      (icomp == XYZ_to_XX){
        pick_tensor_comp_c(XYZ_to_XX, numnod, tensor, component);
    }else if(icomp == XYZ_to_XY){
        pick_tensor_comp_c(XYZ_to_XY, numnod, tensor, component);
    }else if(icomp == XYZ_to_XZ){
        pick_tensor_comp_c(XYZ_to_XZ, numnod, tensor, component);
    }else if(icomp == XYZ_to_YY){
        pick_tensor_comp_c(XYZ_to_YY, numnod, tensor, component);
    }else if(icomp == XYZ_to_YZ){
        pick_tensor_comp_c(XYZ_to_YZ, numnod, tensor, component);
    }else if(icomp == XYZ_to_ZZ){
        pick_tensor_comp_c(XYZ_to_ZZ, numnod, tensor, component);
        
    }else if(icomp == XYZ_to_RR){
        sph_rr_sym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_RT){
        sph_rt_sym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_RP){
        sph_rp_sym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_TT){
            sph_tt_sym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_TP){
        sph_tp_sym_tensor_by_xyz_c(numnod, theta, phi, tensor, component);
    }else if(icomp == XYZ_to_PP){
        sph_pp_sym_tensor_by_xyz_c(numnod, phi, tensor, component);
        
    }else if(icomp == XYZ_to_SS){
        sph_ss_sym_tensor_by_xyz_c(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_SP){
        sph_sp_sym_tensor_by_xyz_c(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_SZ){
        sph_sz_sym_tensor_by_xyz_c(numnod, phi, tensor, component);
    }else if(icomp == XYZ_to_PZ){
        sph_pz_sym_tensor_by_xyz_c(numnod, phi, tensor, component);
        
    }else if(icomp == XYZ_to_MAG){
        printf("No magnitude evaluation currently");
    };
    return;
};

void pick_sym_tensor_component(const int id_coord, const int icomp, const int numnod,
                               const double *theta, const double *phi,
                               const double *tensor, double *component){
    if(id_coord == SPH_COORDINATE){
        pick_sph_sym_tensor_by_sph(icomp, numnod, theta, phi,
                                   tensor, component);
    }else if(id_coord == CYL_COORDINATE){
        pick_sph_sym_tensor_by_cyl(icomp, numnod, theta, phi,
                                   tensor, component);
    } else {
        pick_sph_sym_tensor_by_cyl(icomp, numnod, theta, phi,
                                   tensor, component);
    };
    return;
};
