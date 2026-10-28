/***********************************************************************
 !!*  convert_cyl_asym_tsr_2_sph_c.c
 !!*
 !!*  Created by Hiroaki Matsui on Dec. 3, 2012
 !!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!
 !!   urut =         uzus
 !!   upur = (as) * (-s* usup + z* upuz)
 !!   utup = (as) * ( z* usup + s* upuz)
 !!
 !!   usup = (as) * (-s* upur + z* utup)
 !!   uzus =         urut 
 !!   upuz = (as) * ( z* upur + s* utup)
 ***********************************************************************/

#include <math.h>

#include "convert_cyl_asym_tsr_2_sph_c.h"

void sph_rt_asym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_rt){
    int i;
    for(i=0;i<numnod;i++){
        t_rt[i] = st_cyl[i+1*numnod];
    };
    return;
};

void sph_pr_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_pr){
    int i;
    double tsp, tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i];
        tpz = st_cyl[i+2*numnod];
        t_pr[i] =  tpz * cos(theta[i])
                 - tsp * sin(theta[i]);
    };
    return;
};

void sph_tp_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_tp){
    int i;
    double tsp, tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i];
        tpz = st_cyl[i+2*numnod];
        t_tp[i] =  tpz * sin(theta[i])
                 + tsp * cos(theta[i]);
    };
    return;
};




void sph_sp_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sp){
    int i;
    double tpr, ttp;
    for(i=0;i<numnod;i++){
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_sp[i] = -tpr * sin(theta[i])
                 + ttp * cos(theta[i]);
    };
    return;
};

void sph_zs_asym_tensor_by_sph_c(int numnod, const double *st_sph, double *t_zs){
    int i;
    for(i=0;i<numnod;i++){
        t_zs[i] = st_sph[i];
    };
    return;
};

void sph_pz_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_pz){
    int i;
    double tpr, ttp;
    for(i=0;i<numnod;i++){
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_pz[i] =  tpr * cos(theta[i])
                 + ttp * sin(theta[i]);
    };
    return;
};



void set_PSF_component_name(int ncomp, int icomp, char *comp_name) {
    int id_coord =    kemoview_get_PSF_coordinate_id();
    
    if(id_coord == 1){
        if(icomp == 0){ sprintf(comp_name, "RR");
        }else if(icomp ==  1){ sprintf(comp_name, "R-Theta");
        }else if(icomp ==  2){ sprintf(comp_name, "R-Phi");
        }else if(icomp ==  3){ sprintf(comp_name, "Theta-Theta");
        }else if(icomp ==  4){ sprintf(comp_name, "Theta-Phi");
        }else if(icomp ==  5){ sprintf(comp_name, "Phi-Phi");
            
        }else if(icomp ==  6){ sprintf(comp_name, "SS");
        }else if(icomp ==  7){ sprintf(comp_name, "S-Phi");
        }else if(icomp ==  8){ sprintf(comp_name, "SZ");
        }else if(icomp ==  9){ sprintf(comp_name, "Phi-Z");
        }else if(icomp == 10){ sprintf(comp_name, "ZZ");
            
        }else if(icomp == 11){ sprintf(comp_name, "XX");
        }else if(icomp == 12){ sprintf(comp_name, "XY");
        }else if(icomp == 13){ sprintf(comp_name, "XZ");
        }else if(icomp == 14){ sprintf(comp_name, "YY");
        }else if(icomp == 15){ sprintf(comp_name, "YZ");
            
        }else if(icomp == 16){ sprintf(comp_name, "magnitude");
        };
    }else if(id_coord == 2){
        if(icomp == 0){ sprintf(comp_name, "SS");
        }else if(icomp ==  1){ sprintf(comp_name, "S-Phi");
        }else if(icomp ==  2){ sprintf(comp_name, "SZ");
        }else if(icomp ==  3){ sprintf(comp_name, "Phi-Phi");
        }else if(icomp ==  4){ sprintf(comp_name, "Phi-Z");
        }else if(icomp ==  5){ sprintf(comp_name, "ZZ");
            
        }else if(icomp ==  6){ sprintf(comp_name, "RR");
        }else if(icomp ==  7){ sprintf(comp_name, "R-Theta");
        }else if(icomp ==  8){ sprintf(comp_name, "R-Phi");
        }else if(icomp ==  9){ sprintf(comp_name, "Theta-Theta");
        }else if(icomp == 10){ sprintf(comp_name, "Theta-Phi");
            
        }else if(icomp == 11){ sprintf(comp_name, "XX");
        }else if(icomp == 12){ sprintf(comp_name, "XY");
        }else if(icomp == 13){ sprintf(comp_name, "XZ");
        }else if(icomp == 14){ sprintf(comp_name, "YY");
        }else if(icomp == 15){ sprintf(comp_name, "YZ");
            
        }else if(icomp == 16){ sprintf(comp_name, "magnitude");
        }
    } else {
        if(icomp == 0){ sprintf(comp_name, "XX");
        }else if(icomp ==  1){ sprintf(comp_name, "XY");
        }else if(icomp ==  2){ sprintf(comp_name, "XZ");
        }else if(icomp ==  3){ sprintf(comp_name, "YY");
        }else if(icomp ==  4){ sprintf(comp_name, "YZ");
        }else if(icomp ==  5){ sprintf(comp_name, "ZZ");
            
        }else if(icomp ==  6){ sprintf(comp_name, "RR");
        }else if(icomp ==  7){ sprintf(comp_name, "R-Theta");
        }else if(icomp ==  8){ sprintf(comp_name, "R-Phi");
        }else if(icomp ==  9){ sprintf(comp_name, "Theta-Theta");
        }else if(icomp == 10){ sprintf(comp_name, "Theta-Phi");
        }else if(icomp == 11){ sprintf(comp_name, "Phi-Phi");
            
        }else if(icomp == 12){ sprintf(comp_name, "SS");
        }else if(icomp == 13){ sprintf(comp_name, "S-Phi");
        }else if(icomp == 14){ sprintf(comp_name, "SZ");
        }else if(icomp == 15){ sprintf(comp_name, "Phi-Z");
            
        }else if(icomp == 16){ sprintf(comp_name, "magnitude");
        };
    };
    
    if(icomp == ncomp) sprintf(comp_name,"magnitude");
    
    return;
};

