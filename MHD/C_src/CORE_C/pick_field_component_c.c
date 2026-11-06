/***********************************************************************
 !!  pick_field_component_c.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "pick_field_component_c.h"

float pick_one_component_c(const int id_field,
                           const int id_coord, const int i_comp,
                           double theta, double phi,
                           double *vector){
    float component = 0.0;
    if      (id_field == VECTOR_FLAG){
        component = pick_from_vector(id_coord, i_comp, 
                                     theta, phi, vector);
    }else if(id_field == SYM_TENSOR_FLAG){
        component = pick_from_sym_tensor(id_coord, i_comp, 
                                         theta, phi, vector);
    }else if(id_field == ASYM_TENSOR_FLAG){
        component = pick_from_asym_tensor(id_coord, i_comp, 
                                          theta, phi, vector);
    }else{
        component = (float) vector[0];
    };
    return component;
};


void pick_field_component_c(const int id_field,  const int id_coord, const int i_comp, 
                            const int nnod, double *theta, double *phi, 
                            double *vector, float *component){
    int i;
    if      (id_field == VECTOR_FLAG){
        pick_vector_component(id_coord, i_comp, nnod,
                              theta, phi, vector, component);
    }else if(id_field == SYM_TENSOR_FLAG){
        pick_sym_tensor_component(id_coord, i_comp, nnod, 
                                  theta, phi, vector, component);
    }else if(id_field == ASYM_TENSOR_FLAG){
        pick_asym_tensor_component(id_coord, i_comp, nnod, 
                                   theta, phi, vector, component);
    }else{
        for(i=0;i<nnod;i++){component[i] = (float) vector[i];}
    };
    return;
};

