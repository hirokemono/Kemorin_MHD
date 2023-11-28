/*
//  KemoViewRendererTools.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import "KemoViewRendererTools.h"

@implementation KemoViewRendererTools
- (matrix_float4x4) setMetalViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int i=0;i<4;i++){
        col_wk[i].x = matrix[4*i  ];
        col_wk[i].y = matrix[4*i+1];
        col_wk[i].z = matrix[4*i+2];
        col_wk[i].w = matrix[4*i+3];
    };
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}


- (matrix_float4x4) setMetalModelViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
    //    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}
- (matrix_float4x4) setMetalProjViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
    /* Shift viewport */
    col_wk[2][2] = (0.5*matrix[4*2+2] + 0.5*matrix[4*2+3]);
    col_wk[3][2] = (0.5*matrix[4*3+2] + 0.5*matrix[4*3+3]);
    
    //    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}

- (void)rightMaterialParams:(MaterialParameters *) material
{
    material->ambient[0] = 0.0;
    material->diffuse[0] = 0.0;
    material->specular[0] = 0.0;
    return;
}

- (void)leftMaterialParams:(MaterialParameters *) material
{
    for(int i=1;i<3;i++){
        material->ambient[i] =  0.0;
        material->diffuse[i] =  0.0;
        material->specular[i] = 0.0;
    }
    return;
}
- (void)fillMaterialParams:(MaterialParameters *) material
{
    for(int i=1;i<3;i++){
        material->ambient[i] =  material->ambient[0];
        material->diffuse[i] =  material->diffuse[0];
        material->specular[i] = material->specular[0];
    }
    material->ambient[3] = 1.0;
    material->diffuse[3] = 1.0;
    material->specular[3] = 1.0;
    return;
}


- (void)setMetalColorbuffer:(LightSourceParameters *) lights
                   material:(MaterialParameters *) mats
{
    int i;
    float x, y, z;
    
    mats->ambient[0] =  kemoview_get_material_parameter(AMBIENT_FLAG);
    mats->diffuse[0] =  kemoview_get_material_parameter(DIFFUSE_FLAG);
    mats->specular[0] = kemoview_get_material_parameter(SPECULAR_FLAG);
    mats->shininess =   kemoview_get_material_parameter(SHINENESS_FLAG);
    
    lights->num_lights = kemoview_get_num_light_position();
    for(i=0;i<lights->num_lights;i++){
        kemoview_get_each_light_xyz(i, &x, &y, &z);
        lights->position[i][0] = x;
        lights->position[i][1] = y;
        lights->position[i][2] = z;
        lights->position[i][3] = 1.0;
    };
    return;
};

- (void)setCubeColorbuffer:(LightSourceParameters *) lights
                  material:(MaterialParameters *) mats
{
    int i, j;
    
    struct initial_cube_lighting *init_light = init_inital_cube_lighting();
    lights->num_lights = init_light->num_light;
    for(i=0;i<lights->num_lights;i++){
        for(j=0;j<4;j++){
            lights->position[i][j] = init_light->lightposition[i][j];
        };
    };
    mats->ambient[0] =  init_light->whitelight[0][0];
    mats->diffuse[0] =  init_light->whitelight[1][0];
    mats->specular[0] = init_light->whitelight[2][0];
    mats->shininess =   init_light->shine[0];
    free(init_light);
    return;
};

- (void)set2dProjectionMatrices:(matrix_float4x4 *) cbar_proj_mat
                  MapProjection:(matrix_float4x4 *) map_proj_mat
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    double *orthogonal = orthogonal_projection_mat_c(0.0, kemo_sgl->view_s->nx_frame,
                                                     0.0, kemo_sgl->view_s->ny_frame,
                                                    -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *map_matrices
            = init_projection_matrix_for_map(kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame);
    *cbar_proj_mat =  [self setMetalViewMatrices:cbar_matrices->proj];
    *map_proj_mat =   [self setMetalViewMatrices:map_matrices->proj];
    free(cbar_matrices);
    free(map_matrices);
    free(orthogonal);
    return;
}

- (void)setTransferMatrices:(KemoViewUnites *) monoViewUnites
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(kemo_sgl->view_s);

    monoViewUnites->modelview_mat =  [self setMetalViewMatrices:view_matrices->model];
    monoViewUnites->projection_mat = [self setMetalProjViewMatrices:view_matrices->proj];
    monoViewUnites->normal_mat =     [self setMetalViewMatrices:view_matrices->nrmat];
    free(view_matrices);
    return;
};

- (void) setKemoViewLightings:(struct gl_strided_buffer *) cube_buf
                       unites:(KemoViewUnites *) monoViewUnites
{
    if(cube_buf->num_nod_buf > 0){
        [self setCubeColorbuffer:&(monoViewUnites->lights)
                        material:&(monoViewUnites->material)];
    } else {
        [self setMetalColorbuffer:&(monoViewUnites->lights)
                         material:&(monoViewUnites->material)];
    }
    [self fillMaterialParams:&(monoViewUnites->material)];
    return;
};

@end

