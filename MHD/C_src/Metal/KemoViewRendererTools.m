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
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int i;
    float x, y, z;
    
    mats->ambient[0] =  kemoview_get_material_parameter(kemo_sgl, AMBIENT_FLAG);
    mats->diffuse[0] =  kemoview_get_material_parameter(kemo_sgl, DIFFUSE_FLAG);
    mats->specular[0] = kemoview_get_material_parameter(kemo_sgl, SPECULAR_FLAG);
    mats->shininess =   kemoview_get_material_parameter(kemo_sgl, SHINENESS_FLAG);
    
    lights->num_lights = kemoview_get_num_light_position(kemo_sgl);
    for(i=0;i<lights->num_lights;i++){
        kemoview_get_each_light_xyz(kemo_sgl, i, &x, &y, &z);
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

- (void)colormapToMetalShader:(struct kemoviewer_type *) kemo_sgl
                     colorMap:(KemoViewNormalize *) metalColormap
{
    int i;
    double value, color, opacity;
    
    metalColormap->id_cmap[0] = kemoview_get_PSF_color_param(kemo_sgl, ISET_COLORMAP);
    
    metalColormap->num_normalize[0] = kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_COLOR);
    for(i=0;i<metalColormap->num_normalize[0];i++){
        kemoview_get_PSF_color_items(kemo_sgl, i, &value, &color);
        metalColormap->data_reference[i] =  (float) value;
        metalColormap->data_normalized[i] = (float) color;
    }
    
    metalColormap->num_opacity[0] = kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_OPACITY);
    for(i=0;i<metalColormap->num_opacity[0];i++) {
        kemoview_get_PSF_opacity_items(kemo_sgl, i, &value, &opacity);
        metalColormap->alpha_reference[i] =  (float) value;
        metalColormap->alpha_output[i] = (float) opacity;
    }
    return;
};

- (void)set2dProjectionMatrices:(struct kemoviewer_type *) kemo_sgl
                  MsgProjection:(matrix_float4x4 *) cbar_proj_mat
                  MapProjection:(matrix_float4x4 *) map_proj_mat
{
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

- (void)setTransferMatrices:(struct kemoviewer_type *) kemo_sgl
                     unites:(KemoViewUnites *) monoViewUnites
{
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(kemo_sgl->view_s);

    monoViewUnites->modelview_mat =  [self setMetalViewMatrices:view_matrices->model];
    monoViewUnites->projection_mat = [self setMetalProjViewMatrices:view_matrices->proj];
    monoViewUnites->normal_mat =     [self setMetalViewMatrices:view_matrices->nrmat];
    free(view_matrices);
    return;
};

- (void) setKemoViewLightings:(struct kemoviewer_type *) kemo_sgl
                       unites:(KemoViewUnites *) monoViewUnites
{
    if(kemo_sgl->kemo_buffers->cube_buf->num_nod_buf > 0){
        [self setCubeColorbuffer:&(monoViewUnites->lights)
                        material:&(monoViewUnites->material)];
    } else {
        [self setMetalColorbuffer:&(monoViewUnites->lights)
                         material:&(monoViewUnites->material)
                         kemoview:kemo_sgl];
    }
    [self fillMaterialParams:&(monoViewUnites->material)];
    return;
};

- (void)setKemoViewLightsAndViewMatrices:(struct kemoviewer_type *) kemo_sgl
                               ModelView:(KemoViewUnites *) monoViewUnites
                           MsgProjection:(matrix_float4x4 *) cbar_proj_mat
                           MapProjection:(matrix_float4x4 *) map_proj_mat
{
    [self setKemoViewLightings:kemo_sgl
                        unites:monoViewUnites];

    [self set2dProjectionMatrices:kemo_sgl
                    MsgProjection:cbar_proj_mat
                    MapProjection:map_proj_mat];
    [self setTransferMatrices:kemo_sgl
                       unites:monoViewUnites];
    return;
}


-(void) encodeCopyTexureToPrivate:(id<MTLCommandQueue> *) commandQueue
                          num_pix:(NSUInteger *) pix_xy
                           source:(id<MTLTexture> *) sourceTexture
                           target:(id<MTLTexture> *) targetTexture
{
    // Create a command buffer for GPU work.
    id <MTLCommandBuffer> commandBuffer = [*commandQueue commandBuffer];
    // Encode a blit pass to copy data from the source buffer to the private buffer.
    id <MTLBlitCommandEncoder> blitCommandEncoder = [commandBuffer blitCommandEncoder];
    MTLSize metalSize = MTLSizeMake(pix_xy[0], pix_xy[1], 1);
    [blitCommandEncoder copyFromTexture:*sourceTexture
                            sourceSlice:0
                            sourceLevel:0
                           sourceOrigin:MTLOriginMake(0, 0, 0)
                             sourceSize:metalSize
                              toTexture:*targetTexture
                       destinationSlice:0
                       destinationLevel:0
                      destinationOrigin:MTLOriginMake(0, 0, 0)];
    [blitCommandEncoder endEncoding];
    // Add a completion handler and commit the command buffer.
    [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> cb) {
        // Populate private buffer.
    }];
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return;
}

@end

