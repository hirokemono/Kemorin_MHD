/*
//  KemoViewRendererTools.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoViewRendererTools_h_
#define KemoViewRendererTools_h_

@import Foundation;
@import MetalKit;
@import simd;

#import "AAPLShaderTypes.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "m_transfer_matrices.h"
#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

@interface KemoViewRendererTools : NSObject

typedef struct
{
/*    Texture to render screen to texture */
    matrix_float4x4 modelview_mat;
    matrix_float4x4 projection_mat;
    matrix_float4x4 normal_mat;
    
    LightSourceParameters lights;
    MaterialParameters    material;
} KemoViewUnites;

- (void)rightMaterialParams:(MaterialParameters *) material;
- (void)leftMaterialParams:(MaterialParameters *) material;

- (void)set2dProjectionMatrices:(matrix_float4x4 *) cbar_proj_mat
                  MapProjection:(matrix_float4x4 *) map_proj_mat;
- (void)setTransferMatrices:(KemoViewUnites *) monoViewUnites;
- (void) setKemoViewLightings:(struct gl_strided_buffer *) cube_buf
                       unites:(KemoViewUnites *) monoViewUnites;

@end
#endif /* KemoViewRendererTools_h_ */
