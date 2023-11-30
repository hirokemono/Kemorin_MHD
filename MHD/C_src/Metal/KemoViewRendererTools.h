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
#include "m_kemoview_object_buffers.h"
#include "m_transfer_matrices.h"
#include "m_gl_transfer_matrix.h"

typedef struct
{
/*    Texture to render screen to texture */
    matrix_float4x4 modelview_mat;
    matrix_float4x4 projection_mat;
    matrix_float4x4 normal_mat;
    
    LightSourceParameters lights;
    MaterialParameters    material;
} KemoViewUnites;

@interface KemoViewRendererTools : NSObject

- (void)rightMaterialParams:(MaterialParameters *) material;
- (void)leftMaterialParams:(MaterialParameters *) material;

- (void)set2dProjectionMatrices:(matrix_float4x4 *) cbar_proj_mat
                  MapProjection:(matrix_float4x4 *) map_proj_mat;
- (void)setTransferMatrices:(KemoViewUnites *) monoViewUnites;
- (void) setKemoViewLightings:(struct kemoview_buffers *) kemo_buffers
                       unites:(KemoViewUnites *) monoViewUnites;

- (void)setKemoViewLightsAndViewMatrices:(KemoViewUnites *) monoViewUnites
                           MsgProjection:(matrix_float4x4 *) cbar_proj_mat
                           MapProjection:(matrix_float4x4 *) map_proj_mat;
@end
#endif /* KemoViewRendererTools_h_ */
