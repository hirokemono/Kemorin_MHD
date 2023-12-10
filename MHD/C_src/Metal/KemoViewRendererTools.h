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
    
    NSInteger iColormapMode;
} KemoViewUnites;

@interface KemoViewRendererTools : NSObject

- (void)setTransferMatrices:(struct kemoviewer_type *) kemo_sgl
                     unites:(KemoViewUnites *) monoViewUnites;
- (void) setKemoViewLightings:(struct kemoviewer_type *) kemo_sgl
                       unites:(KemoViewUnites *) monoViewUnites;

- (void)setKemoViewLightsAndViewMatrices:(struct kemoviewer_type *) kemo_sgl
                               ModelView:(KemoViewUnites *) monoViewUnites
                           MsgProjection:(matrix_float4x4 *) cbar_proj_mat
                           MapProjection:(matrix_float4x4 *) map_proj_mat;

-(void) encodeCopyTexureToPrivate:(id<MTLCommandQueue> *) commandQueue
                          num_pix:(NSUInteger *) pix_xy
                           source:(id<MTLTexture> *) sourceTexture
                           target:(id<MTLTexture> *) targetTexture;

@end
#endif /* KemoViewRendererTools_h_ */
