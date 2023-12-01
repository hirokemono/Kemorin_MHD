/*
//  KemoViewMetalBuffers.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoViewMetalBuffers_h_
#define KemoViewMetalBuffers_h_

@import Foundation;
@import MetalKit;
@import simd;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"

#include "m_vertex_buffer.h"
#include "m_colorbar_work.h"
#include "m_kemoview_psf_menu.h"
#include "set_cube_to_buf.h"

@interface KemoViewMetalBuffers : NSObject

- (void)setMetalVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                 buffer:(struct gl_strided_buffer * _Nonnull) buf
                 vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices;

- (void)setPSFTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
               buffer:(struct gl_strided_buffer *_Nonnull) buf
                image:(struct kemo_PSF_texure *_Nonnull) psf_texure
               vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
               texure:(id<MTLTexture> _Nonnull *_Nonnull) texture;

- (void)setTextBoxTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                   buffer:(struct gl_strided_buffer *_Nonnull) buf
                    image:(struct line_text_image *_Nonnull) img
                   vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                   texure:(id<MTLTexture> _Nonnull *_Nonnull) texture;

- (void)setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                buffer:(struct gl_strided_buffer *_Nonnull) buf
              indexbuf:(struct gl_index_buffer *_Nonnull) index_buf
                vertex:(id<MTLBuffer> _Nonnull *_Nonnull) vertices
                 index:(id<MTLBuffer> _Nonnull *_Nonnull) indices;

- (void)setAnaglyphTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                    buffer:(struct gl_strided_buffer *_Nonnull) buf
                    pixels:(NSUInteger *_Nonnull) npix_img
                    vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                      left:(id<MTLTexture> _Nonnull *_Nonnull) leftTexture
                     right:(id<MTLTexture> _Nonnull *_Nonnull) rightTexture;

@end
#endif /* KemoViewMetalBuffers_h_ */
