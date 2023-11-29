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

- (void)setMetalVertexs:(id<MTLDevice> *) device
                 buffer:(struct gl_strided_buffer * _Nonnull) buf
                 vertex:(id<MTLBuffer> *)  vertices;

- (void)setPSFTexture:(id<MTLDevice> *) device
               buffer:(struct gl_strided_buffer *) buf
                image:(struct kemo_PSF_texure *) psf_texure
               vertex:(id<MTLBuffer> *)  vertices
               texure:(id<MTLTexture> *) texture;

- (void)setTextBoxTexture:(id<MTLDevice> *) device
                   buffer:(struct gl_strided_buffer *) buf
                    image:(struct line_text_image *) img
                   vertex:(id<MTLBuffer> *)  vertices
                   texure:(id<MTLTexture> *) texture;

- (void)setCubeVertexs:(id<MTLDevice> *) device
                buffer:(struct gl_strided_buffer *) buf
              indexbuf:(struct gl_index_buffer *) index_buf
                vertex:(id<MTLBuffer> *) vertices
                 index:(id<MTLBuffer> *) indices;
@end
#endif /* KemoViewMetalBuffers_h_ */
