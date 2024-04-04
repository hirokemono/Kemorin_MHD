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

#import "KemoViewShaderTypes.h"
#import "AAPLImage.h"

#include "m_vertex_buffer.h"

@interface KemoViewMetalBuffers : NSObject

- (NSUInteger) setMetalVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                        buffer:(struct gl_strided_buffer * _Nonnull) buf
                        vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices;

- (NSUInteger) setPSFTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                      buffer:(struct gl_strided_buffer *_Nonnull) buf
                       image:(struct gl_texure_image *_Nonnull) psf_texure
                      vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                      texure:(id<MTLTexture> _Nonnull *_Nonnull) texture;

- (NSUInteger) setTextBoxTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                          buffer:(struct gl_textbox_buffer *_Nonnull) buf
                          vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                          texure:(id<MTLTexture> _Nonnull *_Nonnull) texture;

- (NSUInteger) setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
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
