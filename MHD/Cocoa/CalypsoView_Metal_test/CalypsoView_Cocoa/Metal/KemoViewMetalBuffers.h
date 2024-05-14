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

#include "m_vertex_buffer.h"

@interface KemoViewMetalBuffers : NSObject

- (NSUInteger) setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                       buffer:(struct gl_strided_buffer *_Nonnull) buf
                     indexbuf:(struct gl_index_buffer *_Nonnull) index_buf
                       vertex:(id<MTLBuffer> _Nonnull *_Nonnull) vertices
                        index:(id<MTLBuffer> _Nonnull *_Nonnull) indices;

@end
#endif /* KemoViewMetalBuffers_h_ */
