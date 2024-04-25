/*
//  KemoViewMetalBuffers.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import "KemoViewMetalBuffers.h"

@implementation KemoViewMetalBuffers:NSObject

- (NSUInteger) setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                       buffer:(struct gl_strided_buffer *_Nonnull) buf
                     indexbuf:(struct gl_index_buffer *_Nonnull) index_buf
                       vertex:(id<MTLBuffer> _Nonnull *_Nonnull) vertices
                        index:(id<MTLBuffer> _Nonnull *_Nonnull) indices
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        *indices = [*device newBufferWithBytes:index_buf->ie_buf
                                        length:(index_buf->nsize_buf * sizeof(unsigned int))
                                       options:MTLResourceStorageModeShared];
    };
    return buf->num_nod_buf;
};
@end

