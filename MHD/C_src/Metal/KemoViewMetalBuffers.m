/*
//  KemoViewMetalBuffers.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import "KemoViewMetalBuffers.h"

@implementation KemoViewMetalBuffers:NSObject

- (NSUInteger) setMetalVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                        buffer:(struct gl_strided_buffer * _Nonnull) buf
                        vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytesNoCopy:buf->v_buf
                                               length:(buf->nsize_buf * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    };
    return buf->num_nod_buf;
};

- (NSUInteger) setMetalIndices:(id<MTLDevice> _Nonnull *_Nonnull) device
                     indexbuf:(struct gl_index_buffer *_Nonnull) index_buf
                        index:(id<MTLBuffer> _Nonnull *_Nonnull) indices
{
    if(index_buf->ntot_vertex > 0){
        *indices = [*device newBufferWithBytes:index_buf->ie_buf
                                        length:(index_buf->nsize_buf * sizeof(unsigned int))
                                       options:MTLResourceStorageModeShared];
    };
    return index_buf->ntot_vertex;
};

- (NSUInteger) setPSFTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                       image:(struct gl_texure_image *_Nonnull) psf_texure
                      texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
{
    if(psf_texure->texure_npix > 0){
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        lineTextureDescriptor.width =  psf_texure->nipxel_xy[0];
        lineTextureDescriptor.height = psf_texure->nipxel_xy[1];

/*  Calculate the number of bytes per row in the image. */
        NSUInteger bytesPerRow = 4 * lineTextureDescriptor.width;
        MTLRegion region = {
            { 0, 0, 0 },                   // MTLOrigin
            {lineTextureDescriptor.width, lineTextureDescriptor.height, 1} // MTLSize
        };
        
/* Create the texture from the device by using the descriptor */
        *texture = [*device newTextureWithDescriptor:lineTextureDescriptor];
/* Copy the bytes from the data object into the texture */
        [*texture replaceRegion:region
                    mipmapLevel:0
                      withBytes:psf_texure->texure_rgba
                    bytesPerRow:bytesPerRow];
    };
    return psf_texure->texure_npix;
}

- (NSUInteger) setTextBoxTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                          buffer:(struct gl_textbox_buffer *_Nonnull) buf
                          vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                          texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
{
    if(buf->vertex->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->vertex->v_buf)
                                         length:(buf->vertex->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        lineTextureDescriptor.width =  buf->image->nipxel_xy[0];
        lineTextureDescriptor.height = buf->image->nipxel_xy[1];

/*  Calculate the number of bytes per row in the image. */
        NSUInteger bytesPerRow = 4 * lineTextureDescriptor.width;
        MTLRegion region = {
            { 0, 0, 0 },                   // MTLOrigin
            {lineTextureDescriptor.width, lineTextureDescriptor.height, 1} // MTLSize
        };
        
/* Create the texture from the device by using the descriptor */
        *texture = [*device newTextureWithDescriptor:lineTextureDescriptor];
/* Copy the bytes from the data object into the texture */
        [*texture replaceRegion:region
                    mipmapLevel:0
                      withBytes:buf->image->texure_rgba
                    bytesPerRow:bytesPerRow];
    };
    return buf->vertex->num_nod_buf;
}

- (NSUInteger) setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                       buffer:(struct gl_strided_buffer *_Nonnull) buf
                     indexbuf:(struct gl_index_buffer *_Nonnull) index_buf
                       vertex:(id<MTLBuffer> _Nonnull *_Nonnull) vertices
                        index:(id<MTLBuffer> _Nonnull *_Nonnull) indices
{
    if(index_buf->ntot_vertex > 0){
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        *indices = [*device newBufferWithBytes:index_buf->ie_buf
                                        length:(index_buf->ntot_vertex * sizeof(unsigned int))
                                       options:MTLResourceStorageModeShared];
    };
    return index_buf->ntot_vertex;
};

- (void)setAnaglyphTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                    buffer:(struct gl_strided_buffer *_Nonnull) buf
                    pixels:(NSUInteger *_Nonnull) npix_img
                    vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                      left:(id<MTLTexture> _Nonnull *_Nonnull) leftTexture
                     right:(id<MTLTexture> _Nonnull *_Nonnull) rightTexture
{
    struct gl_local_buffer_address point_buf;
    long i;
    if(buf->num_nod_buf > 0){
        for(i=0;i<buf->num_nod_buf;i++){
            set_node_stride_buffer(i, buf, &point_buf);
            buf->v_buf[point_buf.igl_xyzw+1]
                = npix_img[1] - buf->v_buf[point_buf.igl_xyzw+1];
        }
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        for(i=0;i<buf->num_nod_buf;i++){
            set_node_stride_buffer(i, buf, &point_buf);
            buf->v_buf[point_buf.igl_xyzw+1]
                = npix_img[1] - buf->v_buf[point_buf.igl_xyzw+1];
        }
        
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
        lineTextureDescriptor.width =  npix_img[0];
        lineTextureDescriptor.height = npix_img[1];
        lineTextureDescriptor.storageMode = MTLStorageModePrivate;

/* Create the leftTexture from the device by using the descriptor */
        *leftTexture = [*device newTextureWithDescriptor:lineTextureDescriptor];
        *rightTexture = [*device newTextureWithDescriptor:lineTextureDescriptor];
    };
    return;
}

@end
