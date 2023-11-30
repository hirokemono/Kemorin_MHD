/*
//  KemoViewMetalBuffers.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import "KemoViewMetalBuffers.h"

@implementation KemoViewMetalBuffers:NSObject

- (void)setMetalVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
                 buffer:(struct gl_strided_buffer * _Nonnull) buf
                 vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytesNoCopy:buf->v_buf
                                               length:(buf->nsize_buf * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    };
};

- (void)setPSFTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
               buffer:(struct gl_strided_buffer *_Nonnull) buf
                image:(struct kemo_PSF_texure *_Nonnull) psf_texure
               vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
               texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytesNoCopy:buf->v_buf
                                               length:(buf->nsize_buf * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
        
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        lineTextureDescriptor.width =  psf_texure->texure_width;
        lineTextureDescriptor.height = psf_texure->texure_height;

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
    return;
}

- (void)setTextBoxTexture:(id<MTLDevice> _Nonnull *_Nonnull) device
                   buffer:(struct gl_strided_buffer *_Nonnull) buf
                    image:(struct line_text_image *_Nonnull) img
                   vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                   texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        lineTextureDescriptor.width =  img->npix_img[0];
        lineTextureDescriptor.height = img->npix_img[1];

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
                      withBytes:img->imgBMP
                    bytesPerRow:bytesPerRow];
    };
    return;
}

- (void)setCubeVertexs:(id<MTLDevice> _Nonnull *_Nonnull) device
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
};


@end
