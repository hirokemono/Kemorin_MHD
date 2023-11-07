/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_messages_gl.h"

// Main class performing the rendering
@implementation AAPLRenderer
{
    id<MTLDevice> _device;
    
    // The render pipeline generated from the vertex and fragment shaders in the .metal shader file.
    id<MTLRenderPipelineState> _pipelineState[2];
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue[2];
    
    id<MTLFunction> _vertexFunction[2];
    id<MTLFunction> _fragmentFunction[2];

    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices[3];
    // The number of vertices in the vertex buffer.
    NSUInteger _numVertices[3];
    // The Metal texture object
    id<MTLTexture> _texture[3];
    
    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    float           _scalechange;
    matrix_float4x4 _projection_mat[3];

    NSUInteger _frameNum;
    
    
    IBOutlet KemoViewerObject * _singleKemoView;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _frameNum = 0;
    self = [super init];
    if(self)
    {
        NSError *error;

        _device = mtkView.device;

        // Load all the shader files with a .metal file extension in the project.
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];

        _vertexFunction[0] = [defaultLibrary newFunctionWithName:@"Base2dVertexShader"];
        _fragmentFunction[0] = [defaultLibrary newFunctionWithName:@"Base2DfragmentShader"];

        _vertexFunction[1] = [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
        _fragmentFunction[1] = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

        // Configure a pipeline descriptor that is used to create a pipeline state.
        MTLRenderPipelineDescriptor *pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
/*
        pipelineStateDescriptor.label = @"Simple Pipeline";
*/
        pipelineStateDescriptor.label = @"Texture Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[1] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        // Create the command queue
        _commandQueue[1] = [_device newCommandQueue];

        // Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
        //  If the Metal API validation is enabled, you can find out more information about what
        //  went wrong.  (Metal API validation is enabled by default when a debug build is run
        //  from Xcode.)
        NSAssert(_pipelineState[1], @"Failed to create pipeline state: %@", error);


        pipelineStateDescriptor.label = @"Simple Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[0];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[0];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[0] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[0], @"Failed to create pipeline state: %@", error);



        // Create the command queue
        _commandQueue[0] = [_device newCommandQueue];
    }

    return self;
}


/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    int i;
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    struct gl_strided_buffer *msg_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_message_buffer(kemo_sgl->view_s->iflag_retina,
                         kemo_sgl->view_s->nx_frame,
                         kemo_sgl->view_s->ny_frame,
                         kemo_sgl->kemo_mesh->msg_wk, msg_buf);
    
    struct gl_strided_buffer *time_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_timelabel_buffer(kemo_sgl->view_s->iflag_retina,
                           kemo_sgl->view_s->nx_frame,
                           kemo_sgl->view_s->ny_frame,
                           kemo_sgl->kemo_mesh->mesh_m->text_color,
                           kemo_sgl->kemo_mesh->mesh_m->bg_color,
                           kemo_sgl->kemo_psf->psf_a, time_buf);
    
    double *orthogonal;
    struct transfer_matrices *matrices;
    vector_float4 col_wk[4];
    orthogonal = orthogonal_projection_mat_c( 0.0, kemo_sgl->kemo_mesh->msg_wk->xwin,
                                              0.0, kemo_sgl->kemo_mesh->msg_wk->ywin,
                                             -1.0, 1.0);
    matrices = plane_transfer_matrices(orthogonal);
    for(i=0;i<4;i++){
        col_wk[i].x = matrices->proj[4*i  ];
        col_wk[i].y = matrices->proj[4*i+1];
        col_wk[i].z = matrices->proj[4*i+2];
        col_wk[i].w = matrices->proj[4*i+3];
    };
    _projection_mat[1] = simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
    free(matrices);
    free(orthogonal);


    
    orthogonal = orthogonal_projection_mat_c( 0.0, kemo_sgl->kemo_psf->psf_a->tlabel_wk->xwin,
                                              0.0, kemo_sgl->kemo_psf->psf_a->tlabel_wk->ywin,
                                             -1.0, 1.0);
    matrices = plane_transfer_matrices(orthogonal);
    for(i=0;i<4;i++){
        col_wk[i].x = matrices->proj[4*i  ];
        col_wk[i].y = matrices->proj[4*i+1];
        col_wk[i].z = matrices->proj[4*i+2];
        col_wk[i].w = matrices->proj[4*i+3];
    };
    _projection_mat[2] = simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
    free(matrices);
    free(orthogonal);

    
    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow;
/* Construct message texture */
//    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor.width = kemo_sgl->kemo_mesh->msg_wk->npix_x;
    textureDescriptor.height = kemo_sgl->kemo_mesh->msg_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[1] = [_device newTextureWithDescriptor:textureDescriptor];
    
    // Calculate the number of bytes per row in the image.
    bytesPerRow = 4 * textureDescriptor.width;
    
    MTLRegion region = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor.width, textureDescriptor.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture[1] replaceRegion:region
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_mesh->msg_wk->msgBMP
                   bytesPerRow:bytesPerRow];

    MTLTextureDescriptor *textureDescriptor2 = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow2;
    /* Construct time texture */
//    textureDescriptor2.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor2.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor2.width =  kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_x;
    textureDescriptor2.height = kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[2] = [_device newTextureWithDescriptor:textureDescriptor2];
    
    // Calculate the number of bytes per row in the image.
    bytesPerRow2 = 4 * textureDescriptor2.width;
    MTLRegion region2 = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor2.width, textureDescriptor2.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture[2] replaceRegion:region2
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->tlabel_wk->numBMP
                   bytesPerRow:bytesPerRow2];

    int j, icou;
/*
    icou = 0;
    for(i=0;i<kemo_sgl->kemo_mesh->msg_wk->npix_y;i++){
        printf("line: %d Pixels:", i);
        for(j=0;j<kemo_sgl->kemo_mesh->msg_wk->npix_x;j++){
            printf("%d ", (int) kemo_sgl->kemo_mesh->msg_wk->msgBMP[icou]);
            icou = icou + 1;
        }
        printf("\n");
    }
 */
    icou = 0;
    for(i=0;i<kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_y;i++){
//        printf("line: %d MTL Pixels:", i);
        for(j=0;j<kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_x;j++){
            kemo_sgl->kemo_psf->psf_a->tlabel_wk->numBMP[icou] = (unsigned char) 255;
//            printf("%d ", (int) kemo_sgl->kemo_psf->psf_a->tlabel_wk->numBMP[icou]);
            icou = icou + 1;
        }
//        printf("\n");
    }
    _frameNum++;
    _scalechange = 0.2 + (1.0 + 0.2 * sin(_frameNum * 0.1));
    
    // Set up a simple MTLBuffer with the vertices, including position and texture coordinates
    int n_quad_vertex = 6;
    static const AAPLVertex quadVertices[] =
    {
        // Pixel positions, Color coordinates
        { {  0.5f,  -0.5f },  { 1.f, 0.f, 0.f, 1.f } },
        { { -0.5f,  -0.5f },  { 0.f, 1.f, 0.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f, 1.f, 1.f } },

        { {  0.5f,  -0.5f },  { 1.f, 0.f, 0.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f, 1.f, 1.f } },
        { {  0.5f,   0.5f },  { 1.f, 0.f, 1.f, 1.f } },
    };
    _vertices[0] = [_device newBufferWithBytes:quadVertices
                                        length:(n_quad_vertex*sizeof(AAPLVertex))
                                       options:MTLResourceStorageModeShared];


    // Create a vertex buffer, and initialize it with the quadVertices array
    _vertices[1] = [_device newBufferWithBytes:((KemoViewVertex *) msg_buf->v_buf)
                                        length:(n_quad_vertex*sizeof(KemoViewVertex))
                                       options:MTLResourceStorageModeShared];
    _vertices[2] = [_device newBufferWithBytes:((KemoViewVertex *) time_buf->v_buf)
                                        length:(n_quad_vertex*sizeof(KemoViewVertex))
                                       options:MTLResourceStorageModeShared];
    
    float tako2[2];
    float tako3[2];
    for(i=0;i<msg_buf->num_nod_buf;i++){
        tako2[0] = msg_buf->v_buf[i*msg_buf->ncomp_buf + msg_buf->ist_tex  ];
        tako2[1] = msg_buf->v_buf[i*msg_buf->ncomp_buf + msg_buf->ist_tex+1];
        printf("tako2 %d %f %f\n", i, tako2[0], tako2[1]);
    }
    for(i=0;i<msg_buf->num_nod_buf;i++){
        tako3[0] = time_buf->v_buf[i*time_buf->ncomp_buf + time_buf->ist_tex  ];
        tako3[1] = time_buf->v_buf[i*time_buf->ncomp_buf + time_buf->ist_tex+1];
        printf("tako3 %d %f %f\n", i, tako3[0], tako3[1]);
    }

    
    // Calculate the number of vertices by dividing the byte length by the size of each vertex
    _numVertices[0] = n_quad_vertex;
    _numVertices[1] = msg_buf->num_nod_buf;
    _numVertices[2] = time_buf->num_nod_buf;

     // Create a new command buffer for each render pass to the current drawable.
    id<MTLCommandBuffer> commandBuffer = [_commandQueue[1] commandBuffer];
    commandBuffer.label = @"MyCommand";

    // Obtain a renderPassDescriptor generated from the view's drawable textures.
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil)
    {
        // Create a render command encoder.
        id<MTLRenderCommandEncoder> renderEncoder =
        [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        renderEncoder.label = @"MyRenderEncoder";

        // Set the region of the drawable to draw into.
        [renderEncoder setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, 0.0, 1.0 }];
        
/*  Commands to render screen message */
        [renderEncoder setRenderPipelineState:_pipelineState[1]];

        // Pass in the parameter data.
        [renderEncoder setVertexBuffer:_vertices[1]
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        
        [renderEncoder setVertexBytes:&_projection_mat[1]
                               length:sizeof(_projection_mat[1])
                              atIndex:AAPLOrthogonalMatrix];

        // Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
        ///  to the 'colorMap' argument in the 'samplingShader' function because its
        //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index.
        [renderEncoder setFragmentTexture:_texture[1]
                                  atIndex:AAPLTextureIndexBaseColor];

        // Draw the triangles.
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:_numVertices[1]];

/*  Commands to render time label */
        if(_numVertices[2] > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[1]];
            [renderEncoder setVertexBuffer:_vertices[2]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat[2]
                                   length:sizeof(_projection_mat[2])
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder setFragmentTexture:_texture[2]
                                      atIndex:AAPLTextureIndexBaseColor];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:_numVertices[2]];
        };


/*  Commands to render simple quadrature */
        [renderEncoder setRenderPipelineState:_pipelineState[0]];
        [renderEncoder setVertexBuffer:_vertices[0]
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [renderEncoder setVertexBytes:&_viewportSize
                                length:sizeof(_viewportSize)
                               atIndex:AAPLVertexInputIndexViewportSize];
        [renderEncoder setVertexBytes:&_scalechange
                                length:sizeof(_scalechange)
                               atIndex:AAPLVertexInputIndexScale];
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:n_quad_vertex];




        [renderEncoder endEncoding];

        // Schedule a present once the framebuffer is complete using the current drawable.
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer  commit];

    free(time_buf->v_buf);
    free(time_buf);
    free(msg_buf->v_buf);
    free(msg_buf);
}


/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
