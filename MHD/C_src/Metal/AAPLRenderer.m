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
    id<MTLBuffer> _vertices[2];
    // The number of vertices in the vertex buffer.
    NSUInteger _numVertices[2];
    // The Metal texture object
    id<MTLTexture> _texture[2];
    
    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    float           _scalechange;
    matrix_float4x4 _projection_mat;

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

        _vertexFunction[0] = [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
        _fragmentFunction[0] = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];

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
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    struct gl_strided_buffer *cbar_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_message_buffer(kemo_sgl->view_s->iflag_retina,
                         kemo_sgl->view_s->nx_frame,
                         kemo_sgl->view_s->ny_frame,
                         kemo_sgl->kemo_mesh->msg_wk, cbar_buf);

    double *orthogonal = orthogonal_projection_mat_c(0.0,
                                                     kemo_sgl->kemo_mesh->msg_wk->xwin,
                                                     0.0,
                                                     kemo_sgl->kemo_mesh->msg_wk->ywin,
                                                     -1.0, 1.0);
    int i;
    
    struct transfer_matrices *matrices = plane_transfer_matrices(orthogonal);
    free(orthogonal);
    
    vector_float4   col_wk[4];
    for(i=0;i<4;i++){
        col_wk[i].x = matrices->proj[4*i  ];
        col_wk[i].y = matrices->proj[4*i+1];
        col_wk[i].z = matrices->proj[4*i+2];
        col_wk[i].w = matrices->proj[4*i+3];
    };
    _projection_mat = simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);

  
    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor.width = kemo_sgl->kemo_mesh->msg_wk->npix_x;
    textureDescriptor.height = kemo_sgl->kemo_mesh->msg_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[1] = [_device newTextureWithDescriptor:textureDescriptor];
    
    // Calculate the number of bytes per row in the image.
    NSUInteger bytesPerRow = 4 * textureDescriptor.width;
    
    MTLRegion region = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor.width, textureDescriptor.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture[1] replaceRegion:region
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_mesh->msg_wk->msgBMP
                bytesPerRow:bytesPerRow];
    
    _frameNum++;
//    _scalechange = 0.2 + (1.0 + 0.2 * sin(_frameNum * 0.1));
    _scalechange = 1.0;
    
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

    // Pixel positions, Color coordinates
    AAPLVertexWithTexture *quadTextureVertices;
    if((quadTextureVertices = (AAPLVertexWithTexture *) malloc(n_quad_vertex * sizeof(AAPLVertexWithTexture))) == NULL){
        printf("malloc error for AAPLVertexWithTexture\n");
        exit(0);
    };
    quadTextureVertices = (AAPLVertexWithTexture *) cbar_buf->v_buf;


    // Create a vertex buffer, and initialize it with the quadVertices array
    _vertices[1] = [_device newBufferWithBytes:quadTextureVertices
                                        length:(n_quad_vertex*sizeof(AAPLVertexWithTexture))
                                       options:MTLResourceStorageModeShared];
    // Calculate the number of vertices by dividing the byte length by the size of each vertex
    _numVertices[0] = n_quad_vertex;
    _numVertices[1] = n_quad_vertex;

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
        
        [renderEncoder setRenderPipelineState:_pipelineState[1]];

        // Pass in the parameter data.
        [renderEncoder setVertexBuffer:_vertices[1]
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        
        [renderEncoder setVertexBytes:&_viewportSize
                               length:sizeof(_viewportSize)
                              atIndex:AAPLVertexInputIndexViewportSize];

        [renderEncoder setVertexBytes:&_scalechange
                               length:sizeof(_scalechange)
                              atIndex:AAPLVertexInputIndexScale];

        [renderEncoder setVertexBytes:&_projection_mat
                               length:sizeof(_projection_mat)
                              atIndex:AAPLOrthogonalMatrix];

        // Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
        ///  to the 'colorMap' argument in the 'samplingShader' function because its
        //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index.
        [renderEncoder setFragmentTexture:_texture[1]
                                  atIndex:AAPLTextureIndexBaseColor];

        // Draw the triangles.
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:n_quad_vertex];

        [renderEncoder endEncoding];

        // Schedule a present once the framebuffer is complete using the current drawable.
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer  commit];

    id<MTLCommandBuffer> commandBuffer2 = [_commandQueue[0] commandBuffer];
    commandBuffer2.label = @"MyCommand";
    MTLRenderPassDescriptor *renderPassDescriptor2 = view.currentRenderPassDescriptor;
    if(renderPassDescriptor2 != nil){
        id<MTLRenderCommandEncoder> renderEncoder2 =
        [commandBuffer2 renderCommandEncoderWithDescriptor:renderPassDescriptor2];
        renderEncoder2.label = @"MyRenderEncoder2";

        // Set the region of the drawable to draw into.
        [renderEncoder2 setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, 0.0, 1.0 }];
        
        [renderEncoder2 setRenderPipelineState:_pipelineState[0]];

        // Pass in the parameter data.
        [renderEncoder2 setVertexBuffer:_vertices[0]
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        
        [renderEncoder2 setVertexBytes:&_viewportSize
                                length:sizeof(_viewportSize)
                               atIndex:AAPLVertexInputIndexViewportSize];

        [renderEncoder2 setVertexBytes:&_scalechange
                                length:sizeof(_scalechange)
                               atIndex:AAPLVertexInputIndexScale];
/*
        [renderEncoder setVertexBytes:&_projection_mat
                               length:sizeof(_projection_mat)
                              atIndex:AAPLOrthogonalMatrix];
 */
        // Draw the triangles.
        [renderEncoder2 drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:n_quad_vertex];

        [renderEncoder2 endEncoding];
        [commandBuffer2 presentDrawable:view.currentDrawable];
    }


    free(matrices);
    free(cbar_buf->v_buf);
    free(cbar_buf);

    // Finalize rendering here & push the command buffer to the GPU.
    [commandBuffer2 commit];
}


/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
