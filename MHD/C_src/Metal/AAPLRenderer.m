/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "draw_messages_gl.h"

// Main class performing the rendering
@implementation AAPLRenderer
{
    id<MTLDevice> _device;
    
    // The render pipeline generated from the vertex and fragment shaders in the .metal shader file.
    id<MTLRenderPipelineState> _pipelineState;
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices;
    // The number of vertices in the vertex buffer.
    NSUInteger _numVertices;
    // The Metal texture object
    id<MTLTexture> _texture;
    
    // The current size of the view, used as an input to the vertex shader.
    vector_uint2 _viewportSize;
    float        _scalechange;
    
    NSUInteger _frameNum;
    
    
    IBOutlet KemoViewerObject * _singleKemoView;
}

- (id<MTLTexture>)loadTextureUsingAAPLImage: (NSURL *) url {
    
    AAPLImage * image = [[AAPLImage alloc] initWithTGAFileAtLocation:url];
    
    NSAssert(image, @"Failed to create the image from %@", url.absoluteString);

    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    
    // Indicate that each pixel has a blue, green, red, and alpha channel, where each channel is
    // an 8-bit unsigned normalized value (i.e. 0 maps to 0.0 and 255 maps to 1.0)
    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    
    // Set the pixel dimensions of the texture
    textureDescriptor.width = image.width;
    textureDescriptor.height = image.height;
    
    // Create the texture from the device by using the descriptor
    id<MTLTexture> texture = [_device newTextureWithDescriptor:textureDescriptor];
    
    // Calculate the number of bytes per row in the image.
    NSUInteger bytesPerRow = 4 * image.width;
    
    MTLRegion region = {
        { 0, 0, 0 },                   // MTLOrigin
        {image.width, image.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [texture replaceRegion:region
                mipmapLevel:0
                  withBytes:image.data.bytes
                bytesPerRow:bytesPerRow];
    return texture;
}


- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _frameNum = 0;
    self = [super init];
    if(self)
    {
        NSError *error;

        _device = mtkView.device;

        NSURL *imageFileLocation = [[NSBundle mainBundle] URLForResource:@"Image"
                                                           withExtension:@"tga"];
//        _texture = [self loadTextureUsingAAPLImage: imageFileLocation];

        // Load all the shader files with a .metal file extension in the project.
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
/*
        id<MTLFunction> vertexFunction = [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
        id<MTLFunction> fragmentFunction = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];
*/
        id<MTLFunction> vertexFunction = [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
        id<MTLFunction> fragmentFunction = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

        // Configure a pipeline descriptor that is used to create a pipeline state.
        MTLRenderPipelineDescriptor *pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
/*
        pipelineStateDescriptor.label = @"Simple Pipeline";
*/
        pipelineStateDescriptor.label = @"Texture Pipeline";
        pipelineStateDescriptor.vertexFunction = vertexFunction;
        pipelineStateDescriptor.fragmentFunction = fragmentFunction;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                 error:&error];
                
        // Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
        //  If the Metal API validation is enabled, you can find out more information about what
        //  went wrong.  (Metal API validation is enabled by default when a debug build is run
        //  from Xcode.)
        NSAssert(_pipelineState, @"Failed to create pipeline state: %@", error);

        // Create the command queue
        _commandQueue = [_device newCommandQueue];
    }

    return self;
}


/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    printf("Pointer %p\n", kemo_sgl->kemo_VAOs->msg_VAO);
    struct gl_strided_buffer *cbar_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_message_VAO(kemo_sgl->view_s->iflag_retina,
                    kemo_sgl->view_s->nx_frame,
                    kemo_sgl->view_s->ny_frame,
                    kemo_sgl->kemo_mesh->msg_wk,
                    kemo_sgl->kemo_VAOs->msg_VAO, cbar_buf);
    int ist_xyz = cbar_buf->ist_xyz;
    int ist_tex = cbar_buf->ist_tex;
    int ncomp_buf = cbar_buf->ncomp_buf;
    for(int i=0;i<cbar_buf->num_nod_buf;i++){
        printf("Position Buffer: %d %f %f %f \n", i,
               cbar_buf->v_buf[i*ncomp_buf+ist_xyz  ],
               cbar_buf->v_buf[i*ncomp_buf+ist_xyz+1],
               cbar_buf->v_buf[i*ncomp_buf+ist_xyz+2]);
    };

    double orthogonal[16];
    orthogonal_glmat_c(0.0, kemo_sgl->kemo_mesh->msg_wk->xwin,
                       0.0, kemo_sgl->kemo_mesh->msg_wk->ywin,
                       -1.0, 1.0, orthogonal);
    struct transfer_matrices *matrices = plane_transfer_matrices(orthogonal);

    float xyzw[3][cbar_buf->num_nod_buf];
    for(int i=0;i<cbar_buf->num_nod_buf;i++){
        xyzw[0][i] =  matrices->proj[0] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz]
                 + matrices->proj[4] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz+1]
                 + matrices->proj[8] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz+2]
                 + matrices->proj[12];
        xyzw[1][i] =  matrices->proj[1] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz]
                 + matrices->proj[5] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz+1]
                 + matrices->proj[9] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz+2]
                 + matrices->proj[13];
        xyzw[2][i] =  matrices->proj[2] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz]
                 + matrices->proj[6] *  cbar_buf->v_buf[i*ncomp_buf+ist_xyz+1]
                 + matrices->proj[10] * cbar_buf->v_buf[i*ncomp_buf+ist_xyz+2]
                 + matrices->proj[14];
        printf("Position Buffer truns: %d %f %f %f \n", i,
               xyzw[0][i], xyzw[1][i], xyzw[2][i]);
    };

    
/*
 for(int i=0;i<cbar_buf->num_nod_buf;i++){
     printf("vertex Buffer: %d %f %f \n", i,
            cbar_buf->v_buf[i*cbar_buf->ncomp_buf+cbar_buf->ist_tex],
            cbar_buf->v_buf[i*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]);
 };
    printf("image Buffer: %d x %d of %d\n",
           kemo_sgl->kemo_mesh->msg_wk->npix_x,
           kemo_sgl->kemo_mesh->msg_wk->npix_y,
           kemo_sgl->kemo_mesh->msg_wk->npixel);
*/
    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor.width = kemo_sgl->kemo_mesh->msg_wk->npix_x;
    textureDescriptor.height = kemo_sgl->kemo_mesh->msg_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture = [_device newTextureWithDescriptor:textureDescriptor];
    
    // Calculate the number of bytes per row in the image.
    NSUInteger bytesPerRow = 4 * textureDescriptor.width;
    
    MTLRegion region = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor.width, textureDescriptor.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture replaceRegion:region
                mipmapLevel:0
                  withBytes:kemo_sgl->kemo_mesh->msg_wk->msgBMP
                bytesPerRow:bytesPerRow];
    
    _frameNum++;
//    _scalechange = 0.2 + (1.0 + 0.2 * sin(_frameNum * 0.1));
    _scalechange = 1.0;
    
    // Set up a simple MTLBuffer with the vertices, including position and texture coordinates
/*
   static const AAPLVertex triangleVertices[] =
    {
        // 2D positions,    RGBA colors
        { {  0.5f,  -0.5f }, { 1, 0, 0, 1 } },
        { { -0.5f,  -0.5f }, { 0, 1, 0, 1 } },
        { {  0.0f,   0.5f }, { 0, 0, 1, 1 } },
    };
    int n_tri_vertex = 3;
*/

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
     const AAPLVertexWithTexture quadTextureVertices[] =
    {
        // Pixel positions, Color coordinates
/*      { { -0.7f,  -0.2f, 0.f },  { 0.f, 0.f } },
        { {  0.7f,  -0.2f, 0.f },  { 1.f, 0.f } },
        { {  0.7f,   0.2f, 0.f },  { 1.f, 1.f } },

        { {  0.7f,   0.2f, 0.f },  { 1.f, 1.f } },
        { { -0.7f,   0.2f, 0.f },  { 0.f, 1.f } },
        { { -0.7f,  -0.2f, 0.f },  { 0.f, 0.f } },
*/
 // Pixel positions, Color coordinates
/*
 { { -0.7f,  -0.2f, 0.f },
   {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {  0.7f,  -0.2f, 0.f  },
   {cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {  0.7f,   0.2f, 0.f  },
   {cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },

 { {  0.7f,   0.2f, 0.f  },
   {cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { { -0.7f,   0.2f, 0.f  },
   {cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { { -0.7f,  -0.2f, 0.f  },
   {cbar_buf->v_buf[5*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
    };
*/
        { {xyzw[0][0],  xyzw[1][0], xyzw[2][0] },
          {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
        { {xyzw[0][1],  xyzw[1][1], xyzw[2][1] },
          {cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
        { {xyzw[0][2],  xyzw[1][2], xyzw[2][2] },
          {cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },

        { {xyzw[0][3],  xyzw[1][3], xyzw[2][3] },
          {cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
        { {xyzw[0][4],  xyzw[1][4], xyzw[2][4] },
          {cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
        { {xyzw[0][5],  xyzw[1][5], xyzw[2][5] },
          {cbar_buf->v_buf[5*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
           cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
           };
/*
 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[1*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[2*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },

 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[3*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[4*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
 { {cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_xyz+1]},
   {cbar_buf->v_buf[5*cbar_buf->ncomp_buf+cbar_buf->ist_tex  ],
    cbar_buf->v_buf[0*cbar_buf->ncomp_buf+cbar_buf->ist_tex+1]} },
    };
 */
    int n_quad_vertex = 6;

    // Create a vertex buffer, and initialize it with the quadVertices array
    _vertices = [_device newBufferWithBytes:quadTextureVertices
                                     length:sizeof(quadTextureVertices)
                                    options:MTLResourceStorageModeShared];
    // Calculate the number of vertices by dividing the byte length by the size of each vertex
    _numVertices = sizeof(AAPLVertexWithTexture) / sizeof(AAPLVertex);

     // Create a new command buffer for each render pass to the current drawable.
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
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
        
        [renderEncoder setRenderPipelineState:_pipelineState];

        // Pass in the parameter data.
        [renderEncoder setVertexBytes:quadTextureVertices
                               length:sizeof(quadTextureVertices)
                              atIndex:AAPLVertexInputIndexVertices];
        
        [renderEncoder setVertexBytes:&_viewportSize
                               length:sizeof(_viewportSize)
                              atIndex:AAPLVertexInputIndexViewportSize];

        [renderEncoder setVertexBytes:&_scalechange
                               length:sizeof(_scalechange)
                              atIndex:AAPLVertexInputIndexScale];

        [renderEncoder setVertexBytes:orthogonal
                               length:sizeof(orthogonal)
                              atIndex:AAPLOrthogonalMatrix];

        // Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
        ///  to the 'colorMap' argument in the 'samplingShader' function because its
        //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index.
        [renderEncoder setFragmentTexture:_texture
                                  atIndex:AAPLTextureIndexBaseColor];

        // Draw the triangle.
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:n_quad_vertex];

        [renderEncoder endEncoding];

        // Schedule a present once the framebuffer is complete using the current drawable.
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    free(matrices);
    free(cbar_buf->v_buf);
    free(cbar_buf);

    // Finalize rendering here & push the command buffer to the GPU.
    [commandBuffer commit];
}


/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
