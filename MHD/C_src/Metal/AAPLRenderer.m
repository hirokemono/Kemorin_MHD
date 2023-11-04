/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"

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
        _texture = [self loadTextureUsingAAPLImage: imageFileLocation];

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
    _frameNum++;
    _scalechange = 0.5 + (1.0 + 0.5 * sin(_frameNum * 0.1));
    
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
    static const AAPLVertexWithTexture quadTextureVertices[] =
    {
        // Pixel positions, Color coordinates
        { {  0.5f,  -0.5f },  { 1.f, 1.f } },
        { { -0.5f,  -0.5f },  { 0.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f } },

        { {  0.5f,  -0.5f },  { 1.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f } },
        { {  0.5f,   0.5f },  { 1.f, 0.f } },
    };
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
