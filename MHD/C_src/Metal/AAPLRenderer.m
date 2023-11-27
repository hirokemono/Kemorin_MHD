/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation AAPLRenderer
{

    id<MTLDevice> _device;

/*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture;
/* Render pass descriptor to draw to the texture */
    MTLRenderPassDescriptor* _textureRenderPassDescriptor;

    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;
    id<MTLDepthStencilState> _noDepthState;

    KemoViewMetalBuffers   _kemoViewMetalBuf;
    KemoViewMetalShaders   _kemoViewShaders;
    KemoViewMetalPipelines _kemoViewPipelines;

    /* Texture to render to and then sample from. */
    id<MTLTexture> _renderLeftTexture;
    id<MTLTexture> _renderRightTexture;
    KemoViewMetalPipelines _kemoOffscreenPipelines;


    id<MTLRenderCommandEncoder> _renderEncoder;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    
    KemoViewUnites _monoViewUnites;
    KemoViewUnites _rightViewUnites;
    KemoViewUnites _leftViewUnites;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;

    int _icou_lr;

    NSUInteger _frameNum;
    
    IBOutlet KemoViewerObject * _singleKemoView;
}

-(void) loadShaderLibrary: (id<MTLDevice> *) device
{
    // Load all the shader files with a .metal file extension in the project.
   id<MTLLibrary> defaultLibrary = [*device newDefaultLibrary];
   _kemoViewShaders.base2DVertexFunction =   [defaultLibrary newFunctionWithName:@"Base2dVertexShader"];
   _kemoViewShaders.base2DFragmentFunction = [defaultLibrary newFunctionWithName:@"Base2DfragmentShader"];

   _kemoViewShaders.simple2DVertexFunction =   [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
   _kemoViewShaders.simple2DFragmentFunction = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];

   _kemoViewShaders.texured2DVertexFunction =   [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
   _kemoViewShaders.texured2DFragmentFunction = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

   _kemoViewShaders.phongVertexFunction =   [defaultLibrary newFunctionWithName:@"PhongVertexShader"];
   _kemoViewShaders.phongFragmentFunction = [defaultLibrary newFunctionWithName:@"PhongFragmentShader"];

   _kemoViewShaders.simpleVertexFunction =   [defaultLibrary newFunctionWithName:@"SimpleVertexShader"];
   _kemoViewShaders.simpleFragmentFunction = [defaultLibrary newFunctionWithName:@"SimpleFragmentShader"];

   _kemoViewShaders.texuredPhongVertexFunction = [defaultLibrary newFunctionWithName:@"PhongTexureVertexShader"];
   _kemoViewShaders.texuredPhongFragmentFunction = [defaultLibrary newFunctionWithName:@"PhongTextureFragmentShader"];

   _kemoViewShaders.PhongAnaglyphVertexFunction = [defaultLibrary newFunctionWithName:@"PhongAnagriphVertexShader"];
   _kemoViewShaders.PhongAnaglyphFragmentFunction = [defaultLibrary newFunctionWithName:@"PhongAnagriphFragmentShader"];

   _kemoViewShaders.texuredVertexFunction =   [defaultLibrary newFunctionWithName:@"SimpleTexureVertexShader"];
   _kemoViewShaders.texuredFragmentFunction = [defaultLibrary newFunctionWithName:@"SimpleTextureFragmentShader"];
    return;
}

-(void) initKemoViewPipelines:(nonnull MTKView *)mtkView
                      shaders:(KemoViewMetalShaders *) kemoViewShaders
                    pipelines:(KemoViewMetalPipelines *) kemoViewPipelines
                  targetPixel:(MTLPixelFormat) pixelformat
{
    NSError *error;
    id<MTLDevice> device = mtkView.device;

    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"2D Texture Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texured2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texured2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemoViewPipelines->texured2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                       error:&error];
/* Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
//  If the Metal API validation is enabled, you can find out more information about what
//  went wrong.  (Metal API validation is enabled by default when a debug build is run
//  from Xcode.) */
    NSAssert(kemoViewPipelines->texured2DPipelineState, @"Failed to create pipeline state: %@", error);

/*  Create pipeline for simple 2D rendering */
    pipelineStateDescriptor.label = @"2D transpearent Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simple2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simple2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemoViewPipelines->trans2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                    error:&error];
    NSAssert(kemoViewPipelines->trans2DPipelineState, @"Failed to create pipeline state: %@", error);

    /*  Create pipeline for simple rendering */
    pipelineStateDescriptor.label = @"2D Simple Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simple2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simple2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = NO;
    
    kemoViewPipelines->simple2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                     error:&error];
    NSAssert(kemoViewPipelines->simple2DPipelineState, @"Failed to create pipeline state: %@", error);


/*  Create pipeline for Basic rendering */
    pipelineStateDescriptor.label = @"Base Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->base2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->base2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    kemoViewPipelines->base2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemoViewPipelines->base2DPipelineState, @"Failed to create pipeline state: %@", error);

    /* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Simple Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simpleVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simpleFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    kemoViewPipelines->simplePipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemoViewPipelines->simplePipelineState, @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"Phong Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->phongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->phongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
    kemoViewPipelines->phongPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemoViewPipelines->phongPipelineState, @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredPhongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredPhongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
    kemoViewPipelines->phongTexturedPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                          error:&error];
    NSAssert(kemoViewPipelines->phongTexturedPipelineState, @"Failed to create pipeline state: %@", error);

    /* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"Phong Anaglyph Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->phongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->phongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOne;
        
    kemoViewPipelines->phongAnaglyphPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemoViewPipelines->phongAnaglyphPipelineState, @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemoViewPipelines->texuredPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                    error:&error];
    NSAssert(kemoViewPipelines->texuredPipelineState, @"Failed to create pipeline state: %@", error);
    return;
}


- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _frameNum = 0;
    _icou_lr = 0;
    self = [super init];
    if(self)
    {
        _device = mtkView.device;

// Indicate that each pixel in the depth buffer is a 32-bit floating point value.
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

/* Load all the shader files with a .metal file extension in the project. */
        [self loadShaderLibrary:&_device];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        [self initKemoViewPipelines:mtkView
                            shaders:&_kemoViewShaders
                          pipelines:&_kemoViewPipelines
                        targetPixel:mtkView.colorPixelFormat];
/* Configure a pipeline descriptor that is used to create a pipeline state. */

//        printf("frame %f %f \n", mtkView.drawableSize.width, mtkView.drawableSize.height);

        MTLTextureDescriptor *texDescriptor = [MTLTextureDescriptor new];
        texDescriptor.textureType = MTLTextureType2D;
        texDescriptor.width =  mtkView.drawableSize.width;
        texDescriptor.height = mtkView.drawableSize.height;
        texDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        texDescriptor.usage = MTLTextureUsageRenderTarget |
                              MTLTextureUsageShaderRead;

        _renderLeftTexture =  [_device newTextureWithDescriptor:texDescriptor];
        _renderRightTexture = [_device newTextureWithDescriptor:texDescriptor];

        [self initKemoViewPipelines:mtkView
                            shaders:&_kemoViewShaders
                          pipelines:&_kemoOffscreenPipelines
                        targetPixel:_renderLeftTexture.pixelFormat];


/* Add Depth buffer description in command */
        MTLDepthStencilDescriptor *depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState = [_device newDepthStencilStateWithDescriptor:depthDescriptor];

        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = NO;
        _noDepthState = [_device newDepthStencilStateWithDescriptor:depthDescriptor];
    }
/* Create the command queue */
    _commandQueue = [_device newCommandQueue];
    return self;
}

- (matrix_float4x4) setMetalViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int i=0;i<4;i++){
        col_wk[i].x = matrix[4*i  ];
        col_wk[i].y = matrix[4*i+1];
        col_wk[i].z = matrix[4*i+2];
        col_wk[i].w = matrix[4*i+3];
    };
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}


- (matrix_float4x4) setMetalModelViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
//    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}
- (matrix_float4x4) setMetalProjViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
/* Shift viewport */
    col_wk[2][2] = (0.5*matrix[4*2+2] + 0.5*matrix[4*2+3]);
    col_wk[3][2] = (0.5*matrix[4*3+2] + 0.5*matrix[4*3+3]);

//    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}

- (void)drawSolidWithSimple:(struct gl_strided_buffer *) buf
                    encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                     vertex:(id<MTLBuffer> *) vertices
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:_depthState];
        }
        
        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.simplePipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawTexureWithPhong:(struct gl_strided_buffer *) buf
                    encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                     vertex:(id<MTLBuffer> *) vertices
                     texure:(id<MTLTexture> *) texture
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:_depthState];
        }

        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.phongTexturedPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];

        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];

        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawCubeWithPhong:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                 pipeline:(id<MTLRenderPipelineState> *) pipeline
                   vertex:(id<MTLBuffer> *) vertices
                    index:(id<MTLBuffer> *) indices
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setCullMode:MTLCullModeBack];
//        [*renderEncoder setDepthStencilState:_depthState];
        [*renderEncoder setDepthStencilState:_noDepthState];

        [*renderEncoder setRenderPipelineState:*pipeline];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                  indexCount:buf->num_nod_buf
                                   indexType:MTLIndexTypeUInt32
                                 indexBuffer:*indices
                           indexBufferOffset:0];
    };
}

- (void)drawSolidWithPhong:(struct gl_strided_buffer *) buf
                   encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                    vertex:(id<MTLBuffer> *) vertices
                    unites:(KemoViewUnites *) monoViewUnites
                     sides:(int) iflag_surface
                     solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:_depthState];
        }

        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.phongPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawLineObject:(struct gl_strided_buffer *) buf
               encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                vertex:(id<MTLBuffer> *)  vertices
                unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:_depthState];
        
        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.simplePipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLProjectionMatrix];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                          vertexStart:0
                          vertexCount:buf->num_nod_buf];
    }
};

- (void)drawMapSolidObjext:(struct gl_strided_buffer *) buf
                   encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                    vertex:(id<MTLBuffer> *) vertices
                projection:(matrix_float4x4 *) projection_mat;
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState: _kemoViewPipelines.trans2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:buf->num_nod_buf];
    };

}

- (void)drawMapLineObjext:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                   vertex:(id<MTLBuffer> *) vertices
               projection:(matrix_float4x4 *) projection_mat;
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.simple2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                          vertexStart:0
                          vertexCount:buf->num_nod_buf];
    };

}

- (void)drawTextBoxObjext:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                   vertex:(id<MTLBuffer> *)  vertices
                   texure:(id<MTLTexture> *) texture
               projection:(matrix_float4x4 *) projection_mat;
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState:_kemoViewPipelines.texured2DPipelineState];
/* Pass in the parameter data. */
        [*renderEncoder setVertexBuffer:*vertices
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLOrthogonalMatrix];

/* Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
///  to the 'colorMap' argument in the 'samplingShader' function because its
//   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index. */
        [*renderEncoder setFragmentTexture:*texture
                                  atIndex:AAPLTextureIndexBaseColor];
/* Draw the triangles. */
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:buf->num_nod_buf];
    };
}

- (void)setCubeVertexs:(struct gl_strided_buffer *) buf
              indexbuf:(struct gl_index_buffer *) index_buf
                vertex:(id<MTLBuffer> *) vertices
                 index:(id<MTLBuffer> *) indices
{
    if(buf->num_nod_buf > 0){
        *vertices = [_device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        *indices = [_device newBufferWithBytes:index_buf->ie_buf
                                        length:(index_buf->nsize_buf * sizeof(unsigned int))
                                       options:MTLResourceStorageModeShared];
    };
};

- (void)setMetalVertexs:(struct gl_strided_buffer *) buf
                 vertex:(id<MTLBuffer> *)  vertices
{
    if(buf->num_nod_buf > 0){
        *vertices = [_device newBufferWithBytesNoCopy:buf->v_buf
                                               length:(buf->nsize_buf * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    };
};

- (void)setPSFTexture:(struct gl_strided_buffer *) buf
                image:(struct kemo_PSF_texure *) psf_texure
                vertex:(id<MTLBuffer> *)  vertices
                texure:(id<MTLTexture> *) texture
{
    if(buf->num_nod_buf > 0){
        *vertices = [_device newBufferWithBytesNoCopy:buf->v_buf
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
        *texture = [_device newTextureWithDescriptor:lineTextureDescriptor];
/* Copy the bytes from the data object into the texture */
        [*texture replaceRegion:region
                    mipmapLevel:0
                      withBytes:psf_texure->texure_rgba
                    bytesPerRow:bytesPerRow];
    };

}

- (void)setTextBoxTexture:(struct gl_strided_buffer *) buf
                    image:(struct line_text_image *) img
                   vertex:(id<MTLBuffer> *)  vertices
                   texure:(id<MTLTexture> *) texture
{
    if(buf->num_nod_buf > 0){
        *vertices = [_device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
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
        *texture = [_device newTextureWithDescriptor:lineTextureDescriptor];
/* Copy the bytes from the data object into the texture */
        [*texture replaceRegion:region
                    mipmapLevel:0
                      withBytes:img->imgBMP
                    bytesPerRow:bytesPerRow];
    };

}

- (void)rightMaterialParams:(MaterialParameters *) material
{
    material->ambient[0] = 0.0;
    material->diffuse[0] = 0.0;
    material->specular[0] = 0.0;
    return;
}

- (void)leftMaterialParams:(MaterialParameters *) material
{
    for(int i=1;i<3;i++){
        material->ambient[i] =  0.0;
        material->diffuse[i] =  0.0;
        material->specular[i] = 0.0;
    }
    return;
}

- (void)fillMaterialParams:(MaterialParameters *) material
{
    for(int i=1;i<3;i++){
        material->ambient[i] =  material->ambient[0];
        material->diffuse[i] =  material->diffuse[0];
        material->specular[i] = material->specular[0];
    }
    material->ambient[3] = 1.0;
    material->diffuse[3] = 1.0;
    material->specular[3] = 1.0;
    return;
}

- (void)setMetalColorbuffer:(LightSourceParameters *) lights
                   material:(MaterialParameters *) mats
{
    int i;
    float x, y, z;
    
    mats->ambient[0] =  kemoview_get_material_parameter(AMBIENT_FLAG);
    mats->diffuse[0] =  kemoview_get_material_parameter(DIFFUSE_FLAG);
    mats->specular[0] = kemoview_get_material_parameter(SPECULAR_FLAG);
    mats->shininess =   kemoview_get_material_parameter(SHINENESS_FLAG);
    
    lights->num_lights = kemoview_get_num_light_position();
    for(i=0;i<lights->num_lights;i++){
        kemoview_get_each_light_xyz(i, &x, &y, &z);
        lights->position[i][0] = x;
        lights->position[i][1] = y;
        lights->position[i][2] = z;
        lights->position[i][3] = 1.0;
    };
    return;
};

- (void)setCubeColorbuffer:(LightSourceParameters *) lights
                  material:(MaterialParameters *) mats
{
    int i, j;
    
    struct initial_cube_lighting *init_light = init_inital_cube_lighting();
    lights->num_lights = init_light->num_light;
    for(i=0;i<lights->num_lights;i++){
        for(j=0;j<4;j++){
            lights->position[i][j] = init_light->lightposition[i][j];
        };
    };
    mats->ambient[0] =  init_light->whitelight[0][0];
    mats->diffuse[0] =  init_light->whitelight[1][0];
    mats->specular[0] = init_light->whitelight[2][0];
    mats->shininess =   init_light->shine[0];
    free(init_light);
    return;
};

- (void)setTransferMatrices:(KemoViewUnites *) monoViewUnites
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    double *orthogonal = orthogonal_projection_mat_c(0.0, kemo_sgl->view_s->nx_frame,
                                                     0.0, kemo_sgl->view_s->ny_frame,
                                                    -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(kemo_sgl->view_s);
    struct transfer_matrices *map_matrices
            = init_projection_matrix_for_map(kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame);
    free(orthogonal);

    _cbar_proj_mat =  [self setMetalViewMatrices:cbar_matrices->proj];
    _map_proj_mat =   [self setMetalViewMatrices:map_matrices->proj];

    monoViewUnites->modelview_mat =  [self setMetalViewMatrices:view_matrices->model];
    monoViewUnites->projection_mat = [self setMetalProjViewMatrices:view_matrices->proj];
    monoViewUnites->normal_mat =     [self setMetalViewMatrices:view_matrices->nrmat];
    free(cbar_matrices);
    free(view_matrices);
    free(map_matrices);
    return;
};

- (void) setKemoViewLightings:(struct gl_strided_buffer *) cube_buf
                       unites:(KemoViewUnites *) monoViewUnites
{
    if(cube_buf->num_nod_buf > 0){
        [self setCubeColorbuffer:&(monoViewUnites->lights)
                        material:&(monoViewUnites->material)];
    } else {
        [self setMetalColorbuffer:&(monoViewUnites->lights)
                         material:&(monoViewUnites->material)];
    }
    [self fillMaterialParams:&(monoViewUnites->material)];
    return;
};

- (void) releaseKemoViewMetalBuffers:(struct kemoview_buffers *) kemo_buffers
                               views:(struct view_element *) view_s
{
    if(view_s->iflag_view_type == VIEW_MAP){
        if(kemo_buffers->MAP_solid_buf->num_nod_buf > 0)   {[_kemoViewMetalBuf.mapSolidVertice release];};
        if(kemo_buffers->MAP_isoline_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.mapLinesVertice release];};
    }else{
        if(kemo_buffers->PSF_stxur_buf->num_nod_buf > 0){
            [_kemoViewMetalBuf.psfSTexureVertice release];
            [_kemoViewMetalBuf.psfSolidTexure    release];
        };
        
        if(kemo_buffers->axis_buf->num_nod_buf > 0)        {[_kemoViewMetalBuf.axisVertice     release];};
        if(kemo_buffers->PSF_solid_buf->num_nod_buf > 0)   {[_kemoViewMetalBuf.psfSolidVertice release];};
        if(kemo_buffers->PSF_isoline_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.psfLinesVertice release];};
        if(kemo_buffers->PSF_arrow_buf->num_nod_buf > 0)   {[_kemoViewMetalBuf.psfArrowVertice release];};

        if(kemo_buffers->FLINE_tube_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.fieldTubeVertice release];};
        if(kemo_buffers->FLINE_line_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.fieldLineVertice release];};
        
        if(kemo_buffers->mesh_node_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.meshNodeVertice   release];};
        if(kemo_buffers->mesh_grid_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.meshGridVertice   release];};
        if(kemo_buffers->mesh_solid_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.meshSolidVertice release];};
        
        /*  Set Cube Vertex buffer */
        if(kemo_buffers->cube_buf->num_nod_buf > 0){
            [_kemoViewMetalBuf.cubeVertice release];
            [_kemoViewMetalBuf.cubeIndex   release];
        };
        
        /*  Set transparent vertexs */
        if(kemo_buffers->PSF_ttxur_buf->num_nod_buf > 0){
            [_kemoViewMetalBuf.psfTTexureVertice release];
            [_kemoViewMetalBuf.psfTransTexure release];
        };
        
        if(kemo_buffers->PSF_trns_buf->num_nod_buf > 0)  {[_kemoViewMetalBuf.psfTransVertice release];};
        if(kemo_buffers->mesh_trns_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.meshTransVertice release];};
    };
    
    if(kemo_buffers->coast_buf->num_nod_buf > 0)    {[_kemoViewMetalBuf.coastVertice   release];};
    if(kemo_buffers->sph_grid_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.sphGridVertice release];};

    if(kemo_buffers->cbar_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.colorBarVertice release];};
    if(kemo_buffers->min_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.minLabelVertice release];
        [_kemoViewMetalBuf.minLabelTexure  release];
    };
    if(kemo_buffers->max_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.maxLabelVertice release];
        [_kemoViewMetalBuf.maxLabelTexure  release];
    };
    if(kemo_buffers->zero_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.zeroLabelVertice release];
        [_kemoViewMetalBuf.zeroLabelTexure  release];
    };
    if(kemo_buffers->time_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.timeLabelVertice release];
        [_kemoViewMetalBuf.timeLabelTexure  release];
    };
    if(kemo_buffers->msg_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.messageVertice release];
        [_kemoViewMetalBuf.messageTexure  release];
    };
    return;
}

- (void) setKemoViewMetalBuffers:(struct kemoview_buffers *) kemo_buffers
                           views:(struct view_element *) view_s
                            PSFs:(struct kemoview_psf *) kemo_psf
                       fieldline:(struct kemoview_fline *) kemo_fline
{
    if(view_s->iflag_view_type == VIEW_MAP){
        /*  Commands to render map projection */
        [self setMetalVertexs:kemo_buffers->MAP_solid_buf
                       vertex:&_kemoViewMetalBuf.mapSolidVertice];
        [self setMetalVertexs:kemo_buffers->MAP_isoline_buf
                       vertex:&_kemoViewMetalBuf.mapLinesVertice];
        [self setMetalVertexs:kemo_buffers->coast_buf
                       vertex:&_kemoViewMetalBuf.coastVertice];
        [self setMetalVertexs:kemo_buffers->sph_grid_buf
                       vertex:&_kemoViewMetalBuf.sphGridVertice];
    }else{
        [self setPSFTexture:kemo_buffers->PSF_stxur_buf
                      image:kemo_psf->psf_a->psf_texure
                     vertex:&_kemoViewMetalBuf.psfSTexureVertice
                     texure:&_kemoViewMetalBuf.psfSolidTexure];
        
        [self setMetalVertexs:kemo_buffers->coast_buf
                       vertex:&_kemoViewMetalBuf.coastVertice];
        [self setMetalVertexs:kemo_buffers->sph_grid_buf
                       vertex:&_kemoViewMetalBuf.sphGridVertice];
        [self setMetalVertexs:kemo_buffers->axis_buf
                       vertex:&_kemoViewMetalBuf.axisVertice];

        [self setMetalVertexs:kemo_buffers->PSF_solid_buf
                       vertex:&_kemoViewMetalBuf.psfSolidVertice];
        [self setMetalVertexs:kemo_buffers->PSF_isoline_buf
                       vertex:&_kemoViewMetalBuf.psfLinesVertice];
        [self setMetalVertexs:kemo_buffers->PSF_arrow_buf
                       vertex:&_kemoViewMetalBuf.psfArrowVertice];
        
        if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            [self setMetalVertexs:kemo_buffers->FLINE_tube_buf
                           vertex:&_kemoViewMetalBuf.fieldTubeVertice];
        };
        [self setMetalVertexs:kemo_buffers->FLINE_line_buf
                       vertex:&_kemoViewMetalBuf.fieldLineVertice];
        
        [self setMetalVertexs:kemo_buffers->mesh_node_buf
                       vertex:&_kemoViewMetalBuf.meshNodeVertice];
        [self setMetalVertexs:kemo_buffers->mesh_grid_buf
                       vertex:&_kemoViewMetalBuf.meshGridVertice];
        [self setMetalVertexs:kemo_buffers->mesh_solid_buf
                       vertex:&_kemoViewMetalBuf.meshSolidVertice];
        
/*  Set Cube Vertex buffer */
        [self setCubeVertexs:kemo_buffers->cube_buf
                    indexbuf:kemo_buffers->cube_index_buf
                      vertex:&_kemoViewMetalBuf.cubeVertice
                       index:&_kemoViewMetalBuf.cubeIndex];
        
/*  Set transparent vertexs */
        [self setPSFTexture:kemo_buffers->PSF_ttxur_buf
                      image:kemo_psf->psf_a->psf_texure
                     vertex:&_kemoViewMetalBuf.psfTTexureVertice
                     texure:&_kemoViewMetalBuf.psfTransTexure];
        [self setMetalVertexs:kemo_buffers->PSF_trns_buf
                       vertex:&_kemoViewMetalBuf.psfTransVertice];
        [self setMetalVertexs:kemo_buffers->mesh_trns_buf
                       vertex:&_kemoViewMetalBuf.meshTransVertice];
    };
    
    [self setMetalVertexs:kemo_buffers->cbar_buf
                   vertex:&_kemoViewMetalBuf.colorBarVertice];
    [self setTextBoxTexture:kemo_buffers->min_buf
                      image:kemo_buffers->cbar_min_image
                     vertex:&_kemoViewMetalBuf.minLabelVertice
                     texure:&_kemoViewMetalBuf.minLabelTexure];
    [self setTextBoxTexture:kemo_buffers->max_buf
                      image:kemo_buffers->cbar_max_image
                     vertex:&_kemoViewMetalBuf.maxLabelVertice
                     texure:&_kemoViewMetalBuf.maxLabelTexure];
    [self setTextBoxTexture:kemo_buffers->zero_buf
                      image:kemo_buffers->cbar_zero_image
                     vertex:&_kemoViewMetalBuf.zeroLabelVertice
                     texure:&_kemoViewMetalBuf.zeroLabelTexure];
    [self setTextBoxTexture:kemo_buffers->time_buf
                      image:kemo_buffers->tlabel_image
                     vertex:&_kemoViewMetalBuf.timeLabelVertice
                     texure:&_kemoViewMetalBuf.timeLabelTexure];
    [self setTextBoxTexture:kemo_buffers->msg_buf
                      image:kemo_buffers->message_image
                     vertex:&_kemoViewMetalBuf.messageVertice
                     texure:&_kemoViewMetalBuf.messageTexure];
    return;
}


- (void) encodeMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   buffer:(struct kemoview_buffers *) kemo_buffers
               projection:(matrix_float4x4 *) map_proj_mat
{
/*  Commands to render map projection */
    [self drawMapSolidObjext:kemo_buffers->MAP_solid_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.mapSolidVertice
                  projection:map_proj_mat];
/*  Commands to render isolines on map */
    [self drawMapSolidObjext:kemo_buffers->MAP_isoline_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.mapLinesVertice
                  projection:map_proj_mat];
/*  Commands to render Coastline on map */
    [self drawMapLineObjext:kemo_buffers->coast_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.coastVertice
                 projection:map_proj_mat];
/*  Commands to render grids on map */
    [self drawMapLineObjext:kemo_buffers->sph_grid_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.sphGridVertice
                 projection:map_proj_mat];
    return;
}

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                      buffer:(struct kemoview_buffers *) kemo_buffers
                        mesh:(struct kemoview_mesh *) kemo_mesh
                      unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                       encoder:renderEncoder
                        vertex:&_kemoViewMetalBuf.psfSTexureVertice
                        texure:&_kemoViewMetalBuf.psfSolidTexure
                        unites:monoViewUnites
                         sides:BOTH_SURFACES
                         solid:SMOOTH_SHADE];
    [self drawSolidWithSimple:kemo_buffers->axis_buf
                      encoder:renderEncoder
                       vertex:&_kemoViewMetalBuf.axisVertice
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfSolidVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.fieldLineVertice
                  unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.meshGridVertice
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.meshSolidVertice
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                   pipeline:&_kemoViewPipelines.phongPipelineState
                     vertex:&_kemoViewMetalBuf.cubeVertice
                      index:&_kemoViewMetalBuf.cubeIndex
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.coastVertice
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.sphGridVertice
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                       vertex:&_kemoViewMetalBuf.psfTTexureVertice
                       texure:&_kemoViewMetalBuf.psfTransTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.meshTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    return;
}

- (void) encode3DObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  buffer:(struct kemoview_buffers *) kemo_buffers
               fieldline:(struct kemoview_fline *) kemo_fline
                    mesh:(struct kemoview_mesh *) kemo_mesh
                  unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                       encoder:renderEncoder
                        vertex:&_kemoViewMetalBuf.psfSTexureVertice
                        texure:&_kemoViewMetalBuf.psfSolidTexure
                        unites:monoViewUnites
                         sides:BOTH_SURFACES
                         solid:SMOOTH_SHADE];
    [self drawSolidWithSimple:kemo_buffers->axis_buf
                      encoder:renderEncoder
                       vertex:&_kemoViewMetalBuf.axisVertice
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfSolidVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_isoline_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfLinesVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_arrow_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfArrowVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
        [self drawSolidWithPhong:kemo_buffers->FLINE_tube_buf
                         encoder:renderEncoder
                          vertex:&_kemoViewMetalBuf.fieldTubeVertice
                          unites:monoViewUnites
                           sides:BOTH_SURFACES
                           solid:SMOOTH_SHADE];
    };
       
    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.fieldLineVertice
                  unites:monoViewUnites];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_node_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.meshNodeVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.meshGridVertice
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.meshSolidVertice
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                   pipeline:&_kemoViewPipelines.phongPipelineState
                     vertex:&_kemoViewMetalBuf.cubeVertice
                      index:&_kemoViewMetalBuf.cubeIndex
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.coastVertice
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
                  vertex:&_kemoViewMetalBuf.sphGridVertice
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                       vertex:&_kemoViewMetalBuf.psfTTexureVertice
                       texure:&_kemoViewMetalBuf.psfTransTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.psfTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.meshTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    return;
}


- (void) encodeLabelObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                     buffer:(struct kemoview_buffers *) kemo_buffers
                 projection:(matrix_float4x4 *) projection_mat
{
/*  Commands to render colorbar  box */
    [self drawMapSolidObjext:kemo_buffers->cbar_buf
                     encoder:renderEncoder
                      vertex:&_kemoViewMetalBuf.colorBarVertice
                  projection:projection_mat];
/*  Commands to render colorbar  label */
    [self drawTextBoxObjext:kemo_buffers->min_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.minLabelVertice
                     texure:&_kemoViewMetalBuf.minLabelTexure
                 projection:projection_mat];
    [self drawTextBoxObjext:kemo_buffers->max_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.maxLabelVertice
                     texure:&_kemoViewMetalBuf.maxLabelTexure
                 projection:projection_mat];
    [self drawTextBoxObjext:kemo_buffers->zero_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.zeroLabelVertice
                     texure:&_kemoViewMetalBuf.zeroLabelTexure
                 projection:projection_mat];

/*  Commands to render time label */
    [self drawTextBoxObjext:kemo_buffers->time_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.timeLabelVertice
                     texure:&_kemoViewMetalBuf.timeLabelTexure
                 projection:projection_mat];
/*  Commands to render colorbar  box */
    [self drawTextBoxObjext:kemo_buffers->msg_buf
                    encoder:renderEncoder
                     vertex:&_kemoViewMetalBuf.messageVertice
                     texure:&_kemoViewMetalBuf.messageTexure
                 projection:projection_mat];
    return;
}

- (void) encodeKemoViewers:(id<MTLRenderCommandEncoder>  *) renderEncoder
                    buffer:(struct kemoview_buffers *) kemo_buffers
                     views:(struct view_element *) view_s
                 fieldline:(struct kemoview_fline *) kemo_fline
                      mesh:(struct kemoview_mesh *) kemo_mesh
                    unites:(KemoViewUnites *) monoViewUnites
{
    if(view_s->iflag_view_type == VIEW_MAP){
        [self encodeMapObjects:&_renderEncoder
                        buffer:kemo_buffers
                    projection:&_map_proj_mat];
    } else {
        [self encode3DObjects:renderEncoder
                       buffer:kemo_buffers
                    fieldline:kemo_fline
                         mesh:kemo_mesh
                       unites:monoViewUnites];
    };
    [self encodeLabelObjects:renderEncoder
                      buffer:kemo_buffers
                  projection:&_cbar_proj_mat];
    return;
}

- (void) encodeSimpleView:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   buffer:(struct kemoview_buffers *) kemo_buffers
                     mesh:(struct kemoview_mesh *) kemo_mesh
                   unites:(KemoViewUnites *) monoViewUnites
{
    [self encodeSimpleObjects:renderEncoder
                   buffer:kemo_buffers
                     mesh:kemo_mesh
                   unites:monoViewUnites];
    [self encodeLabelObjects:renderEncoder
                      buffer:kemo_buffers
                  projection:&_cbar_proj_mat];
    return;
}

-(void) takoview:(nonnull MTKView *)view
         eyeflag:(int) iflag_lr
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();

/* Create a new command buffer for each render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"KemoViewerCommands";
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";

        if(kemoview_get_draw_mode() == SIMPLE_DRAW){
            [self encodeSimpleView:&_renderEncoder
                            buffer:kemo_sgl->kemo_buffers
                              mesh:kemo_sgl->kemo_mesh
                            unites:&_monoViewUnites];
        }else{
            [self encodeKemoViewers:&_renderEncoder
                             buffer:kemo_sgl->kemo_buffers
                              views:kemo_sgl->view_s
                          fieldline:kemo_sgl->kemo_fline
                               mesh:kemo_sgl->kemo_mesh
                             unites:&_monoViewUnites];
        };

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer  commit];
    return;
}

-(unsigned char *) getRenderedbyMetalToBGRA:(nonnull MTKView *)view
{
    /*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture = view.currentDrawable.texture;
    NSUInteger height = _imageOutputTexture.height;
    NSUInteger width =  _imageOutputTexture.width;
    NSUInteger bpRaw = 4 * _imageOutputTexture.width;
    NSUInteger num_pixel = _imageOutputTexture.width * _imageOutputTexture.height;
    unsigned char *bgra = (unsigned char *) malloc(4*num_pixel * sizeof(unsigned char));

    [_imageOutputTexture getBytes:bgra
                      bytesPerRow:bpRaw
                       fromRegion:MTLRegionMake2D(0, 0, width, height)
                      mipmapLevel:0];
    return bgra;
};

- (void)drawKemoMetalView:(nonnull MTKView *)view
                  eyeflag:(int) iflag_lr
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    if(kemoview_get_draw_mode() == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:kemo_sgl->kemo_buffers
                                    views:kemo_sgl->view_s];

        kemoview_const_buffers(kemo_sgl);

        [self setKemoViewMetalBuffers:kemo_sgl->kemo_buffers
                                views:kemo_sgl->view_s
                                 PSFs:kemo_sgl->kemo_psf
                            fieldline:kemo_sgl->kemo_fline];
    };

    [self setKemoViewLightings:kemo_sgl->kemo_buffers->cube_buf
                        unites:&_monoViewUnites];

    int iflag = kemoview_get_view_type_flag();
    if(iflag == VIEW_STEREO){
//        [self leftMaterialParams:&_monoViewUnites.material];

        update_left_projection_struct(kemo_sgl->view_s);
        modify_left_view_by_struct(kemo_sgl->view_s);
        [self setTransferMatrices:&_monoViewUnites];
        [self takoview:view eyeflag:-1];
        unsigned char * leftBGRA =  [self getRenderedbyMetalToBGRA:view];

        update_projection_struct(kemo_sgl->view_s);
        modify_view_by_struct(kemo_sgl->view_s);

//        [self rightMaterialParams:&_monoViewUnites.material];
        
        update_right_projection_struct(kemo_sgl->view_s);
        modify_right_view_by_struct(kemo_sgl->view_s);
        [self setTransferMatrices:&_monoViewUnites];
        [self takoview:view eyeflag:1];
        unsigned char * rightBGRA =  [self getRenderedbyMetalToBGRA:view];

        update_projection_struct(kemo_sgl->view_s);
        modify_view_by_struct(kemo_sgl->view_s);
        
        for(int i=0;(view.drawableSize.width * view.drawableSize.height);i++){
            rightBGRA[4*i+2] = leftBGRA[4*i+2];
            rightBGRA[4*i+3] = leftBGRA[4*i+3];
        }
    }else{
        [self setTransferMatrices:&_monoViewUnites];
    };

    [self takoview:view eyeflag:0];
    return;
}




/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    int iflag_lr = 0;
    [self drawKemoMetalView:view
                    eyeflag:iflag_lr];
    return;
}

/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
