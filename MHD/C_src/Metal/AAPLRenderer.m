/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
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
    id<MTLDepthStencilState> _depthState2;
    id<MTLDepthStencilState> _noDepthState;

    KemoViewMetalBuffers     _kemoViewMetalBuf;

    KemoViewMetalShaders   _kemoViewShaders;
    KemoView3DPipelines    _kemoViewPipelines;
    KemoView3DPipelines    _kemoAnaglyphPipelines;

    /* Texture to render to and then sample from. */
    id<MTLTexture> _renderLeftTexture;
    id<MTLTexture> _renderRightTexture;


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

-(void) loadShaderLibrary: (id<MTLLibrary> *) defaultLibrary
{
    // Load all the shader files with a .metal file extension in the project.
   _kemoViewShaders.phongVertexFunction =   [*defaultLibrary newFunctionWithName:@"PhongVertexShader"];
   _kemoViewShaders.phongFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongFragmentShader"];

   _kemoViewShaders.simpleVertexFunction =   [*defaultLibrary newFunctionWithName:@"SimpleVertexShader"];
   _kemoViewShaders.simpleFragmentFunction = [*defaultLibrary newFunctionWithName:@"SimpleFragmentShader"];

   _kemoViewShaders.texuredPhongVertexFunction = [*defaultLibrary newFunctionWithName:@"PhongTexureVertexShader"];
   _kemoViewShaders.texuredPhongFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongTextureFragmentShader"];

   _kemoViewShaders.PhongAnaglyphVertexFunction = [*defaultLibrary newFunctionWithName:@"PhongAnagriphVertexShader"];
   _kemoViewShaders.PhongAnaglyphFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongAnagriphFragmentShader"];

   _kemoViewShaders.texuredVertexFunction =   [*defaultLibrary newFunctionWithName:@"SimpleTexureVertexShader"];
   _kemoViewShaders.texuredFragmentFunction = [*defaultLibrary newFunctionWithName:@"SimpleTextureFragmentShader"];
    return;
}

-(void) addKemoView3DPipelines:(nonnull MTKView *)mtkView
                       shaders:(KemoViewMetalShaders *) kemoViewShaders
                     pipelines:(KemoView3DPipelines *) kemo3DPipelines
                   targetPixel:(MTLPixelFormat) pixelformat
{
    NSError *error;
    id<MTLDevice> device = mtkView.device;

    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
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
        
    kemo3DPipelines->phongPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemo3DPipelines->phongPipelineState, @"Failed to create pipeline state: %@", error);


/* Configure a pipeline descriptor that is used to create a pipeline state. */
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
        
    kemo3DPipelines->phongTexturedPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                          error:&error];
    NSAssert(kemo3DPipelines->phongTexturedPipelineState, @"Failed to create pipeline state: %@", error);
    
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Simple Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simpleVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simpleFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    kemo3DPipelines->simplePipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemo3DPipelines->simplePipelineState, @"Failed to create pipeline state: %@", error);

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
        
    kemo3DPipelines->texuredPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                        error:&error];
    NSAssert(kemo3DPipelines->texuredPipelineState, @"Failed to create pipeline state: %@", error);

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
        
    kemo3DPipelines->phongAnaglyphPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemo3DPipelines->phongAnaglyphPipelineState, @"Failed to create pipeline state: %@", error);
}

-(void) addKemoViewAnaglyphPipelines:(nonnull MTKView *)mtkView
                             shaders:(KemoViewMetalShaders *) kemoViewShaders
                           pipelines:(KemoView3DPipelines *) kemoAnaglyphPipelines
                         targetPixel:(MTLPixelFormat) pixelformat
{
    NSError *error;
    id<MTLDevice> device = mtkView.device;

    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
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
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOne;
        
    kemoAnaglyphPipelines->phongPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemoAnaglyphPipelines->phongPipelineState, @"Failed to create pipeline state: %@", error);


/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredPhongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredPhongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOne;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOne;
        
    kemoAnaglyphPipelines->phongTexturedPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                          error:&error];
    NSAssert(kemoAnaglyphPipelines->phongTexturedPipelineState, @"Failed to create pipeline state: %@", error);
    
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Simple Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simpleVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simpleFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    kemoAnaglyphPipelines->simplePipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemoAnaglyphPipelines->simplePipelineState, @"Failed to create pipeline state: %@", error);
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _kemoRendererTools = [[KemoViewRendererTools alloc] init];
    _kemo2DRenderer = [[KemoView2DRenderer alloc] init];
    _frameNum = 0;
    _icou_lr = 0;
    self = [super init];
    if(self)
    {
        _device = mtkView.device;

// Indicate that each pixel in the depth buffer is a 32-bit floating point value.
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

/* Load all the shader files with a .metal file extension in the project. */
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
        [self loadShaderLibrary:&defaultLibrary];
        [_kemo2DRenderer add2DShaderLibrary:&defaultLibrary];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        [_kemo2DRenderer addKemoView2DPipelines:mtkView
                         targetPixel:mtkView.colorPixelFormat];
        [self addKemoView3DPipelines:mtkView
                            shaders:&_kemoViewShaders
                          pipelines:&_kemoViewPipelines
                        targetPixel:mtkView.colorPixelFormat];
        [self addKemoViewAnaglyphPipelines:mtkView
                                   shaders:&_kemoViewShaders
                                 pipelines:&_kemoAnaglyphPipelines
                               targetPixel:mtkView.colorPixelFormat];

/* Add Depth buffer description in command */
        MTLDepthStencilDescriptor *depthDescriptor;
        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState =  [_device newDepthStencilStateWithDescriptor:depthDescriptor];

        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState2 = [_device newDepthStencilStateWithDescriptor:depthDescriptor];

        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = NO;
        _noDepthState = [_device newDepthStencilStateWithDescriptor:depthDescriptor];
    }
/* Create the command queue */
    _commandQueue = [_device newCommandQueue];
    return self;
}


- (void)drawSolidWithSimple:(struct gl_strided_buffer *) buf
                    encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
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
            [*renderEncoder setDepthStencilState:*depthState];
        }
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->simplePipelineState];
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
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
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
            [*renderEncoder setDepthStencilState:*depthState];
        }

        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongTexturedPipelineState];
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
                pipelines:(KemoView3DPipelines *) kemo3DPipelines
                    depth:(id<MTLDepthStencilState> *) depthState
                   vertex:(id<MTLBuffer> *) vertices
                    index:(id<MTLBuffer> *) indices
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:*depthState];

        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
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
                 pipelines:(KemoView3DPipelines *) kemo3DPipelines
                     depth:(id<MTLDepthStencilState> *) depthState
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
            [*renderEncoder setDepthStencilState:*depthState];
        }

        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
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
             pipelines:(KemoView3DPipelines *) kemo3DPipelines
                 depth:(id<MTLDepthStencilState> *) depthState
                vertex:(id<MTLBuffer> *)  vertices
                unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:*depthState];

        [*renderEncoder setRenderPipelineState:kemo3DPipelines->simplePipelineState];
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



- (void) releaseTransparentMetalBuffers:(struct kemoview_buffers *) kemo_buffers
{
    /*  Release transparent vertexs */
    if(kemo_buffers->PSF_ttxur_buf->num_nod_buf > 0){
        [_kemoViewMetalBuf.psfTTexureVertice release];
        [_kemoViewMetalBuf.psfTransTexure release];
    };
    
    if(kemo_buffers->PSF_trns_buf->num_nod_buf > 0)  {[_kemoViewMetalBuf.psfTransVertice release];};
    if(kemo_buffers->mesh_trns_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.meshTransVertice release];};
    return;
}

- (void) releaseKemoViewMetalBuffers:(struct kemoview_buffers *) kemo_buffers
{
    if(kemoview_get_view_type_flag() == VIEW_MAP){
        [_kemo2DRenderer releaseMapMetalBuffers:kemo_buffers];
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
        
        if(kemo_buffers->coast_buf->num_nod_buf > 0)    {[_kemoViewMetalBuf.coastVertice   release];};
        if(kemo_buffers->sph_grid_buf->num_nod_buf > 0) {[_kemoViewMetalBuf.sphGridVertice release];};

        /*  Set Cube Vertex buffer */
        if(kemo_buffers->cube_buf->num_nod_buf > 0){
            [_kemoViewMetalBuf.cubeVertice release];
            [_kemoViewMetalBuf.cubeIndex   release];
        };
        
        /*  Release transparent vertexs */
        [self releaseTransparentMetalBuffers:kemo_buffers];
    };
    
    [_kemo2DRenderer releaseMsgMetalBuffers:kemo_buffers];
    return;
}

- (void) setTransparentMetalBuffers:(struct kemoview_buffers *) kemo_buffers
                               PSFs:(struct kemoview_psf *) kemo_psf
{
/*  Set transparent vertexs */
    [self setPSFTexture:kemo_buffers->PSF_ttxur_buf
                  image:kemo_psf->psf_a->psf_texure
                 vertex:&_kemoViewMetalBuf.psfTTexureVertice
                 texure:&_kemoViewMetalBuf.psfTransTexure];
    [self setMetalVertexs:kemo_buffers->PSF_trns_buf
                   vertex:&_kemoViewMetalBuf.psfTransVertice];
    [self setMetalVertexs:kemo_buffers->mesh_trns_buf
                   vertex:&_kemoViewMetalBuf.meshTransVertice];
    return;
}

- (void) setKemoViewMetalBuffers:(struct kemoview_buffers *) kemo_buffers
                            PSFs:(struct kemoview_psf *) kemo_psf
                       fieldline:(struct kemoview_fline *) kemo_fline
{
    if(kemoview_get_view_type_flag() == VIEW_MAP){
        [_kemo2DRenderer setMapMetalBuffers:&_device
                                    buffers:kemo_buffers];
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
        [self setTransparentMetalBuffers:kemo_buffers
                                    PSFs:kemo_psf];
    };
    
    [_kemo2DRenderer setMessageMetalBuffers:&_device
                                    buffers:kemo_buffers];
    return;
}

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   pipelines:(KemoView3DPipelines *) kemo3DPipelines
                       depth:(id<MTLDepthStencilState> *) depthState
                      buffer:(struct kemoview_buffers *) kemo_buffers
                        mesh:(struct kemoview_mesh *) kemo_mesh
                      unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&_kemoViewMetalBuf.psfSTexureVertice
                       texure:&_kemoViewMetalBuf.psfSolidTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->axis_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.axisVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfSolidVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.fieldLineVertice
                  unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.meshGridVertice
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.meshSolidVertice
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                     vertex:&_kemoViewMetalBuf.cubeVertice
                      index:&_kemoViewMetalBuf.cubeIndex
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.coastVertice
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.sphGridVertice
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&_kemoViewMetalBuf.psfTTexureVertice
                       texure:&_kemoViewMetalBuf.psfTransTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.meshTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    return;
}

- (void) encode3DObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
               pipelines:(KemoView3DPipelines *) kemo3DPipelines
                   depth:(id<MTLDepthStencilState> *) depthState
                  buffer:(struct kemoview_buffers *) kemo_buffers
               fieldline:(struct kemoview_fline *) kemo_fline
                    mesh:(struct kemoview_mesh *) kemo_mesh
                  unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&_kemoViewMetalBuf.psfSTexureVertice
                       texure:&_kemoViewMetalBuf.psfSolidTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->axis_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.axisVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfSolidVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_isoline_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfLinesVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_arrow_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfArrowVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
        [self drawSolidWithPhong:kemo_buffers->FLINE_tube_buf
                         encoder:renderEncoder
                       pipelines:kemo3DPipelines
                           depth:depthState
                          vertex:&_kemoViewMetalBuf.fieldTubeVertice
                          unites:monoViewUnites
                           sides:BOTH_SURFACES
                           solid:SMOOTH_SHADE];
    };
       
    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.fieldLineVertice
                  unites:monoViewUnites];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_node_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.meshNodeVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.meshGridVertice
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.meshSolidVertice
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                     vertex:&_kemoViewMetalBuf.cubeVertice
                      index:&_kemoViewMetalBuf.cubeIndex
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.coastVertice
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&_kemoViewMetalBuf.sphGridVertice
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&_kemoViewMetalBuf.psfTTexureVertice
                       texure:&_kemoViewMetalBuf.psfTransTexure
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.psfTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&_kemoViewMetalBuf.meshTransVertice
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    return;
}

- (void) encodeKemoViewers:(id<MTLRenderCommandEncoder>  *) renderEncoder
                    buffer:(struct kemoview_buffers *) kemo_buffers
                     views:(struct view_element *) view_s
                 fieldline:(struct kemoview_fline *) kemo_fline
                      mesh:(struct kemoview_mesh *) kemo_mesh
                    unites:(KemoViewUnites *) monoViewUnites
                   eyeflag:(int) iflag_lr
{
    int iflag_view = kemoview_get_view_type_flag();
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_buffers
                               projection:&_map_proj_mat];
    } else if(iflag_view == VIEW_STEREO){
        [_kemoRendererTools set2dProjectionMatrices:&_cbar_proj_mat
                                      MapProjection:&_map_proj_mat];
        if(iflag_lr < 0){
            update_left_projection_struct(view_s);
            modify_left_view_by_struct(view_s);
            [_kemoRendererTools setTransferMatrices:&_leftViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_buffers->cube_buf
                                              unites:&_leftViewUnites];
            [self encode3DObjects:renderEncoder
                        pipelines:&_kemoViewPipelines
                            depth:&_depthState
                           buffer:kemo_buffers
                        fieldline:kemo_fline
                             mesh:kemo_mesh
                           unites:&_leftViewUnites];
        }else if(iflag_lr > 0){
            update_right_projection_struct(view_s);
            modify_right_view_by_struct(view_s);
            [_kemoRendererTools setTransferMatrices:&_rightViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_buffers->cube_buf
                                              unites:&_rightViewUnites];
            [self encode3DObjects:renderEncoder
                        pipelines:&_kemoViewPipelines
                            depth:&_depthState
                           buffer:kemo_buffers
                        fieldline:kemo_fline
                             mesh:kemo_mesh
                           unites:&_rightViewUnites];
        }else{
            update_left_projection_struct(view_s);
            modify_left_view_by_struct(view_s);
            [_kemoRendererTools setTransferMatrices:&_leftViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_buffers->cube_buf
                                              unites:&_leftViewUnites];
            [_kemoRendererTools leftMaterialParams:&_leftViewUnites.material];
            [self encode3DObjects:renderEncoder
                        pipelines:&_kemoViewPipelines
                            depth:&_depthState
                           buffer:kemo_buffers
                        fieldline:kemo_fline
                             mesh:kemo_mesh
                           unites:&_leftViewUnites];

            [_kemoRendererTools setTransferMatrices:&_rightViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_buffers->cube_buf
                                              unites:&_rightViewUnites];
            [_kemoRendererTools rightMaterialParams:&_rightViewUnites.material];
            [self encode3DObjects:renderEncoder
                        pipelines:&_kemoViewPipelines
                            depth:&_depthState2
                           buffer:kemo_buffers
                        fieldline:kemo_fline
                             mesh:kemo_mesh
                           unites:&_rightViewUnites];
        }
    } else {
        [self encode3DObjects:renderEncoder
                    pipelines:&_kemoViewPipelines
                        depth:&_depthState
                       buffer:kemo_buffers
                    fieldline:kemo_fline
                         mesh:kemo_mesh
                       unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_buffers
                               projection:&_cbar_proj_mat];
    return;
}

- (void) encodeSimpleView:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   buffer:(struct kemoview_buffers *) kemo_buffers
                     mesh:(struct kemoview_mesh *) kemo_mesh
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(kemoview_get_view_type_flag() == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_buffers
                               projection:&_map_proj_mat];
    }else{
        [self encodeSimpleObjects:renderEncoder
                        pipelines:&_kemoViewPipelines
                            depth:&_depthState
                           buffer:kemo_buffers
                             mesh:kemo_mesh
                           unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_buffers
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
                             unites:&_monoViewUnites
                            eyeflag:iflag_lr];
        };

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer  commit];
    [commandBuffer waitUntilCompleted];
    return;
}


- (void)drawKemoMetalView:(nonnull MTKView *)view
                  eyeflag:(int) iflag_lr
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    if(kemoview_get_draw_mode() == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:kemo_sgl->kemo_buffers];

        kemoview_const_buffers(kemo_sgl);

        [self setKemoViewMetalBuffers:kemo_sgl->kemo_buffers
                                 PSFs:kemo_sgl->kemo_psf
                            fieldline:kemo_sgl->kemo_fline];
    }else if(kemoview_get_draw_mode() == FAST_DRAW){
        if(kemoview_get_view_type_flag() != VIEW_MAP){
            [self releaseTransparentMetalBuffers:kemo_sgl->kemo_buffers];
            kemoview_transparent_buffers(kemo_sgl);
            [self setTransparentMetalBuffers:kemo_sgl->kemo_buffers
                                        PSFs:kemo_sgl->kemo_psf];
        };
    };

    [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers->cube_buf
                                      unites:&_monoViewUnites];

    [_kemoRendererTools set2dProjectionMatrices:&_cbar_proj_mat
                                  MapProjection:&_map_proj_mat];
    [_kemoRendererTools setTransferMatrices:&_monoViewUnites];

    [self takoview:view eyeflag:iflag_lr];
    return;
}

// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    [self drawKemoMetalView:view
                    eyeflag:0];
    return;
}

// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
