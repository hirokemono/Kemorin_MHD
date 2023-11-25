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

/*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture;
/* Render pass descriptor to draw to the texture */
    MTLRenderPassDescriptor* _textureRenderPassDescriptor;

    // The render pipeline generated from the vertex and fragment shaders in the .metal shader file.
    id<MTLRenderPipelineState> _pipelineState[40];
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    id<MTLFunction> _vertexFunction[7];
    id<MTLFunction> _fragmentFunction[7];

    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;
    id<MTLDepthStencilState> _noDepthState;

    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices[61];
    // The Metal texture object
    id<MTLTexture> _texture[30];
/*  Index buffer for initial cube */
    id<MTLBuffer> _index_buffer;

    id<MTLRenderCommandEncoder> _renderEncoder;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    
    KemoViewUnites _monoViewUnites;
    KemoViewUnites _rightViewUnites;
    KemoViewUnites _leftViewUnites;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;

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

// Indicate that each pixel in the depth buffer is a 32-bit floating point value.
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

         // Load all the shader files with a .metal file extension in the project.
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
        _vertexFunction[0] =   [defaultLibrary newFunctionWithName:@"Base2dVertexShader"];
        _fragmentFunction[0] = [defaultLibrary newFunctionWithName:@"Base2DfragmentShader"];

        _vertexFunction[1] =   [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
        _fragmentFunction[1] = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];

        _vertexFunction[2] =   [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
        _fragmentFunction[2] = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

        _vertexFunction[3] =   [defaultLibrary newFunctionWithName:@"PhongVertexShader"];
        _fragmentFunction[3] = [defaultLibrary newFunctionWithName:@"PhongFragmentShader"];

        _vertexFunction[4] =   [defaultLibrary newFunctionWithName:@"SimpleVertexShader"];
        _fragmentFunction[4] = [defaultLibrary newFunctionWithName:@"SimpleFragmentShader"];

        _vertexFunction[5] =   [defaultLibrary newFunctionWithName:@"PhongTexureVertexShader"];
        _fragmentFunction[5] = [defaultLibrary newFunctionWithName:@"PhongTextureFragmentShader"];

        _vertexFunction[6] =   [defaultLibrary newFunctionWithName:@"SimpleTexureVertexShader"];
        _fragmentFunction[6] = [defaultLibrary newFunctionWithName:@"SimpleTextureFragmentShader"];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        MTLRenderPipelineDescriptor *pipelineStateDescriptor;
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"2D Texture Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[2];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[2];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
        _pipelineState[2] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
/* Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
//  If the Metal API validation is enabled, you can find out more information about what
//  went wrong.  (Metal API validation is enabled by default when a debug build is run
//  from Xcode.) */
        NSAssert(_pipelineState[2], @"Failed to create pipeline state: %@", error);

/*  Create pipeline for simple 2D rendering */
        pipelineStateDescriptor.label = @"2D transpearent Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
        _pipelineState[1] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[1], @"Failed to create pipeline state: %@", error);

        /*  Create pipeline for simple rendering */
        pipelineStateDescriptor.label = @"2D Simple Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = NO;
        
        _pipelineState[3] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[3], @"Failed to create pipeline state: %@", error);


/*  Create pipeline for Basic rendering */
        pipelineStateDescriptor.label = @"Base Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[0];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[0];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[0] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[0], @"Failed to create pipeline state: %@", error);

        /* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor.label = @"Simple Shader Pipeline";
        pipelineStateDescriptor.vertexFunction =   _vertexFunction[4];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[4];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[5] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[5], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Phong Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[3];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[3];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
            
        _pipelineState[4] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[4], @"Failed to create pipeline state: %@", error);

        _pipelineState[15] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[15], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Texure Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[6];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[6];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
            
        _pipelineState[6] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[6], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Texure Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[6];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[6];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
        _pipelineState[7] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                       error:&error];
        NSAssert(_pipelineState[7], @"Failed to create pipeline state: %@", error);

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
        
        [*renderEncoder setRenderPipelineState:_pipelineState[5]];
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

- (void)drawTexureWithSimple:(struct gl_strided_buffer *) buf
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

        [*renderEncoder setRenderPipelineState:_pipelineState[6]];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];

        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawCubeWithPhong:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
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

        [*renderEncoder setRenderPipelineState:_pipelineState[4]];
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


-(void) initImageOutputTextures
{
/* Initialize texture for screen output */
    MTLTextureDescriptor *texureDecriptor = [MTLTextureDescriptor new];
    texureDecriptor.textureType = MTLTextureType2D;
    texureDecriptor.width =  _viewportSize.x;
    texureDecriptor.height = _viewportSize.y;
    texureDecriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
    texureDecriptor.usage = MTLTextureUsageRenderTarget;

    _imageOutputTexture = [_device newTextureWithDescriptor:texureDecriptor];

/*Setup render pass*/
    _textureRenderPassDescriptor = [MTLRenderPassDescriptor new];
    _textureRenderPassDescriptor.colorAttachments[0].texture = _imageOutputTexture;
    _textureRenderPassDescriptor.colorAttachments[0].loadAction = MTLLoadActionClear;
    _textureRenderPassDescriptor.colorAttachments[0].clearColor = MTLClearColorMake(1, 1, 1, 1);
    _textureRenderPassDescriptor.colorAttachments[0].storeAction = MTLStoreActionStore;

    [_textureRenderPassDescriptor release];
    [_imageOutputTexture release];


    NSError *error;
    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

    pipelineStateDescriptor.label = @"Offscreen Phong Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   _vertexFunction[3];
    pipelineStateDescriptor.fragmentFunction = _fragmentFunction[3];
    pipelineStateDescriptor.depthAttachmentPixelFormat =      _imageOutputTexture.depth;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = _imageOutputTexture.pixelFormat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
    _pipelineState[14] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                       error:&error];
    NSAssert(_pipelineState[14], @"Failed to create pipeline state: %@", error);
    return;
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

        [*renderEncoder setRenderPipelineState:_pipelineState[4]];
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

        [*renderEncoder setRenderPipelineState:_pipelineState[5]];
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
                                   atIndex:AAPLTextureImageIndex];

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
        
        [*renderEncoder setRenderPipelineState:_pipelineState[5]];
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
        [*renderEncoder setRenderPipelineState:_pipelineState[1]];
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
        [*renderEncoder setRenderPipelineState:_pipelineState[3]];
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
        [*renderEncoder setRenderPipelineState:_pipelineState[2]];
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
                image:(struct psf_menu_val *) img
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
        lineTextureDescriptor.width =  img->texture_width;
        lineTextureDescriptor.height = img->texture_height;

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
                      withBytes:img->texture_rgba
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
        if(kemo_buffers->MAP_solid_buf->num_nod_buf > 0)   {[_vertices[10] release];};
        if(kemo_buffers->MAP_isoline_buf->num_nod_buf > 0) {[_vertices[11] release];};
        if(kemo_buffers->coast_buf->num_nod_buf > 0)       {[_vertices[12] release];};
        if(kemo_buffers->sph_grid_buf->num_nod_buf > 0)    {[_vertices[13] release];};
    }else{
        if(kemo_buffers->PSF_stxur_buf->num_nod_buf > 0){
            [_vertices[43] release];
            [_texture[11]  release];
        };
        
        if(kemo_buffers->axis_buf->num_nod_buf > 0)        {[_vertices[43] release];};
        if(kemo_buffers->PSF_solid_buf->num_nod_buf > 0)   {[_vertices[35] release];};
        if(kemo_buffers->PSF_isoline_buf->num_nod_buf > 0) {[_vertices[31] release];};
        if(kemo_buffers->PSF_arrow_buf->num_nod_buf > 0)   {[_vertices[32] release];};
       
        if(kemo_buffers->FLINE_tube_buf->num_nod_buf > 0) {[_vertices[55] release];};
        if(kemo_buffers->FLINE_line_buf->num_nod_buf > 0) {[_vertices[56] release];};
        
        if(kemo_buffers->mesh_node_buf->num_nod_buf > 0) {[_vertices[51] release];};
        if(kemo_buffers->mesh_grid_buf->num_nod_buf > 0) {[_vertices[52] release];};
        if(kemo_buffers->mesh_solid_buf->num_nod_buf > 0) {[_vertices[53] release];};
        
        /*  Set Cube Vertex buffer */
        if(kemo_buffers->cube_buf->num_nod_buf > 0){
            [_vertices[30] release];
            [_index_buffer release];
        };
        
        /*  Set transparent vertexs */
        if(kemo_buffers->PSF_ttxur_buf->num_nod_buf > 0){
            [_vertices[38] release];
            [_texture[12]  release];
        };
        
        if(kemo_buffers->PSF_trns_buf->num_nod_buf > 0) {[_vertices[37] release];};
        if(kemo_buffers->mesh_trns_buf->num_nod_buf > 0) {[_vertices[54] release];};
    };
    
    if(kemo_buffers->cbar_buf->num_nod_buf > 0) {[_vertices[3] release];};
    if(kemo_buffers->min_buf->num_nod_buf > 0){
        [_vertices[4] release];
        [_texture[4]  release];
    };
    if(kemo_buffers->max_buf->num_nod_buf > 0){
        [_vertices[5] release];
        [_texture[5]  release];
    };
    if(kemo_buffers->zero_buf->num_nod_buf > 0){
        [_vertices[6] release];
        [_texture[6]  release];
    };
    if(kemo_buffers->time_buf->num_nod_buf > 0){
        [_vertices[2] release];
        [_texture[2]  release];
    };
    if(kemo_buffers->msg_buf->num_nod_buf > 0){
        [_vertices[1] release];
        [_texture[1]  release];
    };
    return;
}

- (void) setKemoViewMetalBuffers:(struct kemoview_buffers *) kemo_buffers
                           views:(struct view_element *) view_s
                       fieldline:(struct kemoview_fline *) kemo_fline
{
    if(view_s->iflag_view_type == VIEW_MAP){
        /*  Commands to render map projection */
        [self setMetalVertexs:kemo_buffers->MAP_solid_buf
                       vertex:&_vertices[10]];
        [self setMetalVertexs:kemo_buffers->MAP_isoline_buf
                       vertex:&_vertices[11]];
        [self setMetalVertexs:kemo_buffers->coast_buf
                       vertex:&_vertices[12]];
        [self setMetalVertexs:kemo_buffers->sph_grid_buf
                       vertex:&_vertices[13]];
    }else{
        [self setPSFTexture:kemo_buffers->PSF_stxur_buf
                      image:kemo_buffers->psf_stexure
                     vertex:&_vertices[36]
                     texure:&_texture[11]];
        
        [self setMetalVertexs:kemo_buffers->coast_buf
                       vertex:&_vertices[41]];
        [self setMetalVertexs:kemo_buffers->sph_grid_buf
                       vertex:&_vertices[42]];
        [self setMetalVertexs:kemo_buffers->axis_buf
                       vertex:&_vertices[43]];

        [self setMetalVertexs:kemo_buffers->PSF_solid_buf
                       vertex:&_vertices[35]];
        [self setMetalVertexs:kemo_buffers->PSF_isoline_buf
                       vertex:&_vertices[31]];
        [self setMetalVertexs:kemo_buffers->PSF_arrow_buf
                       vertex:&_vertices[32]];
        
        if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
            [self setMetalVertexs:kemo_buffers->FLINE_tube_buf
                           vertex:&_vertices[55]];
        };
        [self setMetalVertexs:kemo_buffers->FLINE_line_buf
                       vertex:&_vertices[56]];
        
        [self setMetalVertexs:kemo_buffers->mesh_node_buf
                       vertex:&_vertices[51]];
        [self setMetalVertexs:kemo_buffers->mesh_grid_buf
                       vertex:&_vertices[52]];
        [self setMetalVertexs:kemo_buffers->mesh_solid_buf
                       vertex:&_vertices[53]];
        
/*  Set Cube Vertex buffer */
        [self setCubeVertexs:kemo_buffers->cube_buf
                    indexbuf:kemo_buffers->cube_index_buf
                      vertex:&_vertices[30]
                       index:&_index_buffer];
        
/*  Set transparent vertexs */
        [self setPSFTexture:kemo_buffers->PSF_ttxur_buf
                      image:kemo_buffers->psf_ttexure
                     vertex:&_vertices[38]
                     texure:&_texture[12]];
        [self setMetalVertexs:kemo_buffers->PSF_trns_buf
                       vertex:&_vertices[37]];
        [self setMetalVertexs:kemo_buffers->mesh_trns_buf
                       vertex:&_vertices[54]];
    };
    
    [self setMetalVertexs:kemo_buffers->cbar_buf
                   vertex:&_vertices[3]];
    [self setTextBoxTexture:kemo_buffers->min_buf
                      image:kemo_buffers->cbar_min_image
                     vertex:&_vertices[4]
                     texure:&_texture[4]];
    [self setTextBoxTexture:kemo_buffers->max_buf
                      image:kemo_buffers->cbar_max_image
                     vertex:&_vertices[5]
                     texure:&_texture[5]];
    [self setTextBoxTexture:kemo_buffers->zero_buf
                      image:kemo_buffers->cbar_zero_image
                     vertex:&_vertices[6]
                     texure:&_texture[6]];
    [self setTextBoxTexture:kemo_buffers->time_buf
                      image:kemo_buffers->tlabel_image
                     vertex:&_vertices[2]
                     texure:&_texture[2]];
    [self setTextBoxTexture:kemo_buffers->msg_buf
                      image:kemo_buffers->message_image
                     vertex:&_vertices[1]
                     texure:&_texture[1]];
    return;
}


- (void) encodeMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   buffer:(struct kemoview_buffers *) kemo_buffers
               projection:(matrix_float4x4 *) map_proj_mat
{
/*  Commands to render map projection */
    [self drawMapSolidObjext:kemo_buffers->MAP_solid_buf
                     encoder:renderEncoder
                      vertex:&_vertices[10]
                  projection:map_proj_mat];
/*  Commands to render isolines on map */
    [self drawMapSolidObjext:kemo_buffers->MAP_isoline_buf
                     encoder:renderEncoder
                      vertex:&_vertices[11]
                  projection:map_proj_mat];
/*  Commands to render Coastline on map */
    [self drawMapLineObjext:kemo_buffers->coast_buf
                    encoder:renderEncoder
                     vertex:&_vertices[12]
                 projection:map_proj_mat];
/*  Commands to render grids on map */
    [self drawMapLineObjext:kemo_buffers->sph_grid_buf
                    encoder:renderEncoder
                     vertex:&_vertices[13]
                 projection:map_proj_mat];
    return;
}

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                      buffer:(struct kemoview_buffers *) kemo_buffers
                        mesh:(struct kemoview_mesh *) kemo_mesh
                      unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithSimple:kemo_buffers->PSF_stxur_buf
                       encoder:renderEncoder
                        vertex:&_vertices[36]
                        texure:&_texture[11]
                        unites:monoViewUnites
                         sides:BOTH_SURFACES
                         solid:SMOOTH_SHADE];
    /*
     [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
     encoder:renderEncoder
     vertex:&_vertices[36]
     texure:&_texture[11]
     unites:monoViewUnites
     sides:BOTH_SURFACES
     solid:SMOOTH_SHADE];
     */
    [self drawSolidWithSimple:kemo_buffers->axis_buf
                      encoder:renderEncoder
                       vertex:&_vertices[43]
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                      vertex:&_vertices[35]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
                  vertex:&_vertices[56]
                  unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
                  vertex:&_vertices[52]
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                      vertex:&_vertices[53]
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                     vertex:&_vertices[30]
                      index:&_index_buffer
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
                  vertex:&_vertices[41]
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
                  vertex:&_vertices[42]
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                       vertex:&_vertices[38]
                       texure:&_texture[12]
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                      vertex:&_vertices[37]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                      vertex:&_vertices[54]
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
    [self drawTexureWithSimple:kemo_buffers->PSF_stxur_buf
                       encoder:renderEncoder
                        vertex:&_vertices[36]
                        texure:&_texture[11]
                        unites:monoViewUnites
                         sides:BOTH_SURFACES
                         solid:SMOOTH_SHADE];
    /*
     [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
     encoder:renderEncoder
     vertex:&_vertices[36]
     texure:&_texture[11]
     unites:monoViewUnites
     sides:BOTH_SURFACES
     solid:SMOOTH_SHADE];
     */
    [self drawSolidWithSimple:kemo_buffers->axis_buf
                      encoder:renderEncoder
                       vertex:&_vertices[43]
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                      vertex:&_vertices[35]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_isoline_buf
                     encoder:renderEncoder
                      vertex:&_vertices[31]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_arrow_buf
                     encoder:renderEncoder
                      vertex:&_vertices[32]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
        [self drawSolidWithPhong:kemo_buffers->FLINE_tube_buf
                         encoder:renderEncoder
                          vertex:&_vertices[55]
                          unites:monoViewUnites
                           sides:BOTH_SURFACES
                           solid:SMOOTH_SHADE];
    };
       
    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
                  vertex:&_vertices[56]
                  unites:monoViewUnites];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_node_buf
                     encoder:renderEncoder
                      vertex:&_vertices[51]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
                  vertex:&_vertices[52]
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                      vertex:&_vertices[53]
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                     vertex:&_vertices[30]
                      index:&_index_buffer
                     unites:monoViewUnites];

    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
                  vertex:&_vertices[41]
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
                  vertex:&_vertices[42]
                  unites:monoViewUnites];

/*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                       vertex:&_vertices[38]
                       texure:&_texture[12]
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                      vertex:&_vertices[37]
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                      vertex:&_vertices[54]
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
                      vertex:&_vertices[3]
                  projection:projection_mat];
/*  Commands to render colorbar  label */
    [self drawTextBoxObjext:kemo_buffers->min_buf
                    encoder:renderEncoder
                     vertex:&_vertices[4]
                     texure:&_texture[4]
                 projection:projection_mat];
    [self drawTextBoxObjext:kemo_buffers->max_buf
                    encoder:renderEncoder
                     vertex:&_vertices[5]
                     texure:&_texture[5]
                 projection:projection_mat];
    [self drawTextBoxObjext:kemo_buffers->zero_buf
                    encoder:renderEncoder
                     vertex:&_vertices[6]
                     texure:&_texture[6]
                 projection:projection_mat];

/*  Commands to render time label */
    [self drawTextBoxObjext:kemo_buffers->time_buf
                    encoder:renderEncoder
                     vertex:&_vertices[2]
                     texure:&_texture[2]
                 projection:projection_mat];
/*  Commands to render colorbar  box */
    [self drawTextBoxObjext:kemo_buffers->msg_buf
                    encoder:renderEncoder
                     vertex:&_vertices[1]
                     texure:&_texture[1]
                 projection:projection_mat];
    return;
}

- (void) encodeKemoViewers:(id<MTLRenderCommandEncoder>  *) renderEncoder
                    buffer:(struct kemoview_buffers *) kemo_buffers
                     views:(struct view_element *) view_s
                 fieldline:(struct kemoview_fline *) kemo_fline
                      mesh:(struct kemoview_mesh *) kemo_mesh
                    unites:(KemoViewUnites *) monoViewUnites
                      left:(KemoViewUnites *) leftViewUnites
                     right:(KemoViewUnites *) rightViewUnites
{
    if(view_s->iflag_view_type == VIEW_MAP){
        [self encodeMapObjects:&_renderEncoder
                        buffer:kemo_buffers
                    projection:&_map_proj_mat];
    } else if(view_s->iflag_view_type == VIEW_STEREO){
        [self encode3DObjects:renderEncoder
                       buffer:kemo_buffers
                    fieldline:kemo_fline
                         mesh:kemo_mesh
                       unites:leftViewUnites];
        [self encode3DObjects:renderEncoder
                       buffer:kemo_buffers
                    fieldline:kemo_fline
                         mesh:kemo_mesh
                       unites:rightViewUnites];
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

- (void)drawKemoMetalView:(nonnull MTKView *)view
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();

    if(kemoview_get_draw_mode() == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:kemo_sgl->kemo_buffers
                                    views:kemo_sgl->view_s];

        kemoview_const_buffers(kemo_sgl);

        [self setKemoViewMetalBuffers:kemo_sgl->kemo_buffers
                                views:kemo_sgl->view_s
                            fieldline:kemo_sgl->kemo_fline];
    };

    [self setKemoViewLightings:kemo_sgl->kemo_buffers->cube_buf
                        unites:&_monoViewUnites];

    [self setKemoViewLightings:kemo_sgl->kemo_buffers->cube_buf
                        unites:&_leftViewUnites];
    [self leftMaterialParams:&(_leftViewUnites.material)];

    [self setKemoViewLightings:kemo_sgl->kemo_buffers->cube_buf
                        unites:&_rightViewUnites];
    [self rightMaterialParams:&(_rightViewUnites.material)];

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
                               left:&_leftViewUnites
                              right:&_rightViewUnites];
        };

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer  commit];
    return;
}




/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    [self setTransferMatrices:&_monoViewUnites];
/*
    update_left_projection_struct(kemo_sgl->view_s);
    modify_left_view_by_struct(kemo_sgl->view_s);
    [self setTransferMatrices:&_leftViewUnites];

    update_right_projection_struct(kemo_sgl->view_s);
    modify_right_view_by_struct(kemo_sgl->view_s);
    [self setTransferMatrices:&_rightViewUnites];
  */
    [self drawKemoMetalView:view];
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
