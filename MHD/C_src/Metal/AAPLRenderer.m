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
    id<MTLRenderPipelineState> _pipelineState[40];
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    id<MTLFunction> _vertexFunction[7];
    id<MTLFunction> _fragmentFunction[7];

    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;

    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices[61];
    // The Metal texture object
    id<MTLTexture> _texture[30];
/*  Index buffer for initial cube */
    id<MTLBuffer> _index_buffer;

    id<MTLRenderCommandEncoder> _renderEncoder;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    
    matrix_float4x4 _modelview_mat;
    matrix_float4x4 _projection_mat;
    matrix_float4x4 _normal_mat;

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

        _pipelineState[14] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[14], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Texure Phong Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[5];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[5];
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
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
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
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
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
                   lights:(LightSourceParameters *) lights
                materials:(MaterialParameters *) material
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:_depthState];

        [*renderEncoder setRenderPipelineState:_pipelineState[4]];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&_normal_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:lights
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:material
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [_renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                  indexCount:buf->num_nod_buf
                                   indexType:MTLIndexTypeUInt32
                                 indexBuffer:*indices
                           indexBufferOffset:0];
    };
}

- (void)drawSolidWithPhong:(struct gl_strided_buffer *) buf
                   encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                    vertex:(id<MTLBuffer> *) vertices
                    lights:(LightSourceParameters *) lights
                 materials:(MaterialParameters *) material
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
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&_normal_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:lights
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:material
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
                     lights:(LightSourceParameters *) lights
                  materials:(MaterialParameters *) material
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
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&_normal_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:lights
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:material
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
        [*renderEncoder setVertexBytes:&_modelview_mat
                               length:sizeof(matrix_float4x4)
                              atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
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
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState:_pipelineState[2]];
/* Pass in the parameter data. */
        [*renderEncoder setVertexBuffer:*vertices
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&_cbar_proj_mat
                               length:sizeof(_cbar_proj_mat)
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
    for(i=1;i<3;i++){
        mats->ambient[i] =  mats->ambient[0];
        mats->diffuse[i] =  mats->diffuse[0];
        mats->specular[i] = mats->specular[0];
    }
    mats->ambient[3] = 1.0;
    mats->diffuse[3] = 1.0;
    mats->specular[3] = 1.0;
    return;
};

- (void)setCubeColorbuffer:(LightSourceParameters *) lights
                  material:(MaterialParameters *) mats
{
    int i, j;
    float x, y, z;
    
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
    for(i=1;i<3;i++){
        mats->ambient[i] =  mats->ambient[0];
        mats->diffuse[i] =  mats->diffuse[0];
        mats->specular[i] = mats->specular[0];
    }
    mats->ambient[3] =  1.0;
    mats->diffuse[3] =  1.0;
    mats->specular[3] = 1.0;
    free(init_light);
    return;
};

- (void)setTransferMatrices:(struct view_element *) view_s
{
    double *orthogonal = orthogonal_projection_mat_c(0.0, view_s->nx_frame,
                                                     0.0, view_s->ny_frame,
                                                    -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(view_s);
    struct transfer_matrices *map_matrices
            = init_projection_matrix_for_map(view_s->nx_frame, view_s->ny_frame);
    free(orthogonal);

    _cbar_proj_mat =  [self setMetalViewMatrices:cbar_matrices->proj];
    _map_proj_mat =   [self setMetalViewMatrices:map_matrices->proj];

    _modelview_mat =  [self setMetalViewMatrices:view_matrices->model];
    _projection_mat = [self setMetalProjViewMatrices:view_matrices->proj];
    _normal_mat =     [self setMetalViewMatrices:view_matrices->nrmat];
    free(cbar_matrices);
    free(view_matrices);
    free(map_matrices);
    return;
};

/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    int i;
    int iflag;
    int iflag_psf = 0;
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    
/* Set transfer matrices */
    [self setTransferMatrices:kemo_sgl->view_s];
    
    struct psf_menu_val *psf_stexure;
    struct psf_menu_val *psf_ttexure;
    if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = sort_by_patch_distance_psfs(kemo_sgl->kemo_psf->psf_d,
                                                kemo_sgl->kemo_psf->psf_m,
                                                kemo_sgl->kemo_psf->psf_a,
                                                kemo_sgl->view_s);
        iflag_psf = check_draw_map(kemo_sgl->kemo_psf->psf_a);

        set_map_patch_buffer(IZERO, kemo_sgl->kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                             kemo_sgl->kemo_psf->psf_a, kemo_sgl->kemo_buffers->MAP_solid_buf);
        set_map_PSF_isolines_buffer(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                    kemo_sgl->kemo_psf->psf_a, kemo_sgl->view_s,
                                    kemo_sgl->kemo_buffers->MAP_isoline_buf);

        set_map_coastline_buffer(kemo_sgl->kemo_mesh->mesh_m,
                                 kemo_sgl->kemo_buffers->coast_buf);
        set_map_flame_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->sph_grid_buf);
    } else {
/* Set Axis data into buffer */
        double axis_radius = 4.0;
        set_axis_to_buf(kemo_sgl->view_s, kemo_sgl->kemo_mesh->mesh_m->iflag_draw_axis,
                        kemo_sgl->kemo_mesh->mesh_m->dist_domains,
                        kemo_sgl->kemo_buffers->ncorner_axis,
                        axis_radius, kemo_sgl->kemo_buffers->axis_buf);

        iflag_psf = sort_by_patch_distance_psfs(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                                kemo_sgl->kemo_psf->psf_a, kemo_sgl->view_s);
        iflag_psf = iflag_psf + check_draw_psf(kemo_sgl->kemo_psf->psf_a);

        set_color_code_for_psfs(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                kemo_sgl->kemo_psf->psf_a);
        const_PSF_solid_objects_buffer(kemo_sgl->view_s, kemo_sgl->kemo_psf->psf_d,
                                       kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                                       kemo_sgl->kemo_buffers->PSF_solid_buf,
                                       kemo_sgl->kemo_buffers->PSF_stxur_buf,
                                       kemo_sgl->kemo_buffers->PSF_isoline_buf,
                                       kemo_sgl->kemo_buffers->PSF_arrow_buf);
        int ipsf_texure = kemo_sgl->kemo_psf->psf_a->ipsf_viz_far[IZERO]-1;
        psf_stexure = kemo_sgl->kemo_psf->psf_m[ipsf_texure];

        const_PSF_trans_objects_buffer(kemo_sgl->view_s, kemo_sgl->kemo_psf->psf_d,
                                       kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                                       kemo_sgl->kemo_buffers->PSF_trns_buf,
                                       kemo_sgl->kemo_buffers->PSF_ttxur_buf);
        int ist_psf = kemo_sgl->kemo_psf->psf_a->istack_solid_psf_patch;
        ipsf_texure = kemo_sgl->kemo_psf->psf_a->ipsf_viz_far[ist_psf]-1;
        psf_ttexure = kemo_sgl->kemo_psf->psf_m[ipsf_texure];

        set_coastline_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->coast_buf);
        set_sph_flame_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->sph_grid_buf);

        const_fieldlines_buffer(kemo_sgl->kemo_fline->fline_d, kemo_sgl->kemo_fline->fline_m,
                                kemo_sgl->kemo_buffers->FLINE_tube_buf, kemo_sgl->kemo_buffers->FLINE_line_buf);
 
        const_solid_mesh_buffer(kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s,
                                kemo_sgl->kemo_buffers->mesh_solid_buf, kemo_sgl->kemo_buffers->mesh_grid_buf,
                                kemo_sgl->kemo_buffers->mesh_node_buf);
        const_trans_mesh_buffer(kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s,
                                kemo_sgl->kemo_buffers->mesh_trns_buf);

    };
    const_timelabel_buffer(kemo_sgl->view_s->iflag_retina,
                           kemo_sgl->view_s->nx_frame,
                           kemo_sgl->view_s->ny_frame,
                           kemo_sgl->kemo_mesh->mesh_m->text_color,
                           kemo_sgl->kemo_mesh->mesh_m->bg_color,
                           kemo_sgl->kemo_psf->psf_a,
                           kemo_sgl->kemo_buffers->tlabel_image,
                           kemo_sgl->kemo_buffers->time_buf);
    
    const_colorbar_buffer(kemo_sgl->view_s->iflag_retina,
                          kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame,
                          kemo_sgl->kemo_mesh->mesh_m->text_color,
                          kemo_sgl->kemo_mesh->mesh_m->bg_color,
                          kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                          kemo_sgl->kemo_buffers->cbar_buf,
                          kemo_sgl->kemo_buffers->min_buf,
                          kemo_sgl->kemo_buffers->max_buf,
                          kemo_sgl->kemo_buffers->zero_buf);

    const_message_buffer(kemo_sgl->view_s->iflag_retina,
                         kemo_sgl->view_s->nx_frame,
                         kemo_sgl->view_s->ny_frame,
                         kemo_sgl->kemo_buffers->msg_buf,
                         kemo_sgl->kemo_buffers->message_image);
    
/* draw example cube for empty data */

    LightSourceParameters lights;
    MaterialParameters    material;
    iflag = kemo_sgl->kemo_mesh->mesh_m->iflag_draw_mesh
               + iflag_psf + kemo_sgl->kemo_fline->fline_m->iflag_draw_fline;
    if(iflag == 0){
        kemo_sgl->kemo_buffers->cube_buf->num_nod_buf = kemo_sgl->kemo_buffers->cube_index_buf->nsize_buf;
        [self setCubeColorbuffer:&lights
                        material:&material];
    } else {
        kemo_sgl->kemo_buffers->cube_buf->num_nod_buf = 0;
        [self setMetalColorbuffer:&lights
                         material:&material];
    }
   _frameNum++;
    

    // Create a new command buffer for each render pass to the current drawable.
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"MyCommand";

    // Obtain a renderPassDescriptor generated from the view's drawable textures.
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil)
    {
        // Create a render command encoder.
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        
        if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP){
/*  Commands to render map projection */
            [self setMetalVertexs:kemo_sgl->kemo_buffers->MAP_solid_buf
                           vertex:&_vertices[10]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->MAP_isoline_buf
                           vertex:&_vertices[11]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->coast_buf
                           vertex:&_vertices[12]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->sph_grid_buf
                           vertex:&_vertices[13]];
        }else{
            [self setPSFTexture:kemo_sgl->kemo_buffers->PSF_stxur_buf
                          image:psf_stexure
                         vertex:&_vertices[36]
                         texure:&_texture[11]];
            
            [self setMetalVertexs:kemo_sgl->kemo_buffers->axis_buf
                           vertex:&_vertices[43]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->PSF_solid_buf
                           vertex:&_vertices[35]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->PSF_isoline_buf
                           vertex:&_vertices[31]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->PSF_arrow_buf
                           vertex:&_vertices[32]];

            if(kemo_sgl->kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
                [self setMetalVertexs:kemo_sgl->kemo_buffers->FLINE_tube_buf
                               vertex:&_vertices[55]];
            };
            [self setMetalVertexs:kemo_sgl->kemo_buffers->FLINE_line_buf
                           vertex:&_vertices[56]];

            [self setMetalVertexs:kemo_sgl->kemo_buffers->mesh_node_buf
                           vertex:&_vertices[51]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->mesh_grid_buf
                           vertex:&_vertices[52]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->mesh_solid_buf
                           vertex:&_vertices[53]];

/*  Set Cube Vertex buffer */
            [self setCubeVertexs:kemo_sgl->kemo_buffers->cube_buf
                        indexbuf:kemo_sgl->kemo_buffers->cube_index_buf
                          vertex:&_vertices[30]
                           index:&_index_buffer];

/*  Set transparent vertexs */
            [self setPSFTexture:kemo_sgl->kemo_buffers->PSF_ttxur_buf
                          image:psf_ttexure
                         vertex:&_vertices[38]
                         texure:&_texture[12]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->PSF_trns_buf
                           vertex:&_vertices[37]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->mesh_trns_buf
                           vertex:&_vertices[54]];
        };
        
        [self setMetalVertexs:kemo_sgl->kemo_buffers->cbar_buf
                       vertex:&_vertices[3]];
        [self setTextBoxTexture:kemo_sgl->kemo_buffers->min_buf
                          image:kemo_sgl->kemo_buffers->cbar_min_image
                         vertex:&_vertices[4]
                         texure:&_texture[4]];
        [self setTextBoxTexture:kemo_sgl->kemo_buffers->max_buf
                          image:kemo_sgl->kemo_buffers->cbar_max_image
                         vertex:&_vertices[5]
                         texure:&_texture[5]];
        [self setTextBoxTexture:kemo_sgl->kemo_buffers->zero_buf
                          image:kemo_sgl->kemo_buffers->cbar_zero_image
                         vertex:&_vertices[6]
                         texure:&_texture[6]];
        [self setTextBoxTexture:kemo_sgl->kemo_buffers->time_buf
                          image:kemo_sgl->kemo_buffers->tlabel_image
                         vertex:&_vertices[2]
                         texure:&_texture[2]];
        [self setTextBoxTexture:kemo_sgl->kemo_buffers->msg_buf
                          image:kemo_sgl->kemo_buffers->message_image
                         vertex:&_vertices[1]
                         texure:&_texture[1]];


        if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP){
/*  Commands to render map projection */
            [self drawMapSolidObjext:kemo_sgl->kemo_buffers->MAP_solid_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[10]
                          projection:&_map_proj_mat];
/*  Commands to render isolines on map */
            [self drawMapSolidObjext:kemo_sgl->kemo_buffers->MAP_isoline_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[11]
                          projection:&_map_proj_mat];
/*  Commands to render Coastline on map */
            [self drawMapLineObjext:kemo_sgl->kemo_buffers->coast_buf
                            encoder:&_renderEncoder
                             vertex:&_vertices[12]
                         projection:&_map_proj_mat];
/*  Commands to render grids on map */
            [self drawMapLineObjext:kemo_sgl->kemo_buffers->sph_grid_buf
                            encoder:&_renderEncoder
                              vertex:&_vertices[13]
                         projection:&_map_proj_mat];
        } else {
            [self drawTexureWithPhong:kemo_sgl->kemo_buffers->PSF_stxur_buf
                              encoder:&_renderEncoder
                               vertex:&_vertices[36]
                               texure:&_texture[11]
                               lights:&lights
                            materials:&material
                                sides:BOTH_SURFACES
                                solid:SMOOTH_SHADE];

            [self drawSolidWithSimple:kemo_sgl->kemo_buffers->axis_buf
                              encoder:&_renderEncoder
                               vertex:&_vertices[43]
                                sides:BOTH_SURFACES
                                solid:SMOOTH_SHADE];

            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->PSF_solid_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[35]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:SMOOTH_SHADE];
            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->PSF_isoline_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[31]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:SMOOTH_SHADE];
            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->PSF_arrow_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[32]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:SMOOTH_SHADE];

            if(kemo_sgl->kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
                [self drawSolidWithPhong:kemo_sgl->kemo_buffers->FLINE_tube_buf
                                 encoder:&_renderEncoder
                                  vertex:&_vertices[55]
                                  lights:&lights
                               materials:&material
                                   sides:BOTH_SURFACES
                                   solid:SMOOTH_SHADE];
            };
            
            [self setMetalVertexs:kemo_sgl->kemo_buffers->coast_buf
                           vertex:&_vertices[41]];
            [self setMetalVertexs:kemo_sgl->kemo_buffers->sph_grid_buf
                           vertex:&_vertices[42]];

            
            [self drawLineObject:kemo_sgl->kemo_buffers->FLINE_line_buf
                         encoder:&_renderEncoder
                          vertex:&_vertices[56]];

            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->mesh_node_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[51]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:SMOOTH_SHADE];
            [self drawLineObject:kemo_sgl->kemo_buffers->mesh_grid_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[52]];
            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->mesh_solid_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[53]
                              lights:&lights
                           materials:&material
                               sides:kemo_sgl->kemo_mesh->mesh_m->polygon_mode
                               solid:SMOOTH_SHADE];

            [self drawCubeWithPhong:kemo_sgl->kemo_buffers->cube_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[30]
                              index:&_index_buffer
                              lights:&lights
                           materials:&material];

            [self drawLineObject:kemo_sgl->kemo_buffers->coast_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[41]];
            [self drawLineObject:kemo_sgl->kemo_buffers->sph_grid_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[42]];
/*  Draw transparent objects */
            [self drawTexureWithPhong:kemo_sgl->kemo_buffers->PSF_ttxur_buf
                              encoder:&_renderEncoder
                               vertex:&_vertices[38]
                               texure:&_texture[12]
                               lights:&lights
                            materials:&material
                                sides:BOTH_SURFACES
                                solid:FLAT_SHADE];

            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->PSF_trns_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[37]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:FLAT_SHADE];
            
            [self drawSolidWithPhong:kemo_sgl->kemo_buffers->mesh_trns_buf
                             encoder:&_renderEncoder
                              vertex:&_vertices[54]
                              lights:&lights
                           materials:&material
                               sides:BOTH_SURFACES
                               solid:FLAT_SHADE];
        };

/*  Commands to render colorbar  box */
        [self drawMapSolidObjext:kemo_sgl->kemo_buffers->cbar_buf
                         encoder:&_renderEncoder
                          vertex:&_vertices[3]
                      projection:&_cbar_proj_mat];
/*  Commands to render colorbar  label */
        [self drawTextBoxObjext:kemo_sgl->kemo_buffers->min_buf
                        encoder:&_renderEncoder
                         vertex:&_vertices[4]
                         texure:&_texture[4]];
        [self drawTextBoxObjext:kemo_sgl->kemo_buffers->max_buf
                        encoder:&_renderEncoder
                         vertex:&_vertices[5]
                         texure:&_texture[5]];
        [self drawTextBoxObjext:kemo_sgl->kemo_buffers->zero_buf
                        encoder:&_renderEncoder
                         vertex:&_vertices[6]
                         texure:&_texture[6]];

/*  Commands to render time label */
        [self drawTextBoxObjext:kemo_sgl->kemo_buffers->time_buf
                        encoder:&_renderEncoder
                         vertex:&_vertices[2]
                         texure:&_texture[2]];
/*  Commands to render colorbar  box */
        [self drawTextBoxObjext:kemo_sgl->kemo_buffers->msg_buf
                        encoder:&_renderEncoder
                         vertex:&_vertices[1]
                         texure:&_texture[1]];

        [_renderEncoder endEncoding];

        // Schedule a present once the framebuffer is complete using the current drawable.
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer  commit];

//    [_texture[1] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[1] release];
//    [_texture[2] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[2] release];
//    [_texture[4] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[4] release];
//    [_texture[5] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[5] release];
//    [_texture[6] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[6] release];

    if(kemo_sgl->kemo_buffers->MAP_solid_buf->num_nod_buf > 0){
//        [_vertices[10] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[10] release];
    };

    if(kemo_sgl->kemo_buffers->MAP_isoline_buf->num_nod_buf > 0){
//        [_vertices[11] setPurgeableState:MTLPurgeableStateEmpty];
//        [_vertices[11] release];
    };

    if(kemo_sgl->kemo_buffers->coast_buf->num_nod_buf > 0){
//        [_vertices[12] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[12] release];
    };
    
    if(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf > 0){
//        [_vertices[13] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[13] release];
    };

    if(kemo_sgl->kemo_buffers->axis_buf->num_nod_buf > 0){[_vertices[43] release];};

//    [_vertices[0] setPurgeableState:MTLPurgeableStateEmpty];
//    [_vertices[0] release];
}



/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
