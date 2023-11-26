/*
//  TexureShader.metal
//
//
//  Created by Hiroaki Matsui on 11/20/23.
*/
#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

// Include header shared between this Metal shader code and C code executing Metal API commands
#include "AAPLShaderTypes.h"

// Vertex shader outputs and fragment shader inputs
struct RasterizerData
{
    // The [[position]] attribute qualifier of this member indicates this value is
    // the clip space position of the vertex when this structure is returned from
    // the vertex shader
    float4 position2d [[position]];
    
    // Since this member does not have a special attribute, the rasterizer
    // interpolates its value with the values of the other triangle vertices
    // and then passes the interpolated value to the fragment shader for each
    // fragment in the triangle.
    float4 pixelSpaceColor;
    
    float2 texurePosition;
};

// Vertex Function
vertex RasterizerData
SimpleTexureVertexShader(uint vertexID [[ vertex_id ]],
                         constant KemoViewVertex *vertexArray [[ buffer(AAPLVertexInputIndexVertices) ]],
                         constant matrix_float4x4 *ModelViewMatrixPointer [[buffer(AAPLModelViewMatrix)]],
                         constant matrix_float4x4 *ProjectionMatrixPointer [[buffer(AAPLProjectionMatrix)]]
                  )
{
    RasterizerData out;

// Index into the array of positions to get the current vertex.
    float4 objectSpacePosition = vertexArray[vertexID].position;
    float4 pixelSpaceColor =     vertexArray[vertexID].color;
    float2 texurePosition =      vertexArray[vertexID].textureCoordinate;

    matrix_float4x4 modelViewMatrix = matrix_float4x4(*ModelViewMatrixPointer);
    matrix_float4x4 projectionMatrix = matrix_float4x4(*ProjectionMatrixPointer);

    float4 pixelSpacePosition;
    pixelSpacePosition =     objectSpacePosition;
    pixelSpacePosition.w =   1.0;
    pixelSpacePosition =     modelViewMatrix *  pixelSpacePosition;
    
    out.position2d =         projectionMatrix * pixelSpacePosition;

    out.pixelSpaceColor =  pixelSpaceColor;
    out.texurePosition =   texurePosition;
    return out;
}

// Fragment function
fragment float4
SimpleTextureFragmentShader(RasterizerData in [[stage_in]],
                            texture2d<float> colorTexture [[ texture(AAPLTextureIndexBaseColor) ]])
{
    constexpr sampler textureSampler(mag_filter::linear,
                                     min_filter::linear);

/* Sample the texture to obtain a color */
    const float4 colorSample = colorTexture.sample(textureSampler, in.texurePosition);
/* return the color of the texture */
    return float4(colorSample);
}

