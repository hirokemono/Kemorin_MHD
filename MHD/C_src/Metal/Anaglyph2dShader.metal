/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Metal shaders used for this sample
*/

#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

// Include header shared between this Metal shader code and C code executing Metal API commands
#include "KemoViewShaderTypes.h"

// Vertex shader outputs and fragment shader inputs
struct RasterizerData
{
    // The [[position]] attribute qualifier of this member indicates this value is
    // the clip space position of the vertex when this structure is returned from
    // the vertex shader
    float4 position2d [[position]];

    // Since this member does not have a special attribute qualifier, the rasterizer
    // will interpolate its value with values of other vertices making up the triangle
    // and pass that interpolated value to the fragment shader for each fragment in
    // that triangle.
    float2 textureCoordinate;

};

// Vertex Function
vertex RasterizerData
AnaglyphVertexShader(uint vertexID [[ vertex_id ]],
             constant KemoViewVertex *vertexArray [[ buffer(AAPLVertexInputIndexVertices) ]],
             constant matrix_float4x4 *OrthogonalMatrixPointer [[
                 buffer(AAPLOrthogonalMatrix)]])
{

    RasterizerData out;

    // Index into the array of positions to get the current vertex.
    //   Positions are specified in pixel dimensions (i.e. a value of 100 is 100 pixels from
    //   the origin)
    float3 pixelSpacePosition = vertexArray[vertexID].position.xyz;
    matrix_float4x4 OrthogonalMatrix = matrix_float4x4(*OrthogonalMatrixPointer);

    // To convert from positions in pixel space to positions in clip-space,
    //  divide the pixel coordinates by half the size of the viewport.
    out.position2d.xyz = pixelSpacePosition;
    out.position2d.w = 1.0;
    out.position2d = OrthogonalMatrix * out.position2d;

    out.position2d.z = 0.0;
    out.position2d.w = 1.0;

    // Pass the input textureCoordinate straight to the output RasterizerData. This value will be
    //   interpolated with the other textureCoordinate values in the vertices that make up the
    //   triangle.
    out.textureCoordinate = vertexArray[vertexID].textureCoordinate;

    return out;
}

// Fragment function
fragment float4
AnaglyphFragmentShader(RasterizerData in [[stage_in]],
                       texture2d<float> leftTexture  [[ texture(AAPLTextureIndexBaseColor) ]],
                       texture2d<float> rightTexture [[ texture(AAPLTextureIndexRight) ]])
{
    constexpr sampler textureSampler (mag_filter::linear,
                                      min_filter::linear);

    // Sample the texture to obtain a color
    const float4 leftSample =   leftTexture.sample(textureSampler, in.textureCoordinate);
    const float4 rightSample = rightTexture.sample(textureSampler, in.textureCoordinate);

/* Merge right and left texure */
//    float a = 1.0;
    float r = 0.299 * leftSample.r + 0.587 * leftSample.g + 0.114 * leftSample.b;
    float4 colorSample = float4(r, rightSample.g, rightSample.b, 1.0);
    return colorSample;
}

