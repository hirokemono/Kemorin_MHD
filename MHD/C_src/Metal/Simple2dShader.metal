/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Metal shaders used for this sample
*/

#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

// Include header shared between this Metal shader code and C code executing Metal API commands.
#include "AAPLShaderTypes.h"

// Vertex shader outputs and fragment shader inputs
struct RasterizerData
{
    // The [[position]] attribute of this member indicates that this value
    // is the clip space position of the vertex when this structure is
    // returned from the vertex function.
    float4 position2d [[position]];

    // Since this member does not have a special attribute, the rasterizer
    // interpolates its value with the values of the other triangle vertices
    // and then passes the interpolated value to the fragment shader for each
    // fragment in the triangle.
    float4 color;
};

vertex RasterizerData
Simple2dVertexShader(uint vertexID [[ vertex_id ]],
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
//    out.position2d = vector_float4(0.0, 0.0, 0.0, 1.0);
//    out.position2d.xy = pixelSpacePosition / (viewportSize / 2.0);
    out.position2d.xyz = pixelSpacePosition;
    out.position2d.w = 1.0;
    out.position2d = OrthogonalMatrix * out.position2d;

//    out.position2d.x = out.position2d.x * scale * aspectRatio;
//    out.position2d.y = out.position2d.y * scale;
//    out.position2d.xyz =   pixelSpacePosition;
    out.position2d.z = 0.0;
    out.position2d.w = 1.0;

    return out;
}

fragment float4
Simple2DfragmentShader(RasterizerData in [[stage_in]])
{
    // Return the interpolated color.
    return in.color;
}

