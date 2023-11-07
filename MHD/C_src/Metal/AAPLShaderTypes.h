/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header containing types and enum constants shared between Metal shaders and C/ObjC source
*/

#ifndef AAPLShaderTypes_h
#define AAPLShaderTypes_h

#include <simd/simd.h>

// Buffer index values shared between shader and C code to ensure Metal shader buffer inputs
// match Metal API buffer set calls.
typedef enum AAPLVertexInputIndex
{
    AAPLVertexInputIndexVertices =     0,
    AAPLVertexInputIndexViewportSize = 1,
    AAPLVertexInputIndexScale =        2,
    AAPLOrthogonalMatrix =             3,
} AAPLVertexInputIndex;

// Texture index values shared between shader and C code to ensure Metal shader buffer inputs match
//   Metal API texture set calls
typedef enum AAPLTextureIndex
{
    AAPLTextureIndexBaseColor = 0,
} AAPLTextureIndex;

//  This structure defines the layout of vertices sent to the vertex
//  shader. This header is shared between the .metal shader and C code, to guarantee that
//  the layout of the vertex array in the C code matches the layout that the .metal
//  vertex shader expects.
typedef struct
{
    vector_float2 position;
    vector_float4 color;
} AAPLVertex;

typedef struct
{
    // Positions in pixel space. A value of 100 indicates 100 pixels from the origin/center.
    vector_float3 position;
    // RGBA Color in pixel space. A value of 100 indicates 100 pixels from the origin/center.
    vector_float4 color;
    // Normal vector in pixel space. A value of 100 indicates 100 pixels from the origin/center.
    vector_float4 normal;
 
    // 2D texture coordinate
    vector_float2 textureCoordinate;

    // data value in pixel space. A value of 100 indicates 100 pixels from the origin/center.
    vector_float2  data;
} AAPLVertexWithTexture;

#endif /* AAPLShaderTypes_h */
