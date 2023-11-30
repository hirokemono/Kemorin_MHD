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

    
    AAPLModelViewMatrix =   1,
    AAPLProjectionMatrix =  2,
    AAPLModelNormalMatrix = 3,
    
    LeftModelViewMatrix =   4,
    LeftProjectionMatrix =  5,
    LeftModelNormalMatrix = 6,
    
    RightModelViewMatrix =   7,
    RightProjectionMatrix =  8,
    RightModelNormalMatrix = 9,
    
    AAPLOrthogonalMatrix = 1,
} AAPLVertexInputIndex;

typedef enum AAPLLightInputIndex
{
    AAPLLightsParams =      0,
    AAPLMaterialParams =    1,
} AAPLLightInputIndex;

// Texture index values shared between shader and C code to ensure Metal shader buffer inputs match
//   Metal API texture set calls
typedef enum AAPLTextureIndex
{
    AAPLTextureIndexBaseColor = 0,
    AAPLTextureIndexRight =     1,
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
    vector_float3 position;
/* RGBA Color in pixel space. A value of 100 indicates 100 pixels
  from the origin/center. */
    vector_float4 color;
} KemoView2DVertex;

typedef struct
{
/* Positions in pixel space. A value of 100 indicates 100 pixels
     from the origin/center. */
    vector_float4 position;
/* RGBA Color in pixel space. A value of 100 indicates 100 pixels
      from the origin/center. */
    vector_float4 color;
/* Normal vector in pixel space. A value of 100 indicates 100 pixels
 from the origin/center. */
    vector_float4 normal;
 
/*  2D texture coordinate */
    vector_float2 textureCoordinate;

/* data value in pixel space. */
    vector_float2  data;
} KemoViewVertex;

typedef struct{
//    vector_float4 ambient[12];              // Aclarri
//    vector_float4 diffuse[12];              // Dcli
//    vector_float4 specular[12];             // Scli
    vector_float4 position[12];             // Ppli
//    vector_float4 halfVector[12];           // Derived: Hi
//    vector_float3 spotDirection[12];        // Sdli
//    float spotExponent[12];        // Srli
//    float spotCutoff[12];          // Crli
    // (range: [0.0,90.0], 180.0)
//    float spotCosCutoff[12];       // Derived: cos(Crli)
    // (range: [1.0,0.0],-1.0)
//    float constantAttenuation[12];   // K0
//    float linearAttenuation[12];     // K1
//    float quadraticAttenuation[12];  // K2
    int num_lights;
} LightSourceParameters;

typedef struct{
//    vector_float4 emission;    // Ecm
    vector_float4 ambient;     // Acm
    vector_float4 diffuse;     // Dcm
    vector_float4 specular;    // Scm
    float         shininess;  // Srm
} MaterialParameters;

#endif /* AAPLShaderTypes_h */
