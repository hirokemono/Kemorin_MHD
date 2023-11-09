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
    
    AAPLOrthogonalMatrix = 1,
} AAPLVertexInputIndex;

typedef enum AAPLLightInputIndex
{
    AAPLNumLights =      0,
    AAPLLightsParams =   1,
    AAPLMaterialParams = 2,
} AAPLLightInputIndex;


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
    vector_float3 position;
/* RGBA Color in pixel space. A value of 100 indicates 100 pixels
  from the origin/center. */
    vector_float4 color;
} KemoView2DVertex;

typedef struct
{
/* Positions in pixel space. A value of 100 indicates 100 pixels
     from the origin/center. */
    vector_float3 position;
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
    vector_float4 ambient;              // Aclarri
    vector_float4 diffuse;              // Dcli
    vector_float4 specular;             // Scli
    vector_float4 position;             // Ppli
    vector_float4 halfVector;           // Derived: Hi
    vector_float3 spotDirection;        // Sdli
    float spotExponent;        // Srli
    float spotCutoff;          // Crli
    // (range: [0.0,90.0], 180.0)
    float spotCosCutoff;       // Derived: cos(Crli)
    // (range: [1.0,0.0],-1.0)
    float constantAttenuation;   // K0
    float linearAttenuation;     // K1
    float quadraticAttenuation;  // K2
} LightSourceParameters;

typedef struct{
    vector_float4 emission;    // Ecm
    vector_float4 ambient;     // Acm
    vector_float4 diffuse;     // Dcm
    vector_float4 specular;    // Scm
    float         shininess;  // Srm
} MaterialParameters;

#endif /* AAPLShaderTypes_h */
