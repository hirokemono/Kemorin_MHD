/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Metal shaders used for this sample
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
    float4 color;
    
    float3 normal;
    float3 pixelSpacePosition;
};

// Vertex Function
vertex RasterizerData
PhongVertexShader(uint vertexID [[ vertex_id ]],
                  constant KemoViewVertex *vertexArray [[ buffer(AAPLVertexInputIndexVertices) ]],
                  constant matrix_float4x4 *ModelViewMatrixPointer [[buffer(AAPLModelViewMatrix)]],
                  constant matrix_float4x4 *ProjectionMatrixPointer [[buffer(AAPLProjectionMatrix)]],
                  constant matrix_float3x3 *ModelNormalMatrixPointer [[buffer(AAPLModelNormalMatrix)]]
                  )
{

    RasterizerData out;

    // Index into the array of positions to get the current vertex.
    //   Positions are specified in pixel dimensions (i.e. a value of 100 is 100 pixels from
    //   the origin)
    float3 pixelSpacePosition = vertexArray[vertexID].position.xyz;
    float3 pixelSpaceNormal =   vertexArray[vertexID].normal.xyz;
    matrix_float4x4 ModelViewMatrix = matrix_float4x4(*ModelViewMatrixPointer);
    matrix_float4x4 ProjectionMatrix = matrix_float4x4(*ProjectionMatrixPointer);
    matrix_float3x3 ModelNormalMatrix = matrix_float3x3(*ModelNormalMatrixPointer);

    out.pixelSpacePosition = pixelSpacePosition;
    out.position2d.xyz =     pixelSpacePosition;
    out.position2d.w = 1.0;
    out.position2d = ModelViewMatrix *  out.position2d;
    out.position2d = ProjectionMatrix * out.position2d;

    out.normal =   normalize(ModelNormalMatrix * pixelSpaceNormal);
    return out;
}

// Fragment function
fragment float4
PhongFragmentShader(RasterizerData in [[stage_in]],
                    constant int *numLightsPointer[[buffer(AAPLNumLights)]],
                    constant LightSourceParameters *lightsParamsPointer[[buffer(AAPLLightsParams)]],
                    constant MaterialParameters *frontMaterialParamsPointer[[buffer(AAPLMaterialParams)]]
                    )
{
    int numLights = int(*numLightsPointer);
    LightSourceParameters lightsParameters;
    MaterialParameters FrontMaterialParams = MaterialParameters(*frontMaterialParamsPointer);
    float4 materialAmbient =   FrontMaterialParams.ambient;
    float4 materialDiffuse =   FrontMaterialParams.diffuse;
    float4 materialSpecular =  FrontMaterialParams.specular;
    float  shininess = FrontMaterialParams.shininess;

    float3 view = normalize(in.pixelSpacePosition.xyz);
    float3 fnormal = in.normal;
    float3 halfway;
    float3 light;
    float diffuseDir;
    float product;
    float specular;
    
    float4 tmpSpecular;
    tmpSpecular.xyz = materialSpecular.xyz;
    tmpSpecular.w = in.color.w;

    float4 out_Color = 0.0;
    for (int i=0; i<numLights;i++){
        lightsParameters = LightSourceParameters(lightsParamsPointer[i]);
        light = normalize(lightsParameters.position.xyz - in.pixelSpacePosition);
        diffuseDir = dot(light, fnormal);

        halfway = normalize(light - view);
        product = max(dot(fnormal, halfway), 0.0);
        specular = pow(product, shininess);
        
        out_Color = out_Color + in.color * materialAmbient
                              + in.color * materialDiffuse * abs(diffuseDir)
                              + tmpSpecular * specular;
    }
    return out_Color;
}

