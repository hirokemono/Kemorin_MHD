
/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Metal shaders for Phong Shading with texture
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
    
    float4 pixelSpaceNormal;
    float4 pixelSpacePosition;
    float2 texurePosition;
};

// Vertex Function
vertex RasterizerData
PhongTexureVertexShader(uint vertexID [[ vertex_id ]],
                        constant KemoViewVertex *vertexArray [[ buffer(AAPLVertexInputIndexVertices) ]],
                        constant matrix_float4x4 *ModelViewMatrixPointer [[buffer(AAPLModelViewMatrix)]],
                        constant matrix_float4x4 *ProjectionMatrixPointer [[buffer(AAPLProjectionMatrix)]],
                        constant matrix_float4x4 *ModelNormalMatrixPointer [[buffer(AAPLModelNormalMatrix)]]
                  )
{
    RasterizerData out;

// Index into the array of positions to get the current vertex.
    float4 objectSpacePosition = vertexArray[vertexID].position;
    float4 objectSpaceNormal =   vertexArray[vertexID].normal;
    float4 pixelSpaceColor =     vertexArray[vertexID].color;
    float2 texurePosition =      vertexArray[vertexID].textureCoordinate;

    matrix_float4x4 modelViewMatrix = matrix_float4x4(*ModelViewMatrixPointer);
    matrix_float4x4 projectionMatrix = matrix_float4x4(*ProjectionMatrixPointer);
    matrix_float4x4 modelNormalMatrix = matrix_float4x4(*ModelNormalMatrixPointer);

    out.pixelSpacePosition =     objectSpacePosition;
    out.pixelSpacePosition.w =   1.0;
    out.pixelSpacePosition =     modelViewMatrix *  out.pixelSpacePosition;
    
    out.position2d =             projectionMatrix * out.pixelSpacePosition;

    out.pixelSpaceNormal =   objectSpaceNormal;
    out.pixelSpaceNormal.w = 1.0;
    out.pixelSpaceNormal =   modelNormalMatrix * out.pixelSpaceNormal;
    
    out.pixelSpaceColor =  pixelSpaceColor;
    out.texurePosition =   texurePosition;
    return out;
}

// Fragment function
fragment float4
PhongTextureFragmentShader(RasterizerData in [[stage_in]],
                           constant LightSourceParameters *lightsParamsPointer[[buffer(AAPLLightsParams)]],
                           constant MaterialParameters *frontMaterialParamsPointer[[buffer(AAPLMaterialParams)]],
                           texture2d<float> colorTexture [[ texture(AAPLTextureIndexBaseColor) ]])
{
    constexpr sampler textureSampler(mag_filter::linear,
                                     min_filter::linear);

/* Sample the texture to obtain a color */
    float4 colorSample = colorTexture.sample(textureSampler, in.texurePosition);
    colorSample.a = in.pixelSpaceColor.a;

    LightSourceParameters lightsParameters = LightSourceParameters(*lightsParamsPointer);
    int numLights = lightsParameters.num_lights;
    
    MaterialParameters FrontMaterialParams = MaterialParameters(*frontMaterialParamsPointer);
    float4 materialAmbient =   FrontMaterialParams.ambient;
    float4 materialDiffuse =   FrontMaterialParams.diffuse;
    float4 materialSpecular =  FrontMaterialParams.specular;
    float  shininess =         FrontMaterialParams.shininess;

    float3 view =    normalize(in.pixelSpacePosition.xyz);
    float3 fnormal = normalize(in.pixelSpaceNormal.xyz);
    float3 halfway;
    float3 light;
    float diffuseDir;
    float product;
    float specular;
    
    float4 tmpSpecular;
    tmpSpecular.xyz = materialSpecular.xyz;
    tmpSpecular.w =   colorSample.a;

    float4 out_Color = 0.0;
    for (int i=0; i<numLights;i++){
        light =      normalize(lightsParameters.position[i].xyz - in.pixelSpacePosition.xyz);
        diffuseDir = dot(light, fnormal);
        halfway =    normalize(light - view);
        product =    max(dot(fnormal, halfway), 0.0);
        specular =   pow(product, shininess);
        
        out_Color = out_Color + colorSample * materialAmbient
                              + colorSample * materialDiffuse * abs(diffuseDir)
                              + tmpSpecular * specular;
    }
    return out_Color;
}

