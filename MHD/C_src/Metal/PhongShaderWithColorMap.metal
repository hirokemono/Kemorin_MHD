/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Metal shaders for Phong Shading
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
    float2 pixelSpaceData;

    float4 pixelSpaceNormal;
    float4 pixelSpacePosition;
};


float3 color_rainbow_c(float rnorm){
    float purple = 0.0;
    float blue =   0.1;
    float ocean =  0.325;
    float green =  0.55;
    float yellow = 0.775;
    float red =    1.0;
            
    float r = 0.0;
    float g = 0.0;
    float b = 0.0;
    if (rnorm < purple){
        r = 0.5;
    } else if (rnorm >= purple && rnorm < blue){
        r = 0.5 - 5.0 * rnorm;
    } else if (rnorm >= blue && rnorm < green){
        r = 0.0;
    } else if (rnorm >= green && rnorm < yellow){
        r = (rnorm-green) * 40.0 / 9.0;
    } else {
        r = 1.0;
    }
    
    if (rnorm < blue){
        g = 0.0;
    } else if (rnorm >= blue && rnorm < ocean){
        g = (rnorm-blue) * 40.0 / 9.0;
    } else if (rnorm >= ocean && rnorm < yellow){
        g = 1.0;
    } else if (rnorm >= yellow && rnorm < red){
        g = 1.0 - (rnorm-yellow) * 40.0 / 9.0;
    } else {
        g = 0.0;
    }
    
    if (rnorm < blue){
        b = 1.0;
    } else if (rnorm >= blue && rnorm < ocean){
        b = 1.0;
    } else if (rnorm >= ocean && rnorm < green){
        b = 1.0 - (rnorm-ocean) * 40.0 / 9.0;
    } else {
        b = 0.0;
    }
    return float3(r, g, b);
}


// Vertex Function
vertex RasterizerData
PhongColorMapVertexShader(uint vertexID [[ vertex_id ]],
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
    float2 pixelSpaceData =      vertexArray[vertexID].data;

    matrix_float4x4 modelViewMatrix = matrix_float4x4(*ModelViewMatrixPointer);
    matrix_float4x4 projectionMatrix = matrix_float4x4(*ProjectionMatrixPointer);
    matrix_float4x4 modelNormalMatrix = matrix_float4x4(*ModelNormalMatrixPointer);

    out.pixelSpacePosition =     objectSpacePosition;
    out.pixelSpacePosition.w =   1.0;
    out.pixelSpacePosition =     modelViewMatrix *  out.pixelSpacePosition;
    
    out.position2d =             projectionMatrix * out.pixelSpacePosition;

    out.pixelSpaceNormal =   objectSpaceNormal;
    out.pixelSpaceNormal.w = 0.0;
    out.pixelSpaceNormal =   modelNormalMatrix * out.pixelSpaceNormal;
    
    out.pixelSpaceColor =   pixelSpaceColor;
    out.pixelSpaceData =    pixelSpaceData;
    return out;
}

// Fragment function
fragment float4
PhongColorMapFragmentShader(RasterizerData in [[stage_in]],
                    constant LightSourceParameters *lightsParamsPointer[[buffer(AAPLLightsParams)]],
                    constant MaterialParameters *frontMaterialParamsPointer[[buffer(AAPLMaterialParams)]])
{
    LightSourceParameters lightsParameters = LightSourceParameters(*lightsParamsPointer);
    int numLights = lightsParameters.num_lights;
    
    MaterialParameters FrontMaterialParams = MaterialParameters(*frontMaterialParamsPointer);
    float4 materialAmbient =   FrontMaterialParams.ambient;
    float4 materialDiffuse =   FrontMaterialParams.diffuse;
    float4 materialSpecular =  FrontMaterialParams.specular;
    float  shininess =         FrontMaterialParams.shininess;

    float4 pixelSpaceColor = float4(color_rainbow_c(in.pixelSpaceData.x).xyz, in.pixelSpaceData.y);
    
    float3 view =    normalize(in.pixelSpacePosition.xyz);
    float3 fnormal = normalize(in.pixelSpaceNormal.xyz);
    float3 halfway;
    float3 light;
    float diffuseDir;
    float product;
    float specular;
    
    float4 tmpSpecular;
    tmpSpecular.xyz = materialSpecular.xyz;
    tmpSpecular.w = pixelSpaceColor.w;

    float4 out_Color = 0.0;
    for (int i=0; i<numLights;i++){
        light =      normalize(lightsParameters.position[i].xyz - in.pixelSpacePosition.xyz);
        diffuseDir = dot(light, fnormal);
        halfway =    normalize(light - view);
        product =    max(dot(fnormal, halfway), 0.0);
        specular =   pow(product, shininess);
        
        out_Color = out_Color + pixelSpaceColor * materialAmbient
                              + pixelSpaceColor * materialDiffuse * abs(diffuseDir)
                              + tmpSpecular * specular;
    }
    return out_Color;
}

