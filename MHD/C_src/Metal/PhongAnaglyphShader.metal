/*
//  PhongAnaglyphShader.metal
//  
//
//  Created by Hiroaki Matsui on 11/25/23.
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
    float4 Position2d  [[position]];
    float4 diffLeft2d;
    float4 diffRight2d;
    
    // Since this member does not have a special attribute, the rasterizer
    // interpolates its value with the values of the other triangle vertices
    // and then passes the interpolated value to the fragment shader for each
    // fragment in the triangle.
    float4 pixelSpaceColor;
    
    float4 pixelSpacePosition;
    float4 diffPixelLeft;
    float4 diffPixelRight;

    float4 pixelSpaceNormal;
    float4 diffNormalLeft;
    float4 diffNormalRight;
};

// Vertex Function
vertex RasterizerData
PhongAnagriphVertexShader(uint vertexID [[ vertex_id ]],
                          constant KemoViewVertex *vertexArray [[ buffer(AAPLVertexInputIndexVertices) ]],
                          constant matrix_float4x4 *ModelViewMatrixPointer [[buffer(AAPLModelViewMatrix)]],
                          constant matrix_float4x4 *ProjectionMatrixPointer [[buffer(AAPLProjectionMatrix)]],
                          constant matrix_float4x4 *ModelNormalMatrixPointer [[buffer(AAPLModelNormalMatrix)]],
                          constant matrix_float4x4 *LeftModelViewMatrixPointer [[buffer(LeftModelViewMatrix)]],
                          constant matrix_float4x4 *LeftProjectionMatrixPointer [[buffer(LeftProjectionMatrix)]],
                          constant matrix_float4x4 *LeftModelNormalMatrixPointer [[buffer(LeftModelNormalMatrix)]],
                          constant matrix_float4x4 *RightModelViewMatrixPointer [[buffer(RightModelViewMatrix)]],
                          constant matrix_float4x4 *RightProjectionMatrixPointer [[buffer(RightProjectionMatrix)]],
                          constant matrix_float4x4 *RightModelNormalMatrixPointer [[buffer(RightModelNormalMatrix)]])
{
    RasterizerData out;

// Index into the array of positions to get the current vertex.
    float4 objectSpacePosition = vertexArray[vertexID].position;
    float4 objectSpaceNormal =   vertexArray[vertexID].normal;
    float4 pixelSpaceColor =     vertexArray[vertexID].color;

    matrix_float4x4 ModelViewMatrix = matrix_float4x4(*ModelViewMatrixPointer);
    matrix_float4x4 ProjectionMatrix = matrix_float4x4(*ProjectionMatrixPointer);
    matrix_float4x4 ModelNormalMatrix = matrix_float4x4(*ModelNormalMatrixPointer);

    matrix_float4x4 leftModelViewMatrix = matrix_float4x4(*LeftModelViewMatrixPointer);
    matrix_float4x4 leftProjectionMatrix = matrix_float4x4(*LeftProjectionMatrixPointer);
    matrix_float4x4 leftModelNormalMatrix = matrix_float4x4(*LeftModelNormalMatrixPointer);

    matrix_float4x4 rightModelViewMatrix = matrix_float4x4(*RightModelViewMatrixPointer);
    matrix_float4x4 rightProjectionMatrix = matrix_float4x4(*RightProjectionMatrixPointer);
    matrix_float4x4 rightModelNormalMatrix = matrix_float4x4(*RightModelNormalMatrixPointer);

    out.pixelSpacePosition =     objectSpacePosition;
    out.pixelSpacePosition.w =   1.0;

    float4 leftPixelSpacePosition =  leftModelViewMatrix *  out.pixelSpacePosition;
    float4 rightPixelSpacePosition = rightModelViewMatrix * out.pixelSpacePosition;
    out.pixelSpacePosition =         ModelViewMatrix *      out.pixelSpacePosition;
    out.diffPixelLeft =           leftPixelSpacePosition -  out.pixelSpacePosition;
    out.diffPixelRight =          rightPixelSpacePosition - out.pixelSpacePosition;


    out.Position2d =                 ProjectionMatrix *      out.pixelSpacePosition;
    float4 leftPosition2d =          leftProjectionMatrix *  leftPixelSpacePosition;
    float4 rightPosition2d =         rightProjectionMatrix * rightPixelSpacePosition;
    out.diffLeft2d =                 leftPosition2d -  out.Position2d;
    out.diffRight2d =                rightPosition2d - out.Position2d;

    out.pixelSpaceNormal =   objectSpaceNormal;
    out.pixelSpaceNormal.w = 0.0;
    float4 leftpixelSpaceNormal =   leftModelNormalMatrix *  out.pixelSpaceNormal;
    float4 rightpixelSpaceNormal =  rightModelNormalMatrix * out.pixelSpaceNormal;
    out.pixelSpaceNormal =          ModelNormalMatrix *      out.pixelSpaceNormal;

    out.diffNormalLeft =  leftpixelSpaceNormal -  out.pixelSpaceNormal;
    out.diffNormalRight = rightpixelSpaceNormal - out.pixelSpaceNormal;

    out.pixelSpaceColor =   pixelSpaceColor;
    return out;
}

// Fragment function
fragment float4
PhongAnagriphFragmentShader(RasterizerData in [[stage_in]],
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

    float3 leftPosition =  in.pixelSpacePosition.xyz + in.diffPixelLeft.xyz;
    float3 rightPosition = in.pixelSpacePosition.xyz + in.diffPixelRight.xyz;
    float3 view =          normalize(in.pixelSpacePosition.xyz);
    float3 leftView =      normalize(leftPosition);
    float3 rightView =     normalize(rightPosition);
    float3 fnormal =       normalize(in.pixelSpaceNormal.xyz);
    float3 leftFnormal =   normalize(in.pixelSpaceNormal.xyz +  in.diffNormalLeft.xyz);
    float3 rightFnormal =  normalize(in.pixelSpaceNormal.xyz +  in.diffNormalRight.xyz);

    float3 halfway;
    float3 light;
    float diffuseDir;
    float product;
    float specular;
    
    float4 tmpSpecular;
    tmpSpecular.xyz = materialSpecular.xyz;
    tmpSpecular.w = in.pixelSpaceColor.w;

    float4 out_Color =   0.0;
    float4 left_Color =  0.0;
    float4 right_Color = 0.0;
    for (int i=0; i<numLights;i++){
        light =      normalize(lightsParameters.position[i].xyz - in.pixelSpacePosition.xyz);
        diffuseDir = dot(light, fnormal);
        halfway =    normalize(light - view);
        product =    max(dot(fnormal, halfway), 0.0);
        specular =   pow(product, shininess);

        left_Color.w = left_Color.w + in.pixelSpaceColor.w * materialAmbient.w
                                    + in.pixelSpaceColor.w * materialDiffuse.w * abs(diffuseDir)
                                    + tmpSpecular.w * specular;

        light =      normalize(lightsParameters.position[i].xyz - leftPosition);
        diffuseDir = dot(light, leftFnormal);
        halfway =    normalize(light - leftView);
        product =    max(dot(leftFnormal, halfway), 0.0);
        specular =   pow(product, shininess);
        
        left_Color.r = left_Color.r + in.pixelSpaceColor.r * materialAmbient.r
                                    + in.pixelSpaceColor.r * materialDiffuse.r * abs(diffuseDir)
                                    + tmpSpecular.r * specular;

        light =      normalize(lightsParameters.position[i].xyz - rightPosition);
        diffuseDir = dot(light, rightFnormal);
        halfway =    normalize(light - rightView);
        product =    max(dot(rightFnormal, halfway), 0.0);
        specular =   pow(product, shininess);
        
        right_Color.g = left_Color.g + in.pixelSpaceColor.g * materialAmbient.g
                                     + in.pixelSpaceColor.g * materialDiffuse.g * abs(diffuseDir)
                                     + tmpSpecular.g * specular;
        right_Color.b = left_Color.b + in.pixelSpaceColor.b * materialAmbient.b
                                     + in.pixelSpaceColor.b * materialDiffuse.b * abs(diffuseDir)
                                     + tmpSpecular.b * specular;
    }
//    out_Color = left_Color;
    out_Color = left_Color + right_Color;
    return out_Color;
}

