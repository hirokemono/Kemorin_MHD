// #version 330
//
// phong.vert
//
vec4 color_from_scalar(float x)
{
    return vec4(x*0.01,x*0.04,x*0.1,1.0);
}

layout (location = 0) in vec4  xyz;
layout (location = 1) in vec4  color;
layout (location = 2) in vec4  norm;
layout (location = 3) in vec2  txur;
layout (location = 4) in vec2  data;


uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;
uniform mat4 modelNormalMat;
// uniform KemoViewNormalize colormap;

out vec4  position;
out vec4  normal;
out vec4  ex_Color;

void main(void)
{
    position = xyz;
    position.w = 1.0;
    position = modelViewMat * position;
	normal =   modelNormalMat * norm;
    ex_Color = color_from_scalar(data.x);
	gl_Position =  projectionMat * position;
}
