// #version 330
//
// simple_texture.vert
//

layout (location = 0) in vec4  xyz;
layout (location = 1) in vec2  txur;


uniform mat4 projectionMat;
uniform mat4 modelViewMat;

out vec2 tex_position;

void main(void)
{
    vec4 position = xyz;
    position.w = 1.0;
	position = modelViewMat * position;
	tex_position = txur;

	gl_Position =  projectionMat * position;
}
