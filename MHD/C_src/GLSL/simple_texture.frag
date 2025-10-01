// #version 330
//
// simple_texture.frag
//

in vec2 tex_position;

out vec4 out_Color;

uniform sampler2D image;

void main (void)
{
	out_Color = texture(image, tex_position);
}

