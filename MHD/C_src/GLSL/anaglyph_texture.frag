#version 330
// anaglyph_texture.frag

in vec2 tex_position;

out vec4 out_Color;

uniform sampler2D left_image;
uniform sampler2D right_image;

void main (void)
{
    vec4 left_Color =  texture(left_image, tex_position);
    vec4 right_Color = texture(right_image, tex_position);
    
    float r = 0.299 * left_Color.x + 0.587 * left_Color.y + 0.114 * left_Color.z;
    out_Color = vec4(r, right_Color.y, right_Color.z, left_Color.w);
}

