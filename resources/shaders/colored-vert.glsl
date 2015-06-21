#version 130
uniform mat4 modelView, proj;
uniform vec4 baseColor;

in  vec3 position, in_color;
in  vec2 in_uv;

out vec4 color;

void main()
{
    color       = vec4(in_color,1.0) * baseColor;
    gl_Position = vec4(position,1.0) * modelView * proj;
}
