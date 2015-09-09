#version 130
uniform mat4 modelView,proj;

in  vec3 position, in_color;
in  vec2 in_uv;

out vec3 color;
out vec2 uv;
out vec3 pos;

void main()
{
    uv          = in_uv;
    color       = in_color;
    pos         = position;
    gl_Position = vec4(position, 1) * modelView * proj;
}
