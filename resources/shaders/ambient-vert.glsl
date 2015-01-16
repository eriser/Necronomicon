#version 130
uniform mat4 modelView,proj;

in  vec3 position,in_color;
in  vec2 in_uv;

out vec3 color;
out vec2 uv;

void main()
{
    color          = in_color;
    uv             = in_uv;
    gl_Position    = vec4(position,1.0) * modelView * proj;
}
