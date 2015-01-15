#version 130
uniform mat4 modelView,proj;

in  vec3 position,in_color;
out vec3 color;

void main()
{
    color       = in_color;
    gl_Position = vec4(position,1.0) * modelView * proj;
}
