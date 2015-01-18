#version 130
uniform mat4 modelView,proj;
uniform vec3 baseColor;

in  vec3 position,in_color;
out vec3 color;

void main()
{
    color       = in_color * baseColor;
    gl_Position = vec4(position,1.0) * modelView * proj;
}
