#version 130
uniform vec4 mv1,mv2,mv3,mv4;
uniform vec4 pr1,pr2,pr3,pr4;

in  vec3 position;
in  vec3 in_color;
out vec3 color;

void main()
{
    mat4 modelView = mat4(mv1,mv2,mv3,mv4);
    mat4 proj      = mat4(pr1,pr2,pr3,pr4);

    color       = in_color;
    gl_Position = vec4(position,1.0) * modelView * proj;
}
