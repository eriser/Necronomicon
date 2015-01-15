#version 130
uniform vec4 mv1,mv2,mv3,mv4;
uniform vec4 pr1,pr2,pr3,pr4;

in  vec3  position;
in  vec3  in_color;
in  vec2  in_uv;

out vec3 color;
out vec2 uv;

void main()
{
    mat4 modelView = mat4(mv1,mv2,mv3,mv4);
    mat4 proj      = mat4(pr1,pr2,pr3,pr4);
    color          = in_color;
    uv             = in_uv;
    gl_Position    = vec4(position,1.0) * modelView * proj;
}
