#version 130
uniform sampler2D tex;

in vec3  color;
in vec2  uv;
out vec4 fragColor;

void main()
{
    fragColor = vec4(uv.xy,0.0,1.0);
}
