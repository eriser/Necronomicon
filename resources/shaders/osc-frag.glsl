#version 130
uniform sampler1D tex1;
uniform sampler1D tex2;
uniform sampler1D tex3;

in  vec3 color;
in  vec2 uv;
out vec4 fragColor;

void main()
{
    fragColor = vec4(color,0.35);
}
