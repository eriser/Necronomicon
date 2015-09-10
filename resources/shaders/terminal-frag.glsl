#version 130
uniform sampler1D tex;
uniform float     arg1;
uniform float     arg2;
uniform float     is_active;

in  vec4 color;
in  vec2 uv;
out vec4 fragColor;

void main()
{
    if(is_active > 0)
    {
        fragColor = color;
    }
    else
    {
        fragColor = vec4(0, 0, 0, 0);
    }
}
