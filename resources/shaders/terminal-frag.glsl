#version 130
uniform sampler1D tex;
uniform float     arg1;
uniform float     arg2;
uniform float     is_active;

in  vec3 color;
in  vec2 uv;
out vec4 fragColor;

void main()
{
    if(is_active > 0)
    {
        float a1  = (texture1D(tex, uv.x * 0.5).r * 0.5) + 0.5;
        float a2  = (texture1D(tex, uv.y * 0.5).r * 0.5) + 0.5;
        fragColor = vec4(vec3(arg1 * 1.0, 0, arg2 * 1.0) * vec3(a1, a2, a1), 1.0);
    }
    else
    {
        fragColor = vec4(0.1, 0.1, 0.1, 0.5);
    }
}
