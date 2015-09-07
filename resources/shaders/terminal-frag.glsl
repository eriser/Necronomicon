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
        /* float a1  = (texture1D(tex, uv.x * 1.0).r * 4.0) + 0.5; */
        /* float a2  = (texture1D(tex, uv.y * 1.0).r * 4.0) + 0.5; */
        /* fragColor = vec4(vec3(arg1 * 1.0, 0, arg2 * 1.0) * vec3(a1, a2, a1), 1.0); */
        fragColor = vec4(color, 1);
    }
    else
    {
        fragColor = vec4(0, 0, 0, 0);
    }
}
