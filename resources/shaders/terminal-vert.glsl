#version 130
uniform sampler1D tex;
uniform float     arg1;
uniform float     arg2;
uniform float     is_active;
uniform mat4      modelView,proj;

in  vec3 position, in_color;
in  vec2 in_uv;

out vec3 color;
out vec2 uv;

vec3 toPosition()
{
    float a1, a2, a3;
    if(position.x >= 0)
    {
        a1 = texture1D(tex, abs(position.x) * 1.0).r;
    }
    else
    {
        a1 = texture1D(tex, abs(position.x) * -1.0).r;
    }
    if(position.y >= 0)
    {
        a2 = texture1D(tex, abs(position.y) * 1.0).r;
    }
    else
    {
        a2 = texture1D(tex, abs(position.y) * -1.0).r;
    }
    /* float a2 = texture1D(tex, position.y * 1.0).r; */
    /* float a3 = texture1D(tex, position.z * 2.0).r; */
    return vec3(a1, a2, 0) * 4;
}

void main()
{
    uv    = position.xy;
    color = in_color;
    if(is_active > 0)
    {
        vec3 newPosition = toPosition();
        gl_Position      = vec4(position + newPosition, 1.0) * modelView * proj;
    }
    else
    {
        gl_Position = vec4(position, 1.0) * modelView * proj;
    }
}
