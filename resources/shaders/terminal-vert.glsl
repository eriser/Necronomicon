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

float range = 1.0;

vec3 toPosition(vec3 pos, float a1, float a2)
{
    float xRads = pos.x * 0.0174532925;
    float yRads = pos.y * 0.0174532925;
    return vec3(sin(yRads + a1), sin(xRads + a2), cos(xRads + a1 - yRads + a2)) * range;
}

void main()
{
    uv    = position.xy;
    color = in_color;
    if(is_active > 0)
    {
        float a1         = texture1D(tex, uv.x * 0.5).r;
        float a2         = texture1D(tex, uv.y * 0.5).r;
        vec3 newPosition = toPosition(position, a1, a2);
        gl_Position      = vec4(position + newPosition, 1.0) * modelView * proj;
    }
    else
    {
        gl_Position = vec4(position, 1.0) * modelView * proj;
    }
}
