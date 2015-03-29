#version 130
uniform sampler1D tex1;
uniform sampler1D tex2;
uniform sampler1D tex3;
uniform float time;
uniform mat4  modelView,proj;

in  vec3 position,in_color;
in  vec2 in_uv;

out vec3 color;
out vec2 uv;

vec3 toPosition(vec3 pos)
{
    float xRads = pos.x * 0.0174532925;
    float yRads = pos.y * 0.0174532925;

    float a1    = texture1D(tex1, pos.z * 0.5).r * 0.5;
    float a2    = texture1D(tex2, pos.z * 0.5).r * 0.5;
    float a3    = texture1D(tex3, pos.z * 0.5).r * 0.5;

    return vec3(sin(yRads + a1) * sin(xRads + a3 * a2 * 0.25) , cos(yRads + (a2 - a1) * 0.5) , sin(yRads + a3) * cos(xRads + a2 * -a1 * 0.25) ) * 5.5;
}

void main()
{
    uv               = in_uv;
    vec3 newPosition = toPosition(position);
    color            = ((newPosition / 5.5) + 0.5) * 0.5;
    gl_Position      = vec4(newPosition, 1.0) * modelView * proj;
}
