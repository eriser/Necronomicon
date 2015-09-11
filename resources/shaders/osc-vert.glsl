#version 130
uniform sampler1D tex1;
uniform sampler1D tex2;
uniform sampler1D tex3;
uniform float     time;
uniform mat4      modelView, proj;

in  vec3 position, in_color;
in  vec2 in_uv;

out vec3 color;
out vec2 uv;

float width   = 1;
float oscSize = 1;

vec3 toPosition(vec3 pos)
{
    vec3 p0 = vec3((texture1D(tex1, pos.y).r - 0.5) * oscSize, (texture1D(tex2, pos.y).r - 0.5) * oscSize, (texture1D(tex3, pos.y).r - 0.5) * oscSize);
    vec3 p1 = vec3((texture1D(tex1, pos.z).r - 0.5) * oscSize, (texture1D(tex2, pos.z).r - 0.5) * oscSize, (texture1D(tex3, pos.z).r - 0.5) * oscSize);
    vec3 cp = cross(normalize(p0), normalize(p1));
    vec3 p2 = p0 + cp * width;
    vec3 p3 = p1 + cp * width;

    return p0 * float(pos.x == 0) + p1 * float(pos.x == 1) + p2 * float(pos.x == 2) + p3 * float(pos.x == 3);
}

void main()
{
    uv               = in_uv;
    vec3 newPosition = toPosition(position);
    color           = (normalize(newPosition) + 0.5) * 0.5;
    /* color            = vec3(1, 1, 1); */
    gl_Position      = vec4(newPosition, 1.0) * modelView * proj;
}
