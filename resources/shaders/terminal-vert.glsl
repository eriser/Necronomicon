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

float width  = 0.5;
float radius = 0.75;

vec3 toPosition(vec3 pos)
{
    float u1 = pos.y * 8 * 3.141592654;
    float u2 = pos.z * 8 * 3.141592654;

    vec3 p0 = vec3(sin(u1) * radius, (texture1D(tex, pos.y).r - 0.5) * 6, cos(u1) * radius);
    vec3 p1 = vec3(sin(u2) * radius, (texture1D(tex, pos.z).r - 0.5) * 6, cos(u2) * radius);

    vec3 cp = cross(normalize(p0), normalize(p1));
    vec3 p2 = p0 + cp * width;
    vec3 p3 = p1 + cp * width;

    return p0 * float(pos.x == 0) + p1 * float(pos.x == 1) + p2 * float(pos.x == 2) + p3 * float(pos.x == 3);
}

void main()
{
    uv    = in_uv;
    color = in_color;
    if(is_active > 0)
    {
        vec3 newPosition = toPosition(position);
        color            = newPosition;
        gl_Position      = vec4(newPosition, 1.0) * modelView * proj;
    }
    else
    {
        gl_Position = vec4(position, 1.0) * modelView * proj;
    }
}
