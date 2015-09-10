#version 130

in  vec3 color;
in  vec2 uv;
in  vec3 pos;
out vec4 fragColor;

float thickness = 0.01;
float alpha     = 0.15;
bool within_range(float x)
{
    return x < thickness && x > -thickness;
}

void main()
{
    if(within_range(pos.x))
    {
        fragColor = vec4(color, alpha);
    }
    else if(within_range(pos.z))
    {
        fragColor = vec4(color, alpha);
    }
    else if(within_range(pos.x))
    {
        fragColor = vec4(color, alpha);
    }
    else
    {
        fragColor = vec4(0, 0, 0, 0);
    }
}

