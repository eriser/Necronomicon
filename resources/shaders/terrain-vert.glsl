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

float ffade(float n)
{
    return n * n * n * ( n * ( n * 6.0 - 15.0 ) + 10.0 );
}

float flerp(float t, float a, float b)
{
    return a + t * (b - a);
}

int perm[256] = int[256] (
    151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,
    225,140,36,103,30,69,142,8,99,37,240,21,10,23,190,
    6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
    35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,
    171,168, 68,175,74,165,71,134,139,48,27,166,77,146,
    158,231,83,111,229,122,60,211,133,230,220,105,92,41,
    55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,
    209,76,132,187,208, 89,18,169,200,196,135,130,116,188,
    159,86,164,100,109,198,173,186,3,64,52,217,226,250,124,
    123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,
    47,16,58,17,182,189,28,42,223,183,170,213,119,248,152,
    2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,
    39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,
    246,97,228,251,34,242,193,238,210,144,12,191,179,162,241,
    81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,
    157,184,84,204,176,115,121,50,45,127,4,150,254,138,236,
    205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,
    61,156,180);

float gradient(float x, float y, int hash)
{
    int   h, h2, h3;
    float u, v, h_less_than_4, h2_greater_than_0, h3_greater_than_0;

    h  = hash & 7;
    h2 = h    & 1;
    h3 = h    & 2;

    h_less_than_4 = float(h < 4);
    u = x * h_less_than_4 + y * (1 - h_less_than_4);
    v = y * h_less_than_4 + x * (1 - h_less_than_4);

    h2_greater_than_0 = float(h2 > 0);
    u = -u * h2_greater_than_0 + u * (1 - h2_greater_than_0);

    h3_greater_than_0 = float(h2 > 0);
    u = -v * h3_greater_than_0 + v * (1 - h3_greater_than_0);

    return u + 2 * v;
}

float simplex2(float x, float y)
{
    int   ix0,iy0,ix1,iy1;
    float fx0,fy0,fx1,fy1,s,t,nx0,nx1,n0,n1;

    ix0 = int(x);          // Integer part of x
    iy0 = int(y);	       // Integer part of y
    fx0 = x - ix0;         // Fractional part of x
    fy0 = y - iy0;         // Fractional part of y
    fx1 = fx0 - 1.0;
    fy1	= fy0 - 1.0;
    ix1 = (ix0 + 1) & 255; // Wrapped to 0..255
    iy1 = (iy0 + 1) & 255;
    ix0 = ix0 & 255;
    iy0 = iy0 & 255;

    t = ffade(fy0);
    s = ffade(fx0);

    nx0 = gradient(fx0, fy0, perm[(ix0 + perm[iy0]) & 255]);
    nx1 = gradient(fx0, fy1, perm[(ix0 + perm[iy1]) & 255]);
    n0	= flerp(t, nx0, nx1);

    nx0 = gradient(fx1, fy0, perm[(ix1 + perm[iy0]) & 255]);
    nx1 = gradient(fx1, fy1, perm[(ix1 + perm[iy1]) & 255]);
    n1  = flerp(t, nx0, nx1);

    return 0.507 * flerp(s, n0, n1);
}

float simplex(float x, float y)
{
    // return simplex2(8 * x, 4 * y) + simplex2(4 * x, 2 * y) + simplex2(2 * x, 1 * y);
    //This is off for aesthetic reasons, normally the x and y are matched.
    return simplex2(16 * x, 16 * y) * 0.25 + simplex2(8 * x, 8 * y) * 0.5 + simplex2(4 * x, 4 * y) + simplex2(2 * x, 2 * y) * 2;
}

float columns = 1/ 256.0;
float rows    = 1/ 256.0;

void main()
{
    uv          = in_uv;

    //not quite right
    //transmit uvs from haskell land packed into x?
    //IS THIS CLIPPING!?!?!?
    float a1    = texture1D(tex1, uv.x * 1).r * 0.5;
    float a2    = texture1D(tex2, uv.x * 1).r * 0.5;
    float a3    = texture1D(tex3, uv.x * 1).r * 0.5;

    vec3  pos   = vec3(position.x , position.y + a1 + a2 + a3, position.z );

    float x     = (pos.x * 6) * columns;
    float z     = (pos.z * 6) * rows;
    float y     = (pos.y * 1) + simplex(x * 0.5 + time * 0.05, z * 0.5 + time * 0.041) * 1.65;

    color       = vec3((x * 1.75) * (y * 0.5 + 0.2), y * 0.5 + 0.2, z * (y * 0.5 + 0.2));

    gl_Position = vec4(pos.x, y, pos.z, 1.0) * modelView * proj;
}
