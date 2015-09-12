#version 130
uniform sampler2D tex;

in  vec3 color;
in  vec2 uv;
out vec4 fragColor;

const int   stepCount   = 4;
const float gWeights[4] = float[](0.44908,0.05092,0.05092,0.44908);
const float gOffsets[4] = float[](0.53805,2.06278,2.06278,0.53805);
const float offset      = 0.00333333333 * 0.5;
const float halfOffset  = offset * 0.5;

vec4 gaussianBlur(sampler2D tex0, vec2 centreUV, vec2 halfPixelOffset, vec2 pixelOffset)
{
    vec4 colOut = vec4(0,0,0,0);
    for( int i = 0; i < stepCount; i++ )
    {
        vec2 texCoordOffset = gOffsets[i] * pixelOffset;
        vec3 col            = texture2D(tex0, centreUV + texCoordOffset).xyz + texture2D(tex0, centreUV - texCoordOffset).xyz;
        colOut.xyz         += gWeights[i] * col;
    }
    return colOut * 0.5;
}

void main()
{
    vec4  blurredTex = gaussianBlur(tex,uv,vec2(halfOffset,halfOffset),vec2(offset,offset));
    float brightness = (blurredTex.x + blurredTex.y + blurredTex.z);
    vec4  normalTex  = texture2D(tex,uv);
    fragColor        = vec4(color,1.0) * (normalTex + (blurredTex + vec4(brightness,brightness,brightness,0.0)));
}

/* int   rowSize = 7; */
/* const float gaussianKernel[49] = float[]( */
/*         0, 0, 0.001, 0.001, 0.001, 0, 0, */
/*         0, 0.002, 0.012, 0.02, 0.012, 0.002, 0, */
/*         0.001, 0.012, 0.068, 0.109, 0.068, 0.012, 0.001, */
/*         0.001, 0.02, 0.109, 0.172, 0.109, 0.02, 0.001, */
/*         0.001, 0.012, 0.068, 0.109, 0.068, 0.012, 0.001, */
/*         0, 0.002, 0.012, 0.02, 0.012, 0.002, 0, */
/*         0, 0, 0.001, 0.001, 0.001, 0, 0 */
/*         ); */

/* vec3 gaussianBlur() */
/* { */
/*     vec3 accumulator = vec3(0, 0, 0); */
/*     for(int y = 0; y < rowSize; ++y) */
/*     { */
/*         for(int x = 0; x < rowSize; ++x) */
/*         { */
/*             accumulator += texture2D(tex, uv + vec2(offset * (x - 3), offset * (y - 3))).xyz * gaussianKernel[(y * rowSize) + x]; */
/*         } */
/*     } */
/*     return accumulator; */
/* } */

/* void main() */
/* { */
/*     vec3  blurredTex = gaussianBlur(); */
/*     vec4  normalTex  = texture2D(tex,uv); */
/*     float brightness = (blurredTex.x + blurredTex.y + blurredTex.z); */
/*     fragColor        = normalTex + (vec4(blurredTex, 0) + vec4(brightness, brightness, brightness, 0.0)); */
/* } */
