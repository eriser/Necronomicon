#version 130
uniform sampler2D tex;

in  vec3 color;
in  vec2 uv;
out vec4 fragColor;

const int   stepCount   = 4;
const float gWeights[4] = float[](0.44908,0.05092,0.05092,0.44908);
const float gOffsets[4] = float[](0.53805,2.06278,2.06278,0.53805);
const float offset      = 0.00333333333 * 1;
const float halfOffset  = offset * 0.5;

vec4 gaussianBlur( sampler2D tex0, vec2 centreUV, vec2 halfPixelOffset, vec2 pixelOffset )
{
    vec4 colOut = vec4(0,0,0,0);

    for( int i = 0; i < stepCount; i++ )
    {
        vec2 texCoordOffset = gOffsets[i] * pixelOffset;
        vec3 col = texture2D(tex0,centreUV + texCoordOffset).xyz + texture2D(tex0,centreUV - texCoordOffset).xyz;
        colOut.xyz += gWeights[i] * col;
    }
    return colOut * 0.5;
}

void main()
{
    vec4  blurredTex = gaussianBlur(tex,uv,vec2(halfOffset,halfOffset),vec2(offset,offset));
    float brightness = (blurredTex.x + blurredTex.y + blurredTex.z);
    vec4  normalTex  = texture2D(tex,uv);
    fragColor = vec4(color,1.0) * (normalTex + (blurredTex + vec4(brightness,brightness,brightness,0.0)));
}
