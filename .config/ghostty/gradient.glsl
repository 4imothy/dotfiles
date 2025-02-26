void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord.xy / iResolution.xy;

    float gradientFactor = uv.x * 0.9;

    vec3 gradientStartColor = vec3(48.0, 52.0, 70.0) / 255;
    vec3 gradientEndColor = vec3(24.0, 24.0, 37.0) / 255;

    vec3 gradientColor = mix(gradientStartColor, gradientEndColor, gradientFactor);

    vec4 terminalColor = texture(iChannel0, uv);

    float mask = 1 - step(0.6, dot(terminalColor.rgb, vec3(1.0)));
    vec3 blendedColor = mix(terminalColor.rgb, gradientColor, mask);

    fragColor = vec4(blendedColor, terminalColor.a);
}

