void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec3 bgColor = vec3(48.0, 52.0, 70.0) / 255;

    vec4 terminalColor = texture(iChannel0, fragCoord.xy / iResolution.xy);
    float mask = 1 - step(0.6, dot(terminalColor.rgb, vec3(1.0)));
    vec3 blendedColor = mix(terminalColor.rgb, bgColor, mask);

    fragColor = vec4(blendedColor, terminalColor.a);
}
