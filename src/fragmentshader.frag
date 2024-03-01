#version 330 core
in vec3 ourColor;
out vec4 FragColor;
out vec4 color;

void main()
{
    // color = vec4(ourColor, 1.0f);
    // FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    FragColor = vec4(ourColor, 1.0f); 
}
