/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
 */

#include <stdio.h>
#include <GL/glew.h>
#include <stdlib.h>

typedef struct
{
    GLuint t;
    GLuint u;
} texture_t;

typedef struct
{
    GLfloat x;
} scalar_t;

typedef struct
{
    GLfloat x;
    GLfloat y;
} vec2_t;

typedef struct
{
    GLfloat x;
    GLfloat y;
    GLfloat z;
} vec3_t;

typedef struct
{
    GLfloat x;
    GLfloat y;
    GLfloat z;
    GLfloat w;
} vec4_t;

typedef struct
{
    GLint type;
    GLint location;
    union
    {
        texture_t texture_uniform;
        scalar_t  scalar_uniform;
        vec2_t    vec2_uniform;
        vec3_t    vec3_uniform;
        vec4_t    vec4_uniform;
    };
} uniform_t;

typedef struct
{
    GLuint is_active;
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint start;
    GLuint end;
    GLint  count;

    GLint    vertex_vad_n;
    GLint    vertex_vad_s;
    GLfloat* vertex_vad_p;

    GLint    color_vad_n;
    GLint    color_vad_s;
    GLfloat* color_vad_p;

    GLint    uv_vad_n;
    GLint    uv_vad_s;
    GLfloat* uv_vad_p;

    GLuint shader_program;
    GLuint vertex_attribute_location;
    GLuint color_attribute_location;
    GLuint uv_attribute_location;

    GLfloat m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33;
    GLint uniform_length;
    GLint padding;
    uniform_t* uniforms;

    GLint model_View_location;
    GLint proj_location;
} render_data_t;

GLfloat* model_view_ptr;

void init_c_opengl()
{
    GLenum err = glewInit();
    if(GLEW_OK != err)
    {
        /* Problem: glewInit failed, something is seriously wrong. */
        fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
    }
    else
    {
        fprintf(stdout, "Status: Using GLEW %s\n", glewGetString(GLEW_VERSION));
    }

    model_view_ptr = malloc(sizeof(GLfloat) * 16);
}

void draw_render_data( render_data_t* render_data, GLuint length
                     , GLfloat v00, GLfloat v01, GLfloat v02, GLfloat v03
                     , GLfloat v10, GLfloat v11, GLfloat v12, GLfloat v13
                     , GLfloat v20, GLfloat v21, GLfloat v22, GLfloat v23
                     , GLfloat v30, GLfloat v31, GLfloat v32, GLfloat v33
                     , GLfloat* proj_ptr)
{
    GLuint i;
    GLuint u;
    uniform_t* uniforms;
    render_data_t rd;
    uniform_t uniform;
    int render_num = 0;
    for(i = 0; i < length; i++)
    {
        if(!render_data->is_active)
        {
            render_data++;
            continue;
        }

        render_num++;

        rd = *render_data;
        glUseProgram(rd.shader_program);
        uniforms = rd.uniforms;

        for(u = 0; u < rd.uniform_length; u++)
        {
            uniform = *uniforms;
            switch(uniform.type)
            {
                case 0:
                    glActiveTexture(uniform.texture_uniform.u);
                    glBindTexture(GL_TEXTURE_2D, uniform.texture_uniform.t);
                    glUniform1i(uniform.location, uniform.texture_uniform.t);
                    break;
                case 1:
                    glUniform1f(uniform.location, uniform.scalar_uniform.x);
                    break;
                case 2:
                    glUniform2f(uniform.location, uniform.vec2_uniform.x, uniform.vec2_uniform.y);
                    break;
                case 3:
                    glUniform3f(uniform.location, uniform.vec3_uniform.x, uniform.vec3_uniform.y, uniform.vec3_uniform.z);
                    break;
                case 4:
                    glUniform4f(uniform.location, uniform.vec4_uniform.x, uniform.vec4_uniform.y, uniform.vec4_uniform.z, uniform.vec4_uniform.w);
                    break;
            }
            uniforms++;
        }

        model_view_ptr[0]  = v00 * rd.m00 + v01 * rd.m10 + v02 * rd.m20 + v03 * rd.m30;
        model_view_ptr[1]  = v00 * rd.m01 + v01 * rd.m11 + v02 * rd.m21 + v03 * rd.m31;
        model_view_ptr[2]  = v00 * rd.m02 + v01 * rd.m12 + v02 * rd.m22 + v03 * rd.m32;
        model_view_ptr[3]  = v00 * rd.m03 + v01 * rd.m13 + v02 * rd.m23 + v03 * rd.m33;

        model_view_ptr[4]  = v10 * rd.m00 + v11 * rd.m10 + v12 * rd.m20 + v13 * rd.m30;
        model_view_ptr[5]  = v10 * rd.m01 + v11 * rd.m11 + v12 * rd.m21 + v13 * rd.m31;
        model_view_ptr[6]  = v10 * rd.m02 + v11 * rd.m12 + v12 * rd.m22 + v13 * rd.m32;
        model_view_ptr[7]  = v10 * rd.m03 + v11 * rd.m13 + v12 * rd.m23 + v13 * rd.m33;

        model_view_ptr[8]  = v20 * rd.m00 + v21 * rd.m10 + v22 * rd.m20 + v23 * rd.m30;
        model_view_ptr[9]  = v20 * rd.m01 + v21 * rd.m11 + v22 * rd.m21 + v23 * rd.m31;
        model_view_ptr[10] = v20 * rd.m02 + v21 * rd.m12 + v22 * rd.m22 + v23 * rd.m32;
        model_view_ptr[11] = v20 * rd.m03 + v21 * rd.m13 + v22 * rd.m23 + v23 * rd.m33;

        model_view_ptr[12] = v30 * rd.m00 + v31 * rd.m10 + v32 * rd.m20 + v33 * rd.m30;
        model_view_ptr[13] = v30 * rd.m01 + v31 * rd.m11 + v32 * rd.m21 + v33 * rd.m31;
        model_view_ptr[14] = v30 * rd.m02 + v31 * rd.m12 + v32 * rd.m22 + v33 * rd.m32;
        model_view_ptr[15] = v30 * rd.m03 + v31 * rd.m13 + v32 * rd.m23 + v33 * rd.m33;

        glUniformMatrix4fv(rd.model_View_location, 1, 0, model_view_ptr);
        glUniformMatrix4fv(rd.proj_location, 1, 0, proj_ptr);

        glBindBuffer(GL_ARRAY_BUFFER, rd.vertex_buffer);

        glVertexAttribPointer(rd.vertex_attribute_location, rd.vertex_vad_n, GL_FLOAT, GL_FALSE, rd.vertex_vad_s, rd.vertex_vad_p);
        glEnableVertexAttribArray(rd.vertex_attribute_location);

        glVertexAttribPointer(rd.color_attribute_location, rd.color_vad_n, GL_FLOAT, GL_FALSE, rd.color_vad_s, rd.color_vad_p);
        glEnableVertexAttribArray(rd.color_attribute_location);

        glVertexAttribPointer(rd.uv_attribute_location, rd.uv_vad_n, GL_FLOAT, GL_FALSE, rd.uv_vad_s, rd.uv_vad_p);
        glEnableVertexAttribArray(rd.uv_attribute_location);

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, rd.index_buffer);
        glDrawRangeElements(GL_TRIANGLES, rd.start, rd.end, rd.count, GL_UNSIGNED_INT, 0);
        render_data++;
    }
}
