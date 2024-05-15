#include <raylib.h>
#include <libguile.h>
#include <string.h>

// struct slots
static SCM rgtype_Vector2;
static SCM rgtype_Vector3;
static SCM rgtype_Vector4;
static SCM rgtype_Matrix;
static SCM rgtype_Color;
static SCM rgtype_Rectangle;
static SCM rgtype_Image;
static SCM rgtype_Texture;
static SCM rgtype_RenderTexture;
static SCM rgtype_NPatchInfo;
static SCM rgtype_GlyphInfo;
static SCM rgtype_Font;
static SCM rgtype_Camera3D;
static SCM rgtype_Camera2D;
static SCM rgtype_Mesh;
static SCM rgtype_Shader;
static SCM rgtype_MaterialMap;
static SCM rgtype_Material;
static SCM rgtype_Transform;
static SCM rgtype_BoneInfo;
static SCM rgtype_Model;
static SCM rgtype_ModelAnimation;
static SCM rgtype_Ray;
static SCM rgtype_RayCollision;
static SCM rgtype_BoundingBox;
static SCM rgtype_Wave;
static SCM rgtype_AudioStream;
static SCM rgtype_Sound;
static SCM rgtype_Music;
static SCM rgtype_VrDeviceInfo;
static SCM rgtype_VrStereoConfig;
static SCM rgtype_FilePathList;
static SCM rgtype_AutomationEvent;
static SCM rgtype_AutomationEventList;

// struct accessors
SCM rgacc_make_Vector2(SCM x, SCM y) {
    scm_dynwind_begin(0);
    Vector2 *rg_data = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    rg_data->x = scm_to_double(x);
    rg_data->y = scm_to_double(y);
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Vector2_x(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector2, _obj);
    return scm_from_double((*(Vector2*)scm_foreign_object_ref(_obj, 0)).x);
}

SCM rgacc_Vector2_y(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector2, _obj);
    return scm_from_double((*(Vector2*)scm_foreign_object_ref(_obj, 0)).y);
}

SCM rgacc_Vector2_set_x(SCM _obj, SCM x) {
    scm_assert_foreign_object_type(rgtype_Vector2, _obj);
    (*(Vector2*)scm_foreign_object_ref(_obj, 0)).x = scm_to_double(x);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector2_set_y(SCM _obj, SCM y) {
    scm_assert_foreign_object_type(rgtype_Vector2, _obj);
    (*(Vector2*)scm_foreign_object_ref(_obj, 0)).y = scm_to_double(y);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Vector3(SCM x, SCM y, SCM z) {
    scm_dynwind_begin(0);
    Vector3 *rg_data = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    rg_data->x = scm_to_double(x);
    rg_data->y = scm_to_double(y);
    rg_data->z = scm_to_double(z);
    SCM result = scm_make_foreign_object_1(rgtype_Vector3, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Vector3_x(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    return scm_from_double((*(Vector3*)scm_foreign_object_ref(_obj, 0)).x);
}

SCM rgacc_Vector3_y(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    return scm_from_double((*(Vector3*)scm_foreign_object_ref(_obj, 0)).y);
}

SCM rgacc_Vector3_z(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    return scm_from_double((*(Vector3*)scm_foreign_object_ref(_obj, 0)).z);
}

SCM rgacc_Vector3_set_x(SCM _obj, SCM x) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    (*(Vector3*)scm_foreign_object_ref(_obj, 0)).x = scm_to_double(x);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector3_set_y(SCM _obj, SCM y) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    (*(Vector3*)scm_foreign_object_ref(_obj, 0)).y = scm_to_double(y);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector3_set_z(SCM _obj, SCM z) {
    scm_assert_foreign_object_type(rgtype_Vector3, _obj);
    (*(Vector3*)scm_foreign_object_ref(_obj, 0)).z = scm_to_double(z);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Vector4(SCM x, SCM y, SCM z, SCM w) {
    scm_dynwind_begin(0);
    Vector4 *rg_data = scm_gc_malloc_pointerless(sizeof(Vector4), "raylib-guile ptr");
    rg_data->x = scm_to_double(x);
    rg_data->y = scm_to_double(y);
    rg_data->z = scm_to_double(z);
    rg_data->w = scm_to_double(w);
    SCM result = scm_make_foreign_object_1(rgtype_Vector4, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Vector4_x(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    return scm_from_double((*(Vector4*)scm_foreign_object_ref(_obj, 0)).x);
}

SCM rgacc_Vector4_y(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    return scm_from_double((*(Vector4*)scm_foreign_object_ref(_obj, 0)).y);
}

SCM rgacc_Vector4_z(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    return scm_from_double((*(Vector4*)scm_foreign_object_ref(_obj, 0)).z);
}

SCM rgacc_Vector4_w(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    return scm_from_double((*(Vector4*)scm_foreign_object_ref(_obj, 0)).w);
}

SCM rgacc_Vector4_set_x(SCM _obj, SCM x) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    (*(Vector4*)scm_foreign_object_ref(_obj, 0)).x = scm_to_double(x);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector4_set_y(SCM _obj, SCM y) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    (*(Vector4*)scm_foreign_object_ref(_obj, 0)).y = scm_to_double(y);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector4_set_z(SCM _obj, SCM z) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    (*(Vector4*)scm_foreign_object_ref(_obj, 0)).z = scm_to_double(z);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Vector4_set_w(SCM _obj, SCM w) {
    scm_assert_foreign_object_type(rgtype_Vector4, _obj);
    (*(Vector4*)scm_foreign_object_ref(_obj, 0)).w = scm_to_double(w);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Matrix() {
    scm_dynwind_begin(0);
    Matrix *rg_data = scm_gc_malloc_pointerless(sizeof(Matrix), "raylib-guile ptr");
    SCM result = scm_make_foreign_object_1(rgtype_Matrix, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Matrix_m0(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m0);
}

SCM rgacc_Matrix_m4(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m4);
}

SCM rgacc_Matrix_m8(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m8);
}

SCM rgacc_Matrix_m12(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m12);
}

SCM rgacc_Matrix_m1(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m1);
}

SCM rgacc_Matrix_m5(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m5);
}

SCM rgacc_Matrix_m9(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m9);
}

SCM rgacc_Matrix_m13(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m13);
}

SCM rgacc_Matrix_m2(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m2);
}

SCM rgacc_Matrix_m6(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m6);
}

SCM rgacc_Matrix_m10(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m10);
}

SCM rgacc_Matrix_m14(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m14);
}

SCM rgacc_Matrix_m3(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m3);
}

SCM rgacc_Matrix_m7(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m7);
}

SCM rgacc_Matrix_m11(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m11);
}

SCM rgacc_Matrix_m15(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    return scm_from_double((*(Matrix*)scm_foreign_object_ref(_obj, 0)).m15);
}

SCM rgacc_Matrix_set_m0(SCM _obj, SCM m0) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m0 = scm_to_double(m0);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m4(SCM _obj, SCM m4) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m4 = scm_to_double(m4);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m8(SCM _obj, SCM m8) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m8 = scm_to_double(m8);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m12(SCM _obj, SCM m12) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m12 = scm_to_double(m12);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m1(SCM _obj, SCM m1) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m1 = scm_to_double(m1);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m5(SCM _obj, SCM m5) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m5 = scm_to_double(m5);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m9(SCM _obj, SCM m9) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m9 = scm_to_double(m9);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m13(SCM _obj, SCM m13) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m13 = scm_to_double(m13);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m2(SCM _obj, SCM m2) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m2 = scm_to_double(m2);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m6(SCM _obj, SCM m6) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m6 = scm_to_double(m6);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m10(SCM _obj, SCM m10) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m10 = scm_to_double(m10);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m14(SCM _obj, SCM m14) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m14 = scm_to_double(m14);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m3(SCM _obj, SCM m3) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m3 = scm_to_double(m3);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m7(SCM _obj, SCM m7) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m7 = scm_to_double(m7);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m11(SCM _obj, SCM m11) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m11 = scm_to_double(m11);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Matrix_set_m15(SCM _obj, SCM m15) {
    scm_assert_foreign_object_type(rgtype_Matrix, _obj);
    (*(Matrix*)scm_foreign_object_ref(_obj, 0)).m15 = scm_to_double(m15);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Color(SCM r, SCM g, SCM b, SCM a) {
    scm_dynwind_begin(0);
    Color *rg_data = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    rg_data->r = scm_to_uchar(r);
    rg_data->g = scm_to_uchar(g);
    rg_data->b = scm_to_uchar(b);
    rg_data->a = scm_to_uchar(a);
    SCM result = scm_make_foreign_object_1(rgtype_Color, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Color_r(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    return scm_from_uchar((*(Color*)scm_foreign_object_ref(_obj, 0)).r);
}

SCM rgacc_Color_g(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    return scm_from_uchar((*(Color*)scm_foreign_object_ref(_obj, 0)).g);
}

SCM rgacc_Color_b(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    return scm_from_uchar((*(Color*)scm_foreign_object_ref(_obj, 0)).b);
}

SCM rgacc_Color_a(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    return scm_from_uchar((*(Color*)scm_foreign_object_ref(_obj, 0)).a);
}

SCM rgacc_Color_set_r(SCM _obj, SCM r) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    (*(Color*)scm_foreign_object_ref(_obj, 0)).r = scm_to_uchar(r);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Color_set_g(SCM _obj, SCM g) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    (*(Color*)scm_foreign_object_ref(_obj, 0)).g = scm_to_uchar(g);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Color_set_b(SCM _obj, SCM b) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    (*(Color*)scm_foreign_object_ref(_obj, 0)).b = scm_to_uchar(b);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Color_set_a(SCM _obj, SCM a) {
    scm_assert_foreign_object_type(rgtype_Color, _obj);
    (*(Color*)scm_foreign_object_ref(_obj, 0)).a = scm_to_uchar(a);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Rectangle(SCM x, SCM y, SCM width, SCM height) {
    scm_dynwind_begin(0);
    Rectangle *rg_data = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    rg_data->x = scm_to_double(x);
    rg_data->y = scm_to_double(y);
    rg_data->width = scm_to_double(width);
    rg_data->height = scm_to_double(height);
    SCM result = scm_make_foreign_object_1(rgtype_Rectangle, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Rectangle_x(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    return scm_from_double((*(Rectangle*)scm_foreign_object_ref(_obj, 0)).x);
}

SCM rgacc_Rectangle_y(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    return scm_from_double((*(Rectangle*)scm_foreign_object_ref(_obj, 0)).y);
}

SCM rgacc_Rectangle_width(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    return scm_from_double((*(Rectangle*)scm_foreign_object_ref(_obj, 0)).width);
}

SCM rgacc_Rectangle_height(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    return scm_from_double((*(Rectangle*)scm_foreign_object_ref(_obj, 0)).height);
}

SCM rgacc_Rectangle_set_x(SCM _obj, SCM x) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    (*(Rectangle*)scm_foreign_object_ref(_obj, 0)).x = scm_to_double(x);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Rectangle_set_y(SCM _obj, SCM y) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    (*(Rectangle*)scm_foreign_object_ref(_obj, 0)).y = scm_to_double(y);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Rectangle_set_width(SCM _obj, SCM width) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    (*(Rectangle*)scm_foreign_object_ref(_obj, 0)).width = scm_to_double(width);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Rectangle_set_height(SCM _obj, SCM height) {
    scm_assert_foreign_object_type(rgtype_Rectangle, _obj);
    (*(Rectangle*)scm_foreign_object_ref(_obj, 0)).height = scm_to_double(height);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Texture(SCM id, SCM width, SCM height, SCM mipmaps, SCM format) {
    scm_dynwind_begin(0);
    Texture *rg_data = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    rg_data->id = scm_to_uint(id);
    rg_data->width = scm_to_int(width);
    rg_data->height = scm_to_int(height);
    rg_data->mipmaps = scm_to_int(mipmaps);
    rg_data->format = scm_to_int(format);
    SCM result = scm_make_foreign_object_1(rgtype_Texture, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Texture_id(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    return scm_from_uint((*(Texture*)scm_foreign_object_ref(_obj, 0)).id);
}

SCM rgacc_Texture_width(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    return scm_from_int((*(Texture*)scm_foreign_object_ref(_obj, 0)).width);
}

SCM rgacc_Texture_height(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    return scm_from_int((*(Texture*)scm_foreign_object_ref(_obj, 0)).height);
}

SCM rgacc_Texture_mipmaps(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    return scm_from_int((*(Texture*)scm_foreign_object_ref(_obj, 0)).mipmaps);
}

SCM rgacc_Texture_format(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    return scm_from_int((*(Texture*)scm_foreign_object_ref(_obj, 0)).format);
}

SCM rgacc_Texture_set_id(SCM _obj, SCM id) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    (*(Texture*)scm_foreign_object_ref(_obj, 0)).id = scm_to_uint(id);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Texture_set_width(SCM _obj, SCM width) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    (*(Texture*)scm_foreign_object_ref(_obj, 0)).width = scm_to_int(width);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Texture_set_height(SCM _obj, SCM height) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    (*(Texture*)scm_foreign_object_ref(_obj, 0)).height = scm_to_int(height);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Texture_set_mipmaps(SCM _obj, SCM mipmaps) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    (*(Texture*)scm_foreign_object_ref(_obj, 0)).mipmaps = scm_to_int(mipmaps);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Texture_set_format(SCM _obj, SCM format) {
    scm_assert_foreign_object_type(rgtype_Texture, _obj);
    (*(Texture*)scm_foreign_object_ref(_obj, 0)).format = scm_to_int(format);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_RenderTexture(SCM id, SCM texture, SCM depth) {
    scm_dynwind_begin(0);
    RenderTexture *rg_data = scm_gc_malloc_pointerless(sizeof(RenderTexture), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Texture, depth);
    rg_data->id = scm_to_uint(id);
    rg_data->texture = (*(Texture*)scm_foreign_object_ref(texture, 0));
    rg_data->depth = (*(Texture*)scm_foreign_object_ref(depth, 0));
    SCM result = scm_make_foreign_object_1(rgtype_RenderTexture, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_RenderTexture_id(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    return scm_from_uint((*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).id);
}

SCM rgacc_RenderTexture_texture(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    void *v1 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v1_data = (*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).texture;
    memcpy(v1, &v1_data, sizeof(Texture));
    return scm_make_foreign_object_1(rgtype_Texture, v1);
}

SCM rgacc_RenderTexture_depth(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    void *v2 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v2_data = (*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).depth;
    memcpy(v2, &v2_data, sizeof(Texture));
    return scm_make_foreign_object_1(rgtype_Texture, v2);
}

SCM rgacc_RenderTexture_set_id(SCM _obj, SCM id) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    (*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).id = scm_to_uint(id);
    return SCM_UNSPECIFIED;
}

SCM rgacc_RenderTexture_set_texture(SCM _obj, SCM texture) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    (*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).texture = (*(Texture*)scm_foreign_object_ref(texture, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_RenderTexture_set_depth(SCM _obj, SCM depth) {
    scm_assert_foreign_object_type(rgtype_RenderTexture, _obj);
    scm_assert_foreign_object_type(rgtype_Texture, depth);
    (*(RenderTexture*)scm_foreign_object_ref(_obj, 0)).depth = (*(Texture*)scm_foreign_object_ref(depth, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_NPatchInfo(SCM source, SCM left, SCM top, SCM right, SCM bottom, SCM layout) {
    scm_dynwind_begin(0);
    NPatchInfo *rg_data = scm_gc_malloc_pointerless(sizeof(NPatchInfo), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    rg_data->source = (*(Rectangle*)scm_foreign_object_ref(source, 0));
    rg_data->left = scm_to_int(left);
    rg_data->top = scm_to_int(top);
    rg_data->right = scm_to_int(right);
    rg_data->bottom = scm_to_int(bottom);
    rg_data->layout = scm_to_int(layout);
    SCM result = scm_make_foreign_object_1(rgtype_NPatchInfo, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_NPatchInfo_source(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    void *v3 = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    Rectangle v3_data = (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).source;
    memcpy(v3, &v3_data, sizeof(Rectangle));
    return scm_make_foreign_object_1(rgtype_Rectangle, v3);
}

SCM rgacc_NPatchInfo_left(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    return scm_from_int((*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).left);
}

SCM rgacc_NPatchInfo_top(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    return scm_from_int((*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).top);
}

SCM rgacc_NPatchInfo_right(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    return scm_from_int((*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).right);
}

SCM rgacc_NPatchInfo_bottom(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    return scm_from_int((*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).bottom);
}

SCM rgacc_NPatchInfo_layout(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    return scm_from_int((*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).layout);
}

SCM rgacc_NPatchInfo_set_source(SCM _obj, SCM source) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).source = (*(Rectangle*)scm_foreign_object_ref(source, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_NPatchInfo_set_left(SCM _obj, SCM left) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).left = scm_to_int(left);
    return SCM_UNSPECIFIED;
}

SCM rgacc_NPatchInfo_set_top(SCM _obj, SCM top) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).top = scm_to_int(top);
    return SCM_UNSPECIFIED;
}

SCM rgacc_NPatchInfo_set_right(SCM _obj, SCM right) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).right = scm_to_int(right);
    return SCM_UNSPECIFIED;
}

SCM rgacc_NPatchInfo_set_bottom(SCM _obj, SCM bottom) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).bottom = scm_to_int(bottom);
    return SCM_UNSPECIFIED;
}

SCM rgacc_NPatchInfo_set_layout(SCM _obj, SCM layout) {
    scm_assert_foreign_object_type(rgtype_NPatchInfo, _obj);
    (*(NPatchInfo*)scm_foreign_object_ref(_obj, 0)).layout = scm_to_int(layout);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_GlyphInfo(SCM value, SCM offsetX, SCM offsetY, SCM advanceX, SCM image) {
    scm_dynwind_begin(0);
    GlyphInfo *rg_data = scm_gc_malloc_pointerless(sizeof(GlyphInfo), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Image, image);
    rg_data->value = scm_to_int(value);
    rg_data->offsetX = scm_to_int(offsetX);
    rg_data->offsetY = scm_to_int(offsetY);
    rg_data->advanceX = scm_to_int(advanceX);
    rg_data->image = (*(Image*)scm_foreign_object_ref(image, 0));
    SCM result = scm_make_foreign_object_1(rgtype_GlyphInfo, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_GlyphInfo_value(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    return scm_from_int((*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).value);
}

SCM rgacc_GlyphInfo_offsetX(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    return scm_from_int((*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).offsetX);
}

SCM rgacc_GlyphInfo_offsetY(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    return scm_from_int((*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).offsetY);
}

SCM rgacc_GlyphInfo_advanceX(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    return scm_from_int((*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).advanceX);
}

SCM rgacc_GlyphInfo_image(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    void *v4 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v4_data = (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).image;
    memcpy(v4, &v4_data, sizeof(Image));
    return scm_make_foreign_object_1(rgtype_Image, v4);
}

SCM rgacc_GlyphInfo_set_value(SCM _obj, SCM value) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).value = scm_to_int(value);
    return SCM_UNSPECIFIED;
}

SCM rgacc_GlyphInfo_set_offsetX(SCM _obj, SCM offsetX) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).offsetX = scm_to_int(offsetX);
    return SCM_UNSPECIFIED;
}

SCM rgacc_GlyphInfo_set_offsetY(SCM _obj, SCM offsetY) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).offsetY = scm_to_int(offsetY);
    return SCM_UNSPECIFIED;
}

SCM rgacc_GlyphInfo_set_advanceX(SCM _obj, SCM advanceX) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).advanceX = scm_to_int(advanceX);
    return SCM_UNSPECIFIED;
}

SCM rgacc_GlyphInfo_set_image(SCM _obj, SCM image) {
    scm_assert_foreign_object_type(rgtype_GlyphInfo, _obj);
    scm_assert_foreign_object_type(rgtype_Image, image);
    (*(GlyphInfo*)scm_foreign_object_ref(_obj, 0)).image = (*(Image*)scm_foreign_object_ref(image, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Camera3D(SCM position, SCM target, SCM up, SCM fovy, SCM projection) {
    scm_dynwind_begin(0);
    Camera3D *rg_data = scm_gc_malloc_pointerless(sizeof(Camera3D), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, target);
    scm_assert_foreign_object_type(rgtype_Vector3, up);
    rg_data->position = (*(Vector3*)scm_foreign_object_ref(position, 0));
    rg_data->target = (*(Vector3*)scm_foreign_object_ref(target, 0));
    rg_data->up = (*(Vector3*)scm_foreign_object_ref(up, 0));
    rg_data->fovy = scm_to_double(fovy);
    rg_data->projection = scm_to_int(projection);
    SCM result = scm_make_foreign_object_1(rgtype_Camera3D, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Camera3D_position(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    void *v5 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v5_data = (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).position;
    memcpy(v5, &v5_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v5);
}

SCM rgacc_Camera3D_target(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    void *v6 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v6_data = (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).target;
    memcpy(v6, &v6_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v6);
}

SCM rgacc_Camera3D_up(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    void *v7 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v7_data = (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).up;
    memcpy(v7, &v7_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v7);
}

SCM rgacc_Camera3D_fovy(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    return scm_from_double((*(Camera3D*)scm_foreign_object_ref(_obj, 0)).fovy);
}

SCM rgacc_Camera3D_projection(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    return scm_from_int((*(Camera3D*)scm_foreign_object_ref(_obj, 0)).projection);
}

SCM rgacc_Camera3D_set_position(SCM _obj, SCM position) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).position = (*(Vector3*)scm_foreign_object_ref(position, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera3D_set_target(SCM _obj, SCM target) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, target);
    (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).target = (*(Vector3*)scm_foreign_object_ref(target, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera3D_set_up(SCM _obj, SCM up) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, up);
    (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).up = (*(Vector3*)scm_foreign_object_ref(up, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera3D_set_fovy(SCM _obj, SCM fovy) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).fovy = scm_to_double(fovy);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera3D_set_projection(SCM _obj, SCM projection) {
    scm_assert_foreign_object_type(rgtype_Camera3D, _obj);
    (*(Camera3D*)scm_foreign_object_ref(_obj, 0)).projection = scm_to_int(projection);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Camera2D(SCM offset, SCM target, SCM rotation, SCM zoom) {
    scm_dynwind_begin(0);
    Camera2D *rg_data = scm_gc_malloc_pointerless(sizeof(Camera2D), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector2, offset);
    scm_assert_foreign_object_type(rgtype_Vector2, target);
    rg_data->offset = (*(Vector2*)scm_foreign_object_ref(offset, 0));
    rg_data->target = (*(Vector2*)scm_foreign_object_ref(target, 0));
    rg_data->rotation = scm_to_double(rotation);
    rg_data->zoom = scm_to_double(zoom);
    SCM result = scm_make_foreign_object_1(rgtype_Camera2D, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Camera2D_offset(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    void *v8 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v8_data = (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).offset;
    memcpy(v8, &v8_data, sizeof(Vector2));
    return scm_make_foreign_object_1(rgtype_Vector2, v8);
}

SCM rgacc_Camera2D_target(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    void *v9 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v9_data = (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).target;
    memcpy(v9, &v9_data, sizeof(Vector2));
    return scm_make_foreign_object_1(rgtype_Vector2, v9);
}

SCM rgacc_Camera2D_rotation(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    return scm_from_double((*(Camera2D*)scm_foreign_object_ref(_obj, 0)).rotation);
}

SCM rgacc_Camera2D_zoom(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    return scm_from_double((*(Camera2D*)scm_foreign_object_ref(_obj, 0)).zoom);
}

SCM rgacc_Camera2D_set_offset(SCM _obj, SCM offset) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    scm_assert_foreign_object_type(rgtype_Vector2, offset);
    (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).offset = (*(Vector2*)scm_foreign_object_ref(offset, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera2D_set_target(SCM _obj, SCM target) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    scm_assert_foreign_object_type(rgtype_Vector2, target);
    (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).target = (*(Vector2*)scm_foreign_object_ref(target, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera2D_set_rotation(SCM _obj, SCM rotation) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).rotation = scm_to_double(rotation);
    return SCM_UNSPECIFIED;
}

SCM rgacc_Camera2D_set_zoom(SCM _obj, SCM zoom) {
    scm_assert_foreign_object_type(rgtype_Camera2D, _obj);
    (*(Camera2D*)scm_foreign_object_ref(_obj, 0)).zoom = scm_to_double(zoom);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_MaterialMap(SCM texture, SCM color, SCM value) {
    scm_dynwind_begin(0);
    MaterialMap *rg_data = scm_gc_malloc_pointerless(sizeof(MaterialMap), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Color, color);
    rg_data->texture = (*(Texture*)scm_foreign_object_ref(texture, 0));
    rg_data->color = (*(Color*)scm_foreign_object_ref(color, 0));
    rg_data->value = scm_to_double(value);
    SCM result = scm_make_foreign_object_1(rgtype_MaterialMap, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_MaterialMap_texture(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    void *v10 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v10_data = (*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).texture;
    memcpy(v10, &v10_data, sizeof(Texture));
    return scm_make_foreign_object_1(rgtype_Texture, v10);
}

SCM rgacc_MaterialMap_color(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    void *v11 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v11_data = (*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).color;
    memcpy(v11, &v11_data, sizeof(Color));
    return scm_make_foreign_object_1(rgtype_Color, v11);
}

SCM rgacc_MaterialMap_value(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    return scm_from_double((*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).value);
}

SCM rgacc_MaterialMap_set_texture(SCM _obj, SCM texture) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    (*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).texture = (*(Texture*)scm_foreign_object_ref(texture, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_MaterialMap_set_color(SCM _obj, SCM color) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    scm_assert_foreign_object_type(rgtype_Color, color);
    (*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).color = (*(Color*)scm_foreign_object_ref(color, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_MaterialMap_set_value(SCM _obj, SCM value) {
    scm_assert_foreign_object_type(rgtype_MaterialMap, _obj);
    (*(MaterialMap*)scm_foreign_object_ref(_obj, 0)).value = scm_to_double(value);
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Transform(SCM translation, SCM rotation, SCM scale) {
    scm_dynwind_begin(0);
    Transform *rg_data = scm_gc_malloc_pointerless(sizeof(Transform), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector3, translation);
    scm_assert_foreign_object_type(rgtype_Vector4, rotation);
    scm_assert_foreign_object_type(rgtype_Vector3, scale);
    rg_data->translation = (*(Vector3*)scm_foreign_object_ref(translation, 0));
    rg_data->rotation = (*(Vector4*)scm_foreign_object_ref(rotation, 0));
    rg_data->scale = (*(Vector3*)scm_foreign_object_ref(scale, 0));
    SCM result = scm_make_foreign_object_1(rgtype_Transform, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Transform_translation(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    void *v12 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v12_data = (*(Transform*)scm_foreign_object_ref(_obj, 0)).translation;
    memcpy(v12, &v12_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v12);
}

SCM rgacc_Transform_rotation(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    void *v13 = scm_gc_malloc_pointerless(sizeof(Vector4), "raylib-guile ptr");
    Vector4 v13_data = (*(Transform*)scm_foreign_object_ref(_obj, 0)).rotation;
    memcpy(v13, &v13_data, sizeof(Vector4));
    return scm_make_foreign_object_1(rgtype_Vector4, v13);
}

SCM rgacc_Transform_scale(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    void *v14 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v14_data = (*(Transform*)scm_foreign_object_ref(_obj, 0)).scale;
    memcpy(v14, &v14_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v14);
}

SCM rgacc_Transform_set_translation(SCM _obj, SCM translation) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, translation);
    (*(Transform*)scm_foreign_object_ref(_obj, 0)).translation = (*(Vector3*)scm_foreign_object_ref(translation, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Transform_set_rotation(SCM _obj, SCM rotation) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    scm_assert_foreign_object_type(rgtype_Vector4, rotation);
    (*(Transform*)scm_foreign_object_ref(_obj, 0)).rotation = (*(Vector4*)scm_foreign_object_ref(rotation, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Transform_set_scale(SCM _obj, SCM scale) {
    scm_assert_foreign_object_type(rgtype_Transform, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, scale);
    (*(Transform*)scm_foreign_object_ref(_obj, 0)).scale = (*(Vector3*)scm_foreign_object_ref(scale, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Ray(SCM position, SCM direction) {
    scm_dynwind_begin(0);
    Ray *rg_data = scm_gc_malloc_pointerless(sizeof(Ray), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, direction);
    rg_data->position = (*(Vector3*)scm_foreign_object_ref(position, 0));
    rg_data->direction = (*(Vector3*)scm_foreign_object_ref(direction, 0));
    SCM result = scm_make_foreign_object_1(rgtype_Ray, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Ray_position(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Ray, _obj);
    void *v15 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v15_data = (*(Ray*)scm_foreign_object_ref(_obj, 0)).position;
    memcpy(v15, &v15_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v15);
}

SCM rgacc_Ray_direction(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Ray, _obj);
    void *v16 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v16_data = (*(Ray*)scm_foreign_object_ref(_obj, 0)).direction;
    memcpy(v16, &v16_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v16);
}

SCM rgacc_Ray_set_position(SCM _obj, SCM position) {
    scm_assert_foreign_object_type(rgtype_Ray, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    (*(Ray*)scm_foreign_object_ref(_obj, 0)).position = (*(Vector3*)scm_foreign_object_ref(position, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Ray_set_direction(SCM _obj, SCM direction) {
    scm_assert_foreign_object_type(rgtype_Ray, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, direction);
    (*(Ray*)scm_foreign_object_ref(_obj, 0)).direction = (*(Vector3*)scm_foreign_object_ref(direction, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_RayCollision(SCM hit, SCM distance, SCM point, SCM normal) {
    scm_dynwind_begin(0);
    RayCollision *rg_data = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector3, point);
    scm_assert_foreign_object_type(rgtype_Vector3, normal);
    rg_data->hit = scm_to_bool(hit);
    rg_data->distance = scm_to_double(distance);
    rg_data->point = (*(Vector3*)scm_foreign_object_ref(point, 0));
    rg_data->normal = (*(Vector3*)scm_foreign_object_ref(normal, 0));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_RayCollision_hit(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    return scm_from_bool((*(RayCollision*)scm_foreign_object_ref(_obj, 0)).hit);
}

SCM rgacc_RayCollision_distance(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    return scm_from_double((*(RayCollision*)scm_foreign_object_ref(_obj, 0)).distance);
}

SCM rgacc_RayCollision_point(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    void *v17 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v17_data = (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).point;
    memcpy(v17, &v17_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v17);
}

SCM rgacc_RayCollision_normal(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    void *v18 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v18_data = (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).normal;
    memcpy(v18, &v18_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v18);
}

SCM rgacc_RayCollision_set_hit(SCM _obj, SCM hit) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).hit = scm_to_bool(hit);
    return SCM_UNSPECIFIED;
}

SCM rgacc_RayCollision_set_distance(SCM _obj, SCM distance) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).distance = scm_to_double(distance);
    return SCM_UNSPECIFIED;
}

SCM rgacc_RayCollision_set_point(SCM _obj, SCM point) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, point);
    (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).point = (*(Vector3*)scm_foreign_object_ref(point, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_RayCollision_set_normal(SCM _obj, SCM normal) {
    scm_assert_foreign_object_type(rgtype_RayCollision, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, normal);
    (*(RayCollision*)scm_foreign_object_ref(_obj, 0)).normal = (*(Vector3*)scm_foreign_object_ref(normal, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_BoundingBox(SCM min, SCM max) {
    scm_dynwind_begin(0);
    BoundingBox *rg_data = scm_gc_malloc_pointerless(sizeof(BoundingBox), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_Vector3, min);
    scm_assert_foreign_object_type(rgtype_Vector3, max);
    rg_data->min = (*(Vector3*)scm_foreign_object_ref(min, 0));
    rg_data->max = (*(Vector3*)scm_foreign_object_ref(max, 0));
    SCM result = scm_make_foreign_object_1(rgtype_BoundingBox, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_BoundingBox_min(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_BoundingBox, _obj);
    void *v19 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v19_data = (*(BoundingBox*)scm_foreign_object_ref(_obj, 0)).min;
    memcpy(v19, &v19_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v19);
}

SCM rgacc_BoundingBox_max(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_BoundingBox, _obj);
    void *v20 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v20_data = (*(BoundingBox*)scm_foreign_object_ref(_obj, 0)).max;
    memcpy(v20, &v20_data, sizeof(Vector3));
    return scm_make_foreign_object_1(rgtype_Vector3, v20);
}

SCM rgacc_BoundingBox_set_min(SCM _obj, SCM min) {
    scm_assert_foreign_object_type(rgtype_BoundingBox, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, min);
    (*(BoundingBox*)scm_foreign_object_ref(_obj, 0)).min = (*(Vector3*)scm_foreign_object_ref(min, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_BoundingBox_set_max(SCM _obj, SCM max) {
    scm_assert_foreign_object_type(rgtype_BoundingBox, _obj);
    scm_assert_foreign_object_type(rgtype_Vector3, max);
    (*(BoundingBox*)scm_foreign_object_ref(_obj, 0)).max = (*(Vector3*)scm_foreign_object_ref(max, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_make_Sound(SCM stream, SCM frameCount) {
    scm_dynwind_begin(0);
    Sound *rg_data = scm_gc_malloc_pointerless(sizeof(Sound), "raylib-guile ptr");
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    rg_data->stream = (*(AudioStream*)scm_foreign_object_ref(stream, 0));
    rg_data->frameCount = scm_to_uint(frameCount);
    SCM result = scm_make_foreign_object_1(rgtype_Sound, rg_data);
    scm_dynwind_end();
    return result;
}

SCM rgacc_Sound_stream(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Sound, _obj);
    void *v21 = scm_gc_malloc_pointerless(sizeof(AudioStream), "raylib-guile ptr");
    AudioStream v21_data = (*(Sound*)scm_foreign_object_ref(_obj, 0)).stream;
    memcpy(v21, &v21_data, sizeof(AudioStream));
    return scm_make_foreign_object_1(rgtype_AudioStream, v21);
}

SCM rgacc_Sound_frameCount(SCM _obj) {
    scm_assert_foreign_object_type(rgtype_Sound, _obj);
    return scm_from_uint((*(Sound*)scm_foreign_object_ref(_obj, 0)).frameCount);
}

SCM rgacc_Sound_set_stream(SCM _obj, SCM stream) {
    scm_assert_foreign_object_type(rgtype_Sound, _obj);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    (*(Sound*)scm_foreign_object_ref(_obj, 0)).stream = (*(AudioStream*)scm_foreign_object_ref(stream, 0));
    return SCM_UNSPECIFIED;
}

SCM rgacc_Sound_set_frameCount(SCM _obj, SCM frameCount) {
    scm_assert_foreign_object_type(rgtype_Sound, _obj);
    (*(Sound*)scm_foreign_object_ref(_obj, 0)).frameCount = scm_to_uint(frameCount);
    return SCM_UNSPECIFIED;
}


// function definitions
SCM rgfun_InitWindow(SCM width, SCM height, SCM title) {
    scm_dynwind_begin(0);
    char *v22 = scm_to_utf8_stringn(title, NULL);
    scm_dynwind_free(v22);
    SCM result = (InitWindow(scm_to_int(width), scm_to_int(height), v22), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_CloseWindow() {
    scm_dynwind_begin(0);
    SCM result = (CloseWindow(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_WindowShouldClose() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(WindowShouldClose());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowReady() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowReady());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowFullscreen() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowFullscreen());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowHidden() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowHidden());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowMinimized() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowMinimized());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowMaximized() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowMaximized());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowFocused() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowFocused());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowResized() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowResized());
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWindowState(SCM flag) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsWindowState(scm_to_uint(flag)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowState(SCM flags) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowState(scm_to_uint(flags)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ClearWindowState(SCM flags) {
    scm_dynwind_begin(0);
    SCM result = (ClearWindowState(scm_to_uint(flags)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ToggleFullscreen() {
    scm_dynwind_begin(0);
    SCM result = (ToggleFullscreen(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ToggleBorderlessWindowed() {
    scm_dynwind_begin(0);
    SCM result = (ToggleBorderlessWindowed(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_MaximizeWindow() {
    scm_dynwind_begin(0);
    SCM result = (MaximizeWindow(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_MinimizeWindow() {
    scm_dynwind_begin(0);
    SCM result = (MinimizeWindow(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_RestoreWindow() {
    scm_dynwind_begin(0);
    SCM result = (RestoreWindow(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowIcon(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (SetWindowIcon((*(Image*)scm_foreign_object_ref(image, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowIcons(SCM images, SCM count) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, images);
    SCM result = (SetWindowIcons(scm_foreign_object_ref(images, 0), scm_to_int(count)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowTitle(SCM title) {
    scm_dynwind_begin(0);
    char *v23 = scm_to_utf8_stringn(title, NULL);
    scm_dynwind_free(v23);
    SCM result = (SetWindowTitle(v23), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowPosition(SCM x, SCM y) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowPosition(scm_to_int(x), scm_to_int(y)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowMonitor(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowMonitor(scm_to_int(monitor)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowMinSize(SCM width, SCM height) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowMinSize(scm_to_int(width), scm_to_int(height)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowMaxSize(SCM width, SCM height) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowMaxSize(scm_to_int(width), scm_to_int(height)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowSize(SCM width, SCM height) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowSize(scm_to_int(width), scm_to_int(height)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowOpacity(SCM opacity) {
    scm_dynwind_begin(0);
    SCM result = (SetWindowOpacity(scm_to_double(opacity)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetWindowFocused() {
    scm_dynwind_begin(0);
    SCM result = (SetWindowFocused(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetScreenWidth() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetScreenWidth());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetScreenHeight() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetScreenHeight());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRenderWidth() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetRenderWidth());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRenderHeight() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetRenderHeight());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorCount() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorCount());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCurrentMonitor() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetCurrentMonitor());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorPosition(SCM monitor) {
    scm_dynwind_begin(0);
    void *v24 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v24_data = GetMonitorPosition(scm_to_int(monitor));
    memcpy(v24, &v24_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v24);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorWidth(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorWidth(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorHeight(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorHeight(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorPhysicalWidth(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorPhysicalWidth(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorPhysicalHeight(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorPhysicalHeight(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorRefreshRate(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMonitorRefreshRate(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWindowPosition() {
    scm_dynwind_begin(0);
    void *v25 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v25_data = GetWindowPosition();
    memcpy(v25, &v25_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v25);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWindowScaleDPI() {
    scm_dynwind_begin(0);
    void *v26 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v26_data = GetWindowScaleDPI();
    memcpy(v26, &v26_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v26);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMonitorName(SCM monitor) {
    scm_dynwind_begin(0);
    SCM result = scm_from_utf8_string(GetMonitorName(scm_to_int(monitor)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetClipboardText(SCM text) {
    scm_dynwind_begin(0);
    char *v27 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v27);
    SCM result = (SetClipboardText(v27), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetClipboardText() {
    scm_dynwind_begin(0);
    SCM result = scm_from_utf8_string(GetClipboardText());
    scm_dynwind_end();
    return result;
}

SCM rgfun_EnableEventWaiting() {
    scm_dynwind_begin(0);
    SCM result = (EnableEventWaiting(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DisableEventWaiting() {
    scm_dynwind_begin(0);
    SCM result = (DisableEventWaiting(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ShowCursor() {
    scm_dynwind_begin(0);
    SCM result = (ShowCursor(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_HideCursor() {
    scm_dynwind_begin(0);
    SCM result = (HideCursor(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsCursorHidden() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsCursorHidden());
    scm_dynwind_end();
    return result;
}

SCM rgfun_EnableCursor() {
    scm_dynwind_begin(0);
    SCM result = (EnableCursor(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DisableCursor() {
    scm_dynwind_begin(0);
    SCM result = (DisableCursor(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsCursorOnScreen() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsCursorOnScreen());
    scm_dynwind_end();
    return result;
}

SCM rgfun_ClearBackground(SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ClearBackground((*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginDrawing() {
    scm_dynwind_begin(0);
    SCM result = (BeginDrawing(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndDrawing() {
    scm_dynwind_begin(0);
    SCM result = (EndDrawing(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginMode2D(SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera2D, camera);
    SCM result = (BeginMode2D((*(Camera2D*)scm_foreign_object_ref(camera, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndMode2D() {
    scm_dynwind_begin(0);
    SCM result = (EndMode2D(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginMode3D(SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    SCM result = (BeginMode3D((*(Camera3D*)scm_foreign_object_ref(camera, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndMode3D() {
    scm_dynwind_begin(0);
    SCM result = (EndMode3D(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginTextureMode(SCM target) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_RenderTexture, target);
    SCM result = (BeginTextureMode((*(RenderTexture*)scm_foreign_object_ref(target, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndTextureMode() {
    scm_dynwind_begin(0);
    SCM result = (EndTextureMode(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginShaderMode(SCM shader) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    SCM result = (BeginShaderMode((*(Shader*)scm_foreign_object_ref(shader, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndShaderMode() {
    scm_dynwind_begin(0);
    SCM result = (EndShaderMode(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginBlendMode(SCM mode) {
    scm_dynwind_begin(0);
    SCM result = (BeginBlendMode(scm_to_int(mode)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndBlendMode() {
    scm_dynwind_begin(0);
    SCM result = (EndBlendMode(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginScissorMode(SCM x, SCM y, SCM width, SCM height) {
    scm_dynwind_begin(0);
    SCM result = (BeginScissorMode(scm_to_int(x), scm_to_int(y), scm_to_int(width), scm_to_int(height)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndScissorMode() {
    scm_dynwind_begin(0);
    SCM result = (EndScissorMode(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_BeginVrStereoMode(SCM config) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_VrStereoConfig, config);
    SCM result = (BeginVrStereoMode((*(VrStereoConfig*)scm_foreign_object_ref(config, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_EndVrStereoMode() {
    scm_dynwind_begin(0);
    SCM result = (EndVrStereoMode(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadVrStereoConfig(SCM device) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_VrDeviceInfo, device);
    void *v28 = scm_gc_malloc_pointerless(sizeof(VrStereoConfig), "raylib-guile ptr");
    VrStereoConfig v28_data = LoadVrStereoConfig((*(VrDeviceInfo*)scm_foreign_object_ref(device, 0)));
    memcpy(v28, &v28_data, sizeof(VrStereoConfig));
    SCM result = scm_make_foreign_object_1(rgtype_VrStereoConfig, v28);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadVrStereoConfig(SCM config) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_VrStereoConfig, config);
    SCM result = (UnloadVrStereoConfig((*(VrStereoConfig*)scm_foreign_object_ref(config, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadShader(SCM vsFileName, SCM fsFileName) {
    scm_dynwind_begin(0);
    char *v29 = scm_to_utf8_stringn(vsFileName, NULL);
    scm_dynwind_free(v29);
    char *v30 = scm_to_utf8_stringn(fsFileName, NULL);
    scm_dynwind_free(v30);
    void *v31 = scm_gc_malloc_pointerless(sizeof(Shader), "raylib-guile ptr");
    Shader v31_data = LoadShader(v29, v30);
    memcpy(v31, &v31_data, sizeof(Shader));
    SCM result = scm_make_foreign_object_1(rgtype_Shader, v31);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadShaderFromMemory(SCM vsCode, SCM fsCode) {
    scm_dynwind_begin(0);
    char *v32 = scm_to_utf8_stringn(vsCode, NULL);
    scm_dynwind_free(v32);
    char *v33 = scm_to_utf8_stringn(fsCode, NULL);
    scm_dynwind_free(v33);
    void *v34 = scm_gc_malloc_pointerless(sizeof(Shader), "raylib-guile ptr");
    Shader v34_data = LoadShaderFromMemory(v32, v33);
    memcpy(v34, &v34_data, sizeof(Shader));
    SCM result = scm_make_foreign_object_1(rgtype_Shader, v34);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsShaderReady(SCM shader) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    SCM result = scm_from_bool(IsShaderReady((*(Shader*)scm_foreign_object_ref(shader, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetShaderLocation(SCM shader, SCM uniformName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    char *v35 = scm_to_utf8_stringn(uniformName, NULL);
    scm_dynwind_free(v35);
    SCM result = scm_from_int(GetShaderLocation((*(Shader*)scm_foreign_object_ref(shader, 0)), v35));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetShaderLocationAttrib(SCM shader, SCM attribName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    char *v36 = scm_to_utf8_stringn(attribName, NULL);
    scm_dynwind_free(v36);
    SCM result = scm_from_int(GetShaderLocationAttrib((*(Shader*)scm_foreign_object_ref(shader, 0)), v36));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetShaderValueMatrix(SCM shader, SCM locIndex, SCM mat) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    scm_assert_foreign_object_type(rgtype_Matrix, mat);
    SCM result = (SetShaderValueMatrix((*(Shader*)scm_foreign_object_ref(shader, 0)), scm_to_int(locIndex), (*(Matrix*)scm_foreign_object_ref(mat, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetShaderValueTexture(SCM shader, SCM locIndex, SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (SetShaderValueTexture((*(Shader*)scm_foreign_object_ref(shader, 0)), scm_to_int(locIndex), (*(Texture*)scm_foreign_object_ref(texture, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadShader(SCM shader) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Shader, shader);
    SCM result = (UnloadShader((*(Shader*)scm_foreign_object_ref(shader, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetScreenToWorldRay(SCM position, SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    void *v37 = scm_gc_malloc_pointerless(sizeof(Ray), "raylib-guile ptr");
    Ray v37_data = GetScreenToWorldRay((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Camera3D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v37, &v37_data, sizeof(Ray));
    SCM result = scm_make_foreign_object_1(rgtype_Ray, v37);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetScreenToWorldRayEx(SCM position, SCM camera, SCM width, SCM height) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    void *v38 = scm_gc_malloc_pointerless(sizeof(Ray), "raylib-guile ptr");
    Ray v38_data = GetScreenToWorldRayEx((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Camera3D*)scm_foreign_object_ref(camera, 0)), scm_to_int(width), scm_to_int(height));
    memcpy(v38, &v38_data, sizeof(Ray));
    SCM result = scm_make_foreign_object_1(rgtype_Ray, v38);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWorldToScreen(SCM position, SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    void *v39 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v39_data = GetWorldToScreen((*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Camera3D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v39, &v39_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v39);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWorldToScreenEx(SCM position, SCM camera, SCM width, SCM height) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    void *v40 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v40_data = GetWorldToScreenEx((*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Camera3D*)scm_foreign_object_ref(camera, 0)), scm_to_int(width), scm_to_int(height));
    memcpy(v40, &v40_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v40);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWorldToScreen2D(SCM position, SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Camera2D, camera);
    void *v41 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v41_data = GetWorldToScreen2D((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Camera2D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v41, &v41_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v41);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetScreenToWorld2D(SCM position, SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Camera2D, camera);
    void *v42 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v42_data = GetScreenToWorld2D((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Camera2D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v42, &v42_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v42);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCameraMatrix(SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    void *v43 = scm_gc_malloc_pointerless(sizeof(Matrix), "raylib-guile ptr");
    Matrix v43_data = GetCameraMatrix((*(Camera3D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v43, &v43_data, sizeof(Matrix));
    SCM result = scm_make_foreign_object_1(rgtype_Matrix, v43);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCameraMatrix2D(SCM camera) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera2D, camera);
    void *v44 = scm_gc_malloc_pointerless(sizeof(Matrix), "raylib-guile ptr");
    Matrix v44_data = GetCameraMatrix2D((*(Camera2D*)scm_foreign_object_ref(camera, 0)));
    memcpy(v44, &v44_data, sizeof(Matrix));
    SCM result = scm_make_foreign_object_1(rgtype_Matrix, v44);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetTargetFPS(SCM fps) {
    scm_dynwind_begin(0);
    SCM result = (SetTargetFPS(scm_to_int(fps)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFrameTime() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetFrameTime());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTime() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetTime());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFPS() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetFPS());
    scm_dynwind_end();
    return result;
}

SCM rgfun_SwapScreenBuffer() {
    scm_dynwind_begin(0);
    SCM result = (SwapScreenBuffer(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PollInputEvents() {
    scm_dynwind_begin(0);
    SCM result = (PollInputEvents(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_WaitTime(SCM seconds) {
    scm_dynwind_begin(0);
    SCM result = (WaitTime(scm_to_double(seconds)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetRandomSeed(SCM seed) {
    scm_dynwind_begin(0);
    SCM result = (SetRandomSeed(scm_to_uint(seed)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRandomValue(SCM min, SCM max) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetRandomValue(scm_to_int(min), scm_to_int(max)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadRandomSequence(SCM count, SCM min, SCM max) {
    scm_dynwind_begin(0);
    SCM result = scm_from_pointer(LoadRandomSequence(scm_to_uint(count), scm_to_int(min), scm_to_int(max)), NULL);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadRandomSequence(SCM sequence) {
    scm_dynwind_begin(0);
    SCM result = (UnloadRandomSequence(scm_to_pointer(sequence)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_TakeScreenshot(SCM fileName) {
    scm_dynwind_begin(0);
    char *v45 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v45);
    SCM result = (TakeScreenshot(v45), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetConfigFlags(SCM flags) {
    scm_dynwind_begin(0);
    SCM result = (SetConfigFlags(scm_to_uint(flags)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_OpenURL(SCM url) {
    scm_dynwind_begin(0);
    char *v46 = scm_to_utf8_stringn(url, NULL);
    scm_dynwind_free(v46);
    SCM result = (OpenURL(v46), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetTraceLogLevel(SCM logLevel) {
    scm_dynwind_begin(0);
    SCM result = (SetTraceLogLevel(scm_to_int(logLevel)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportDataAsCode(SCM data, SCM dataSize, SCM fileName) {
    scm_dynwind_begin(0);
    char *v47 = scm_to_utf8_stringn(data, NULL);
    scm_dynwind_free(v47);
    char *v48 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v48);
    SCM result = scm_from_bool(ExportDataAsCode(v47, scm_to_int(dataSize), v48));
    scm_dynwind_end();
    return result;
}

SCM rgfun_FileExists(SCM fileName) {
    scm_dynwind_begin(0);
    char *v49 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v49);
    SCM result = scm_from_bool(FileExists(v49));
    scm_dynwind_end();
    return result;
}

SCM rgfun_DirectoryExists(SCM dirPath) {
    scm_dynwind_begin(0);
    char *v50 = scm_to_utf8_stringn(dirPath, NULL);
    scm_dynwind_free(v50);
    SCM result = scm_from_bool(DirectoryExists(v50));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsFileExtension(SCM fileName, SCM ext) {
    scm_dynwind_begin(0);
    char *v51 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v51);
    char *v52 = scm_to_utf8_stringn(ext, NULL);
    scm_dynwind_free(v52);
    SCM result = scm_from_bool(IsFileExtension(v51, v52));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFileLength(SCM fileName) {
    scm_dynwind_begin(0);
    char *v53 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v53);
    SCM result = scm_from_int(GetFileLength(v53));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFileExtension(SCM fileName) {
    scm_dynwind_begin(0);
    char *v54 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v54);
    SCM result = scm_from_utf8_string(GetFileExtension(v54));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFileName(SCM filePath) {
    scm_dynwind_begin(0);
    char *v55 = scm_to_utf8_stringn(filePath, NULL);
    scm_dynwind_free(v55);
    SCM result = scm_from_utf8_string(GetFileName(v55));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFileNameWithoutExt(SCM filePath) {
    scm_dynwind_begin(0);
    char *v56 = scm_to_utf8_stringn(filePath, NULL);
    scm_dynwind_free(v56);
    SCM result = scm_from_utf8_string(GetFileNameWithoutExt(v56));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetDirectoryPath(SCM filePath) {
    scm_dynwind_begin(0);
    char *v57 = scm_to_utf8_stringn(filePath, NULL);
    scm_dynwind_free(v57);
    SCM result = scm_from_utf8_string(GetDirectoryPath(v57));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetPrevDirectoryPath(SCM dirPath) {
    scm_dynwind_begin(0);
    char *v58 = scm_to_utf8_stringn(dirPath, NULL);
    scm_dynwind_free(v58);
    SCM result = scm_from_utf8_string(GetPrevDirectoryPath(v58));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetWorkingDirectory() {
    scm_dynwind_begin(0);
    SCM result = scm_from_utf8_string(GetWorkingDirectory());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetApplicationDirectory() {
    scm_dynwind_begin(0);
    SCM result = scm_from_utf8_string(GetApplicationDirectory());
    scm_dynwind_end();
    return result;
}

SCM rgfun_ChangeDirectory(SCM dir) {
    scm_dynwind_begin(0);
    char *v59 = scm_to_utf8_stringn(dir, NULL);
    scm_dynwind_free(v59);
    SCM result = scm_from_bool(ChangeDirectory(v59));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsPathFile(SCM path) {
    scm_dynwind_begin(0);
    char *v60 = scm_to_utf8_stringn(path, NULL);
    scm_dynwind_free(v60);
    SCM result = scm_from_bool(IsPathFile(v60));
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadDirectoryFiles(SCM dirPath) {
    scm_dynwind_begin(0);
    char *v61 = scm_to_utf8_stringn(dirPath, NULL);
    scm_dynwind_free(v61);
    void *v62 = scm_gc_malloc_pointerless(sizeof(FilePathList), "raylib-guile ptr");
    FilePathList v62_data = LoadDirectoryFiles(v61);
    memcpy(v62, &v62_data, sizeof(FilePathList));
    SCM result = scm_make_foreign_object_1(rgtype_FilePathList, v62);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadDirectoryFilesEx(SCM basePath, SCM filter, SCM scanSubdirs) {
    scm_dynwind_begin(0);
    char *v63 = scm_to_utf8_stringn(basePath, NULL);
    scm_dynwind_free(v63);
    char *v64 = scm_to_utf8_stringn(filter, NULL);
    scm_dynwind_free(v64);
    void *v65 = scm_gc_malloc_pointerless(sizeof(FilePathList), "raylib-guile ptr");
    FilePathList v65_data = LoadDirectoryFilesEx(v63, v64, scm_to_bool(scanSubdirs));
    memcpy(v65, &v65_data, sizeof(FilePathList));
    SCM result = scm_make_foreign_object_1(rgtype_FilePathList, v65);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadDirectoryFiles(SCM files) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_FilePathList, files);
    SCM result = (UnloadDirectoryFiles((*(FilePathList*)scm_foreign_object_ref(files, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsFileDropped() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsFileDropped());
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadDroppedFiles() {
    scm_dynwind_begin(0);
    void *v66 = scm_gc_malloc_pointerless(sizeof(FilePathList), "raylib-guile ptr");
    FilePathList v66_data = LoadDroppedFiles();
    memcpy(v66, &v66_data, sizeof(FilePathList));
    SCM result = scm_make_foreign_object_1(rgtype_FilePathList, v66);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadDroppedFiles(SCM files) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_FilePathList, files);
    SCM result = (UnloadDroppedFiles((*(FilePathList*)scm_foreign_object_ref(files, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFileModTime(SCM fileName) {
    scm_dynwind_begin(0);
    char *v67 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v67);
    SCM result = scm_from_long(GetFileModTime(v67));
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadAutomationEventList(SCM fileName) {
    scm_dynwind_begin(0);
    char *v68 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v68);
    void *v69 = scm_gc_malloc_pointerless(sizeof(AutomationEventList), "raylib-guile ptr");
    AutomationEventList v69_data = LoadAutomationEventList(v68);
    memcpy(v69, &v69_data, sizeof(AutomationEventList));
    SCM result = scm_make_foreign_object_1(rgtype_AutomationEventList, v69);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadAutomationEventList(SCM list) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AutomationEventList, list);
    SCM result = (UnloadAutomationEventList((*(AutomationEventList*)scm_foreign_object_ref(list, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportAutomationEventList(SCM list, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AutomationEventList, list);
    char *v70 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v70);
    SCM result = scm_from_bool(ExportAutomationEventList((*(AutomationEventList*)scm_foreign_object_ref(list, 0)), v70));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAutomationEventList(SCM list) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AutomationEventList, list);
    SCM result = (SetAutomationEventList(scm_foreign_object_ref(list, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAutomationEventBaseFrame(SCM frame) {
    scm_dynwind_begin(0);
    SCM result = (SetAutomationEventBaseFrame(scm_to_int(frame)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_StartAutomationEventRecording() {
    scm_dynwind_begin(0);
    SCM result = (StartAutomationEventRecording(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_StopAutomationEventRecording() {
    scm_dynwind_begin(0);
    SCM result = (StopAutomationEventRecording(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PlayAutomationEvent(SCM event) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AutomationEvent, event);
    SCM result = (PlayAutomationEvent((*(AutomationEvent*)scm_foreign_object_ref(event, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsKeyPressed(SCM key) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsKeyPressed(scm_to_int(key)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsKeyPressedRepeat(SCM key) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsKeyPressedRepeat(scm_to_int(key)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsKeyDown(SCM key) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsKeyDown(scm_to_int(key)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsKeyReleased(SCM key) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsKeyReleased(scm_to_int(key)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsKeyUp(SCM key) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsKeyUp(scm_to_int(key)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetKeyPressed() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetKeyPressed());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCharPressed() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetCharPressed());
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetExitKey(SCM key) {
    scm_dynwind_begin(0);
    SCM result = (SetExitKey(scm_to_int(key)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGamepadAvailable(SCM gamepad) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGamepadAvailable(scm_to_int(gamepad)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGamepadName(SCM gamepad) {
    scm_dynwind_begin(0);
    SCM result = scm_from_utf8_string(GetGamepadName(scm_to_int(gamepad)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGamepadButtonPressed(SCM gamepad, SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGamepadButtonPressed(scm_to_int(gamepad), scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGamepadButtonDown(SCM gamepad, SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGamepadButtonDown(scm_to_int(gamepad), scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGamepadButtonReleased(SCM gamepad, SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGamepadButtonReleased(scm_to_int(gamepad), scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGamepadButtonUp(SCM gamepad, SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGamepadButtonUp(scm_to_int(gamepad), scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGamepadButtonPressed() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetGamepadButtonPressed());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGamepadAxisCount(SCM gamepad) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetGamepadAxisCount(scm_to_int(gamepad)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGamepadAxisMovement(SCM gamepad, SCM axis) {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetGamepadAxisMovement(scm_to_int(gamepad), scm_to_int(axis)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetGamepadMappings(SCM mappings) {
    scm_dynwind_begin(0);
    char *v71 = scm_to_utf8_stringn(mappings, NULL);
    scm_dynwind_free(v71);
    SCM result = scm_from_int(SetGamepadMappings(v71));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetGamepadVibration(SCM gamepad, SCM leftMotor, SCM rightMotor) {
    scm_dynwind_begin(0);
    SCM result = (SetGamepadVibration(scm_to_int(gamepad), scm_to_double(leftMotor), scm_to_double(rightMotor)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMouseButtonPressed(SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsMouseButtonPressed(scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMouseButtonDown(SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsMouseButtonDown(scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMouseButtonReleased(SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsMouseButtonReleased(scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMouseButtonUp(SCM button) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsMouseButtonUp(scm_to_int(button)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMouseX() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMouseX());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMouseY() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetMouseY());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMousePosition() {
    scm_dynwind_begin(0);
    void *v72 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v72_data = GetMousePosition();
    memcpy(v72, &v72_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v72);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMouseDelta() {
    scm_dynwind_begin(0);
    void *v73 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v73_data = GetMouseDelta();
    memcpy(v73, &v73_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v73);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMousePosition(SCM x, SCM y) {
    scm_dynwind_begin(0);
    SCM result = (SetMousePosition(scm_to_int(x), scm_to_int(y)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMouseOffset(SCM offsetX, SCM offsetY) {
    scm_dynwind_begin(0);
    SCM result = (SetMouseOffset(scm_to_int(offsetX), scm_to_int(offsetY)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMouseScale(SCM scaleX, SCM scaleY) {
    scm_dynwind_begin(0);
    SCM result = (SetMouseScale(scm_to_double(scaleX), scm_to_double(scaleY)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMouseWheelMove() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetMouseWheelMove());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMouseWheelMoveV() {
    scm_dynwind_begin(0);
    void *v74 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v74_data = GetMouseWheelMoveV();
    memcpy(v74, &v74_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v74);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMouseCursor(SCM cursor) {
    scm_dynwind_begin(0);
    SCM result = (SetMouseCursor(scm_to_int(cursor)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTouchX() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetTouchX());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTouchY() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetTouchY());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTouchPosition(SCM index) {
    scm_dynwind_begin(0);
    void *v75 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v75_data = GetTouchPosition(scm_to_int(index));
    memcpy(v75, &v75_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v75);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTouchPointId(SCM index) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetTouchPointId(scm_to_int(index)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetTouchPointCount() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetTouchPointCount());
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetGesturesEnabled(SCM flags) {
    scm_dynwind_begin(0);
    SCM result = (SetGesturesEnabled(scm_to_uint(flags)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsGestureDetected(SCM gesture) {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsGestureDetected(scm_to_uint(gesture)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGestureDetected() {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetGestureDetected());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGestureHoldDuration() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetGestureHoldDuration());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGestureDragVector() {
    scm_dynwind_begin(0);
    void *v76 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v76_data = GetGestureDragVector();
    memcpy(v76, &v76_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v76);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGestureDragAngle() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetGestureDragAngle());
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGesturePinchVector() {
    scm_dynwind_begin(0);
    void *v77 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v77_data = GetGesturePinchVector();
    memcpy(v77, &v77_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v77);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGesturePinchAngle() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetGesturePinchAngle());
    scm_dynwind_end();
    return result;
}

SCM rgfun_UpdateCamera(SCM camera, SCM mode) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    SCM result = (UpdateCamera(scm_foreign_object_ref(camera, 0), scm_to_int(mode)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UpdateCameraPro(SCM camera, SCM movement, SCM rotation, SCM zoom) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    scm_assert_foreign_object_type(rgtype_Vector3, movement);
    scm_assert_foreign_object_type(rgtype_Vector3, rotation);
    SCM result = (UpdateCameraPro(scm_foreign_object_ref(camera, 0), (*(Vector3*)scm_foreign_object_ref(movement, 0)), (*(Vector3*)scm_foreign_object_ref(rotation, 0)), scm_to_double(zoom)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetShapesTexture(SCM texture, SCM source) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    SCM result = (SetShapesTexture((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Rectangle*)scm_foreign_object_ref(source, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetShapesTexture() {
    scm_dynwind_begin(0);
    void *v78 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v78_data = GetShapesTexture();
    memcpy(v78, &v78_data, sizeof(Texture));
    SCM result = scm_make_foreign_object_1(rgtype_Texture, v78);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetShapesTextureRectangle() {
    scm_dynwind_begin(0);
    void *v79 = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    Rectangle v79_data = GetShapesTextureRectangle();
    memcpy(v79, &v79_data, sizeof(Rectangle));
    SCM result = scm_make_foreign_object_1(rgtype_Rectangle, v79);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPixel(SCM posX, SCM posY, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPixel(scm_to_int(posX), scm_to_int(posY), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPixelV(SCM position, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPixelV((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawLine(SCM startPosX, SCM startPosY, SCM endPosX, SCM endPosY, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawLine(scm_to_int(startPosX), scm_to_int(startPosY), scm_to_int(endPosX), scm_to_int(endPosY), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawLineV(SCM startPos, SCM endPos, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, startPos);
    scm_assert_foreign_object_type(rgtype_Vector2, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawLineV((*(Vector2*)scm_foreign_object_ref(startPos, 0)), (*(Vector2*)scm_foreign_object_ref(endPos, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawLineEx(SCM startPos, SCM endPos, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, startPos);
    scm_assert_foreign_object_type(rgtype_Vector2, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawLineEx((*(Vector2*)scm_foreign_object_ref(startPos, 0)), (*(Vector2*)scm_foreign_object_ref(endPos, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawLineBezier(SCM startPos, SCM endPos, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, startPos);
    scm_assert_foreign_object_type(rgtype_Vector2, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawLineBezier((*(Vector2*)scm_foreign_object_ref(startPos, 0)), (*(Vector2*)scm_foreign_object_ref(endPos, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircle(SCM centerX, SCM centerY, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircle(scm_to_int(centerX), scm_to_int(centerY), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleSector(SCM center, SCM radius, SCM startAngle, SCM endAngle, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircleSector((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), scm_to_double(startAngle), scm_to_double(endAngle), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleSectorLines(SCM center, SCM radius, SCM startAngle, SCM endAngle, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircleSectorLines((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), scm_to_double(startAngle), scm_to_double(endAngle), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleGradient(SCM centerX, SCM centerY, SCM radius, SCM color1, SCM color2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color1);
    scm_assert_foreign_object_type(rgtype_Color, color2);
    SCM result = (DrawCircleGradient(scm_to_int(centerX), scm_to_int(centerY), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color1, 0)), (*(Color*)scm_foreign_object_ref(color2, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleV(SCM center, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircleV((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleLines(SCM centerX, SCM centerY, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircleLines(scm_to_int(centerX), scm_to_int(centerY), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircleLinesV(SCM center, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircleLinesV((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawEllipse(SCM centerX, SCM centerY, SCM radiusH, SCM radiusV, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawEllipse(scm_to_int(centerX), scm_to_int(centerY), scm_to_double(radiusH), scm_to_double(radiusV), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawEllipseLines(SCM centerX, SCM centerY, SCM radiusH, SCM radiusV, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawEllipseLines(scm_to_int(centerX), scm_to_int(centerY), scm_to_double(radiusH), scm_to_double(radiusV), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRing(SCM center, SCM innerRadius, SCM outerRadius, SCM startAngle, SCM endAngle, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRing((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(innerRadius), scm_to_double(outerRadius), scm_to_double(startAngle), scm_to_double(endAngle), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRingLines(SCM center, SCM innerRadius, SCM outerRadius, SCM startAngle, SCM endAngle, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRingLines((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(innerRadius), scm_to_double(outerRadius), scm_to_double(startAngle), scm_to_double(endAngle), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangle(SCM posX, SCM posY, SCM width, SCM height, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangle(scm_to_int(posX), scm_to_int(posY), scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleV(SCM position, SCM size, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Vector2, size);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleV((*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Vector2*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleRec(SCM rec, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleRec((*(Rectangle*)scm_foreign_object_ref(rec, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectanglePro(SCM rec, SCM origin, SCM rotation, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Vector2, origin);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectanglePro((*(Rectangle*)scm_foreign_object_ref(rec, 0)), (*(Vector2*)scm_foreign_object_ref(origin, 0)), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleGradientV(SCM posX, SCM posY, SCM width, SCM height, SCM color1, SCM color2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color1);
    scm_assert_foreign_object_type(rgtype_Color, color2);
    SCM result = (DrawRectangleGradientV(scm_to_int(posX), scm_to_int(posY), scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color1, 0)), (*(Color*)scm_foreign_object_ref(color2, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleGradientH(SCM posX, SCM posY, SCM width, SCM height, SCM color1, SCM color2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color1);
    scm_assert_foreign_object_type(rgtype_Color, color2);
    SCM result = (DrawRectangleGradientH(scm_to_int(posX), scm_to_int(posY), scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color1, 0)), (*(Color*)scm_foreign_object_ref(color2, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleGradientEx(SCM rec, SCM col1, SCM col2, SCM col3, SCM col4) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, col1);
    scm_assert_foreign_object_type(rgtype_Color, col2);
    scm_assert_foreign_object_type(rgtype_Color, col3);
    scm_assert_foreign_object_type(rgtype_Color, col4);
    SCM result = (DrawRectangleGradientEx((*(Rectangle*)scm_foreign_object_ref(rec, 0)), (*(Color*)scm_foreign_object_ref(col1, 0)), (*(Color*)scm_foreign_object_ref(col2, 0)), (*(Color*)scm_foreign_object_ref(col3, 0)), (*(Color*)scm_foreign_object_ref(col4, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleLines(SCM posX, SCM posY, SCM width, SCM height, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleLines(scm_to_int(posX), scm_to_int(posY), scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleLinesEx(SCM rec, SCM lineThick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleLinesEx((*(Rectangle*)scm_foreign_object_ref(rec, 0)), scm_to_double(lineThick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleRounded(SCM rec, SCM roundness, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleRounded((*(Rectangle*)scm_foreign_object_ref(rec, 0)), scm_to_double(roundness), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleRoundedLines(SCM rec, SCM roundness, SCM segments, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleRoundedLines((*(Rectangle*)scm_foreign_object_ref(rec, 0)), scm_to_double(roundness), scm_to_int(segments), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRectangleRoundedLinesEx(SCM rec, SCM roundness, SCM segments, SCM lineThick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRectangleRoundedLinesEx((*(Rectangle*)scm_foreign_object_ref(rec, 0)), scm_to_double(roundness), scm_to_int(segments), scm_to_double(lineThick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTriangle(SCM v1, SCM v2, SCM v3, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, v1);
    scm_assert_foreign_object_type(rgtype_Vector2, v2);
    scm_assert_foreign_object_type(rgtype_Vector2, v3);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawTriangle((*(Vector2*)scm_foreign_object_ref(v1, 0)), (*(Vector2*)scm_foreign_object_ref(v2, 0)), (*(Vector2*)scm_foreign_object_ref(v3, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTriangleLines(SCM v1, SCM v2, SCM v3, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, v1);
    scm_assert_foreign_object_type(rgtype_Vector2, v2);
    scm_assert_foreign_object_type(rgtype_Vector2, v3);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawTriangleLines((*(Vector2*)scm_foreign_object_ref(v1, 0)), (*(Vector2*)scm_foreign_object_ref(v2, 0)), (*(Vector2*)scm_foreign_object_ref(v3, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPoly(SCM center, SCM sides, SCM radius, SCM rotation, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPoly((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_int(sides), scm_to_double(radius), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPolyLines(SCM center, SCM sides, SCM radius, SCM rotation, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPolyLines((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_int(sides), scm_to_double(radius), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPolyLinesEx(SCM center, SCM sides, SCM radius, SCM rotation, SCM lineThick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPolyLinesEx((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_int(sides), scm_to_double(radius), scm_to_double(rotation), scm_to_double(lineThick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineLinear(SCM points, SCM pointCount, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineLinear(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineBasis(SCM points, SCM pointCount, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineBasis(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineCatmullRom(SCM points, SCM pointCount, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineCatmullRom(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineBezierQuadratic(SCM points, SCM pointCount, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineBezierQuadratic(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineBezierCubic(SCM points, SCM pointCount, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineBezierCubic(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineSegmentLinear(SCM p1, SCM p2, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineSegmentLinear((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineSegmentBasis(SCM p1, SCM p2, SCM p3, SCM p4, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineSegmentBasis((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineSegmentCatmullRom(SCM p1, SCM p2, SCM p3, SCM p4, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineSegmentCatmullRom((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineSegmentBezierQuadratic(SCM p1, SCM c2, SCM p3, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, c2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineSegmentBezierQuadratic((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(c2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSplineSegmentBezierCubic(SCM p1, SCM c2, SCM c3, SCM p4, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, c2);
    scm_assert_foreign_object_type(rgtype_Vector2, c3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSplineSegmentBezierCubic((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(c2, 0)), (*(Vector2*)scm_foreign_object_ref(c3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetSplinePointLinear(SCM startPos, SCM endPos, SCM t) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, startPos);
    scm_assert_foreign_object_type(rgtype_Vector2, endPos);
    void *v80 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v80_data = GetSplinePointLinear((*(Vector2*)scm_foreign_object_ref(startPos, 0)), (*(Vector2*)scm_foreign_object_ref(endPos, 0)), scm_to_double(t));
    memcpy(v80, &v80_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v80);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetSplinePointBasis(SCM p1, SCM p2, SCM p3, SCM p4, SCM t) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    void *v81 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v81_data = GetSplinePointBasis((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(t));
    memcpy(v81, &v81_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v81);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetSplinePointCatmullRom(SCM p1, SCM p2, SCM p3, SCM p4, SCM t) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    void *v82 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v82_data = GetSplinePointCatmullRom((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(t));
    memcpy(v82, &v82_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v82);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetSplinePointBezierQuad(SCM p1, SCM c2, SCM p3, SCM t) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, c2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    void *v83 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v83_data = GetSplinePointBezierQuad((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(c2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0)), scm_to_double(t));
    memcpy(v83, &v83_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v83);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetSplinePointBezierCubic(SCM p1, SCM c2, SCM c3, SCM p4, SCM t) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, c2);
    scm_assert_foreign_object_type(rgtype_Vector2, c3);
    scm_assert_foreign_object_type(rgtype_Vector2, p4);
    void *v84 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v84_data = GetSplinePointBezierCubic((*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(c2, 0)), (*(Vector2*)scm_foreign_object_ref(c3, 0)), (*(Vector2*)scm_foreign_object_ref(p4, 0)), scm_to_double(t));
    memcpy(v84, &v84_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v84);
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionRecs(SCM rec1, SCM rec2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec1);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec2);
    SCM result = scm_from_bool(CheckCollisionRecs((*(Rectangle*)scm_foreign_object_ref(rec1, 0)), (*(Rectangle*)scm_foreign_object_ref(rec2, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionCircles(SCM center1, SCM radius1, SCM center2, SCM radius2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center1);
    scm_assert_foreign_object_type(rgtype_Vector2, center2);
    SCM result = scm_from_bool(CheckCollisionCircles((*(Vector2*)scm_foreign_object_ref(center1, 0)), scm_to_double(radius1), (*(Vector2*)scm_foreign_object_ref(center2, 0)), scm_to_double(radius2)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionCircleRec(SCM center, SCM radius, SCM rec) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    SCM result = scm_from_bool(CheckCollisionCircleRec((*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), (*(Rectangle*)scm_foreign_object_ref(rec, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionPointRec(SCM point, SCM rec) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, point);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    SCM result = scm_from_bool(CheckCollisionPointRec((*(Vector2*)scm_foreign_object_ref(point, 0)), (*(Rectangle*)scm_foreign_object_ref(rec, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionPointCircle(SCM point, SCM center, SCM radius) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, point);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    SCM result = scm_from_bool(CheckCollisionPointCircle((*(Vector2*)scm_foreign_object_ref(point, 0)), (*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_double(radius)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionPointTriangle(SCM point, SCM p1, SCM p2, SCM p3) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, point);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    scm_assert_foreign_object_type(rgtype_Vector2, p3);
    SCM result = scm_from_bool(CheckCollisionPointTriangle((*(Vector2*)scm_foreign_object_ref(point, 0)), (*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), (*(Vector2*)scm_foreign_object_ref(p3, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionPointPoly(SCM point, SCM points, SCM pointCount) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, point);
    scm_assert_foreign_object_type(rgtype_Vector2, points);
    SCM result = scm_from_bool(CheckCollisionPointPoly((*(Vector2*)scm_foreign_object_ref(point, 0)), scm_foreign_object_ref(points, 0), scm_to_int(pointCount)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionPointLine(SCM point, SCM p1, SCM p2, SCM threshold) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector2, point);
    scm_assert_foreign_object_type(rgtype_Vector2, p1);
    scm_assert_foreign_object_type(rgtype_Vector2, p2);
    SCM result = scm_from_bool(CheckCollisionPointLine((*(Vector2*)scm_foreign_object_ref(point, 0)), (*(Vector2*)scm_foreign_object_ref(p1, 0)), (*(Vector2*)scm_foreign_object_ref(p2, 0)), scm_to_int(threshold)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCollisionRec(SCM rec1, SCM rec2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec1);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec2);
    void *v85 = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    Rectangle v85_data = GetCollisionRec((*(Rectangle*)scm_foreign_object_ref(rec1, 0)), (*(Rectangle*)scm_foreign_object_ref(rec2, 0)));
    memcpy(v85, &v85_data, sizeof(Rectangle));
    SCM result = scm_make_foreign_object_1(rgtype_Rectangle, v85);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImage(SCM fileName) {
    scm_dynwind_begin(0);
    char *v86 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v86);
    void *v87 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v87_data = LoadImage(v86);
    memcpy(v87, &v87_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v87);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageRaw(SCM fileName, SCM width, SCM height, SCM format, SCM headerSize) {
    scm_dynwind_begin(0);
    char *v88 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v88);
    void *v89 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v89_data = LoadImageRaw(v88, scm_to_int(width), scm_to_int(height), scm_to_int(format), scm_to_int(headerSize));
    memcpy(v89, &v89_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v89);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageSvg(SCM fileNameOrString, SCM width, SCM height) {
    scm_dynwind_begin(0);
    char *v90 = scm_to_utf8_stringn(fileNameOrString, NULL);
    scm_dynwind_free(v90);
    void *v91 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v91_data = LoadImageSvg(v90, scm_to_int(width), scm_to_int(height));
    memcpy(v91, &v91_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v91);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageAnimFromMemory(SCM fileType, SCM fileData, SCM dataSize, SCM frames) {
    scm_dynwind_begin(0);
    char *v92 = scm_to_utf8_stringn(fileType, NULL);
    scm_dynwind_free(v92);
    char *v93 = scm_to_utf8_stringn(fileData, NULL);
    scm_dynwind_free(v93);
    void *v94 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v94_data = LoadImageAnimFromMemory(v92, v93, scm_to_int(dataSize), scm_to_pointer(frames));
    memcpy(v94, &v94_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v94);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageFromMemory(SCM fileType, SCM fileData, SCM dataSize) {
    scm_dynwind_begin(0);
    char *v95 = scm_to_utf8_stringn(fileType, NULL);
    scm_dynwind_free(v95);
    char *v96 = scm_to_utf8_stringn(fileData, NULL);
    scm_dynwind_free(v96);
    void *v97 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v97_data = LoadImageFromMemory(v95, v96, scm_to_int(dataSize));
    memcpy(v97, &v97_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v97);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageFromTexture(SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    void *v98 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v98_data = LoadImageFromTexture((*(Texture*)scm_foreign_object_ref(texture, 0)));
    memcpy(v98, &v98_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v98);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadImageFromScreen() {
    scm_dynwind_begin(0);
    void *v99 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v99_data = LoadImageFromScreen();
    memcpy(v99, &v99_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v99);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsImageReady(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = scm_from_bool(IsImageReady((*(Image*)scm_foreign_object_ref(image, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadImage(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (UnloadImage((*(Image*)scm_foreign_object_ref(image, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportImage(SCM image, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    char *v100 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v100);
    SCM result = scm_from_bool(ExportImage((*(Image*)scm_foreign_object_ref(image, 0)), v100));
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportImageToMemory(SCM image, SCM fileType, SCM fileSize) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    char *v101 = scm_to_utf8_stringn(fileType, NULL);
    scm_dynwind_free(v101);
    SCM result = scm_from_utf8_string(ExportImageToMemory((*(Image*)scm_foreign_object_ref(image, 0)), v101, scm_to_pointer(fileSize)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportImageAsCode(SCM image, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    char *v102 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v102);
    SCM result = scm_from_bool(ExportImageAsCode((*(Image*)scm_foreign_object_ref(image, 0)), v102));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageColor(SCM width, SCM height, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v103 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v103_data = GenImageColor(scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color, 0)));
    memcpy(v103, &v103_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v103);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageGradientLinear(SCM width, SCM height, SCM direction, SCM start, SCM end) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, start);
    scm_assert_foreign_object_type(rgtype_Color, end);
    void *v104 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v104_data = GenImageGradientLinear(scm_to_int(width), scm_to_int(height), scm_to_int(direction), (*(Color*)scm_foreign_object_ref(start, 0)), (*(Color*)scm_foreign_object_ref(end, 0)));
    memcpy(v104, &v104_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v104);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageGradientRadial(SCM width, SCM height, SCM density, SCM inner, SCM outer) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, inner);
    scm_assert_foreign_object_type(rgtype_Color, outer);
    void *v105 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v105_data = GenImageGradientRadial(scm_to_int(width), scm_to_int(height), scm_to_double(density), (*(Color*)scm_foreign_object_ref(inner, 0)), (*(Color*)scm_foreign_object_ref(outer, 0)));
    memcpy(v105, &v105_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v105);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageGradientSquare(SCM width, SCM height, SCM density, SCM inner, SCM outer) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, inner);
    scm_assert_foreign_object_type(rgtype_Color, outer);
    void *v106 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v106_data = GenImageGradientSquare(scm_to_int(width), scm_to_int(height), scm_to_double(density), (*(Color*)scm_foreign_object_ref(inner, 0)), (*(Color*)scm_foreign_object_ref(outer, 0)));
    memcpy(v106, &v106_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v106);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageChecked(SCM width, SCM height, SCM checksX, SCM checksY, SCM col1, SCM col2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, col1);
    scm_assert_foreign_object_type(rgtype_Color, col2);
    void *v107 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v107_data = GenImageChecked(scm_to_int(width), scm_to_int(height), scm_to_int(checksX), scm_to_int(checksY), (*(Color*)scm_foreign_object_ref(col1, 0)), (*(Color*)scm_foreign_object_ref(col2, 0)));
    memcpy(v107, &v107_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v107);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageWhiteNoise(SCM width, SCM height, SCM factor) {
    scm_dynwind_begin(0);
    void *v108 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v108_data = GenImageWhiteNoise(scm_to_int(width), scm_to_int(height), scm_to_double(factor));
    memcpy(v108, &v108_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v108);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImagePerlinNoise(SCM width, SCM height, SCM offsetX, SCM offsetY, SCM scale) {
    scm_dynwind_begin(0);
    void *v109 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v109_data = GenImagePerlinNoise(scm_to_int(width), scm_to_int(height), scm_to_int(offsetX), scm_to_int(offsetY), scm_to_double(scale));
    memcpy(v109, &v109_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v109);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageCellular(SCM width, SCM height, SCM tileSize) {
    scm_dynwind_begin(0);
    void *v110 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v110_data = GenImageCellular(scm_to_int(width), scm_to_int(height), scm_to_int(tileSize));
    memcpy(v110, &v110_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v110);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenImageText(SCM width, SCM height, SCM text) {
    scm_dynwind_begin(0);
    char *v111 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v111);
    void *v112 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v112_data = GenImageText(scm_to_int(width), scm_to_int(height), v111);
    memcpy(v112, &v112_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v112);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageCopy(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    void *v113 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v113_data = ImageCopy((*(Image*)scm_foreign_object_ref(image, 0)));
    memcpy(v113, &v113_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v113);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageFromImage(SCM image, SCM rec) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    void *v114 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v114_data = ImageFromImage((*(Image*)scm_foreign_object_ref(image, 0)), (*(Rectangle*)scm_foreign_object_ref(rec, 0)));
    memcpy(v114, &v114_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v114);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageText(SCM text, SCM fontSize, SCM color) {
    scm_dynwind_begin(0);
    char *v115 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v115);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v116 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v116_data = ImageText(v115, scm_to_int(fontSize), (*(Color*)scm_foreign_object_ref(color, 0)));
    memcpy(v116, &v116_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v116);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageTextEx(SCM font, SCM text, SCM fontSize, SCM spacing, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v117 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v117);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    void *v118 = scm_gc_malloc_pointerless(sizeof(Image), "raylib-guile ptr");
    Image v118_data = ImageTextEx((*(Font*)scm_foreign_object_ref(font, 0)), v117, scm_to_double(fontSize), scm_to_double(spacing), (*(Color*)scm_foreign_object_ref(tint, 0)));
    memcpy(v118, &v118_data, sizeof(Image));
    SCM result = scm_make_foreign_object_1(rgtype_Image, v118);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageFormat(SCM image, SCM newFormat) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageFormat(scm_foreign_object_ref(image, 0), scm_to_int(newFormat)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageToPOT(SCM image, SCM fill) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, fill);
    SCM result = (ImageToPOT(scm_foreign_object_ref(image, 0), (*(Color*)scm_foreign_object_ref(fill, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageCrop(SCM image, SCM crop) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Rectangle, crop);
    SCM result = (ImageCrop(scm_foreign_object_ref(image, 0), (*(Rectangle*)scm_foreign_object_ref(crop, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageAlphaCrop(SCM image, SCM threshold) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageAlphaCrop(scm_foreign_object_ref(image, 0), scm_to_double(threshold)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageAlphaClear(SCM image, SCM color, SCM threshold) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageAlphaClear(scm_foreign_object_ref(image, 0), (*(Color*)scm_foreign_object_ref(color, 0)), scm_to_double(threshold)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageAlphaMask(SCM image, SCM alphaMask) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Image, alphaMask);
    SCM result = (ImageAlphaMask(scm_foreign_object_ref(image, 0), (*(Image*)scm_foreign_object_ref(alphaMask, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageAlphaPremultiply(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageAlphaPremultiply(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageBlurGaussian(SCM image, SCM blurSize) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageBlurGaussian(scm_foreign_object_ref(image, 0), scm_to_int(blurSize)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageKernelConvolution(SCM image, SCM kernel, SCM kernelSize) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageKernelConvolution(scm_foreign_object_ref(image, 0), scm_to_pointer(kernel), scm_to_int(kernelSize)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageResize(SCM image, SCM newWidth, SCM newHeight) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageResize(scm_foreign_object_ref(image, 0), scm_to_int(newWidth), scm_to_int(newHeight)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageResizeNN(SCM image, SCM newWidth, SCM newHeight) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageResizeNN(scm_foreign_object_ref(image, 0), scm_to_int(newWidth), scm_to_int(newHeight)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageResizeCanvas(SCM image, SCM newWidth, SCM newHeight, SCM offsetX, SCM offsetY, SCM fill) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, fill);
    SCM result = (ImageResizeCanvas(scm_foreign_object_ref(image, 0), scm_to_int(newWidth), scm_to_int(newHeight), scm_to_int(offsetX), scm_to_int(offsetY), (*(Color*)scm_foreign_object_ref(fill, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageMipmaps(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageMipmaps(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDither(SCM image, SCM rBpp, SCM gBpp, SCM bBpp, SCM aBpp) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageDither(scm_foreign_object_ref(image, 0), scm_to_int(rBpp), scm_to_int(gBpp), scm_to_int(bBpp), scm_to_int(aBpp)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageFlipVertical(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageFlipVertical(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageFlipHorizontal(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageFlipHorizontal(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageRotate(SCM image, SCM degrees) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageRotate(scm_foreign_object_ref(image, 0), scm_to_int(degrees)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageRotateCW(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageRotateCW(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageRotateCCW(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageRotateCCW(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorTint(SCM image, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageColorTint(scm_foreign_object_ref(image, 0), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorInvert(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageColorInvert(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorGrayscale(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageColorGrayscale(scm_foreign_object_ref(image, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorContrast(SCM image, SCM contrast) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageColorContrast(scm_foreign_object_ref(image, 0), scm_to_double(contrast)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorBrightness(SCM image, SCM brightness) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    SCM result = (ImageColorBrightness(scm_foreign_object_ref(image, 0), scm_to_int(brightness)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageColorReplace(SCM image, SCM color, SCM replace) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, color);
    scm_assert_foreign_object_type(rgtype_Color, replace);
    SCM result = (ImageColorReplace(scm_foreign_object_ref(image, 0), (*(Color*)scm_foreign_object_ref(color, 0)), (*(Color*)scm_foreign_object_ref(replace, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadImageColors(SCM colors) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, colors);
    SCM result = (UnloadImageColors(scm_foreign_object_ref(colors, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadImagePalette(SCM colors) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, colors);
    SCM result = (UnloadImagePalette(scm_foreign_object_ref(colors, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetImageAlphaBorder(SCM image, SCM threshold) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    void *v119 = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    Rectangle v119_data = GetImageAlphaBorder((*(Image*)scm_foreign_object_ref(image, 0)), scm_to_double(threshold));
    memcpy(v119, &v119_data, sizeof(Rectangle));
    SCM result = scm_make_foreign_object_1(rgtype_Rectangle, v119);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetImageColor(SCM image, SCM x, SCM y) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    void *v120 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v120_data = GetImageColor((*(Image*)scm_foreign_object_ref(image, 0)), scm_to_int(x), scm_to_int(y));
    memcpy(v120, &v120_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v120);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageClearBackground(SCM dst, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageClearBackground(scm_foreign_object_ref(dst, 0), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawPixel(SCM dst, SCM posX, SCM posY, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawPixel(scm_foreign_object_ref(dst, 0), scm_to_int(posX), scm_to_int(posY), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawPixelV(SCM dst, SCM position, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawPixelV(scm_foreign_object_ref(dst, 0), (*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawLine(SCM dst, SCM startPosX, SCM startPosY, SCM endPosX, SCM endPosY, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawLine(scm_foreign_object_ref(dst, 0), scm_to_int(startPosX), scm_to_int(startPosY), scm_to_int(endPosX), scm_to_int(endPosY), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawLineV(SCM dst, SCM start, SCM end, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Vector2, start);
    scm_assert_foreign_object_type(rgtype_Vector2, end);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawLineV(scm_foreign_object_ref(dst, 0), (*(Vector2*)scm_foreign_object_ref(start, 0)), (*(Vector2*)scm_foreign_object_ref(end, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawCircle(SCM dst, SCM centerX, SCM centerY, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawCircle(scm_foreign_object_ref(dst, 0), scm_to_int(centerX), scm_to_int(centerY), scm_to_int(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawCircleV(SCM dst, SCM center, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawCircleV(scm_foreign_object_ref(dst, 0), (*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_int(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawCircleLines(SCM dst, SCM centerX, SCM centerY, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawCircleLines(scm_foreign_object_ref(dst, 0), scm_to_int(centerX), scm_to_int(centerY), scm_to_int(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawCircleLinesV(SCM dst, SCM center, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Vector2, center);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawCircleLinesV(scm_foreign_object_ref(dst, 0), (*(Vector2*)scm_foreign_object_ref(center, 0)), scm_to_int(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawRectangle(SCM dst, SCM posX, SCM posY, SCM width, SCM height, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawRectangle(scm_foreign_object_ref(dst, 0), scm_to_int(posX), scm_to_int(posY), scm_to_int(width), scm_to_int(height), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawRectangleV(SCM dst, SCM position, SCM size, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Vector2, size);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawRectangleV(scm_foreign_object_ref(dst, 0), (*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Vector2*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawRectangleRec(SCM dst, SCM rec, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawRectangleRec(scm_foreign_object_ref(dst, 0), (*(Rectangle*)scm_foreign_object_ref(rec, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawRectangleLines(SCM dst, SCM rec, SCM thick, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Rectangle, rec);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawRectangleLines(scm_foreign_object_ref(dst, 0), (*(Rectangle*)scm_foreign_object_ref(rec, 0)), scm_to_int(thick), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDraw(SCM dst, SCM src, SCM srcRec, SCM dstRec, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Image, src);
    scm_assert_foreign_object_type(rgtype_Rectangle, srcRec);
    scm_assert_foreign_object_type(rgtype_Rectangle, dstRec);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (ImageDraw(scm_foreign_object_ref(dst, 0), (*(Image*)scm_foreign_object_ref(src, 0)), (*(Rectangle*)scm_foreign_object_ref(srcRec, 0)), (*(Rectangle*)scm_foreign_object_ref(dstRec, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawText(SCM dst, SCM text, SCM posX, SCM posY, SCM fontSize, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    char *v121 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v121);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (ImageDrawText(scm_foreign_object_ref(dst, 0), v121, scm_to_int(posX), scm_to_int(posY), scm_to_int(fontSize), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ImageDrawTextEx(SCM dst, SCM font, SCM text, SCM position, SCM fontSize, SCM spacing, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, dst);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v122 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v122);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (ImageDrawTextEx(scm_foreign_object_ref(dst, 0), (*(Font*)scm_foreign_object_ref(font, 0)), v122, (*(Vector2*)scm_foreign_object_ref(position, 0)), scm_to_double(fontSize), scm_to_double(spacing), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadTexture(SCM fileName) {
    scm_dynwind_begin(0);
    char *v123 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v123);
    void *v124 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v124_data = LoadTexture(v123);
    memcpy(v124, &v124_data, sizeof(Texture));
    SCM result = scm_make_foreign_object_1(rgtype_Texture, v124);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadTextureFromImage(SCM image) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    void *v125 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v125_data = LoadTextureFromImage((*(Image*)scm_foreign_object_ref(image, 0)));
    memcpy(v125, &v125_data, sizeof(Texture));
    SCM result = scm_make_foreign_object_1(rgtype_Texture, v125);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadTextureCubemap(SCM image, SCM layout) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    void *v126 = scm_gc_malloc_pointerless(sizeof(Texture), "raylib-guile ptr");
    Texture v126_data = LoadTextureCubemap((*(Image*)scm_foreign_object_ref(image, 0)), scm_to_int(layout));
    memcpy(v126, &v126_data, sizeof(Texture));
    SCM result = scm_make_foreign_object_1(rgtype_Texture, v126);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadRenderTexture(SCM width, SCM height) {
    scm_dynwind_begin(0);
    void *v127 = scm_gc_malloc_pointerless(sizeof(RenderTexture), "raylib-guile ptr");
    RenderTexture v127_data = LoadRenderTexture(scm_to_int(width), scm_to_int(height));
    memcpy(v127, &v127_data, sizeof(RenderTexture));
    SCM result = scm_make_foreign_object_1(rgtype_RenderTexture, v127);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsTextureReady(SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = scm_from_bool(IsTextureReady((*(Texture*)scm_foreign_object_ref(texture, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadTexture(SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (UnloadTexture((*(Texture*)scm_foreign_object_ref(texture, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsRenderTextureReady(SCM target) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_RenderTexture, target);
    SCM result = scm_from_bool(IsRenderTextureReady((*(RenderTexture*)scm_foreign_object_ref(target, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadRenderTexture(SCM target) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_RenderTexture, target);
    SCM result = (UnloadRenderTexture((*(RenderTexture*)scm_foreign_object_ref(target, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenTextureMipmaps(SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (GenTextureMipmaps(scm_foreign_object_ref(texture, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetTextureFilter(SCM texture, SCM filter) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (SetTextureFilter((*(Texture*)scm_foreign_object_ref(texture, 0)), scm_to_int(filter)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetTextureWrap(SCM texture, SCM wrap) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (SetTextureWrap((*(Texture*)scm_foreign_object_ref(texture, 0)), scm_to_int(wrap)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTexture(SCM texture, SCM posX, SCM posY, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTexture((*(Texture*)scm_foreign_object_ref(texture, 0)), scm_to_int(posX), scm_to_int(posY), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextureV(SCM texture, SCM position, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextureV((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextureEx(SCM texture, SCM position, SCM rotation, SCM scale, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextureEx((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Vector2*)scm_foreign_object_ref(position, 0)), scm_to_double(rotation), scm_to_double(scale), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextureRec(SCM texture, SCM source, SCM position, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextureRec((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Rectangle*)scm_foreign_object_ref(source, 0)), (*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTexturePro(SCM texture, SCM source, SCM dest, SCM origin, SCM rotation, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    scm_assert_foreign_object_type(rgtype_Rectangle, dest);
    scm_assert_foreign_object_type(rgtype_Vector2, origin);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTexturePro((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Rectangle*)scm_foreign_object_ref(source, 0)), (*(Rectangle*)scm_foreign_object_ref(dest, 0)), (*(Vector2*)scm_foreign_object_ref(origin, 0)), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextureNPatch(SCM texture, SCM nPatchInfo, SCM dest, SCM origin, SCM rotation, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_NPatchInfo, nPatchInfo);
    scm_assert_foreign_object_type(rgtype_Rectangle, dest);
    scm_assert_foreign_object_type(rgtype_Vector2, origin);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextureNPatch((*(Texture*)scm_foreign_object_ref(texture, 0)), (*(NPatchInfo*)scm_foreign_object_ref(nPatchInfo, 0)), (*(Rectangle*)scm_foreign_object_ref(dest, 0)), (*(Vector2*)scm_foreign_object_ref(origin, 0)), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorIsEqual(SCM col1, SCM col2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, col1);
    scm_assert_foreign_object_type(rgtype_Color, col2);
    SCM result = scm_from_bool(ColorIsEqual((*(Color*)scm_foreign_object_ref(col1, 0)), (*(Color*)scm_foreign_object_ref(col2, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_Fade(SCM color, SCM alpha) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v128 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v128_data = Fade((*(Color*)scm_foreign_object_ref(color, 0)), scm_to_double(alpha));
    memcpy(v128, &v128_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v128);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorToInt(SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = scm_from_int(ColorToInt((*(Color*)scm_foreign_object_ref(color, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorNormalize(SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v129 = scm_gc_malloc_pointerless(sizeof(Vector4), "raylib-guile ptr");
    Vector4 v129_data = ColorNormalize((*(Color*)scm_foreign_object_ref(color, 0)));
    memcpy(v129, &v129_data, sizeof(Vector4));
    SCM result = scm_make_foreign_object_1(rgtype_Vector4, v129);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorFromNormalized(SCM normalized) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector4, normalized);
    void *v130 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v130_data = ColorFromNormalized((*(Vector4*)scm_foreign_object_ref(normalized, 0)));
    memcpy(v130, &v130_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v130);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorToHSV(SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v131 = scm_gc_malloc_pointerless(sizeof(Vector3), "raylib-guile ptr");
    Vector3 v131_data = ColorToHSV((*(Color*)scm_foreign_object_ref(color, 0)));
    memcpy(v131, &v131_data, sizeof(Vector3));
    SCM result = scm_make_foreign_object_1(rgtype_Vector3, v131);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorFromHSV(SCM hue, SCM saturation, SCM value) {
    scm_dynwind_begin(0);
    void *v132 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v132_data = ColorFromHSV(scm_to_double(hue), scm_to_double(saturation), scm_to_double(value));
    memcpy(v132, &v132_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v132);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorTint(SCM color, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    void *v133 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v133_data = ColorTint((*(Color*)scm_foreign_object_ref(color, 0)), (*(Color*)scm_foreign_object_ref(tint, 0)));
    memcpy(v133, &v133_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v133);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorBrightness(SCM color, SCM factor) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v134 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v134_data = ColorBrightness((*(Color*)scm_foreign_object_ref(color, 0)), scm_to_double(factor));
    memcpy(v134, &v134_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v134);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorContrast(SCM color, SCM contrast) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v135 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v135_data = ColorContrast((*(Color*)scm_foreign_object_ref(color, 0)), scm_to_double(contrast));
    memcpy(v135, &v135_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v135);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorAlpha(SCM color, SCM alpha) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, color);
    void *v136 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v136_data = ColorAlpha((*(Color*)scm_foreign_object_ref(color, 0)), scm_to_double(alpha));
    memcpy(v136, &v136_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v136);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ColorAlphaBlend(SCM dst, SCM src, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Color, dst);
    scm_assert_foreign_object_type(rgtype_Color, src);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    void *v137 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v137_data = ColorAlphaBlend((*(Color*)scm_foreign_object_ref(dst, 0)), (*(Color*)scm_foreign_object_ref(src, 0)), (*(Color*)scm_foreign_object_ref(tint, 0)));
    memcpy(v137, &v137_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v137);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetColor(SCM hexValue) {
    scm_dynwind_begin(0);
    void *v138 = scm_gc_malloc_pointerless(sizeof(Color), "raylib-guile ptr");
    Color v138_data = GetColor(scm_to_uint(hexValue));
    memcpy(v138, &v138_data, sizeof(Color));
    SCM result = scm_make_foreign_object_1(rgtype_Color, v138);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetPixelDataSize(SCM width, SCM height, SCM format) {
    scm_dynwind_begin(0);
    SCM result = scm_from_int(GetPixelDataSize(scm_to_int(width), scm_to_int(height), scm_to_int(format)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetFontDefault() {
    scm_dynwind_begin(0);
    void *v139 = scm_gc_malloc_pointerless(sizeof(Font), "raylib-guile ptr");
    Font v139_data = GetFontDefault();
    memcpy(v139, &v139_data, sizeof(Font));
    SCM result = scm_make_foreign_object_1(rgtype_Font, v139);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadFont(SCM fileName) {
    scm_dynwind_begin(0);
    char *v140 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v140);
    void *v141 = scm_gc_malloc_pointerless(sizeof(Font), "raylib-guile ptr");
    Font v141_data = LoadFont(v140);
    memcpy(v141, &v141_data, sizeof(Font));
    SCM result = scm_make_foreign_object_1(rgtype_Font, v141);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadFontFromImage(SCM image, SCM key, SCM firstChar) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, image);
    scm_assert_foreign_object_type(rgtype_Color, key);
    void *v142 = scm_gc_malloc_pointerless(sizeof(Font), "raylib-guile ptr");
    Font v142_data = LoadFontFromImage((*(Image*)scm_foreign_object_ref(image, 0)), (*(Color*)scm_foreign_object_ref(key, 0)), scm_to_int(firstChar));
    memcpy(v142, &v142_data, sizeof(Font));
    SCM result = scm_make_foreign_object_1(rgtype_Font, v142);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsFontReady(SCM font) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    SCM result = scm_from_bool(IsFontReady((*(Font*)scm_foreign_object_ref(font, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadFontData(SCM glyphs, SCM glyphCount) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_GlyphInfo, glyphs);
    SCM result = (UnloadFontData(scm_foreign_object_ref(glyphs, 0), scm_to_int(glyphCount)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadFont(SCM font) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    SCM result = (UnloadFont((*(Font*)scm_foreign_object_ref(font, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportFontAsCode(SCM font, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v143 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v143);
    SCM result = scm_from_bool(ExportFontAsCode((*(Font*)scm_foreign_object_ref(font, 0)), v143));
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawFPS(SCM posX, SCM posY) {
    scm_dynwind_begin(0);
    SCM result = (DrawFPS(scm_to_int(posX), scm_to_int(posY)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawText(SCM text, SCM posX, SCM posY, SCM fontSize, SCM color) {
    scm_dynwind_begin(0);
    char *v144 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v144);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawText(v144, scm_to_int(posX), scm_to_int(posY), scm_to_int(fontSize), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextEx(SCM font, SCM text, SCM position, SCM fontSize, SCM spacing, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v145 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v145);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextEx((*(Font*)scm_foreign_object_ref(font, 0)), v145, (*(Vector2*)scm_foreign_object_ref(position, 0)), scm_to_double(fontSize), scm_to_double(spacing), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextPro(SCM font, SCM text, SCM position, SCM origin, SCM rotation, SCM fontSize, SCM spacing, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v146 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v146);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Vector2, origin);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextPro((*(Font*)scm_foreign_object_ref(font, 0)), v146, (*(Vector2*)scm_foreign_object_ref(position, 0)), (*(Vector2*)scm_foreign_object_ref(origin, 0)), scm_to_double(rotation), scm_to_double(fontSize), scm_to_double(spacing), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTextCodepoint(SCM font, SCM codepoint, SCM position, SCM fontSize, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    scm_assert_foreign_object_type(rgtype_Vector2, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawTextCodepoint((*(Font*)scm_foreign_object_ref(font, 0)), scm_to_int(codepoint), (*(Vector2*)scm_foreign_object_ref(position, 0)), scm_to_double(fontSize), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetTextLineSpacing(SCM spacing) {
    scm_dynwind_begin(0);
    SCM result = (SetTextLineSpacing(scm_to_int(spacing)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_MeasureText(SCM text, SCM fontSize) {
    scm_dynwind_begin(0);
    char *v147 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v147);
    SCM result = scm_from_int(MeasureText(v147, scm_to_int(fontSize)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_MeasureTextEx(SCM font, SCM text, SCM fontSize, SCM spacing) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    char *v148 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v148);
    void *v149 = scm_gc_malloc_pointerless(sizeof(Vector2), "raylib-guile ptr");
    Vector2 v149_data = MeasureTextEx((*(Font*)scm_foreign_object_ref(font, 0)), v148, scm_to_double(fontSize), scm_to_double(spacing));
    memcpy(v149, &v149_data, sizeof(Vector2));
    SCM result = scm_make_foreign_object_1(rgtype_Vector2, v149);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGlyphIndex(SCM font, SCM codepoint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    SCM result = scm_from_int(GetGlyphIndex((*(Font*)scm_foreign_object_ref(font, 0)), scm_to_int(codepoint)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGlyphInfo(SCM font, SCM codepoint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    void *v150 = scm_gc_malloc_pointerless(sizeof(GlyphInfo), "raylib-guile ptr");
    GlyphInfo v150_data = GetGlyphInfo((*(Font*)scm_foreign_object_ref(font, 0)), scm_to_int(codepoint));
    memcpy(v150, &v150_data, sizeof(GlyphInfo));
    SCM result = scm_make_foreign_object_1(rgtype_GlyphInfo, v150);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetGlyphAtlasRec(SCM font, SCM codepoint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Font, font);
    void *v151 = scm_gc_malloc_pointerless(sizeof(Rectangle), "raylib-guile ptr");
    Rectangle v151_data = GetGlyphAtlasRec((*(Font*)scm_foreign_object_ref(font, 0)), scm_to_int(codepoint));
    memcpy(v151, &v151_data, sizeof(Rectangle));
    SCM result = scm_make_foreign_object_1(rgtype_Rectangle, v151);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetCodepointCount(SCM text) {
    scm_dynwind_begin(0);
    char *v152 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v152);
    SCM result = scm_from_int(GetCodepointCount(v152));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextIsEqual(SCM text1, SCM text2) {
    scm_dynwind_begin(0);
    char *v153 = scm_to_utf8_stringn(text1, NULL);
    scm_dynwind_free(v153);
    char *v154 = scm_to_utf8_stringn(text2, NULL);
    scm_dynwind_free(v154);
    SCM result = scm_from_bool(TextIsEqual(v153, v154));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextLength(SCM text) {
    scm_dynwind_begin(0);
    char *v155 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v155);
    SCM result = scm_from_uint(TextLength(v155));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextSubtext(SCM text, SCM position, SCM length) {
    scm_dynwind_begin(0);
    char *v156 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v156);
    SCM result = scm_from_utf8_string(TextSubtext(v156, scm_to_int(position), scm_to_int(length)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextReplace(SCM text, SCM replace, SCM by) {
    scm_dynwind_begin(0);
    char *v157 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v157);
    char *v158 = scm_to_utf8_stringn(replace, NULL);
    scm_dynwind_free(v158);
    char *v159 = scm_to_utf8_stringn(by, NULL);
    scm_dynwind_free(v159);
    SCM result = scm_from_utf8_string(TextReplace(v157, v158, v159));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextInsert(SCM text, SCM insert, SCM position) {
    scm_dynwind_begin(0);
    char *v160 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v160);
    char *v161 = scm_to_utf8_stringn(insert, NULL);
    scm_dynwind_free(v161);
    SCM result = scm_from_utf8_string(TextInsert(v160, v161, scm_to_int(position)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextFindIndex(SCM text, SCM find) {
    scm_dynwind_begin(0);
    char *v162 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v162);
    char *v163 = scm_to_utf8_stringn(find, NULL);
    scm_dynwind_free(v163);
    SCM result = scm_from_int(TextFindIndex(v162, v163));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextToUpper(SCM text) {
    scm_dynwind_begin(0);
    char *v164 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v164);
    SCM result = scm_from_utf8_string(TextToUpper(v164));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextToLower(SCM text) {
    scm_dynwind_begin(0);
    char *v165 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v165);
    SCM result = scm_from_utf8_string(TextToLower(v165));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextToPascal(SCM text) {
    scm_dynwind_begin(0);
    char *v166 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v166);
    SCM result = scm_from_utf8_string(TextToPascal(v166));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextToInteger(SCM text) {
    scm_dynwind_begin(0);
    char *v167 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v167);
    SCM result = scm_from_int(TextToInteger(v167));
    scm_dynwind_end();
    return result;
}

SCM rgfun_TextToFloat(SCM text) {
    scm_dynwind_begin(0);
    char *v168 = scm_to_utf8_stringn(text, NULL);
    scm_dynwind_free(v168);
    SCM result = scm_from_double(TextToFloat(v168));
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawLine3D(SCM startPos, SCM endPos, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, startPos);
    scm_assert_foreign_object_type(rgtype_Vector3, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawLine3D((*(Vector3*)scm_foreign_object_ref(startPos, 0)), (*(Vector3*)scm_foreign_object_ref(endPos, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPoint3D(SCM position, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPoint3D((*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCircle3D(SCM center, SCM radius, SCM rotationAxis, SCM rotationAngle, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, center);
    scm_assert_foreign_object_type(rgtype_Vector3, rotationAxis);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCircle3D((*(Vector3*)scm_foreign_object_ref(center, 0)), scm_to_double(radius), (*(Vector3*)scm_foreign_object_ref(rotationAxis, 0)), scm_to_double(rotationAngle), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTriangle3D(SCM v1, SCM v2, SCM v3, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, v1);
    scm_assert_foreign_object_type(rgtype_Vector3, v2);
    scm_assert_foreign_object_type(rgtype_Vector3, v3);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawTriangle3D((*(Vector3*)scm_foreign_object_ref(v1, 0)), (*(Vector3*)scm_foreign_object_ref(v2, 0)), (*(Vector3*)scm_foreign_object_ref(v3, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawTriangleStrip3D(SCM points, SCM pointCount, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, points);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawTriangleStrip3D(scm_foreign_object_ref(points, 0), scm_to_int(pointCount), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCube(SCM position, SCM width, SCM height, SCM length, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCube((*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(width), scm_to_double(height), scm_to_double(length), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCubeV(SCM position, SCM size, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, size);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCubeV((*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector3*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCubeWires(SCM position, SCM width, SCM height, SCM length, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCubeWires((*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(width), scm_to_double(height), scm_to_double(length), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCubeWiresV(SCM position, SCM size, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, size);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCubeWiresV((*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector3*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSphere(SCM centerPos, SCM radius, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, centerPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSphere((*(Vector3*)scm_foreign_object_ref(centerPos, 0)), scm_to_double(radius), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSphereEx(SCM centerPos, SCM radius, SCM rings, SCM slices, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, centerPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSphereEx((*(Vector3*)scm_foreign_object_ref(centerPos, 0)), scm_to_double(radius), scm_to_int(rings), scm_to_int(slices), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawSphereWires(SCM centerPos, SCM radius, SCM rings, SCM slices, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, centerPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawSphereWires((*(Vector3*)scm_foreign_object_ref(centerPos, 0)), scm_to_double(radius), scm_to_int(rings), scm_to_int(slices), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCylinder(SCM position, SCM radiusTop, SCM radiusBottom, SCM height, SCM slices, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCylinder((*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(radiusTop), scm_to_double(radiusBottom), scm_to_double(height), scm_to_int(slices), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCylinderEx(SCM startPos, SCM endPos, SCM startRadius, SCM endRadius, SCM sides, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, startPos);
    scm_assert_foreign_object_type(rgtype_Vector3, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCylinderEx((*(Vector3*)scm_foreign_object_ref(startPos, 0)), (*(Vector3*)scm_foreign_object_ref(endPos, 0)), scm_to_double(startRadius), scm_to_double(endRadius), scm_to_int(sides), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCylinderWires(SCM position, SCM radiusTop, SCM radiusBottom, SCM height, SCM slices, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCylinderWires((*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(radiusTop), scm_to_double(radiusBottom), scm_to_double(height), scm_to_int(slices), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCylinderWiresEx(SCM startPos, SCM endPos, SCM startRadius, SCM endRadius, SCM sides, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, startPos);
    scm_assert_foreign_object_type(rgtype_Vector3, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCylinderWiresEx((*(Vector3*)scm_foreign_object_ref(startPos, 0)), (*(Vector3*)scm_foreign_object_ref(endPos, 0)), scm_to_double(startRadius), scm_to_double(endRadius), scm_to_int(sides), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCapsule(SCM startPos, SCM endPos, SCM radius, SCM slices, SCM rings, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, startPos);
    scm_assert_foreign_object_type(rgtype_Vector3, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCapsule((*(Vector3*)scm_foreign_object_ref(startPos, 0)), (*(Vector3*)scm_foreign_object_ref(endPos, 0)), scm_to_double(radius), scm_to_int(slices), scm_to_int(rings), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawCapsuleWires(SCM startPos, SCM endPos, SCM radius, SCM slices, SCM rings, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, startPos);
    scm_assert_foreign_object_type(rgtype_Vector3, endPos);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawCapsuleWires((*(Vector3*)scm_foreign_object_ref(startPos, 0)), (*(Vector3*)scm_foreign_object_ref(endPos, 0)), scm_to_double(radius), scm_to_int(slices), scm_to_int(rings), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawPlane(SCM centerPos, SCM size, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, centerPos);
    scm_assert_foreign_object_type(rgtype_Vector2, size);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawPlane((*(Vector3*)scm_foreign_object_ref(centerPos, 0)), (*(Vector2*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawRay(SCM ray, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawRay((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawGrid(SCM slices, SCM spacing) {
    scm_dynwind_begin(0);
    SCM result = (DrawGrid(scm_to_int(slices), scm_to_double(spacing)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadModel(SCM fileName) {
    scm_dynwind_begin(0);
    char *v169 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v169);
    void *v170 = scm_gc_malloc_pointerless(sizeof(Model), "raylib-guile ptr");
    Model v170_data = LoadModel(v169);
    memcpy(v170, &v170_data, sizeof(Model));
    SCM result = scm_make_foreign_object_1(rgtype_Model, v170);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadModelFromMesh(SCM mesh) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    void *v171 = scm_gc_malloc_pointerless(sizeof(Model), "raylib-guile ptr");
    Model v171_data = LoadModelFromMesh((*(Mesh*)scm_foreign_object_ref(mesh, 0)));
    memcpy(v171, &v171_data, sizeof(Model));
    SCM result = scm_make_foreign_object_1(rgtype_Model, v171);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsModelReady(SCM model) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    SCM result = scm_from_bool(IsModelReady((*(Model*)scm_foreign_object_ref(model, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadModel(SCM model) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    SCM result = (UnloadModel((*(Model*)scm_foreign_object_ref(model, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetModelBoundingBox(SCM model) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    void *v172 = scm_gc_malloc_pointerless(sizeof(BoundingBox), "raylib-guile ptr");
    BoundingBox v172_data = GetModelBoundingBox((*(Model*)scm_foreign_object_ref(model, 0)));
    memcpy(v172, &v172_data, sizeof(BoundingBox));
    SCM result = scm_make_foreign_object_1(rgtype_BoundingBox, v172);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawModel(SCM model, SCM position, SCM scale, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawModel((*(Model*)scm_foreign_object_ref(model, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(scale), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawModelEx(SCM model, SCM position, SCM rotationAxis, SCM rotationAngle, SCM scale, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, rotationAxis);
    scm_assert_foreign_object_type(rgtype_Vector3, scale);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawModelEx((*(Model*)scm_foreign_object_ref(model, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector3*)scm_foreign_object_ref(rotationAxis, 0)), scm_to_double(rotationAngle), (*(Vector3*)scm_foreign_object_ref(scale, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawModelWires(SCM model, SCM position, SCM scale, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawModelWires((*(Model*)scm_foreign_object_ref(model, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(scale), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawModelWiresEx(SCM model, SCM position, SCM rotationAxis, SCM rotationAngle, SCM scale, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, rotationAxis);
    scm_assert_foreign_object_type(rgtype_Vector3, scale);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawModelWiresEx((*(Model*)scm_foreign_object_ref(model, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector3*)scm_foreign_object_ref(rotationAxis, 0)), scm_to_double(rotationAngle), (*(Vector3*)scm_foreign_object_ref(scale, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawBoundingBox(SCM box, SCM color) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_BoundingBox, box);
    scm_assert_foreign_object_type(rgtype_Color, color);
    SCM result = (DrawBoundingBox((*(BoundingBox*)scm_foreign_object_ref(box, 0)), (*(Color*)scm_foreign_object_ref(color, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawBillboard(SCM camera, SCM texture, SCM position, SCM size, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawBillboard((*(Camera3D*)scm_foreign_object_ref(camera, 0)), (*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), scm_to_double(size), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawBillboardRec(SCM camera, SCM texture, SCM source, SCM position, SCM size, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector2, size);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawBillboardRec((*(Camera3D*)scm_foreign_object_ref(camera, 0)), (*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Rectangle*)scm_foreign_object_ref(source, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector2*)scm_foreign_object_ref(size, 0)), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawBillboardPro(SCM camera, SCM texture, SCM source, SCM position, SCM up, SCM size, SCM origin, SCM rotation, SCM tint) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Camera3D, camera);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    scm_assert_foreign_object_type(rgtype_Rectangle, source);
    scm_assert_foreign_object_type(rgtype_Vector3, position);
    scm_assert_foreign_object_type(rgtype_Vector3, up);
    scm_assert_foreign_object_type(rgtype_Vector2, size);
    scm_assert_foreign_object_type(rgtype_Vector2, origin);
    scm_assert_foreign_object_type(rgtype_Color, tint);
    SCM result = (DrawBillboardPro((*(Camera3D*)scm_foreign_object_ref(camera, 0)), (*(Texture*)scm_foreign_object_ref(texture, 0)), (*(Rectangle*)scm_foreign_object_ref(source, 0)), (*(Vector3*)scm_foreign_object_ref(position, 0)), (*(Vector3*)scm_foreign_object_ref(up, 0)), (*(Vector2*)scm_foreign_object_ref(size, 0)), (*(Vector2*)scm_foreign_object_ref(origin, 0)), scm_to_double(rotation), (*(Color*)scm_foreign_object_ref(tint, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UploadMesh(SCM mesh, SCM dynamic) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    SCM result = (UploadMesh(scm_foreign_object_ref(mesh, 0), scm_to_bool(dynamic)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadMesh(SCM mesh) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    SCM result = (UnloadMesh((*(Mesh*)scm_foreign_object_ref(mesh, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawMesh(SCM mesh, SCM material, SCM transform) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    scm_assert_foreign_object_type(rgtype_Material, material);
    scm_assert_foreign_object_type(rgtype_Matrix, transform);
    SCM result = (DrawMesh((*(Mesh*)scm_foreign_object_ref(mesh, 0)), (*(Material*)scm_foreign_object_ref(material, 0)), (*(Matrix*)scm_foreign_object_ref(transform, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_DrawMeshInstanced(SCM mesh, SCM material, SCM transforms, SCM instances) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    scm_assert_foreign_object_type(rgtype_Material, material);
    scm_assert_foreign_object_type(rgtype_Matrix, transforms);
    SCM result = (DrawMeshInstanced((*(Mesh*)scm_foreign_object_ref(mesh, 0)), (*(Material*)scm_foreign_object_ref(material, 0)), scm_foreign_object_ref(transforms, 0), scm_to_int(instances)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMeshBoundingBox(SCM mesh) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    void *v173 = scm_gc_malloc_pointerless(sizeof(BoundingBox), "raylib-guile ptr");
    BoundingBox v173_data = GetMeshBoundingBox((*(Mesh*)scm_foreign_object_ref(mesh, 0)));
    memcpy(v173, &v173_data, sizeof(BoundingBox));
    SCM result = scm_make_foreign_object_1(rgtype_BoundingBox, v173);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshTangents(SCM mesh) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    SCM result = (GenMeshTangents(scm_foreign_object_ref(mesh, 0)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportMesh(SCM mesh, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    char *v174 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v174);
    SCM result = scm_from_bool(ExportMesh((*(Mesh*)scm_foreign_object_ref(mesh, 0)), v174));
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportMeshAsCode(SCM mesh, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    char *v175 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v175);
    SCM result = scm_from_bool(ExportMeshAsCode((*(Mesh*)scm_foreign_object_ref(mesh, 0)), v175));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshPoly(SCM sides, SCM radius) {
    scm_dynwind_begin(0);
    void *v176 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v176_data = GenMeshPoly(scm_to_int(sides), scm_to_double(radius));
    memcpy(v176, &v176_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v176);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshPlane(SCM width, SCM length, SCM resX, SCM resZ) {
    scm_dynwind_begin(0);
    void *v177 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v177_data = GenMeshPlane(scm_to_double(width), scm_to_double(length), scm_to_int(resX), scm_to_int(resZ));
    memcpy(v177, &v177_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v177);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshCube(SCM width, SCM height, SCM length) {
    scm_dynwind_begin(0);
    void *v178 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v178_data = GenMeshCube(scm_to_double(width), scm_to_double(height), scm_to_double(length));
    memcpy(v178, &v178_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v178);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshSphere(SCM radius, SCM rings, SCM slices) {
    scm_dynwind_begin(0);
    void *v179 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v179_data = GenMeshSphere(scm_to_double(radius), scm_to_int(rings), scm_to_int(slices));
    memcpy(v179, &v179_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v179);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshHemiSphere(SCM radius, SCM rings, SCM slices) {
    scm_dynwind_begin(0);
    void *v180 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v180_data = GenMeshHemiSphere(scm_to_double(radius), scm_to_int(rings), scm_to_int(slices));
    memcpy(v180, &v180_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v180);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshCylinder(SCM radius, SCM height, SCM slices) {
    scm_dynwind_begin(0);
    void *v181 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v181_data = GenMeshCylinder(scm_to_double(radius), scm_to_double(height), scm_to_int(slices));
    memcpy(v181, &v181_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v181);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshCone(SCM radius, SCM height, SCM slices) {
    scm_dynwind_begin(0);
    void *v182 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v182_data = GenMeshCone(scm_to_double(radius), scm_to_double(height), scm_to_int(slices));
    memcpy(v182, &v182_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v182);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshTorus(SCM radius, SCM size, SCM radSeg, SCM sides) {
    scm_dynwind_begin(0);
    void *v183 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v183_data = GenMeshTorus(scm_to_double(radius), scm_to_double(size), scm_to_int(radSeg), scm_to_int(sides));
    memcpy(v183, &v183_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v183);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshKnot(SCM radius, SCM size, SCM radSeg, SCM sides) {
    scm_dynwind_begin(0);
    void *v184 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v184_data = GenMeshKnot(scm_to_double(radius), scm_to_double(size), scm_to_int(radSeg), scm_to_int(sides));
    memcpy(v184, &v184_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v184);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshHeightmap(SCM heightmap, SCM size) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, heightmap);
    scm_assert_foreign_object_type(rgtype_Vector3, size);
    void *v185 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v185_data = GenMeshHeightmap((*(Image*)scm_foreign_object_ref(heightmap, 0)), (*(Vector3*)scm_foreign_object_ref(size, 0)));
    memcpy(v185, &v185_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v185);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GenMeshCubicmap(SCM cubicmap, SCM cubeSize) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Image, cubicmap);
    scm_assert_foreign_object_type(rgtype_Vector3, cubeSize);
    void *v186 = scm_gc_malloc_pointerless(sizeof(Mesh), "raylib-guile ptr");
    Mesh v186_data = GenMeshCubicmap((*(Image*)scm_foreign_object_ref(cubicmap, 0)), (*(Vector3*)scm_foreign_object_ref(cubeSize, 0)));
    memcpy(v186, &v186_data, sizeof(Mesh));
    SCM result = scm_make_foreign_object_1(rgtype_Mesh, v186);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadMaterialDefault() {
    scm_dynwind_begin(0);
    void *v187 = scm_gc_malloc_pointerless(sizeof(Material), "raylib-guile ptr");
    Material v187_data = LoadMaterialDefault();
    memcpy(v187, &v187_data, sizeof(Material));
    SCM result = scm_make_foreign_object_1(rgtype_Material, v187);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMaterialReady(SCM material) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Material, material);
    SCM result = scm_from_bool(IsMaterialReady((*(Material*)scm_foreign_object_ref(material, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadMaterial(SCM material) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Material, material);
    SCM result = (UnloadMaterial((*(Material*)scm_foreign_object_ref(material, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMaterialTexture(SCM material, SCM mapType, SCM texture) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Material, material);
    scm_assert_foreign_object_type(rgtype_Texture, texture);
    SCM result = (SetMaterialTexture(scm_foreign_object_ref(material, 0), scm_to_int(mapType), (*(Texture*)scm_foreign_object_ref(texture, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetModelMeshMaterial(SCM model, SCM meshId, SCM materialId) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    SCM result = (SetModelMeshMaterial(scm_foreign_object_ref(model, 0), scm_to_int(meshId), scm_to_int(materialId)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UpdateModelAnimation(SCM model, SCM anim, SCM frame) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_ModelAnimation, anim);
    SCM result = (UpdateModelAnimation((*(Model*)scm_foreign_object_ref(model, 0)), (*(ModelAnimation*)scm_foreign_object_ref(anim, 0)), scm_to_int(frame)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadModelAnimation(SCM anim) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_ModelAnimation, anim);
    SCM result = (UnloadModelAnimation((*(ModelAnimation*)scm_foreign_object_ref(anim, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsModelAnimationValid(SCM model, SCM anim) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Model, model);
    scm_assert_foreign_object_type(rgtype_ModelAnimation, anim);
    SCM result = scm_from_bool(IsModelAnimationValid((*(Model*)scm_foreign_object_ref(model, 0)), (*(ModelAnimation*)scm_foreign_object_ref(anim, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionSpheres(SCM center1, SCM radius1, SCM center2, SCM radius2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Vector3, center1);
    scm_assert_foreign_object_type(rgtype_Vector3, center2);
    SCM result = scm_from_bool(CheckCollisionSpheres((*(Vector3*)scm_foreign_object_ref(center1, 0)), scm_to_double(radius1), (*(Vector3*)scm_foreign_object_ref(center2, 0)), scm_to_double(radius2)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionBoxes(SCM box1, SCM box2) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_BoundingBox, box1);
    scm_assert_foreign_object_type(rgtype_BoundingBox, box2);
    SCM result = scm_from_bool(CheckCollisionBoxes((*(BoundingBox*)scm_foreign_object_ref(box1, 0)), (*(BoundingBox*)scm_foreign_object_ref(box2, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_CheckCollisionBoxSphere(SCM box, SCM center, SCM radius) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_BoundingBox, box);
    scm_assert_foreign_object_type(rgtype_Vector3, center);
    SCM result = scm_from_bool(CheckCollisionBoxSphere((*(BoundingBox*)scm_foreign_object_ref(box, 0)), (*(Vector3*)scm_foreign_object_ref(center, 0)), scm_to_double(radius)));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRayCollisionSphere(SCM ray, SCM center, SCM radius) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_Vector3, center);
    void *v188 = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    RayCollision v188_data = GetRayCollisionSphere((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(Vector3*)scm_foreign_object_ref(center, 0)), scm_to_double(radius));
    memcpy(v188, &v188_data, sizeof(RayCollision));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, v188);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRayCollisionBox(SCM ray, SCM box) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_BoundingBox, box);
    void *v189 = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    RayCollision v189_data = GetRayCollisionBox((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(BoundingBox*)scm_foreign_object_ref(box, 0)));
    memcpy(v189, &v189_data, sizeof(RayCollision));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, v189);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRayCollisionMesh(SCM ray, SCM mesh, SCM transform) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_Mesh, mesh);
    scm_assert_foreign_object_type(rgtype_Matrix, transform);
    void *v190 = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    RayCollision v190_data = GetRayCollisionMesh((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(Mesh*)scm_foreign_object_ref(mesh, 0)), (*(Matrix*)scm_foreign_object_ref(transform, 0)));
    memcpy(v190, &v190_data, sizeof(RayCollision));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, v190);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRayCollisionTriangle(SCM ray, SCM p1, SCM p2, SCM p3) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_Vector3, p1);
    scm_assert_foreign_object_type(rgtype_Vector3, p2);
    scm_assert_foreign_object_type(rgtype_Vector3, p3);
    void *v191 = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    RayCollision v191_data = GetRayCollisionTriangle((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(Vector3*)scm_foreign_object_ref(p1, 0)), (*(Vector3*)scm_foreign_object_ref(p2, 0)), (*(Vector3*)scm_foreign_object_ref(p3, 0)));
    memcpy(v191, &v191_data, sizeof(RayCollision));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, v191);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetRayCollisionQuad(SCM ray, SCM p1, SCM p2, SCM p3, SCM p4) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Ray, ray);
    scm_assert_foreign_object_type(rgtype_Vector3, p1);
    scm_assert_foreign_object_type(rgtype_Vector3, p2);
    scm_assert_foreign_object_type(rgtype_Vector3, p3);
    scm_assert_foreign_object_type(rgtype_Vector3, p4);
    void *v192 = scm_gc_malloc_pointerless(sizeof(RayCollision), "raylib-guile ptr");
    RayCollision v192_data = GetRayCollisionQuad((*(Ray*)scm_foreign_object_ref(ray, 0)), (*(Vector3*)scm_foreign_object_ref(p1, 0)), (*(Vector3*)scm_foreign_object_ref(p2, 0)), (*(Vector3*)scm_foreign_object_ref(p3, 0)), (*(Vector3*)scm_foreign_object_ref(p4, 0)));
    memcpy(v192, &v192_data, sizeof(RayCollision));
    SCM result = scm_make_foreign_object_1(rgtype_RayCollision, v192);
    scm_dynwind_end();
    return result;
}

SCM rgfun_InitAudioDevice() {
    scm_dynwind_begin(0);
    SCM result = (InitAudioDevice(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_CloseAudioDevice() {
    scm_dynwind_begin(0);
    SCM result = (CloseAudioDevice(), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsAudioDeviceReady() {
    scm_dynwind_begin(0);
    SCM result = scm_from_bool(IsAudioDeviceReady());
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMasterVolume(SCM volume) {
    scm_dynwind_begin(0);
    SCM result = (SetMasterVolume(scm_to_double(volume)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMasterVolume() {
    scm_dynwind_begin(0);
    SCM result = scm_from_double(GetMasterVolume());
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadWave(SCM fileName) {
    scm_dynwind_begin(0);
    char *v193 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v193);
    void *v194 = scm_gc_malloc_pointerless(sizeof(Wave), "raylib-guile ptr");
    Wave v194_data = LoadWave(v193);
    memcpy(v194, &v194_data, sizeof(Wave));
    SCM result = scm_make_foreign_object_1(rgtype_Wave, v194);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadWaveFromMemory(SCM fileType, SCM fileData, SCM dataSize) {
    scm_dynwind_begin(0);
    char *v195 = scm_to_utf8_stringn(fileType, NULL);
    scm_dynwind_free(v195);
    char *v196 = scm_to_utf8_stringn(fileData, NULL);
    scm_dynwind_free(v196);
    void *v197 = scm_gc_malloc_pointerless(sizeof(Wave), "raylib-guile ptr");
    Wave v197_data = LoadWaveFromMemory(v195, v196, scm_to_int(dataSize));
    memcpy(v197, &v197_data, sizeof(Wave));
    SCM result = scm_make_foreign_object_1(rgtype_Wave, v197);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsWaveReady(SCM wave) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    SCM result = scm_from_bool(IsWaveReady((*(Wave*)scm_foreign_object_ref(wave, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadSound(SCM fileName) {
    scm_dynwind_begin(0);
    char *v198 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v198);
    void *v199 = scm_gc_malloc_pointerless(sizeof(Sound), "raylib-guile ptr");
    Sound v199_data = LoadSound(v198);
    memcpy(v199, &v199_data, sizeof(Sound));
    SCM result = scm_make_foreign_object_1(rgtype_Sound, v199);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadSoundFromWave(SCM wave) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    void *v200 = scm_gc_malloc_pointerless(sizeof(Sound), "raylib-guile ptr");
    Sound v200_data = LoadSoundFromWave((*(Wave*)scm_foreign_object_ref(wave, 0)));
    memcpy(v200, &v200_data, sizeof(Sound));
    SCM result = scm_make_foreign_object_1(rgtype_Sound, v200);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadSoundAlias(SCM source) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, source);
    void *v201 = scm_gc_malloc_pointerless(sizeof(Sound), "raylib-guile ptr");
    Sound v201_data = LoadSoundAlias((*(Sound*)scm_foreign_object_ref(source, 0)));
    memcpy(v201, &v201_data, sizeof(Sound));
    SCM result = scm_make_foreign_object_1(rgtype_Sound, v201);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsSoundReady(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = scm_from_bool(IsSoundReady((*(Sound*)scm_foreign_object_ref(sound, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadWave(SCM wave) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    SCM result = (UnloadWave((*(Wave*)scm_foreign_object_ref(wave, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadSound(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (UnloadSound((*(Sound*)scm_foreign_object_ref(sound, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadSoundAlias(SCM alias) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, alias);
    SCM result = (UnloadSoundAlias((*(Sound*)scm_foreign_object_ref(alias, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportWave(SCM wave, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    char *v202 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v202);
    SCM result = scm_from_bool(ExportWave((*(Wave*)scm_foreign_object_ref(wave, 0)), v202));
    scm_dynwind_end();
    return result;
}

SCM rgfun_ExportWaveAsCode(SCM wave, SCM fileName) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    char *v203 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v203);
    SCM result = scm_from_bool(ExportWaveAsCode((*(Wave*)scm_foreign_object_ref(wave, 0)), v203));
    scm_dynwind_end();
    return result;
}

SCM rgfun_PlaySound(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (PlaySound((*(Sound*)scm_foreign_object_ref(sound, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_StopSound(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (StopSound((*(Sound*)scm_foreign_object_ref(sound, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PauseSound(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (PauseSound((*(Sound*)scm_foreign_object_ref(sound, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ResumeSound(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (ResumeSound((*(Sound*)scm_foreign_object_ref(sound, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsSoundPlaying(SCM sound) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = scm_from_bool(IsSoundPlaying((*(Sound*)scm_foreign_object_ref(sound, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetSoundVolume(SCM sound, SCM volume) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (SetSoundVolume((*(Sound*)scm_foreign_object_ref(sound, 0)), scm_to_double(volume)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetSoundPitch(SCM sound, SCM pitch) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (SetSoundPitch((*(Sound*)scm_foreign_object_ref(sound, 0)), scm_to_double(pitch)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetSoundPan(SCM sound, SCM pan) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Sound, sound);
    SCM result = (SetSoundPan((*(Sound*)scm_foreign_object_ref(sound, 0)), scm_to_double(pan)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_WaveCopy(SCM wave) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    void *v204 = scm_gc_malloc_pointerless(sizeof(Wave), "raylib-guile ptr");
    Wave v204_data = WaveCopy((*(Wave*)scm_foreign_object_ref(wave, 0)));
    memcpy(v204, &v204_data, sizeof(Wave));
    SCM result = scm_make_foreign_object_1(rgtype_Wave, v204);
    scm_dynwind_end();
    return result;
}

SCM rgfun_WaveCrop(SCM wave, SCM initSample, SCM finalSample) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    SCM result = (WaveCrop(scm_foreign_object_ref(wave, 0), scm_to_int(initSample), scm_to_int(finalSample)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_WaveFormat(SCM wave, SCM sampleRate, SCM sampleSize, SCM channels) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Wave, wave);
    SCM result = (WaveFormat(scm_foreign_object_ref(wave, 0), scm_to_int(sampleRate), scm_to_int(sampleSize), scm_to_int(channels)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadMusicStream(SCM fileName) {
    scm_dynwind_begin(0);
    char *v205 = scm_to_utf8_stringn(fileName, NULL);
    scm_dynwind_free(v205);
    void *v206 = scm_gc_malloc_pointerless(sizeof(Music), "raylib-guile ptr");
    Music v206_data = LoadMusicStream(v205);
    memcpy(v206, &v206_data, sizeof(Music));
    SCM result = scm_make_foreign_object_1(rgtype_Music, v206);
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadMusicStreamFromMemory(SCM fileType, SCM data, SCM dataSize) {
    scm_dynwind_begin(0);
    char *v207 = scm_to_utf8_stringn(fileType, NULL);
    scm_dynwind_free(v207);
    char *v208 = scm_to_utf8_stringn(data, NULL);
    scm_dynwind_free(v208);
    void *v209 = scm_gc_malloc_pointerless(sizeof(Music), "raylib-guile ptr");
    Music v209_data = LoadMusicStreamFromMemory(v207, v208, scm_to_int(dataSize));
    memcpy(v209, &v209_data, sizeof(Music));
    SCM result = scm_make_foreign_object_1(rgtype_Music, v209);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMusicReady(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = scm_from_bool(IsMusicReady((*(Music*)scm_foreign_object_ref(music, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (UnloadMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PlayMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (PlayMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsMusicStreamPlaying(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = scm_from_bool(IsMusicStreamPlaying((*(Music*)scm_foreign_object_ref(music, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UpdateMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (UpdateMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_StopMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (StopMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PauseMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (PauseMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ResumeMusicStream(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (ResumeMusicStream((*(Music*)scm_foreign_object_ref(music, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SeekMusicStream(SCM music, SCM position) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (SeekMusicStream((*(Music*)scm_foreign_object_ref(music, 0)), scm_to_double(position)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMusicVolume(SCM music, SCM volume) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (SetMusicVolume((*(Music*)scm_foreign_object_ref(music, 0)), scm_to_double(volume)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMusicPitch(SCM music, SCM pitch) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (SetMusicPitch((*(Music*)scm_foreign_object_ref(music, 0)), scm_to_double(pitch)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetMusicPan(SCM music, SCM pan) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = (SetMusicPan((*(Music*)scm_foreign_object_ref(music, 0)), scm_to_double(pan)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMusicTimeLength(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = scm_from_double(GetMusicTimeLength((*(Music*)scm_foreign_object_ref(music, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_GetMusicTimePlayed(SCM music) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_Music, music);
    SCM result = scm_from_double(GetMusicTimePlayed((*(Music*)scm_foreign_object_ref(music, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_LoadAudioStream(SCM sampleRate, SCM sampleSize, SCM channels) {
    scm_dynwind_begin(0);
    void *v210 = scm_gc_malloc_pointerless(sizeof(AudioStream), "raylib-guile ptr");
    AudioStream v210_data = LoadAudioStream(scm_to_uint(sampleRate), scm_to_uint(sampleSize), scm_to_uint(channels));
    memcpy(v210, &v210_data, sizeof(AudioStream));
    SCM result = scm_make_foreign_object_1(rgtype_AudioStream, v210);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsAudioStreamReady(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = scm_from_bool(IsAudioStreamReady((*(AudioStream*)scm_foreign_object_ref(stream, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_UnloadAudioStream(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (UnloadAudioStream((*(AudioStream*)scm_foreign_object_ref(stream, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsAudioStreamProcessed(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = scm_from_bool(IsAudioStreamProcessed((*(AudioStream*)scm_foreign_object_ref(stream, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_PlayAudioStream(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (PlayAudioStream((*(AudioStream*)scm_foreign_object_ref(stream, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_PauseAudioStream(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (PauseAudioStream((*(AudioStream*)scm_foreign_object_ref(stream, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_ResumeAudioStream(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (ResumeAudioStream((*(AudioStream*)scm_foreign_object_ref(stream, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_IsAudioStreamPlaying(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = scm_from_bool(IsAudioStreamPlaying((*(AudioStream*)scm_foreign_object_ref(stream, 0))));
    scm_dynwind_end();
    return result;
}

SCM rgfun_StopAudioStream(SCM stream) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (StopAudioStream((*(AudioStream*)scm_foreign_object_ref(stream, 0))), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAudioStreamVolume(SCM stream, SCM volume) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (SetAudioStreamVolume((*(AudioStream*)scm_foreign_object_ref(stream, 0)), scm_to_double(volume)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAudioStreamPitch(SCM stream, SCM pitch) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (SetAudioStreamPitch((*(AudioStream*)scm_foreign_object_ref(stream, 0)), scm_to_double(pitch)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAudioStreamPan(SCM stream, SCM pan) {
    scm_dynwind_begin(0);
    scm_assert_foreign_object_type(rgtype_AudioStream, stream);
    SCM result = (SetAudioStreamPan((*(AudioStream*)scm_foreign_object_ref(stream, 0)), scm_to_double(pan)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}

SCM rgfun_SetAudioStreamBufferSizeDefault(SCM size) {
    scm_dynwind_begin(0);
    SCM result = (SetAudioStreamBufferSizeDefault(scm_to_int(size)), SCM_UNSPECIFIED);
    scm_dynwind_end();
    return result;
}


// guile extension entry point
void init_raylib_guile(void) {
    // expose raylib structs to guile
    SCM slots = scm_list_1 (scm_from_utf8_symbol ("data"));
    rgtype_Vector2 = scm_make_foreign_object_type(scm_from_utf8_symbol("Vector2"), slots, NULL);
    rgtype_Vector3 = scm_make_foreign_object_type(scm_from_utf8_symbol("Vector3"), slots, NULL);
    rgtype_Vector4 = scm_make_foreign_object_type(scm_from_utf8_symbol("Vector4"), slots, NULL);
    rgtype_Matrix = scm_make_foreign_object_type(scm_from_utf8_symbol("Matrix"), slots, NULL);
    rgtype_Color = scm_make_foreign_object_type(scm_from_utf8_symbol("Color"), slots, NULL);
    rgtype_Rectangle = scm_make_foreign_object_type(scm_from_utf8_symbol("Rectangle"), slots, NULL);
    rgtype_Image = scm_make_foreign_object_type(scm_from_utf8_symbol("Image"), slots, NULL);
    rgtype_Texture = scm_make_foreign_object_type(scm_from_utf8_symbol("Texture"), slots, NULL);
    rgtype_RenderTexture = scm_make_foreign_object_type(scm_from_utf8_symbol("RenderTexture"), slots, NULL);
    rgtype_NPatchInfo = scm_make_foreign_object_type(scm_from_utf8_symbol("NPatchInfo"), slots, NULL);
    rgtype_GlyphInfo = scm_make_foreign_object_type(scm_from_utf8_symbol("GlyphInfo"), slots, NULL);
    rgtype_Font = scm_make_foreign_object_type(scm_from_utf8_symbol("Font"), slots, NULL);
    rgtype_Camera3D = scm_make_foreign_object_type(scm_from_utf8_symbol("Camera3D"), slots, NULL);
    rgtype_Camera2D = scm_make_foreign_object_type(scm_from_utf8_symbol("Camera2D"), slots, NULL);
    rgtype_Mesh = scm_make_foreign_object_type(scm_from_utf8_symbol("Mesh"), slots, NULL);
    rgtype_Shader = scm_make_foreign_object_type(scm_from_utf8_symbol("Shader"), slots, NULL);
    rgtype_MaterialMap = scm_make_foreign_object_type(scm_from_utf8_symbol("MaterialMap"), slots, NULL);
    rgtype_Material = scm_make_foreign_object_type(scm_from_utf8_symbol("Material"), slots, NULL);
    rgtype_Transform = scm_make_foreign_object_type(scm_from_utf8_symbol("Transform"), slots, NULL);
    rgtype_BoneInfo = scm_make_foreign_object_type(scm_from_utf8_symbol("BoneInfo"), slots, NULL);
    rgtype_Model = scm_make_foreign_object_type(scm_from_utf8_symbol("Model"), slots, NULL);
    rgtype_ModelAnimation = scm_make_foreign_object_type(scm_from_utf8_symbol("ModelAnimation"), slots, NULL);
    rgtype_Ray = scm_make_foreign_object_type(scm_from_utf8_symbol("Ray"), slots, NULL);
    rgtype_RayCollision = scm_make_foreign_object_type(scm_from_utf8_symbol("RayCollision"), slots, NULL);
    rgtype_BoundingBox = scm_make_foreign_object_type(scm_from_utf8_symbol("BoundingBox"), slots, NULL);
    rgtype_Wave = scm_make_foreign_object_type(scm_from_utf8_symbol("Wave"), slots, NULL);
    rgtype_AudioStream = scm_make_foreign_object_type(scm_from_utf8_symbol("AudioStream"), slots, NULL);
    rgtype_Sound = scm_make_foreign_object_type(scm_from_utf8_symbol("Sound"), slots, NULL);
    rgtype_Music = scm_make_foreign_object_type(scm_from_utf8_symbol("Music"), slots, NULL);
    rgtype_VrDeviceInfo = scm_make_foreign_object_type(scm_from_utf8_symbol("VrDeviceInfo"), slots, NULL);
    rgtype_VrStereoConfig = scm_make_foreign_object_type(scm_from_utf8_symbol("VrStereoConfig"), slots, NULL);
    rgtype_FilePathList = scm_make_foreign_object_type(scm_from_utf8_symbol("FilePathList"), slots, NULL);
    rgtype_AutomationEvent = scm_make_foreign_object_type(scm_from_utf8_symbol("AutomationEvent"), slots, NULL);
    rgtype_AutomationEventList = scm_make_foreign_object_type(scm_from_utf8_symbol("AutomationEventList"), slots, NULL);
    // expose raylib accessors to guile
    scm_c_define_gsubr("make-Sound", 2, 0, 0, rgacc_make_Sound);
    scm_c_define_gsubr("Sound-stream", 1, 0, 0, rgacc_Sound_stream);
    scm_c_define_gsubr("Sound-frameCount", 1, 0, 0, rgacc_Sound_frameCount);
    scm_c_define_gsubr("Sound-set-stream!", 2, 0, 0, rgacc_Sound_set_stream);
    scm_c_define_gsubr("Sound-set-frameCount!", 2, 0, 0, rgacc_Sound_set_frameCount);
    scm_c_define_gsubr("make-BoundingBox", 2, 0, 0, rgacc_make_BoundingBox);
    scm_c_define_gsubr("BoundingBox-min", 1, 0, 0, rgacc_BoundingBox_min);
    scm_c_define_gsubr("BoundingBox-max", 1, 0, 0, rgacc_BoundingBox_max);
    scm_c_define_gsubr("BoundingBox-set-min!", 2, 0, 0, rgacc_BoundingBox_set_min);
    scm_c_define_gsubr("BoundingBox-set-max!", 2, 0, 0, rgacc_BoundingBox_set_max);
    scm_c_define_gsubr("make-RayCollision", 4, 0, 0, rgacc_make_RayCollision);
    scm_c_define_gsubr("RayCollision-hit", 1, 0, 0, rgacc_RayCollision_hit);
    scm_c_define_gsubr("RayCollision-distance", 1, 0, 0, rgacc_RayCollision_distance);
    scm_c_define_gsubr("RayCollision-point", 1, 0, 0, rgacc_RayCollision_point);
    scm_c_define_gsubr("RayCollision-normal", 1, 0, 0, rgacc_RayCollision_normal);
    scm_c_define_gsubr("RayCollision-set-hit!", 2, 0, 0, rgacc_RayCollision_set_hit);
    scm_c_define_gsubr("RayCollision-set-distance!", 2, 0, 0, rgacc_RayCollision_set_distance);
    scm_c_define_gsubr("RayCollision-set-point!", 2, 0, 0, rgacc_RayCollision_set_point);
    scm_c_define_gsubr("RayCollision-set-normal!", 2, 0, 0, rgacc_RayCollision_set_normal);
    scm_c_define_gsubr("make-Ray", 2, 0, 0, rgacc_make_Ray);
    scm_c_define_gsubr("Ray-position", 1, 0, 0, rgacc_Ray_position);
    scm_c_define_gsubr("Ray-direction", 1, 0, 0, rgacc_Ray_direction);
    scm_c_define_gsubr("Ray-set-position!", 2, 0, 0, rgacc_Ray_set_position);
    scm_c_define_gsubr("Ray-set-direction!", 2, 0, 0, rgacc_Ray_set_direction);
    scm_c_define_gsubr("make-Transform", 3, 0, 0, rgacc_make_Transform);
    scm_c_define_gsubr("Transform-translation", 1, 0, 0, rgacc_Transform_translation);
    scm_c_define_gsubr("Transform-rotation", 1, 0, 0, rgacc_Transform_rotation);
    scm_c_define_gsubr("Transform-scale", 1, 0, 0, rgacc_Transform_scale);
    scm_c_define_gsubr("Transform-set-translation!", 2, 0, 0, rgacc_Transform_set_translation);
    scm_c_define_gsubr("Transform-set-rotation!", 2, 0, 0, rgacc_Transform_set_rotation);
    scm_c_define_gsubr("Transform-set-scale!", 2, 0, 0, rgacc_Transform_set_scale);
    scm_c_define_gsubr("make-MaterialMap", 3, 0, 0, rgacc_make_MaterialMap);
    scm_c_define_gsubr("MaterialMap-texture", 1, 0, 0, rgacc_MaterialMap_texture);
    scm_c_define_gsubr("MaterialMap-color", 1, 0, 0, rgacc_MaterialMap_color);
    scm_c_define_gsubr("MaterialMap-value", 1, 0, 0, rgacc_MaterialMap_value);
    scm_c_define_gsubr("MaterialMap-set-texture!", 2, 0, 0, rgacc_MaterialMap_set_texture);
    scm_c_define_gsubr("MaterialMap-set-color!", 2, 0, 0, rgacc_MaterialMap_set_color);
    scm_c_define_gsubr("MaterialMap-set-value!", 2, 0, 0, rgacc_MaterialMap_set_value);
    scm_c_define_gsubr("make-Camera2D", 4, 0, 0, rgacc_make_Camera2D);
    scm_c_define_gsubr("Camera2D-offset", 1, 0, 0, rgacc_Camera2D_offset);
    scm_c_define_gsubr("Camera2D-target", 1, 0, 0, rgacc_Camera2D_target);
    scm_c_define_gsubr("Camera2D-rotation", 1, 0, 0, rgacc_Camera2D_rotation);
    scm_c_define_gsubr("Camera2D-zoom", 1, 0, 0, rgacc_Camera2D_zoom);
    scm_c_define_gsubr("Camera2D-set-offset!", 2, 0, 0, rgacc_Camera2D_set_offset);
    scm_c_define_gsubr("Camera2D-set-target!", 2, 0, 0, rgacc_Camera2D_set_target);
    scm_c_define_gsubr("Camera2D-set-rotation!", 2, 0, 0, rgacc_Camera2D_set_rotation);
    scm_c_define_gsubr("Camera2D-set-zoom!", 2, 0, 0, rgacc_Camera2D_set_zoom);
    scm_c_define_gsubr("make-Camera3D", 5, 0, 0, rgacc_make_Camera3D);
    scm_c_define_gsubr("Camera3D-position", 1, 0, 0, rgacc_Camera3D_position);
    scm_c_define_gsubr("Camera3D-target", 1, 0, 0, rgacc_Camera3D_target);
    scm_c_define_gsubr("Camera3D-up", 1, 0, 0, rgacc_Camera3D_up);
    scm_c_define_gsubr("Camera3D-fovy", 1, 0, 0, rgacc_Camera3D_fovy);
    scm_c_define_gsubr("Camera3D-projection", 1, 0, 0, rgacc_Camera3D_projection);
    scm_c_define_gsubr("Camera3D-set-position!", 2, 0, 0, rgacc_Camera3D_set_position);
    scm_c_define_gsubr("Camera3D-set-target!", 2, 0, 0, rgacc_Camera3D_set_target);
    scm_c_define_gsubr("Camera3D-set-up!", 2, 0, 0, rgacc_Camera3D_set_up);
    scm_c_define_gsubr("Camera3D-set-fovy!", 2, 0, 0, rgacc_Camera3D_set_fovy);
    scm_c_define_gsubr("Camera3D-set-projection!", 2, 0, 0, rgacc_Camera3D_set_projection);
    scm_c_define_gsubr("make-GlyphInfo", 5, 0, 0, rgacc_make_GlyphInfo);
    scm_c_define_gsubr("GlyphInfo-value", 1, 0, 0, rgacc_GlyphInfo_value);
    scm_c_define_gsubr("GlyphInfo-offsetX", 1, 0, 0, rgacc_GlyphInfo_offsetX);
    scm_c_define_gsubr("GlyphInfo-offsetY", 1, 0, 0, rgacc_GlyphInfo_offsetY);
    scm_c_define_gsubr("GlyphInfo-advanceX", 1, 0, 0, rgacc_GlyphInfo_advanceX);
    scm_c_define_gsubr("GlyphInfo-image", 1, 0, 0, rgacc_GlyphInfo_image);
    scm_c_define_gsubr("GlyphInfo-set-value!", 2, 0, 0, rgacc_GlyphInfo_set_value);
    scm_c_define_gsubr("GlyphInfo-set-offsetX!", 2, 0, 0, rgacc_GlyphInfo_set_offsetX);
    scm_c_define_gsubr("GlyphInfo-set-offsetY!", 2, 0, 0, rgacc_GlyphInfo_set_offsetY);
    scm_c_define_gsubr("GlyphInfo-set-advanceX!", 2, 0, 0, rgacc_GlyphInfo_set_advanceX);
    scm_c_define_gsubr("GlyphInfo-set-image!", 2, 0, 0, rgacc_GlyphInfo_set_image);
    scm_c_define_gsubr("make-NPatchInfo", 6, 0, 0, rgacc_make_NPatchInfo);
    scm_c_define_gsubr("NPatchInfo-source", 1, 0, 0, rgacc_NPatchInfo_source);
    scm_c_define_gsubr("NPatchInfo-left", 1, 0, 0, rgacc_NPatchInfo_left);
    scm_c_define_gsubr("NPatchInfo-top", 1, 0, 0, rgacc_NPatchInfo_top);
    scm_c_define_gsubr("NPatchInfo-right", 1, 0, 0, rgacc_NPatchInfo_right);
    scm_c_define_gsubr("NPatchInfo-bottom", 1, 0, 0, rgacc_NPatchInfo_bottom);
    scm_c_define_gsubr("NPatchInfo-layout", 1, 0, 0, rgacc_NPatchInfo_layout);
    scm_c_define_gsubr("NPatchInfo-set-source!", 2, 0, 0, rgacc_NPatchInfo_set_source);
    scm_c_define_gsubr("NPatchInfo-set-left!", 2, 0, 0, rgacc_NPatchInfo_set_left);
    scm_c_define_gsubr("NPatchInfo-set-top!", 2, 0, 0, rgacc_NPatchInfo_set_top);
    scm_c_define_gsubr("NPatchInfo-set-right!", 2, 0, 0, rgacc_NPatchInfo_set_right);
    scm_c_define_gsubr("NPatchInfo-set-bottom!", 2, 0, 0, rgacc_NPatchInfo_set_bottom);
    scm_c_define_gsubr("NPatchInfo-set-layout!", 2, 0, 0, rgacc_NPatchInfo_set_layout);
    scm_c_define_gsubr("make-RenderTexture", 3, 0, 0, rgacc_make_RenderTexture);
    scm_c_define_gsubr("RenderTexture-id", 1, 0, 0, rgacc_RenderTexture_id);
    scm_c_define_gsubr("RenderTexture-texture", 1, 0, 0, rgacc_RenderTexture_texture);
    scm_c_define_gsubr("RenderTexture-depth", 1, 0, 0, rgacc_RenderTexture_depth);
    scm_c_define_gsubr("RenderTexture-set-id!", 2, 0, 0, rgacc_RenderTexture_set_id);
    scm_c_define_gsubr("RenderTexture-set-texture!", 2, 0, 0, rgacc_RenderTexture_set_texture);
    scm_c_define_gsubr("RenderTexture-set-depth!", 2, 0, 0, rgacc_RenderTexture_set_depth);
    scm_c_define_gsubr("make-Texture", 5, 0, 0, rgacc_make_Texture);
    scm_c_define_gsubr("Texture-id", 1, 0, 0, rgacc_Texture_id);
    scm_c_define_gsubr("Texture-width", 1, 0, 0, rgacc_Texture_width);
    scm_c_define_gsubr("Texture-height", 1, 0, 0, rgacc_Texture_height);
    scm_c_define_gsubr("Texture-mipmaps", 1, 0, 0, rgacc_Texture_mipmaps);
    scm_c_define_gsubr("Texture-format", 1, 0, 0, rgacc_Texture_format);
    scm_c_define_gsubr("Texture-set-id!", 2, 0, 0, rgacc_Texture_set_id);
    scm_c_define_gsubr("Texture-set-width!", 2, 0, 0, rgacc_Texture_set_width);
    scm_c_define_gsubr("Texture-set-height!", 2, 0, 0, rgacc_Texture_set_height);
    scm_c_define_gsubr("Texture-set-mipmaps!", 2, 0, 0, rgacc_Texture_set_mipmaps);
    scm_c_define_gsubr("Texture-set-format!", 2, 0, 0, rgacc_Texture_set_format);
    scm_c_define_gsubr("make-Rectangle", 4, 0, 0, rgacc_make_Rectangle);
    scm_c_define_gsubr("Rectangle-x", 1, 0, 0, rgacc_Rectangle_x);
    scm_c_define_gsubr("Rectangle-y", 1, 0, 0, rgacc_Rectangle_y);
    scm_c_define_gsubr("Rectangle-width", 1, 0, 0, rgacc_Rectangle_width);
    scm_c_define_gsubr("Rectangle-height", 1, 0, 0, rgacc_Rectangle_height);
    scm_c_define_gsubr("Rectangle-set-x!", 2, 0, 0, rgacc_Rectangle_set_x);
    scm_c_define_gsubr("Rectangle-set-y!", 2, 0, 0, rgacc_Rectangle_set_y);
    scm_c_define_gsubr("Rectangle-set-width!", 2, 0, 0, rgacc_Rectangle_set_width);
    scm_c_define_gsubr("Rectangle-set-height!", 2, 0, 0, rgacc_Rectangle_set_height);
    scm_c_define_gsubr("make-Color", 4, 0, 0, rgacc_make_Color);
    scm_c_define_gsubr("Color-r", 1, 0, 0, rgacc_Color_r);
    scm_c_define_gsubr("Color-g", 1, 0, 0, rgacc_Color_g);
    scm_c_define_gsubr("Color-b", 1, 0, 0, rgacc_Color_b);
    scm_c_define_gsubr("Color-a", 1, 0, 0, rgacc_Color_a);
    scm_c_define_gsubr("Color-set-r!", 2, 0, 0, rgacc_Color_set_r);
    scm_c_define_gsubr("Color-set-g!", 2, 0, 0, rgacc_Color_set_g);
    scm_c_define_gsubr("Color-set-b!", 2, 0, 0, rgacc_Color_set_b);
    scm_c_define_gsubr("Color-set-a!", 2, 0, 0, rgacc_Color_set_a);
    scm_c_define_gsubr("construct-Matrix", 0, 0, 0, rgacc_make_Matrix);
    scm_c_define_gsubr("Matrix-m0", 1, 0, 0, rgacc_Matrix_m0);
    scm_c_define_gsubr("Matrix-m4", 1, 0, 0, rgacc_Matrix_m4);
    scm_c_define_gsubr("Matrix-m8", 1, 0, 0, rgacc_Matrix_m8);
    scm_c_define_gsubr("Matrix-m12", 1, 0, 0, rgacc_Matrix_m12);
    scm_c_define_gsubr("Matrix-m1", 1, 0, 0, rgacc_Matrix_m1);
    scm_c_define_gsubr("Matrix-m5", 1, 0, 0, rgacc_Matrix_m5);
    scm_c_define_gsubr("Matrix-m9", 1, 0, 0, rgacc_Matrix_m9);
    scm_c_define_gsubr("Matrix-m13", 1, 0, 0, rgacc_Matrix_m13);
    scm_c_define_gsubr("Matrix-m2", 1, 0, 0, rgacc_Matrix_m2);
    scm_c_define_gsubr("Matrix-m6", 1, 0, 0, rgacc_Matrix_m6);
    scm_c_define_gsubr("Matrix-m10", 1, 0, 0, rgacc_Matrix_m10);
    scm_c_define_gsubr("Matrix-m14", 1, 0, 0, rgacc_Matrix_m14);
    scm_c_define_gsubr("Matrix-m3", 1, 0, 0, rgacc_Matrix_m3);
    scm_c_define_gsubr("Matrix-m7", 1, 0, 0, rgacc_Matrix_m7);
    scm_c_define_gsubr("Matrix-m11", 1, 0, 0, rgacc_Matrix_m11);
    scm_c_define_gsubr("Matrix-m15", 1, 0, 0, rgacc_Matrix_m15);
    scm_c_define_gsubr("Matrix-set-m0!", 2, 0, 0, rgacc_Matrix_set_m0);
    scm_c_define_gsubr("Matrix-set-m4!", 2, 0, 0, rgacc_Matrix_set_m4);
    scm_c_define_gsubr("Matrix-set-m8!", 2, 0, 0, rgacc_Matrix_set_m8);
    scm_c_define_gsubr("Matrix-set-m12!", 2, 0, 0, rgacc_Matrix_set_m12);
    scm_c_define_gsubr("Matrix-set-m1!", 2, 0, 0, rgacc_Matrix_set_m1);
    scm_c_define_gsubr("Matrix-set-m5!", 2, 0, 0, rgacc_Matrix_set_m5);
    scm_c_define_gsubr("Matrix-set-m9!", 2, 0, 0, rgacc_Matrix_set_m9);
    scm_c_define_gsubr("Matrix-set-m13!", 2, 0, 0, rgacc_Matrix_set_m13);
    scm_c_define_gsubr("Matrix-set-m2!", 2, 0, 0, rgacc_Matrix_set_m2);
    scm_c_define_gsubr("Matrix-set-m6!", 2, 0, 0, rgacc_Matrix_set_m6);
    scm_c_define_gsubr("Matrix-set-m10!", 2, 0, 0, rgacc_Matrix_set_m10);
    scm_c_define_gsubr("Matrix-set-m14!", 2, 0, 0, rgacc_Matrix_set_m14);
    scm_c_define_gsubr("Matrix-set-m3!", 2, 0, 0, rgacc_Matrix_set_m3);
    scm_c_define_gsubr("Matrix-set-m7!", 2, 0, 0, rgacc_Matrix_set_m7);
    scm_c_define_gsubr("Matrix-set-m11!", 2, 0, 0, rgacc_Matrix_set_m11);
    scm_c_define_gsubr("Matrix-set-m15!", 2, 0, 0, rgacc_Matrix_set_m15);
    scm_c_define_gsubr("make-Vector4", 4, 0, 0, rgacc_make_Vector4);
    scm_c_define_gsubr("Vector4-x", 1, 0, 0, rgacc_Vector4_x);
    scm_c_define_gsubr("Vector4-y", 1, 0, 0, rgacc_Vector4_y);
    scm_c_define_gsubr("Vector4-z", 1, 0, 0, rgacc_Vector4_z);
    scm_c_define_gsubr("Vector4-w", 1, 0, 0, rgacc_Vector4_w);
    scm_c_define_gsubr("Vector4-set-x!", 2, 0, 0, rgacc_Vector4_set_x);
    scm_c_define_gsubr("Vector4-set-y!", 2, 0, 0, rgacc_Vector4_set_y);
    scm_c_define_gsubr("Vector4-set-z!", 2, 0, 0, rgacc_Vector4_set_z);
    scm_c_define_gsubr("Vector4-set-w!", 2, 0, 0, rgacc_Vector4_set_w);
    scm_c_define_gsubr("make-Vector3", 3, 0, 0, rgacc_make_Vector3);
    scm_c_define_gsubr("Vector3-x", 1, 0, 0, rgacc_Vector3_x);
    scm_c_define_gsubr("Vector3-y", 1, 0, 0, rgacc_Vector3_y);
    scm_c_define_gsubr("Vector3-z", 1, 0, 0, rgacc_Vector3_z);
    scm_c_define_gsubr("Vector3-set-x!", 2, 0, 0, rgacc_Vector3_set_x);
    scm_c_define_gsubr("Vector3-set-y!", 2, 0, 0, rgacc_Vector3_set_y);
    scm_c_define_gsubr("Vector3-set-z!", 2, 0, 0, rgacc_Vector3_set_z);
    scm_c_define_gsubr("make-Vector2", 2, 0, 0, rgacc_make_Vector2);
    scm_c_define_gsubr("Vector2-x", 1, 0, 0, rgacc_Vector2_x);
    scm_c_define_gsubr("Vector2-y", 1, 0, 0, rgacc_Vector2_y);
    scm_c_define_gsubr("Vector2-set-x!", 2, 0, 0, rgacc_Vector2_set_x);
    scm_c_define_gsubr("Vector2-set-y!", 2, 0, 0, rgacc_Vector2_set_y);
    // expose raylib functions to guile
    scm_c_define_gsubr("InitWindow", 3, 0, 0, rgfun_InitWindow);
    scm_c_define_gsubr("CloseWindow", 0, 0, 0, rgfun_CloseWindow);
    scm_c_define_gsubr("WindowShouldClose", 0, 0, 0, rgfun_WindowShouldClose);
    scm_c_define_gsubr("IsWindowReady", 0, 0, 0, rgfun_IsWindowReady);
    scm_c_define_gsubr("IsWindowFullscreen", 0, 0, 0, rgfun_IsWindowFullscreen);
    scm_c_define_gsubr("IsWindowHidden", 0, 0, 0, rgfun_IsWindowHidden);
    scm_c_define_gsubr("IsWindowMinimized", 0, 0, 0, rgfun_IsWindowMinimized);
    scm_c_define_gsubr("IsWindowMaximized", 0, 0, 0, rgfun_IsWindowMaximized);
    scm_c_define_gsubr("IsWindowFocused", 0, 0, 0, rgfun_IsWindowFocused);
    scm_c_define_gsubr("IsWindowResized", 0, 0, 0, rgfun_IsWindowResized);
    scm_c_define_gsubr("IsWindowState", 1, 0, 0, rgfun_IsWindowState);
    scm_c_define_gsubr("SetWindowState", 1, 0, 0, rgfun_SetWindowState);
    scm_c_define_gsubr("ClearWindowState", 1, 0, 0, rgfun_ClearWindowState);
    scm_c_define_gsubr("ToggleFullscreen", 0, 0, 0, rgfun_ToggleFullscreen);
    scm_c_define_gsubr("ToggleBorderlessWindowed", 0, 0, 0, rgfun_ToggleBorderlessWindowed);
    scm_c_define_gsubr("MaximizeWindow", 0, 0, 0, rgfun_MaximizeWindow);
    scm_c_define_gsubr("MinimizeWindow", 0, 0, 0, rgfun_MinimizeWindow);
    scm_c_define_gsubr("RestoreWindow", 0, 0, 0, rgfun_RestoreWindow);
    scm_c_define_gsubr("SetWindowIcon", 1, 0, 0, rgfun_SetWindowIcon);
    scm_c_define_gsubr("SetWindowIcons", 2, 0, 0, rgfun_SetWindowIcons);
    scm_c_define_gsubr("SetWindowTitle", 1, 0, 0, rgfun_SetWindowTitle);
    scm_c_define_gsubr("SetWindowPosition", 2, 0, 0, rgfun_SetWindowPosition);
    scm_c_define_gsubr("SetWindowMonitor", 1, 0, 0, rgfun_SetWindowMonitor);
    scm_c_define_gsubr("SetWindowMinSize", 2, 0, 0, rgfun_SetWindowMinSize);
    scm_c_define_gsubr("SetWindowMaxSize", 2, 0, 0, rgfun_SetWindowMaxSize);
    scm_c_define_gsubr("SetWindowSize", 2, 0, 0, rgfun_SetWindowSize);
    scm_c_define_gsubr("SetWindowOpacity", 1, 0, 0, rgfun_SetWindowOpacity);
    scm_c_define_gsubr("SetWindowFocused", 0, 0, 0, rgfun_SetWindowFocused);
    scm_c_define_gsubr("GetScreenWidth", 0, 0, 0, rgfun_GetScreenWidth);
    scm_c_define_gsubr("GetScreenHeight", 0, 0, 0, rgfun_GetScreenHeight);
    scm_c_define_gsubr("GetRenderWidth", 0, 0, 0, rgfun_GetRenderWidth);
    scm_c_define_gsubr("GetRenderHeight", 0, 0, 0, rgfun_GetRenderHeight);
    scm_c_define_gsubr("GetMonitorCount", 0, 0, 0, rgfun_GetMonitorCount);
    scm_c_define_gsubr("GetCurrentMonitor", 0, 0, 0, rgfun_GetCurrentMonitor);
    scm_c_define_gsubr("GetMonitorPosition", 1, 0, 0, rgfun_GetMonitorPosition);
    scm_c_define_gsubr("GetMonitorWidth", 1, 0, 0, rgfun_GetMonitorWidth);
    scm_c_define_gsubr("GetMonitorHeight", 1, 0, 0, rgfun_GetMonitorHeight);
    scm_c_define_gsubr("GetMonitorPhysicalWidth", 1, 0, 0, rgfun_GetMonitorPhysicalWidth);
    scm_c_define_gsubr("GetMonitorPhysicalHeight", 1, 0, 0, rgfun_GetMonitorPhysicalHeight);
    scm_c_define_gsubr("GetMonitorRefreshRate", 1, 0, 0, rgfun_GetMonitorRefreshRate);
    scm_c_define_gsubr("GetWindowPosition", 0, 0, 0, rgfun_GetWindowPosition);
    scm_c_define_gsubr("GetWindowScaleDPI", 0, 0, 0, rgfun_GetWindowScaleDPI);
    scm_c_define_gsubr("GetMonitorName", 1, 0, 0, rgfun_GetMonitorName);
    scm_c_define_gsubr("SetClipboardText", 1, 0, 0, rgfun_SetClipboardText);
    scm_c_define_gsubr("GetClipboardText", 0, 0, 0, rgfun_GetClipboardText);
    scm_c_define_gsubr("EnableEventWaiting", 0, 0, 0, rgfun_EnableEventWaiting);
    scm_c_define_gsubr("DisableEventWaiting", 0, 0, 0, rgfun_DisableEventWaiting);
    scm_c_define_gsubr("ShowCursor", 0, 0, 0, rgfun_ShowCursor);
    scm_c_define_gsubr("HideCursor", 0, 0, 0, rgfun_HideCursor);
    scm_c_define_gsubr("IsCursorHidden", 0, 0, 0, rgfun_IsCursorHidden);
    scm_c_define_gsubr("EnableCursor", 0, 0, 0, rgfun_EnableCursor);
    scm_c_define_gsubr("DisableCursor", 0, 0, 0, rgfun_DisableCursor);
    scm_c_define_gsubr("IsCursorOnScreen", 0, 0, 0, rgfun_IsCursorOnScreen);
    scm_c_define_gsubr("ClearBackground", 1, 0, 0, rgfun_ClearBackground);
    scm_c_define_gsubr("BeginDrawing", 0, 0, 0, rgfun_BeginDrawing);
    scm_c_define_gsubr("EndDrawing", 0, 0, 0, rgfun_EndDrawing);
    scm_c_define_gsubr("BeginMode2D", 1, 0, 0, rgfun_BeginMode2D);
    scm_c_define_gsubr("EndMode2D", 0, 0, 0, rgfun_EndMode2D);
    scm_c_define_gsubr("BeginMode3D", 1, 0, 0, rgfun_BeginMode3D);
    scm_c_define_gsubr("EndMode3D", 0, 0, 0, rgfun_EndMode3D);
    scm_c_define_gsubr("BeginTextureMode", 1, 0, 0, rgfun_BeginTextureMode);
    scm_c_define_gsubr("EndTextureMode", 0, 0, 0, rgfun_EndTextureMode);
    scm_c_define_gsubr("BeginShaderMode", 1, 0, 0, rgfun_BeginShaderMode);
    scm_c_define_gsubr("EndShaderMode", 0, 0, 0, rgfun_EndShaderMode);
    scm_c_define_gsubr("BeginBlendMode", 1, 0, 0, rgfun_BeginBlendMode);
    scm_c_define_gsubr("EndBlendMode", 0, 0, 0, rgfun_EndBlendMode);
    scm_c_define_gsubr("BeginScissorMode", 4, 0, 0, rgfun_BeginScissorMode);
    scm_c_define_gsubr("EndScissorMode", 0, 0, 0, rgfun_EndScissorMode);
    scm_c_define_gsubr("BeginVrStereoMode", 1, 0, 0, rgfun_BeginVrStereoMode);
    scm_c_define_gsubr("EndVrStereoMode", 0, 0, 0, rgfun_EndVrStereoMode);
    scm_c_define_gsubr("LoadVrStereoConfig", 1, 0, 0, rgfun_LoadVrStereoConfig);
    scm_c_define_gsubr("UnloadVrStereoConfig", 1, 0, 0, rgfun_UnloadVrStereoConfig);
    scm_c_define_gsubr("LoadShader", 2, 0, 0, rgfun_LoadShader);
    scm_c_define_gsubr("LoadShaderFromMemory", 2, 0, 0, rgfun_LoadShaderFromMemory);
    scm_c_define_gsubr("IsShaderReady", 1, 0, 0, rgfun_IsShaderReady);
    scm_c_define_gsubr("GetShaderLocation", 2, 0, 0, rgfun_GetShaderLocation);
    scm_c_define_gsubr("GetShaderLocationAttrib", 2, 0, 0, rgfun_GetShaderLocationAttrib);
    scm_c_define_gsubr("SetShaderValueMatrix", 3, 0, 0, rgfun_SetShaderValueMatrix);
    scm_c_define_gsubr("SetShaderValueTexture", 3, 0, 0, rgfun_SetShaderValueTexture);
    scm_c_define_gsubr("UnloadShader", 1, 0, 0, rgfun_UnloadShader);
    scm_c_define_gsubr("GetScreenToWorldRay", 2, 0, 0, rgfun_GetScreenToWorldRay);
    scm_c_define_gsubr("GetScreenToWorldRayEx", 4, 0, 0, rgfun_GetScreenToWorldRayEx);
    scm_c_define_gsubr("GetWorldToScreen", 2, 0, 0, rgfun_GetWorldToScreen);
    scm_c_define_gsubr("GetWorldToScreenEx", 4, 0, 0, rgfun_GetWorldToScreenEx);
    scm_c_define_gsubr("GetWorldToScreen2D", 2, 0, 0, rgfun_GetWorldToScreen2D);
    scm_c_define_gsubr("GetScreenToWorld2D", 2, 0, 0, rgfun_GetScreenToWorld2D);
    scm_c_define_gsubr("GetCameraMatrix", 1, 0, 0, rgfun_GetCameraMatrix);
    scm_c_define_gsubr("GetCameraMatrix2D", 1, 0, 0, rgfun_GetCameraMatrix2D);
    scm_c_define_gsubr("SetTargetFPS", 1, 0, 0, rgfun_SetTargetFPS);
    scm_c_define_gsubr("GetFrameTime", 0, 0, 0, rgfun_GetFrameTime);
    scm_c_define_gsubr("GetTime", 0, 0, 0, rgfun_GetTime);
    scm_c_define_gsubr("GetFPS", 0, 0, 0, rgfun_GetFPS);
    scm_c_define_gsubr("SwapScreenBuffer", 0, 0, 0, rgfun_SwapScreenBuffer);
    scm_c_define_gsubr("PollInputEvents", 0, 0, 0, rgfun_PollInputEvents);
    scm_c_define_gsubr("WaitTime", 1, 0, 0, rgfun_WaitTime);
    scm_c_define_gsubr("SetRandomSeed", 1, 0, 0, rgfun_SetRandomSeed);
    scm_c_define_gsubr("GetRandomValue", 2, 0, 0, rgfun_GetRandomValue);
    scm_c_define_gsubr("LoadRandomSequence", 3, 0, 0, rgfun_LoadRandomSequence);
    scm_c_define_gsubr("UnloadRandomSequence", 1, 0, 0, rgfun_UnloadRandomSequence);
    scm_c_define_gsubr("TakeScreenshot", 1, 0, 0, rgfun_TakeScreenshot);
    scm_c_define_gsubr("SetConfigFlags", 1, 0, 0, rgfun_SetConfigFlags);
    scm_c_define_gsubr("OpenURL", 1, 0, 0, rgfun_OpenURL);
    scm_c_define_gsubr("SetTraceLogLevel", 1, 0, 0, rgfun_SetTraceLogLevel);
    scm_c_define_gsubr("ExportDataAsCode", 3, 0, 0, rgfun_ExportDataAsCode);
    scm_c_define_gsubr("FileExists", 1, 0, 0, rgfun_FileExists);
    scm_c_define_gsubr("DirectoryExists", 1, 0, 0, rgfun_DirectoryExists);
    scm_c_define_gsubr("IsFileExtension", 2, 0, 0, rgfun_IsFileExtension);
    scm_c_define_gsubr("GetFileLength", 1, 0, 0, rgfun_GetFileLength);
    scm_c_define_gsubr("GetFileExtension", 1, 0, 0, rgfun_GetFileExtension);
    scm_c_define_gsubr("GetFileName", 1, 0, 0, rgfun_GetFileName);
    scm_c_define_gsubr("GetFileNameWithoutExt", 1, 0, 0, rgfun_GetFileNameWithoutExt);
    scm_c_define_gsubr("GetDirectoryPath", 1, 0, 0, rgfun_GetDirectoryPath);
    scm_c_define_gsubr("GetPrevDirectoryPath", 1, 0, 0, rgfun_GetPrevDirectoryPath);
    scm_c_define_gsubr("GetWorkingDirectory", 0, 0, 0, rgfun_GetWorkingDirectory);
    scm_c_define_gsubr("GetApplicationDirectory", 0, 0, 0, rgfun_GetApplicationDirectory);
    scm_c_define_gsubr("ChangeDirectory", 1, 0, 0, rgfun_ChangeDirectory);
    scm_c_define_gsubr("IsPathFile", 1, 0, 0, rgfun_IsPathFile);
    scm_c_define_gsubr("LoadDirectoryFiles", 1, 0, 0, rgfun_LoadDirectoryFiles);
    scm_c_define_gsubr("LoadDirectoryFilesEx", 3, 0, 0, rgfun_LoadDirectoryFilesEx);
    scm_c_define_gsubr("UnloadDirectoryFiles", 1, 0, 0, rgfun_UnloadDirectoryFiles);
    scm_c_define_gsubr("IsFileDropped", 0, 0, 0, rgfun_IsFileDropped);
    scm_c_define_gsubr("LoadDroppedFiles", 0, 0, 0, rgfun_LoadDroppedFiles);
    scm_c_define_gsubr("UnloadDroppedFiles", 1, 0, 0, rgfun_UnloadDroppedFiles);
    scm_c_define_gsubr("GetFileModTime", 1, 0, 0, rgfun_GetFileModTime);
    scm_c_define_gsubr("LoadAutomationEventList", 1, 0, 0, rgfun_LoadAutomationEventList);
    scm_c_define_gsubr("UnloadAutomationEventList", 1, 0, 0, rgfun_UnloadAutomationEventList);
    scm_c_define_gsubr("ExportAutomationEventList", 2, 0, 0, rgfun_ExportAutomationEventList);
    scm_c_define_gsubr("SetAutomationEventList", 1, 0, 0, rgfun_SetAutomationEventList);
    scm_c_define_gsubr("SetAutomationEventBaseFrame", 1, 0, 0, rgfun_SetAutomationEventBaseFrame);
    scm_c_define_gsubr("StartAutomationEventRecording", 0, 0, 0, rgfun_StartAutomationEventRecording);
    scm_c_define_gsubr("StopAutomationEventRecording", 0, 0, 0, rgfun_StopAutomationEventRecording);
    scm_c_define_gsubr("PlayAutomationEvent", 1, 0, 0, rgfun_PlayAutomationEvent);
    scm_c_define_gsubr("IsKeyPressed", 1, 0, 0, rgfun_IsKeyPressed);
    scm_c_define_gsubr("IsKeyPressedRepeat", 1, 0, 0, rgfun_IsKeyPressedRepeat);
    scm_c_define_gsubr("IsKeyDown", 1, 0, 0, rgfun_IsKeyDown);
    scm_c_define_gsubr("IsKeyReleased", 1, 0, 0, rgfun_IsKeyReleased);
    scm_c_define_gsubr("IsKeyUp", 1, 0, 0, rgfun_IsKeyUp);
    scm_c_define_gsubr("GetKeyPressed", 0, 0, 0, rgfun_GetKeyPressed);
    scm_c_define_gsubr("GetCharPressed", 0, 0, 0, rgfun_GetCharPressed);
    scm_c_define_gsubr("SetExitKey", 1, 0, 0, rgfun_SetExitKey);
    scm_c_define_gsubr("IsGamepadAvailable", 1, 0, 0, rgfun_IsGamepadAvailable);
    scm_c_define_gsubr("GetGamepadName", 1, 0, 0, rgfun_GetGamepadName);
    scm_c_define_gsubr("IsGamepadButtonPressed", 2, 0, 0, rgfun_IsGamepadButtonPressed);
    scm_c_define_gsubr("IsGamepadButtonDown", 2, 0, 0, rgfun_IsGamepadButtonDown);
    scm_c_define_gsubr("IsGamepadButtonReleased", 2, 0, 0, rgfun_IsGamepadButtonReleased);
    scm_c_define_gsubr("IsGamepadButtonUp", 2, 0, 0, rgfun_IsGamepadButtonUp);
    scm_c_define_gsubr("GetGamepadButtonPressed", 0, 0, 0, rgfun_GetGamepadButtonPressed);
    scm_c_define_gsubr("GetGamepadAxisCount", 1, 0, 0, rgfun_GetGamepadAxisCount);
    scm_c_define_gsubr("GetGamepadAxisMovement", 2, 0, 0, rgfun_GetGamepadAxisMovement);
    scm_c_define_gsubr("SetGamepadMappings", 1, 0, 0, rgfun_SetGamepadMappings);
    scm_c_define_gsubr("SetGamepadVibration", 3, 0, 0, rgfun_SetGamepadVibration);
    scm_c_define_gsubr("IsMouseButtonPressed", 1, 0, 0, rgfun_IsMouseButtonPressed);
    scm_c_define_gsubr("IsMouseButtonDown", 1, 0, 0, rgfun_IsMouseButtonDown);
    scm_c_define_gsubr("IsMouseButtonReleased", 1, 0, 0, rgfun_IsMouseButtonReleased);
    scm_c_define_gsubr("IsMouseButtonUp", 1, 0, 0, rgfun_IsMouseButtonUp);
    scm_c_define_gsubr("GetMouseX", 0, 0, 0, rgfun_GetMouseX);
    scm_c_define_gsubr("GetMouseY", 0, 0, 0, rgfun_GetMouseY);
    scm_c_define_gsubr("GetMousePosition", 0, 0, 0, rgfun_GetMousePosition);
    scm_c_define_gsubr("GetMouseDelta", 0, 0, 0, rgfun_GetMouseDelta);
    scm_c_define_gsubr("SetMousePosition", 2, 0, 0, rgfun_SetMousePosition);
    scm_c_define_gsubr("SetMouseOffset", 2, 0, 0, rgfun_SetMouseOffset);
    scm_c_define_gsubr("SetMouseScale", 2, 0, 0, rgfun_SetMouseScale);
    scm_c_define_gsubr("GetMouseWheelMove", 0, 0, 0, rgfun_GetMouseWheelMove);
    scm_c_define_gsubr("GetMouseWheelMoveV", 0, 0, 0, rgfun_GetMouseWheelMoveV);
    scm_c_define_gsubr("SetMouseCursor", 1, 0, 0, rgfun_SetMouseCursor);
    scm_c_define_gsubr("GetTouchX", 0, 0, 0, rgfun_GetTouchX);
    scm_c_define_gsubr("GetTouchY", 0, 0, 0, rgfun_GetTouchY);
    scm_c_define_gsubr("GetTouchPosition", 1, 0, 0, rgfun_GetTouchPosition);
    scm_c_define_gsubr("GetTouchPointId", 1, 0, 0, rgfun_GetTouchPointId);
    scm_c_define_gsubr("GetTouchPointCount", 0, 0, 0, rgfun_GetTouchPointCount);
    scm_c_define_gsubr("SetGesturesEnabled", 1, 0, 0, rgfun_SetGesturesEnabled);
    scm_c_define_gsubr("IsGestureDetected", 1, 0, 0, rgfun_IsGestureDetected);
    scm_c_define_gsubr("GetGestureDetected", 0, 0, 0, rgfun_GetGestureDetected);
    scm_c_define_gsubr("GetGestureHoldDuration", 0, 0, 0, rgfun_GetGestureHoldDuration);
    scm_c_define_gsubr("GetGestureDragVector", 0, 0, 0, rgfun_GetGestureDragVector);
    scm_c_define_gsubr("GetGestureDragAngle", 0, 0, 0, rgfun_GetGestureDragAngle);
    scm_c_define_gsubr("GetGesturePinchVector", 0, 0, 0, rgfun_GetGesturePinchVector);
    scm_c_define_gsubr("GetGesturePinchAngle", 0, 0, 0, rgfun_GetGesturePinchAngle);
    scm_c_define_gsubr("UpdateCamera", 2, 0, 0, rgfun_UpdateCamera);
    scm_c_define_gsubr("UpdateCameraPro", 4, 0, 0, rgfun_UpdateCameraPro);
    scm_c_define_gsubr("SetShapesTexture", 2, 0, 0, rgfun_SetShapesTexture);
    scm_c_define_gsubr("GetShapesTexture", 0, 0, 0, rgfun_GetShapesTexture);
    scm_c_define_gsubr("GetShapesTextureRectangle", 0, 0, 0, rgfun_GetShapesTextureRectangle);
    scm_c_define_gsubr("DrawPixel", 3, 0, 0, rgfun_DrawPixel);
    scm_c_define_gsubr("DrawPixelV", 2, 0, 0, rgfun_DrawPixelV);
    scm_c_define_gsubr("DrawLine", 5, 0, 0, rgfun_DrawLine);
    scm_c_define_gsubr("DrawLineV", 3, 0, 0, rgfun_DrawLineV);
    scm_c_define_gsubr("DrawLineEx", 4, 0, 0, rgfun_DrawLineEx);
    scm_c_define_gsubr("DrawLineBezier", 4, 0, 0, rgfun_DrawLineBezier);
    scm_c_define_gsubr("DrawCircle", 4, 0, 0, rgfun_DrawCircle);
    scm_c_define_gsubr("DrawCircleSector", 6, 0, 0, rgfun_DrawCircleSector);
    scm_c_define_gsubr("DrawCircleSectorLines", 6, 0, 0, rgfun_DrawCircleSectorLines);
    scm_c_define_gsubr("DrawCircleGradient", 5, 0, 0, rgfun_DrawCircleGradient);
    scm_c_define_gsubr("DrawCircleV", 3, 0, 0, rgfun_DrawCircleV);
    scm_c_define_gsubr("DrawCircleLines", 4, 0, 0, rgfun_DrawCircleLines);
    scm_c_define_gsubr("DrawCircleLinesV", 3, 0, 0, rgfun_DrawCircleLinesV);
    scm_c_define_gsubr("DrawEllipse", 5, 0, 0, rgfun_DrawEllipse);
    scm_c_define_gsubr("DrawEllipseLines", 5, 0, 0, rgfun_DrawEllipseLines);
    scm_c_define_gsubr("DrawRing", 7, 0, 0, rgfun_DrawRing);
    scm_c_define_gsubr("DrawRingLines", 7, 0, 0, rgfun_DrawRingLines);
    scm_c_define_gsubr("DrawRectangle", 5, 0, 0, rgfun_DrawRectangle);
    scm_c_define_gsubr("DrawRectangleV", 3, 0, 0, rgfun_DrawRectangleV);
    scm_c_define_gsubr("DrawRectangleRec", 2, 0, 0, rgfun_DrawRectangleRec);
    scm_c_define_gsubr("DrawRectanglePro", 4, 0, 0, rgfun_DrawRectanglePro);
    scm_c_define_gsubr("DrawRectangleGradientV", 6, 0, 0, rgfun_DrawRectangleGradientV);
    scm_c_define_gsubr("DrawRectangleGradientH", 6, 0, 0, rgfun_DrawRectangleGradientH);
    scm_c_define_gsubr("DrawRectangleGradientEx", 5, 0, 0, rgfun_DrawRectangleGradientEx);
    scm_c_define_gsubr("DrawRectangleLines", 5, 0, 0, rgfun_DrawRectangleLines);
    scm_c_define_gsubr("DrawRectangleLinesEx", 3, 0, 0, rgfun_DrawRectangleLinesEx);
    scm_c_define_gsubr("DrawRectangleRounded", 4, 0, 0, rgfun_DrawRectangleRounded);
    scm_c_define_gsubr("DrawRectangleRoundedLines", 4, 0, 0, rgfun_DrawRectangleRoundedLines);
    scm_c_define_gsubr("DrawRectangleRoundedLinesEx", 5, 0, 0, rgfun_DrawRectangleRoundedLinesEx);
    scm_c_define_gsubr("DrawTriangle", 4, 0, 0, rgfun_DrawTriangle);
    scm_c_define_gsubr("DrawTriangleLines", 4, 0, 0, rgfun_DrawTriangleLines);
    scm_c_define_gsubr("DrawPoly", 5, 0, 0, rgfun_DrawPoly);
    scm_c_define_gsubr("DrawPolyLines", 5, 0, 0, rgfun_DrawPolyLines);
    scm_c_define_gsubr("DrawPolyLinesEx", 6, 0, 0, rgfun_DrawPolyLinesEx);
    scm_c_define_gsubr("DrawSplineLinear", 4, 0, 0, rgfun_DrawSplineLinear);
    scm_c_define_gsubr("DrawSplineBasis", 4, 0, 0, rgfun_DrawSplineBasis);
    scm_c_define_gsubr("DrawSplineCatmullRom", 4, 0, 0, rgfun_DrawSplineCatmullRom);
    scm_c_define_gsubr("DrawSplineBezierQuadratic", 4, 0, 0, rgfun_DrawSplineBezierQuadratic);
    scm_c_define_gsubr("DrawSplineBezierCubic", 4, 0, 0, rgfun_DrawSplineBezierCubic);
    scm_c_define_gsubr("DrawSplineSegmentLinear", 4, 0, 0, rgfun_DrawSplineSegmentLinear);
    scm_c_define_gsubr("DrawSplineSegmentBasis", 6, 0, 0, rgfun_DrawSplineSegmentBasis);
    scm_c_define_gsubr("DrawSplineSegmentCatmullRom", 6, 0, 0, rgfun_DrawSplineSegmentCatmullRom);
    scm_c_define_gsubr("DrawSplineSegmentBezierQuadratic", 5, 0, 0, rgfun_DrawSplineSegmentBezierQuadratic);
    scm_c_define_gsubr("DrawSplineSegmentBezierCubic", 6, 0, 0, rgfun_DrawSplineSegmentBezierCubic);
    scm_c_define_gsubr("GetSplinePointLinear", 3, 0, 0, rgfun_GetSplinePointLinear);
    scm_c_define_gsubr("GetSplinePointBasis", 5, 0, 0, rgfun_GetSplinePointBasis);
    scm_c_define_gsubr("GetSplinePointCatmullRom", 5, 0, 0, rgfun_GetSplinePointCatmullRom);
    scm_c_define_gsubr("GetSplinePointBezierQuad", 4, 0, 0, rgfun_GetSplinePointBezierQuad);
    scm_c_define_gsubr("GetSplinePointBezierCubic", 5, 0, 0, rgfun_GetSplinePointBezierCubic);
    scm_c_define_gsubr("CheckCollisionRecs", 2, 0, 0, rgfun_CheckCollisionRecs);
    scm_c_define_gsubr("CheckCollisionCircles", 4, 0, 0, rgfun_CheckCollisionCircles);
    scm_c_define_gsubr("CheckCollisionCircleRec", 3, 0, 0, rgfun_CheckCollisionCircleRec);
    scm_c_define_gsubr("CheckCollisionPointRec", 2, 0, 0, rgfun_CheckCollisionPointRec);
    scm_c_define_gsubr("CheckCollisionPointCircle", 3, 0, 0, rgfun_CheckCollisionPointCircle);
    scm_c_define_gsubr("CheckCollisionPointTriangle", 4, 0, 0, rgfun_CheckCollisionPointTriangle);
    scm_c_define_gsubr("CheckCollisionPointPoly", 3, 0, 0, rgfun_CheckCollisionPointPoly);
    scm_c_define_gsubr("CheckCollisionPointLine", 4, 0, 0, rgfun_CheckCollisionPointLine);
    scm_c_define_gsubr("GetCollisionRec", 2, 0, 0, rgfun_GetCollisionRec);
    scm_c_define_gsubr("LoadImage", 1, 0, 0, rgfun_LoadImage);
    scm_c_define_gsubr("LoadImageRaw", 5, 0, 0, rgfun_LoadImageRaw);
    scm_c_define_gsubr("LoadImageSvg", 3, 0, 0, rgfun_LoadImageSvg);
    scm_c_define_gsubr("LoadImageAnimFromMemory", 4, 0, 0, rgfun_LoadImageAnimFromMemory);
    scm_c_define_gsubr("LoadImageFromMemory", 3, 0, 0, rgfun_LoadImageFromMemory);
    scm_c_define_gsubr("LoadImageFromTexture", 1, 0, 0, rgfun_LoadImageFromTexture);
    scm_c_define_gsubr("LoadImageFromScreen", 0, 0, 0, rgfun_LoadImageFromScreen);
    scm_c_define_gsubr("IsImageReady", 1, 0, 0, rgfun_IsImageReady);
    scm_c_define_gsubr("UnloadImage", 1, 0, 0, rgfun_UnloadImage);
    scm_c_define_gsubr("ExportImage", 2, 0, 0, rgfun_ExportImage);
    scm_c_define_gsubr("ExportImageToMemory", 3, 0, 0, rgfun_ExportImageToMemory);
    scm_c_define_gsubr("ExportImageAsCode", 2, 0, 0, rgfun_ExportImageAsCode);
    scm_c_define_gsubr("GenImageColor", 3, 0, 0, rgfun_GenImageColor);
    scm_c_define_gsubr("GenImageGradientLinear", 5, 0, 0, rgfun_GenImageGradientLinear);
    scm_c_define_gsubr("GenImageGradientRadial", 5, 0, 0, rgfun_GenImageGradientRadial);
    scm_c_define_gsubr("GenImageGradientSquare", 5, 0, 0, rgfun_GenImageGradientSquare);
    scm_c_define_gsubr("GenImageChecked", 6, 0, 0, rgfun_GenImageChecked);
    scm_c_define_gsubr("GenImageWhiteNoise", 3, 0, 0, rgfun_GenImageWhiteNoise);
    scm_c_define_gsubr("GenImagePerlinNoise", 5, 0, 0, rgfun_GenImagePerlinNoise);
    scm_c_define_gsubr("GenImageCellular", 3, 0, 0, rgfun_GenImageCellular);
    scm_c_define_gsubr("GenImageText", 3, 0, 0, rgfun_GenImageText);
    scm_c_define_gsubr("ImageCopy", 1, 0, 0, rgfun_ImageCopy);
    scm_c_define_gsubr("ImageFromImage", 2, 0, 0, rgfun_ImageFromImage);
    scm_c_define_gsubr("ImageText", 3, 0, 0, rgfun_ImageText);
    scm_c_define_gsubr("ImageTextEx", 5, 0, 0, rgfun_ImageTextEx);
    scm_c_define_gsubr("ImageFormat", 2, 0, 0, rgfun_ImageFormat);
    scm_c_define_gsubr("ImageToPOT", 2, 0, 0, rgfun_ImageToPOT);
    scm_c_define_gsubr("ImageCrop", 2, 0, 0, rgfun_ImageCrop);
    scm_c_define_gsubr("ImageAlphaCrop", 2, 0, 0, rgfun_ImageAlphaCrop);
    scm_c_define_gsubr("ImageAlphaClear", 3, 0, 0, rgfun_ImageAlphaClear);
    scm_c_define_gsubr("ImageAlphaMask", 2, 0, 0, rgfun_ImageAlphaMask);
    scm_c_define_gsubr("ImageAlphaPremultiply", 1, 0, 0, rgfun_ImageAlphaPremultiply);
    scm_c_define_gsubr("ImageBlurGaussian", 2, 0, 0, rgfun_ImageBlurGaussian);
    scm_c_define_gsubr("ImageKernelConvolution", 3, 0, 0, rgfun_ImageKernelConvolution);
    scm_c_define_gsubr("ImageResize", 3, 0, 0, rgfun_ImageResize);
    scm_c_define_gsubr("ImageResizeNN", 3, 0, 0, rgfun_ImageResizeNN);
    scm_c_define_gsubr("ImageResizeCanvas", 6, 0, 0, rgfun_ImageResizeCanvas);
    scm_c_define_gsubr("ImageMipmaps", 1, 0, 0, rgfun_ImageMipmaps);
    scm_c_define_gsubr("ImageDither", 5, 0, 0, rgfun_ImageDither);
    scm_c_define_gsubr("ImageFlipVertical", 1, 0, 0, rgfun_ImageFlipVertical);
    scm_c_define_gsubr("ImageFlipHorizontal", 1, 0, 0, rgfun_ImageFlipHorizontal);
    scm_c_define_gsubr("ImageRotate", 2, 0, 0, rgfun_ImageRotate);
    scm_c_define_gsubr("ImageRotateCW", 1, 0, 0, rgfun_ImageRotateCW);
    scm_c_define_gsubr("ImageRotateCCW", 1, 0, 0, rgfun_ImageRotateCCW);
    scm_c_define_gsubr("ImageColorTint", 2, 0, 0, rgfun_ImageColorTint);
    scm_c_define_gsubr("ImageColorInvert", 1, 0, 0, rgfun_ImageColorInvert);
    scm_c_define_gsubr("ImageColorGrayscale", 1, 0, 0, rgfun_ImageColorGrayscale);
    scm_c_define_gsubr("ImageColorContrast", 2, 0, 0, rgfun_ImageColorContrast);
    scm_c_define_gsubr("ImageColorBrightness", 2, 0, 0, rgfun_ImageColorBrightness);
    scm_c_define_gsubr("ImageColorReplace", 3, 0, 0, rgfun_ImageColorReplace);
    scm_c_define_gsubr("UnloadImageColors", 1, 0, 0, rgfun_UnloadImageColors);
    scm_c_define_gsubr("UnloadImagePalette", 1, 0, 0, rgfun_UnloadImagePalette);
    scm_c_define_gsubr("GetImageAlphaBorder", 2, 0, 0, rgfun_GetImageAlphaBorder);
    scm_c_define_gsubr("GetImageColor", 3, 0, 0, rgfun_GetImageColor);
    scm_c_define_gsubr("ImageClearBackground", 2, 0, 0, rgfun_ImageClearBackground);
    scm_c_define_gsubr("ImageDrawPixel", 4, 0, 0, rgfun_ImageDrawPixel);
    scm_c_define_gsubr("ImageDrawPixelV", 3, 0, 0, rgfun_ImageDrawPixelV);
    scm_c_define_gsubr("ImageDrawLine", 6, 0, 0, rgfun_ImageDrawLine);
    scm_c_define_gsubr("ImageDrawLineV", 4, 0, 0, rgfun_ImageDrawLineV);
    scm_c_define_gsubr("ImageDrawCircle", 5, 0, 0, rgfun_ImageDrawCircle);
    scm_c_define_gsubr("ImageDrawCircleV", 4, 0, 0, rgfun_ImageDrawCircleV);
    scm_c_define_gsubr("ImageDrawCircleLines", 5, 0, 0, rgfun_ImageDrawCircleLines);
    scm_c_define_gsubr("ImageDrawCircleLinesV", 4, 0, 0, rgfun_ImageDrawCircleLinesV);
    scm_c_define_gsubr("ImageDrawRectangle", 6, 0, 0, rgfun_ImageDrawRectangle);
    scm_c_define_gsubr("ImageDrawRectangleV", 4, 0, 0, rgfun_ImageDrawRectangleV);
    scm_c_define_gsubr("ImageDrawRectangleRec", 3, 0, 0, rgfun_ImageDrawRectangleRec);
    scm_c_define_gsubr("ImageDrawRectangleLines", 4, 0, 0, rgfun_ImageDrawRectangleLines);
    scm_c_define_gsubr("ImageDraw", 5, 0, 0, rgfun_ImageDraw);
    scm_c_define_gsubr("ImageDrawText", 6, 0, 0, rgfun_ImageDrawText);
    scm_c_define_gsubr("ImageDrawTextEx", 7, 0, 0, rgfun_ImageDrawTextEx);
    scm_c_define_gsubr("LoadTexture", 1, 0, 0, rgfun_LoadTexture);
    scm_c_define_gsubr("LoadTextureFromImage", 1, 0, 0, rgfun_LoadTextureFromImage);
    scm_c_define_gsubr("LoadTextureCubemap", 2, 0, 0, rgfun_LoadTextureCubemap);
    scm_c_define_gsubr("LoadRenderTexture", 2, 0, 0, rgfun_LoadRenderTexture);
    scm_c_define_gsubr("IsTextureReady", 1, 0, 0, rgfun_IsTextureReady);
    scm_c_define_gsubr("UnloadTexture", 1, 0, 0, rgfun_UnloadTexture);
    scm_c_define_gsubr("IsRenderTextureReady", 1, 0, 0, rgfun_IsRenderTextureReady);
    scm_c_define_gsubr("UnloadRenderTexture", 1, 0, 0, rgfun_UnloadRenderTexture);
    scm_c_define_gsubr("GenTextureMipmaps", 1, 0, 0, rgfun_GenTextureMipmaps);
    scm_c_define_gsubr("SetTextureFilter", 2, 0, 0, rgfun_SetTextureFilter);
    scm_c_define_gsubr("SetTextureWrap", 2, 0, 0, rgfun_SetTextureWrap);
    scm_c_define_gsubr("DrawTexture", 4, 0, 0, rgfun_DrawTexture);
    scm_c_define_gsubr("DrawTextureV", 3, 0, 0, rgfun_DrawTextureV);
    scm_c_define_gsubr("DrawTextureEx", 5, 0, 0, rgfun_DrawTextureEx);
    scm_c_define_gsubr("DrawTextureRec", 4, 0, 0, rgfun_DrawTextureRec);
    scm_c_define_gsubr("DrawTexturePro", 6, 0, 0, rgfun_DrawTexturePro);
    scm_c_define_gsubr("DrawTextureNPatch", 6, 0, 0, rgfun_DrawTextureNPatch);
    scm_c_define_gsubr("ColorIsEqual", 2, 0, 0, rgfun_ColorIsEqual);
    scm_c_define_gsubr("Fade", 2, 0, 0, rgfun_Fade);
    scm_c_define_gsubr("ColorToInt", 1, 0, 0, rgfun_ColorToInt);
    scm_c_define_gsubr("ColorNormalize", 1, 0, 0, rgfun_ColorNormalize);
    scm_c_define_gsubr("ColorFromNormalized", 1, 0, 0, rgfun_ColorFromNormalized);
    scm_c_define_gsubr("ColorToHSV", 1, 0, 0, rgfun_ColorToHSV);
    scm_c_define_gsubr("ColorFromHSV", 3, 0, 0, rgfun_ColorFromHSV);
    scm_c_define_gsubr("ColorTint", 2, 0, 0, rgfun_ColorTint);
    scm_c_define_gsubr("ColorBrightness", 2, 0, 0, rgfun_ColorBrightness);
    scm_c_define_gsubr("ColorContrast", 2, 0, 0, rgfun_ColorContrast);
    scm_c_define_gsubr("ColorAlpha", 2, 0, 0, rgfun_ColorAlpha);
    scm_c_define_gsubr("ColorAlphaBlend", 3, 0, 0, rgfun_ColorAlphaBlend);
    scm_c_define_gsubr("GetColor", 1, 0, 0, rgfun_GetColor);
    scm_c_define_gsubr("GetPixelDataSize", 3, 0, 0, rgfun_GetPixelDataSize);
    scm_c_define_gsubr("GetFontDefault", 0, 0, 0, rgfun_GetFontDefault);
    scm_c_define_gsubr("LoadFont", 1, 0, 0, rgfun_LoadFont);
    scm_c_define_gsubr("LoadFontFromImage", 3, 0, 0, rgfun_LoadFontFromImage);
    scm_c_define_gsubr("IsFontReady", 1, 0, 0, rgfun_IsFontReady);
    scm_c_define_gsubr("UnloadFontData", 2, 0, 0, rgfun_UnloadFontData);
    scm_c_define_gsubr("UnloadFont", 1, 0, 0, rgfun_UnloadFont);
    scm_c_define_gsubr("ExportFontAsCode", 2, 0, 0, rgfun_ExportFontAsCode);
    scm_c_define_gsubr("DrawFPS", 2, 0, 0, rgfun_DrawFPS);
    scm_c_define_gsubr("DrawText", 5, 0, 0, rgfun_DrawText);
    scm_c_define_gsubr("DrawTextEx", 6, 0, 0, rgfun_DrawTextEx);
    scm_c_define_gsubr("DrawTextPro", 8, 0, 0, rgfun_DrawTextPro);
    scm_c_define_gsubr("DrawTextCodepoint", 5, 0, 0, rgfun_DrawTextCodepoint);
    scm_c_define_gsubr("SetTextLineSpacing", 1, 0, 0, rgfun_SetTextLineSpacing);
    scm_c_define_gsubr("MeasureText", 2, 0, 0, rgfun_MeasureText);
    scm_c_define_gsubr("MeasureTextEx", 4, 0, 0, rgfun_MeasureTextEx);
    scm_c_define_gsubr("GetGlyphIndex", 2, 0, 0, rgfun_GetGlyphIndex);
    scm_c_define_gsubr("GetGlyphInfo", 2, 0, 0, rgfun_GetGlyphInfo);
    scm_c_define_gsubr("GetGlyphAtlasRec", 2, 0, 0, rgfun_GetGlyphAtlasRec);
    scm_c_define_gsubr("GetCodepointCount", 1, 0, 0, rgfun_GetCodepointCount);
    scm_c_define_gsubr("TextIsEqual", 2, 0, 0, rgfun_TextIsEqual);
    scm_c_define_gsubr("TextLength", 1, 0, 0, rgfun_TextLength);
    scm_c_define_gsubr("TextSubtext", 3, 0, 0, rgfun_TextSubtext);
    scm_c_define_gsubr("TextReplace", 3, 0, 0, rgfun_TextReplace);
    scm_c_define_gsubr("TextInsert", 3, 0, 0, rgfun_TextInsert);
    scm_c_define_gsubr("TextFindIndex", 2, 0, 0, rgfun_TextFindIndex);
    scm_c_define_gsubr("TextToUpper", 1, 0, 0, rgfun_TextToUpper);
    scm_c_define_gsubr("TextToLower", 1, 0, 0, rgfun_TextToLower);
    scm_c_define_gsubr("TextToPascal", 1, 0, 0, rgfun_TextToPascal);
    scm_c_define_gsubr("TextToInteger", 1, 0, 0, rgfun_TextToInteger);
    scm_c_define_gsubr("TextToFloat", 1, 0, 0, rgfun_TextToFloat);
    scm_c_define_gsubr("DrawLine3D", 3, 0, 0, rgfun_DrawLine3D);
    scm_c_define_gsubr("DrawPoint3D", 2, 0, 0, rgfun_DrawPoint3D);
    scm_c_define_gsubr("DrawCircle3D", 5, 0, 0, rgfun_DrawCircle3D);
    scm_c_define_gsubr("DrawTriangle3D", 4, 0, 0, rgfun_DrawTriangle3D);
    scm_c_define_gsubr("DrawTriangleStrip3D", 3, 0, 0, rgfun_DrawTriangleStrip3D);
    scm_c_define_gsubr("DrawCube", 5, 0, 0, rgfun_DrawCube);
    scm_c_define_gsubr("DrawCubeV", 3, 0, 0, rgfun_DrawCubeV);
    scm_c_define_gsubr("DrawCubeWires", 5, 0, 0, rgfun_DrawCubeWires);
    scm_c_define_gsubr("DrawCubeWiresV", 3, 0, 0, rgfun_DrawCubeWiresV);
    scm_c_define_gsubr("DrawSphere", 3, 0, 0, rgfun_DrawSphere);
    scm_c_define_gsubr("DrawSphereEx", 5, 0, 0, rgfun_DrawSphereEx);
    scm_c_define_gsubr("DrawSphereWires", 5, 0, 0, rgfun_DrawSphereWires);
    scm_c_define_gsubr("DrawCylinder", 6, 0, 0, rgfun_DrawCylinder);
    scm_c_define_gsubr("DrawCylinderEx", 6, 0, 0, rgfun_DrawCylinderEx);
    scm_c_define_gsubr("DrawCylinderWires", 6, 0, 0, rgfun_DrawCylinderWires);
    scm_c_define_gsubr("DrawCylinderWiresEx", 6, 0, 0, rgfun_DrawCylinderWiresEx);
    scm_c_define_gsubr("DrawCapsule", 6, 0, 0, rgfun_DrawCapsule);
    scm_c_define_gsubr("DrawCapsuleWires", 6, 0, 0, rgfun_DrawCapsuleWires);
    scm_c_define_gsubr("DrawPlane", 3, 0, 0, rgfun_DrawPlane);
    scm_c_define_gsubr("DrawRay", 2, 0, 0, rgfun_DrawRay);
    scm_c_define_gsubr("DrawGrid", 2, 0, 0, rgfun_DrawGrid);
    scm_c_define_gsubr("LoadModel", 1, 0, 0, rgfun_LoadModel);
    scm_c_define_gsubr("LoadModelFromMesh", 1, 0, 0, rgfun_LoadModelFromMesh);
    scm_c_define_gsubr("IsModelReady", 1, 0, 0, rgfun_IsModelReady);
    scm_c_define_gsubr("UnloadModel", 1, 0, 0, rgfun_UnloadModel);
    scm_c_define_gsubr("GetModelBoundingBox", 1, 0, 0, rgfun_GetModelBoundingBox);
    scm_c_define_gsubr("DrawModel", 4, 0, 0, rgfun_DrawModel);
    scm_c_define_gsubr("DrawModelEx", 6, 0, 0, rgfun_DrawModelEx);
    scm_c_define_gsubr("DrawModelWires", 4, 0, 0, rgfun_DrawModelWires);
    scm_c_define_gsubr("DrawModelWiresEx", 6, 0, 0, rgfun_DrawModelWiresEx);
    scm_c_define_gsubr("DrawBoundingBox", 2, 0, 0, rgfun_DrawBoundingBox);
    scm_c_define_gsubr("DrawBillboard", 5, 0, 0, rgfun_DrawBillboard);
    scm_c_define_gsubr("DrawBillboardRec", 6, 0, 0, rgfun_DrawBillboardRec);
    scm_c_define_gsubr("DrawBillboardPro", 9, 0, 0, rgfun_DrawBillboardPro);
    scm_c_define_gsubr("UploadMesh", 2, 0, 0, rgfun_UploadMesh);
    scm_c_define_gsubr("UnloadMesh", 1, 0, 0, rgfun_UnloadMesh);
    scm_c_define_gsubr("DrawMesh", 3, 0, 0, rgfun_DrawMesh);
    scm_c_define_gsubr("DrawMeshInstanced", 4, 0, 0, rgfun_DrawMeshInstanced);
    scm_c_define_gsubr("GetMeshBoundingBox", 1, 0, 0, rgfun_GetMeshBoundingBox);
    scm_c_define_gsubr("GenMeshTangents", 1, 0, 0, rgfun_GenMeshTangents);
    scm_c_define_gsubr("ExportMesh", 2, 0, 0, rgfun_ExportMesh);
    scm_c_define_gsubr("ExportMeshAsCode", 2, 0, 0, rgfun_ExportMeshAsCode);
    scm_c_define_gsubr("GenMeshPoly", 2, 0, 0, rgfun_GenMeshPoly);
    scm_c_define_gsubr("GenMeshPlane", 4, 0, 0, rgfun_GenMeshPlane);
    scm_c_define_gsubr("GenMeshCube", 3, 0, 0, rgfun_GenMeshCube);
    scm_c_define_gsubr("GenMeshSphere", 3, 0, 0, rgfun_GenMeshSphere);
    scm_c_define_gsubr("GenMeshHemiSphere", 3, 0, 0, rgfun_GenMeshHemiSphere);
    scm_c_define_gsubr("GenMeshCylinder", 3, 0, 0, rgfun_GenMeshCylinder);
    scm_c_define_gsubr("GenMeshCone", 3, 0, 0, rgfun_GenMeshCone);
    scm_c_define_gsubr("GenMeshTorus", 4, 0, 0, rgfun_GenMeshTorus);
    scm_c_define_gsubr("GenMeshKnot", 4, 0, 0, rgfun_GenMeshKnot);
    scm_c_define_gsubr("GenMeshHeightmap", 2, 0, 0, rgfun_GenMeshHeightmap);
    scm_c_define_gsubr("GenMeshCubicmap", 2, 0, 0, rgfun_GenMeshCubicmap);
    scm_c_define_gsubr("LoadMaterialDefault", 0, 0, 0, rgfun_LoadMaterialDefault);
    scm_c_define_gsubr("IsMaterialReady", 1, 0, 0, rgfun_IsMaterialReady);
    scm_c_define_gsubr("UnloadMaterial", 1, 0, 0, rgfun_UnloadMaterial);
    scm_c_define_gsubr("SetMaterialTexture", 3, 0, 0, rgfun_SetMaterialTexture);
    scm_c_define_gsubr("SetModelMeshMaterial", 3, 0, 0, rgfun_SetModelMeshMaterial);
    scm_c_define_gsubr("UpdateModelAnimation", 3, 0, 0, rgfun_UpdateModelAnimation);
    scm_c_define_gsubr("UnloadModelAnimation", 1, 0, 0, rgfun_UnloadModelAnimation);
    scm_c_define_gsubr("IsModelAnimationValid", 2, 0, 0, rgfun_IsModelAnimationValid);
    scm_c_define_gsubr("CheckCollisionSpheres", 4, 0, 0, rgfun_CheckCollisionSpheres);
    scm_c_define_gsubr("CheckCollisionBoxes", 2, 0, 0, rgfun_CheckCollisionBoxes);
    scm_c_define_gsubr("CheckCollisionBoxSphere", 3, 0, 0, rgfun_CheckCollisionBoxSphere);
    scm_c_define_gsubr("GetRayCollisionSphere", 3, 0, 0, rgfun_GetRayCollisionSphere);
    scm_c_define_gsubr("GetRayCollisionBox", 2, 0, 0, rgfun_GetRayCollisionBox);
    scm_c_define_gsubr("GetRayCollisionMesh", 3, 0, 0, rgfun_GetRayCollisionMesh);
    scm_c_define_gsubr("GetRayCollisionTriangle", 4, 0, 0, rgfun_GetRayCollisionTriangle);
    scm_c_define_gsubr("GetRayCollisionQuad", 5, 0, 0, rgfun_GetRayCollisionQuad);
    scm_c_define_gsubr("InitAudioDevice", 0, 0, 0, rgfun_InitAudioDevice);
    scm_c_define_gsubr("CloseAudioDevice", 0, 0, 0, rgfun_CloseAudioDevice);
    scm_c_define_gsubr("IsAudioDeviceReady", 0, 0, 0, rgfun_IsAudioDeviceReady);
    scm_c_define_gsubr("SetMasterVolume", 1, 0, 0, rgfun_SetMasterVolume);
    scm_c_define_gsubr("GetMasterVolume", 0, 0, 0, rgfun_GetMasterVolume);
    scm_c_define_gsubr("LoadWave", 1, 0, 0, rgfun_LoadWave);
    scm_c_define_gsubr("LoadWaveFromMemory", 3, 0, 0, rgfun_LoadWaveFromMemory);
    scm_c_define_gsubr("IsWaveReady", 1, 0, 0, rgfun_IsWaveReady);
    scm_c_define_gsubr("LoadSound", 1, 0, 0, rgfun_LoadSound);
    scm_c_define_gsubr("LoadSoundFromWave", 1, 0, 0, rgfun_LoadSoundFromWave);
    scm_c_define_gsubr("LoadSoundAlias", 1, 0, 0, rgfun_LoadSoundAlias);
    scm_c_define_gsubr("IsSoundReady", 1, 0, 0, rgfun_IsSoundReady);
    scm_c_define_gsubr("UnloadWave", 1, 0, 0, rgfun_UnloadWave);
    scm_c_define_gsubr("UnloadSound", 1, 0, 0, rgfun_UnloadSound);
    scm_c_define_gsubr("UnloadSoundAlias", 1, 0, 0, rgfun_UnloadSoundAlias);
    scm_c_define_gsubr("ExportWave", 2, 0, 0, rgfun_ExportWave);
    scm_c_define_gsubr("ExportWaveAsCode", 2, 0, 0, rgfun_ExportWaveAsCode);
    scm_c_define_gsubr("PlaySound", 1, 0, 0, rgfun_PlaySound);
    scm_c_define_gsubr("StopSound", 1, 0, 0, rgfun_StopSound);
    scm_c_define_gsubr("PauseSound", 1, 0, 0, rgfun_PauseSound);
    scm_c_define_gsubr("ResumeSound", 1, 0, 0, rgfun_ResumeSound);
    scm_c_define_gsubr("IsSoundPlaying", 1, 0, 0, rgfun_IsSoundPlaying);
    scm_c_define_gsubr("SetSoundVolume", 2, 0, 0, rgfun_SetSoundVolume);
    scm_c_define_gsubr("SetSoundPitch", 2, 0, 0, rgfun_SetSoundPitch);
    scm_c_define_gsubr("SetSoundPan", 2, 0, 0, rgfun_SetSoundPan);
    scm_c_define_gsubr("WaveCopy", 1, 0, 0, rgfun_WaveCopy);
    scm_c_define_gsubr("WaveCrop", 3, 0, 0, rgfun_WaveCrop);
    scm_c_define_gsubr("WaveFormat", 4, 0, 0, rgfun_WaveFormat);
    scm_c_define_gsubr("LoadMusicStream", 1, 0, 0, rgfun_LoadMusicStream);
    scm_c_define_gsubr("LoadMusicStreamFromMemory", 3, 0, 0, rgfun_LoadMusicStreamFromMemory);
    scm_c_define_gsubr("IsMusicReady", 1, 0, 0, rgfun_IsMusicReady);
    scm_c_define_gsubr("UnloadMusicStream", 1, 0, 0, rgfun_UnloadMusicStream);
    scm_c_define_gsubr("PlayMusicStream", 1, 0, 0, rgfun_PlayMusicStream);
    scm_c_define_gsubr("IsMusicStreamPlaying", 1, 0, 0, rgfun_IsMusicStreamPlaying);
    scm_c_define_gsubr("UpdateMusicStream", 1, 0, 0, rgfun_UpdateMusicStream);
    scm_c_define_gsubr("StopMusicStream", 1, 0, 0, rgfun_StopMusicStream);
    scm_c_define_gsubr("PauseMusicStream", 1, 0, 0, rgfun_PauseMusicStream);
    scm_c_define_gsubr("ResumeMusicStream", 1, 0, 0, rgfun_ResumeMusicStream);
    scm_c_define_gsubr("SeekMusicStream", 2, 0, 0, rgfun_SeekMusicStream);
    scm_c_define_gsubr("SetMusicVolume", 2, 0, 0, rgfun_SetMusicVolume);
    scm_c_define_gsubr("SetMusicPitch", 2, 0, 0, rgfun_SetMusicPitch);
    scm_c_define_gsubr("SetMusicPan", 2, 0, 0, rgfun_SetMusicPan);
    scm_c_define_gsubr("GetMusicTimeLength", 1, 0, 0, rgfun_GetMusicTimeLength);
    scm_c_define_gsubr("GetMusicTimePlayed", 1, 0, 0, rgfun_GetMusicTimePlayed);
    scm_c_define_gsubr("LoadAudioStream", 3, 0, 0, rgfun_LoadAudioStream);
    scm_c_define_gsubr("IsAudioStreamReady", 1, 0, 0, rgfun_IsAudioStreamReady);
    scm_c_define_gsubr("UnloadAudioStream", 1, 0, 0, rgfun_UnloadAudioStream);
    scm_c_define_gsubr("IsAudioStreamProcessed", 1, 0, 0, rgfun_IsAudioStreamProcessed);
    scm_c_define_gsubr("PlayAudioStream", 1, 0, 0, rgfun_PlayAudioStream);
    scm_c_define_gsubr("PauseAudioStream", 1, 0, 0, rgfun_PauseAudioStream);
    scm_c_define_gsubr("ResumeAudioStream", 1, 0, 0, rgfun_ResumeAudioStream);
    scm_c_define_gsubr("IsAudioStreamPlaying", 1, 0, 0, rgfun_IsAudioStreamPlaying);
    scm_c_define_gsubr("StopAudioStream", 1, 0, 0, rgfun_StopAudioStream);
    scm_c_define_gsubr("SetAudioStreamVolume", 2, 0, 0, rgfun_SetAudioStreamVolume);
    scm_c_define_gsubr("SetAudioStreamPitch", 2, 0, 0, rgfun_SetAudioStreamPitch);
    scm_c_define_gsubr("SetAudioStreamPan", 2, 0, 0, rgfun_SetAudioStreamPan);
    scm_c_define_gsubr("SetAudioStreamBufferSizeDefault", 1, 0, 0, rgfun_SetAudioStreamBufferSizeDefault);
}
