(define-module (raylib)
  #:export (InitWindow
            CloseWindow
            WindowShouldClose
            IsWindowReady
            IsWindowFullscreen
            IsWindowHidden
            IsWindowMinimized
            IsWindowMaximized
            IsWindowFocused
            IsWindowResized
            IsWindowState
            SetWindowState
            ClearWindowState
            ToggleFullscreen
            ToggleBorderlessWindowed
            MaximizeWindow
            MinimizeWindow
            RestoreWindow
            SetWindowIcon
            SetWindowIcons
            SetWindowTitle
            SetWindowPosition
            SetWindowMonitor
            SetWindowMinSize
            SetWindowMaxSize
            SetWindowSize
            SetWindowOpacity
            SetWindowFocused
            GetScreenWidth
            GetScreenHeight
            GetRenderWidth
            GetRenderHeight
            GetMonitorCount
            GetCurrentMonitor
            GetMonitorPosition
            GetMonitorWidth
            GetMonitorHeight
            GetMonitorPhysicalWidth
            GetMonitorPhysicalHeight
            GetMonitorRefreshRate
            GetWindowPosition
            GetWindowScaleDPI
            GetMonitorName
            SetClipboardText
            GetClipboardText
            EnableEventWaiting
            DisableEventWaiting
            ShowCursor
            HideCursor
            IsCursorHidden
            EnableCursor
            DisableCursor
            IsCursorOnScreen
            ClearBackground
            BeginDrawing
            EndDrawing
            BeginMode2D
            EndMode2D
            BeginMode3D
            EndMode3D
            BeginTextureMode
            EndTextureMode
            BeginShaderMode
            EndShaderMode
            BeginBlendMode
            EndBlendMode
            BeginScissorMode
            EndScissorMode
            BeginVrStereoMode
            EndVrStereoMode
            LoadVrStereoConfig
            UnloadVrStereoConfig
            LoadShader
            LoadShaderFromMemory
            IsShaderReady
            GetShaderLocation
            GetShaderLocationAttrib
            SetShaderValueMatrix
            SetShaderValueTexture
            UnloadShader
            GetScreenToWorldRay
            GetScreenToWorldRayEx
            GetWorldToScreen
            GetWorldToScreenEx
            GetWorldToScreen2D
            GetScreenToWorld2D
            GetCameraMatrix
            GetCameraMatrix2D
            SetTargetFPS
            GetFrameTime
            GetTime
            GetFPS
            SwapScreenBuffer
            PollInputEvents
            WaitTime
            SetRandomSeed
            GetRandomValue
            LoadRandomSequence
            UnloadRandomSequence
            TakeScreenshot
            SetConfigFlags
            OpenURL
            SetTraceLogLevel
            ExportDataAsCode
            FileExists
            DirectoryExists
            IsFileExtension
            GetFileLength
            GetFileExtension
            GetFileName
            GetFileNameWithoutExt
            GetDirectoryPath
            GetPrevDirectoryPath
            GetWorkingDirectory
            GetApplicationDirectory
            ChangeDirectory
            IsPathFile
            LoadDirectoryFiles
            LoadDirectoryFilesEx
            UnloadDirectoryFiles
            IsFileDropped
            LoadDroppedFiles
            UnloadDroppedFiles
            GetFileModTime
            LoadAutomationEventList
            UnloadAutomationEventList
            ExportAutomationEventList
            SetAutomationEventList
            SetAutomationEventBaseFrame
            StartAutomationEventRecording
            StopAutomationEventRecording
            PlayAutomationEvent
            IsKeyPressed
            IsKeyPressedRepeat
            IsKeyDown
            IsKeyReleased
            IsKeyUp
            GetKeyPressed
            GetCharPressed
            SetExitKey
            IsGamepadAvailable
            GetGamepadName
            IsGamepadButtonPressed
            IsGamepadButtonDown
            IsGamepadButtonReleased
            IsGamepadButtonUp
            GetGamepadButtonPressed
            GetGamepadAxisCount
            GetGamepadAxisMovement
            SetGamepadMappings
            SetGamepadVibration
            IsMouseButtonPressed
            IsMouseButtonDown
            IsMouseButtonReleased
            IsMouseButtonUp
            GetMouseX
            GetMouseY
            GetMousePosition
            GetMouseDelta
            SetMousePosition
            SetMouseOffset
            SetMouseScale
            GetMouseWheelMove
            GetMouseWheelMoveV
            SetMouseCursor
            GetTouchX
            GetTouchY
            GetTouchPosition
            GetTouchPointId
            GetTouchPointCount
            SetGesturesEnabled
            IsGestureDetected
            GetGestureDetected
            GetGestureHoldDuration
            GetGestureDragVector
            GetGestureDragAngle
            GetGesturePinchVector
            GetGesturePinchAngle
            UpdateCamera
            UpdateCameraPro
            SetShapesTexture
            GetShapesTexture
            GetShapesTextureRectangle
            DrawPixel
            DrawPixelV
            DrawLine
            DrawLineV
            DrawLineEx
            DrawLineBezier
            DrawCircle
            DrawCircleSector
            DrawCircleSectorLines
            DrawCircleGradient
            DrawCircleV
            DrawCircleLines
            DrawCircleLinesV
            DrawEllipse
            DrawEllipseLines
            DrawRing
            DrawRingLines
            DrawRectangle
            DrawRectangleV
            DrawRectangleRec
            DrawRectanglePro
            DrawRectangleGradientV
            DrawRectangleGradientH
            DrawRectangleGradientEx
            DrawRectangleLines
            DrawRectangleLinesEx
            DrawRectangleRounded
            DrawRectangleRoundedLines
            DrawRectangleRoundedLinesEx
            DrawTriangle
            DrawTriangleLines
            DrawPoly
            DrawPolyLines
            DrawPolyLinesEx
            DrawSplineLinear
            DrawSplineBasis
            DrawSplineCatmullRom
            DrawSplineBezierQuadratic
            DrawSplineBezierCubic
            DrawSplineSegmentLinear
            DrawSplineSegmentBasis
            DrawSplineSegmentCatmullRom
            DrawSplineSegmentBezierQuadratic
            DrawSplineSegmentBezierCubic
            GetSplinePointLinear
            GetSplinePointBasis
            GetSplinePointCatmullRom
            GetSplinePointBezierQuad
            GetSplinePointBezierCubic
            CheckCollisionRecs
            CheckCollisionCircles
            CheckCollisionCircleRec
            CheckCollisionPointRec
            CheckCollisionPointCircle
            CheckCollisionPointTriangle
            CheckCollisionPointPoly
            CheckCollisionPointLine
            GetCollisionRec
            LoadImage
            LoadImageRaw
            LoadImageSvg
            LoadImageAnimFromMemory
            LoadImageFromMemory
            LoadImageFromTexture
            LoadImageFromScreen
            IsImageReady
            UnloadImage
            ExportImage
            ExportImageToMemory
            ExportImageAsCode
            GenImageColor
            GenImageGradientLinear
            GenImageGradientRadial
            GenImageGradientSquare
            GenImageChecked
            GenImageWhiteNoise
            GenImagePerlinNoise
            GenImageCellular
            GenImageText
            ImageCopy
            ImageFromImage
            ImageText
            ImageTextEx
            ImageFormat
            ImageToPOT
            ImageCrop
            ImageAlphaCrop
            ImageAlphaClear
            ImageAlphaMask
            ImageAlphaPremultiply
            ImageBlurGaussian
            ImageKernelConvolution
            ImageResize
            ImageResizeNN
            ImageResizeCanvas
            ImageMipmaps
            ImageDither
            ImageFlipVertical
            ImageFlipHorizontal
            ImageRotate
            ImageRotateCW
            ImageRotateCCW
            ImageColorTint
            ImageColorInvert
            ImageColorGrayscale
            ImageColorContrast
            ImageColorBrightness
            ImageColorReplace
            UnloadImageColors
            UnloadImagePalette
            GetImageAlphaBorder
            GetImageColor
            ImageClearBackground
            ImageDrawPixel
            ImageDrawPixelV
            ImageDrawLine
            ImageDrawLineV
            ImageDrawCircle
            ImageDrawCircleV
            ImageDrawCircleLines
            ImageDrawCircleLinesV
            ImageDrawRectangle
            ImageDrawRectangleV
            ImageDrawRectangleRec
            ImageDrawRectangleLines
            ImageDraw
            ImageDrawText
            ImageDrawTextEx
            LoadTexture
            LoadTextureFromImage
            LoadTextureCubemap
            LoadRenderTexture
            IsTextureReady
            UnloadTexture
            IsRenderTextureReady
            UnloadRenderTexture
            GenTextureMipmaps
            SetTextureFilter
            SetTextureWrap
            DrawTexture
            DrawTextureV
            DrawTextureEx
            DrawTextureRec
            DrawTexturePro
            DrawTextureNPatch
            ColorIsEqual
            Fade
            ColorToInt
            ColorNormalize
            ColorFromNormalized
            ColorToHSV
            ColorFromHSV
            ColorTint
            ColorBrightness
            ColorContrast
            ColorAlpha
            ColorAlphaBlend
            GetColor
            GetPixelDataSize
            GetFontDefault
            LoadFont
            LoadFontFromImage
            IsFontReady
            UnloadFontData
            UnloadFont
            ExportFontAsCode
            DrawFPS
            DrawText
            DrawTextEx
            DrawTextPro
            DrawTextCodepoint
            SetTextLineSpacing
            MeasureText
            MeasureTextEx
            GetGlyphIndex
            GetGlyphInfo
            GetGlyphAtlasRec
            GetCodepointCount
            TextIsEqual
            TextLength
            TextSubtext
            TextReplace
            TextInsert
            TextFindIndex
            TextToUpper
            TextToLower
            TextToPascal
            TextToInteger
            TextToFloat
            DrawLine3D
            DrawPoint3D
            DrawCircle3D
            DrawTriangle3D
            DrawTriangleStrip3D
            DrawCube
            DrawCubeV
            DrawCubeWires
            DrawCubeWiresV
            DrawSphere
            DrawSphereEx
            DrawSphereWires
            DrawCylinder
            DrawCylinderEx
            DrawCylinderWires
            DrawCylinderWiresEx
            DrawCapsule
            DrawCapsuleWires
            DrawPlane
            DrawRay
            DrawGrid
            LoadModel
            LoadModelFromMesh
            IsModelReady
            UnloadModel
            GetModelBoundingBox
            DrawModel
            DrawModelEx
            DrawModelWires
            DrawModelWiresEx
            DrawBoundingBox
            DrawBillboard
            DrawBillboardRec
            DrawBillboardPro
            UploadMesh
            UnloadMesh
            DrawMesh
            DrawMeshInstanced
            GetMeshBoundingBox
            GenMeshTangents
            ExportMesh
            ExportMeshAsCode
            GenMeshPoly
            GenMeshPlane
            GenMeshCube
            GenMeshSphere
            GenMeshHemiSphere
            GenMeshCylinder
            GenMeshCone
            GenMeshTorus
            GenMeshKnot
            GenMeshHeightmap
            GenMeshCubicmap
            LoadMaterialDefault
            IsMaterialReady
            UnloadMaterial
            SetMaterialTexture
            SetModelMeshMaterial
            UpdateModelAnimation
            UnloadModelAnimation
            IsModelAnimationValid
            CheckCollisionSpheres
            CheckCollisionBoxes
            CheckCollisionBoxSphere
            GetRayCollisionSphere
            GetRayCollisionBox
            GetRayCollisionMesh
            GetRayCollisionTriangle
            GetRayCollisionQuad
            InitAudioDevice
            CloseAudioDevice
            IsAudioDeviceReady
            SetMasterVolume
            GetMasterVolume
            LoadWave
            LoadWaveFromMemory
            IsWaveReady
            LoadSound
            LoadSoundFromWave
            LoadSoundAlias
            IsSoundReady
            UnloadWave
            UnloadSound
            UnloadSoundAlias
            ExportWave
            ExportWaveAsCode
            PlaySound
            StopSound
            PauseSound
            ResumeSound
            IsSoundPlaying
            SetSoundVolume
            SetSoundPitch
            SetSoundPan
            WaveCopy
            WaveCrop
            WaveFormat
            LoadMusicStream
            LoadMusicStreamFromMemory
            IsMusicReady
            UnloadMusicStream
            PlayMusicStream
            IsMusicStreamPlaying
            UpdateMusicStream
            StopMusicStream
            PauseMusicStream
            ResumeMusicStream
            SeekMusicStream
            SetMusicVolume
            SetMusicPitch
            SetMusicPan
            GetMusicTimeLength
            GetMusicTimePlayed
            LoadAudioStream
            IsAudioStreamReady
            UnloadAudioStream
            IsAudioStreamProcessed
            PlayAudioStream
            PauseAudioStream
            ResumeAudioStream
            IsAudioStreamPlaying
            StopAudioStream
            SetAudioStreamVolume
            SetAudioStreamPitch
            SetAudioStreamPan
            SetAudioStreamBufferSizeDefault
            FLAG_VSYNC_HINT
            FLAG_FULLSCREEN_MODE
            FLAG_WINDOW_RESIZABLE
            FLAG_WINDOW_UNDECORATED
            FLAG_WINDOW_HIDDEN
            FLAG_WINDOW_MINIMIZED
            FLAG_WINDOW_MAXIMIZED
            FLAG_WINDOW_UNFOCUSED
            FLAG_WINDOW_TOPMOST
            FLAG_WINDOW_ALWAYS_RUN
            FLAG_WINDOW_TRANSPARENT
            FLAG_WINDOW_HIGHDPI
            FLAG_WINDOW_MOUSE_PASSTHROUGH
            FLAG_BORDERLESS_WINDOWED_MODE
            FLAG_MSAA_4X_HINT
            FLAG_INTERLACED_HINT
            LOG_ALL
            LOG_TRACE
            LOG_DEBUG
            LOG_INFO
            LOG_WARNING
            LOG_ERROR
            LOG_FATAL
            LOG_NONE
            KEY_NULL
            KEY_APOSTROPHE
            KEY_COMMA
            KEY_MINUS
            KEY_PERIOD
            KEY_SLASH
            KEY_ZERO
            KEY_ONE
            KEY_TWO
            KEY_THREE
            KEY_FOUR
            KEY_FIVE
            KEY_SIX
            KEY_SEVEN
            KEY_EIGHT
            KEY_NINE
            KEY_SEMICOLON
            KEY_EQUAL
            KEY_A
            KEY_B
            KEY_C
            KEY_D
            KEY_E
            KEY_F
            KEY_G
            KEY_H
            KEY_I
            KEY_J
            KEY_K
            KEY_L
            KEY_M
            KEY_N
            KEY_O
            KEY_P
            KEY_Q
            KEY_R
            KEY_S
            KEY_T
            KEY_U
            KEY_V
            KEY_W
            KEY_X
            KEY_Y
            KEY_Z
            KEY_LEFT_BRACKET
            KEY_BACKSLASH
            KEY_RIGHT_BRACKET
            KEY_GRAVE
            KEY_SPACE
            KEY_ESCAPE
            KEY_ENTER
            KEY_TAB
            KEY_BACKSPACE
            KEY_INSERT
            KEY_DELETE
            KEY_RIGHT
            KEY_LEFT
            KEY_DOWN
            KEY_UP
            KEY_PAGE_UP
            KEY_PAGE_DOWN
            KEY_HOME
            KEY_END
            KEY_CAPS_LOCK
            KEY_SCROLL_LOCK
            KEY_NUM_LOCK
            KEY_PRINT_SCREEN
            KEY_PAUSE
            KEY_F1
            KEY_F2
            KEY_F3
            KEY_F4
            KEY_F5
            KEY_F6
            KEY_F7
            KEY_F8
            KEY_F9
            KEY_F10
            KEY_F11
            KEY_F12
            KEY_LEFT_SHIFT
            KEY_LEFT_CONTROL
            KEY_LEFT_ALT
            KEY_LEFT_SUPER
            KEY_RIGHT_SHIFT
            KEY_RIGHT_CONTROL
            KEY_RIGHT_ALT
            KEY_RIGHT_SUPER
            KEY_KB_MENU
            KEY_KP_0
            KEY_KP_1
            KEY_KP_2
            KEY_KP_3
            KEY_KP_4
            KEY_KP_5
            KEY_KP_6
            KEY_KP_7
            KEY_KP_8
            KEY_KP_9
            KEY_KP_DECIMAL
            KEY_KP_DIVIDE
            KEY_KP_MULTIPLY
            KEY_KP_SUBTRACT
            KEY_KP_ADD
            KEY_KP_ENTER
            KEY_KP_EQUAL
            KEY_BACK
            KEY_MENU
            KEY_VOLUME_UP
            KEY_VOLUME_DOWN
            MOUSE_BUTTON_LEFT
            MOUSE_BUTTON_RIGHT
            MOUSE_BUTTON_MIDDLE
            MOUSE_BUTTON_SIDE
            MOUSE_BUTTON_EXTRA
            MOUSE_BUTTON_FORWARD
            MOUSE_BUTTON_BACK
            MOUSE_CURSOR_DEFAULT
            MOUSE_CURSOR_ARROW
            MOUSE_CURSOR_IBEAM
            MOUSE_CURSOR_CROSSHAIR
            MOUSE_CURSOR_POINTING_HAND
            MOUSE_CURSOR_RESIZE_EW
            MOUSE_CURSOR_RESIZE_NS
            MOUSE_CURSOR_RESIZE_NWSE
            MOUSE_CURSOR_RESIZE_NESW
            MOUSE_CURSOR_RESIZE_ALL
            MOUSE_CURSOR_NOT_ALLOWED
            GAMEPAD_BUTTON_UNKNOWN
            GAMEPAD_BUTTON_LEFT_FACE_UP
            GAMEPAD_BUTTON_LEFT_FACE_RIGHT
            GAMEPAD_BUTTON_LEFT_FACE_DOWN
            GAMEPAD_BUTTON_LEFT_FACE_LEFT
            GAMEPAD_BUTTON_RIGHT_FACE_UP
            GAMEPAD_BUTTON_RIGHT_FACE_RIGHT
            GAMEPAD_BUTTON_RIGHT_FACE_DOWN
            GAMEPAD_BUTTON_RIGHT_FACE_LEFT
            GAMEPAD_BUTTON_LEFT_TRIGGER_1
            GAMEPAD_BUTTON_LEFT_TRIGGER_2
            GAMEPAD_BUTTON_RIGHT_TRIGGER_1
            GAMEPAD_BUTTON_RIGHT_TRIGGER_2
            GAMEPAD_BUTTON_MIDDLE_LEFT
            GAMEPAD_BUTTON_MIDDLE
            GAMEPAD_BUTTON_MIDDLE_RIGHT
            GAMEPAD_BUTTON_LEFT_THUMB
            GAMEPAD_BUTTON_RIGHT_THUMB
            GAMEPAD_AXIS_LEFT_X
            GAMEPAD_AXIS_LEFT_Y
            GAMEPAD_AXIS_RIGHT_X
            GAMEPAD_AXIS_RIGHT_Y
            GAMEPAD_AXIS_LEFT_TRIGGER
            GAMEPAD_AXIS_RIGHT_TRIGGER
            MATERIAL_MAP_ALBEDO
            MATERIAL_MAP_METALNESS
            MATERIAL_MAP_NORMAL
            MATERIAL_MAP_ROUGHNESS
            MATERIAL_MAP_OCCLUSION
            MATERIAL_MAP_EMISSION
            MATERIAL_MAP_HEIGHT
            MATERIAL_MAP_CUBEMAP
            MATERIAL_MAP_IRRADIANCE
            MATERIAL_MAP_PREFILTER
            MATERIAL_MAP_BRDF
            SHADER_LOC_VERTEX_POSITION
            SHADER_LOC_VERTEX_TEXCOORD01
            SHADER_LOC_VERTEX_TEXCOORD02
            SHADER_LOC_VERTEX_NORMAL
            SHADER_LOC_VERTEX_TANGENT
            SHADER_LOC_VERTEX_COLOR
            SHADER_LOC_MATRIX_MVP
            SHADER_LOC_MATRIX_VIEW
            SHADER_LOC_MATRIX_PROJECTION
            SHADER_LOC_MATRIX_MODEL
            SHADER_LOC_MATRIX_NORMAL
            SHADER_LOC_VECTOR_VIEW
            SHADER_LOC_COLOR_DIFFUSE
            SHADER_LOC_COLOR_SPECULAR
            SHADER_LOC_COLOR_AMBIENT
            SHADER_LOC_MAP_ALBEDO
            SHADER_LOC_MAP_METALNESS
            SHADER_LOC_MAP_NORMAL
            SHADER_LOC_MAP_ROUGHNESS
            SHADER_LOC_MAP_OCCLUSION
            SHADER_LOC_MAP_EMISSION
            SHADER_LOC_MAP_HEIGHT
            SHADER_LOC_MAP_CUBEMAP
            SHADER_LOC_MAP_IRRADIANCE
            SHADER_LOC_MAP_PREFILTER
            SHADER_LOC_MAP_BRDF
            SHADER_UNIFORM_FLOAT
            SHADER_UNIFORM_VEC2
            SHADER_UNIFORM_VEC3
            SHADER_UNIFORM_VEC4
            SHADER_UNIFORM_INT
            SHADER_UNIFORM_IVEC2
            SHADER_UNIFORM_IVEC3
            SHADER_UNIFORM_IVEC4
            SHADER_UNIFORM_SAMPLER2D
            SHADER_ATTRIB_FLOAT
            SHADER_ATTRIB_VEC2
            SHADER_ATTRIB_VEC3
            SHADER_ATTRIB_VEC4
            PIXELFORMAT_UNCOMPRESSED_GRAYSCALE
            PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA
            PIXELFORMAT_UNCOMPRESSED_R5G6B5
            PIXELFORMAT_UNCOMPRESSED_R8G8B8
            PIXELFORMAT_UNCOMPRESSED_R5G5B5A1
            PIXELFORMAT_UNCOMPRESSED_R4G4B4A4
            PIXELFORMAT_UNCOMPRESSED_R8G8B8A8
            PIXELFORMAT_UNCOMPRESSED_R32
            PIXELFORMAT_UNCOMPRESSED_R32G32B32
            PIXELFORMAT_UNCOMPRESSED_R32G32B32A32
            PIXELFORMAT_UNCOMPRESSED_R16
            PIXELFORMAT_UNCOMPRESSED_R16G16B16
            PIXELFORMAT_UNCOMPRESSED_R16G16B16A16
            PIXELFORMAT_COMPRESSED_DXT1_RGB
            PIXELFORMAT_COMPRESSED_DXT1_RGBA
            PIXELFORMAT_COMPRESSED_DXT3_RGBA
            PIXELFORMAT_COMPRESSED_DXT5_RGBA
            PIXELFORMAT_COMPRESSED_ETC1_RGB
            PIXELFORMAT_COMPRESSED_ETC2_RGB
            PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA
            PIXELFORMAT_COMPRESSED_PVRT_RGB
            PIXELFORMAT_COMPRESSED_PVRT_RGBA
            PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA
            PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA
            TEXTURE_FILTER_POINT
            TEXTURE_FILTER_BILINEAR
            TEXTURE_FILTER_TRILINEAR
            TEXTURE_FILTER_ANISOTROPIC_4X
            TEXTURE_FILTER_ANISOTROPIC_8X
            TEXTURE_FILTER_ANISOTROPIC_16X
            TEXTURE_WRAP_REPEAT
            TEXTURE_WRAP_CLAMP
            TEXTURE_WRAP_MIRROR_REPEAT
            TEXTURE_WRAP_MIRROR_CLAMP
            CUBEMAP_LAYOUT_AUTO_DETECT
            CUBEMAP_LAYOUT_LINE_VERTICAL
            CUBEMAP_LAYOUT_LINE_HORIZONTAL
            CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR
            CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE
            CUBEMAP_LAYOUT_PANORAMA
            FONT_DEFAULT
            FONT_BITMAP
            FONT_SDF
            BLEND_ALPHA
            BLEND_ADDITIVE
            BLEND_MULTIPLIED
            BLEND_ADD_COLORS
            BLEND_SUBTRACT_COLORS
            BLEND_ALPHA_PREMULTIPLY
            BLEND_CUSTOM
            BLEND_CUSTOM_SEPARATE
            GESTURE_NONE
            GESTURE_TAP
            GESTURE_DOUBLETAP
            GESTURE_HOLD
            GESTURE_DRAG
            GESTURE_SWIPE_RIGHT
            GESTURE_SWIPE_LEFT
            GESTURE_SWIPE_UP
            GESTURE_SWIPE_DOWN
            GESTURE_PINCH_IN
            GESTURE_PINCH_OUT
            CAMERA_CUSTOM
            CAMERA_FREE
            CAMERA_ORBITAL
            CAMERA_FIRST_PERSON
            CAMERA_THIRD_PERSON
            CAMERA_PERSPECTIVE
            CAMERA_ORTHOGRAPHIC
            NPATCH_NINE_PATCH
            NPATCH_THREE_PATCH_VERTICAL
            NPATCH_THREE_PATCH_HORIZONTAL
            make-Sound
            Sound-stream
            Sound-frameCount
            Sound-set-stream!
            Sound-set-frameCount!
            make-BoundingBox
            BoundingBox-min
            BoundingBox-max
            BoundingBox-set-min!
            BoundingBox-set-max!
            make-RayCollision
            RayCollision-hit
            RayCollision-distance
            RayCollision-point
            RayCollision-normal
            RayCollision-set-hit!
            RayCollision-set-distance!
            RayCollision-set-point!
            RayCollision-set-normal!
            make-Ray
            Ray-position
            Ray-direction
            Ray-set-position!
            Ray-set-direction!
            make-Transform
            Transform-translation
            Transform-rotation
            Transform-scale
            Transform-set-translation!
            Transform-set-rotation!
            Transform-set-scale!
            make-MaterialMap
            MaterialMap-texture
            MaterialMap-color
            MaterialMap-value
            MaterialMap-set-texture!
            MaterialMap-set-color!
            MaterialMap-set-value!
            make-Camera2D
            Camera2D-offset
            Camera2D-target
            Camera2D-rotation
            Camera2D-zoom
            Camera2D-set-offset!
            Camera2D-set-target!
            Camera2D-set-rotation!
            Camera2D-set-zoom!
            make-Camera3D
            Camera3D-position
            Camera3D-target
            Camera3D-up
            Camera3D-fovy
            Camera3D-projection
            Camera3D-set-position!
            Camera3D-set-target!
            Camera3D-set-up!
            Camera3D-set-fovy!
            Camera3D-set-projection!
            make-GlyphInfo
            GlyphInfo-value
            GlyphInfo-offsetX
            GlyphInfo-offsetY
            GlyphInfo-advanceX
            GlyphInfo-image
            GlyphInfo-set-value!
            GlyphInfo-set-offsetX!
            GlyphInfo-set-offsetY!
            GlyphInfo-set-advanceX!
            GlyphInfo-set-image!
            make-NPatchInfo
            NPatchInfo-source
            NPatchInfo-left
            NPatchInfo-top
            NPatchInfo-right
            NPatchInfo-bottom
            NPatchInfo-layout
            NPatchInfo-set-source!
            NPatchInfo-set-left!
            NPatchInfo-set-top!
            NPatchInfo-set-right!
            NPatchInfo-set-bottom!
            NPatchInfo-set-layout!
            make-RenderTexture
            RenderTexture-id
            RenderTexture-texture
            RenderTexture-depth
            RenderTexture-set-id!
            RenderTexture-set-texture!
            RenderTexture-set-depth!
            make-Texture
            Texture-id
            Texture-width
            Texture-height
            Texture-mipmaps
            Texture-format
            Texture-set-id!
            Texture-set-width!
            Texture-set-height!
            Texture-set-mipmaps!
            Texture-set-format!
            make-Rectangle
            Rectangle-x
            Rectangle-y
            Rectangle-width
            Rectangle-height
            Rectangle-set-x!
            Rectangle-set-y!
            Rectangle-set-width!
            Rectangle-set-height!
            make-Color
            Color-r
            Color-g
            Color-b
            Color-a
            Color-set-r!
            Color-set-g!
            Color-set-b!
            Color-set-a!
            construct-Matrix
            Matrix-m0
            Matrix-m4
            Matrix-m8
            Matrix-m12
            Matrix-m1
            Matrix-m5
            Matrix-m9
            Matrix-m13
            Matrix-m2
            Matrix-m6
            Matrix-m10
            Matrix-m14
            Matrix-m3
            Matrix-m7
            Matrix-m11
            Matrix-m15
            Matrix-set-m0!
            Matrix-set-m4!
            Matrix-set-m8!
            Matrix-set-m12!
            Matrix-set-m1!
            Matrix-set-m5!
            Matrix-set-m9!
            Matrix-set-m13!
            Matrix-set-m2!
            Matrix-set-m6!
            Matrix-set-m10!
            Matrix-set-m14!
            Matrix-set-m3!
            Matrix-set-m7!
            Matrix-set-m11!
            Matrix-set-m15!
            make-Vector4
            Vector4-x
            Vector4-y
            Vector4-z
            Vector4-w
            Vector4-set-x!
            Vector4-set-y!
            Vector4-set-z!
            Vector4-set-w!
            make-Vector3
            Vector3-x
            Vector3-y
            Vector3-z
            Vector3-set-x!
            Vector3-set-y!
            Vector3-set-z!
            make-Vector2
            Vector2-x
            Vector2-y
            Vector2-set-x!
            Vector2-set-y!
            make-Matrix
            LIGHTGRAY
            GRAY
            DARKGRAY
            YELLOW
            GOLD
            ORANGE
            PINK
            RED
            MAROON
            GREEN
            LIME
            DARKGREEN
            SKYBLUE
            BLUE
            DARKBLUE
            PURPLE
            VIOLET
            DARKPURPLE
            BEIGE
            BROWN
            DARKBROWN
            WHITE
            BLACK
            BLANK
            MAGENTA
            RAYWHITE))

(load-extension "libraylib-guile" "init_raylib_guile")

(define (make-Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15)
  (define m (construct-Matrix))
  (Matrix-set-m0! m m0)
  (Matrix-set-m1! m m1)
  (Matrix-set-m2! m m2)
  (Matrix-set-m3! m m3)
  (Matrix-set-m4! m m4)
  (Matrix-set-m5! m m5)
  (Matrix-set-m6! m m6)
  (Matrix-set-m7! m m7)
  (Matrix-set-m8! m m8)
  (Matrix-set-m9! m m9)
  (Matrix-set-m10! m m10)
  (Matrix-set-m11! m m11)
  (Matrix-set-m12! m m12)
  (Matrix-set-m13! m m13)
  (Matrix-set-m14! m m14)
  (Matrix-set-m15! m m15)
  m)
(define FLAG_VSYNC_HINT 64)
(define FLAG_FULLSCREEN_MODE 2)
(define FLAG_WINDOW_RESIZABLE 4)
(define FLAG_WINDOW_UNDECORATED 8)
(define FLAG_WINDOW_HIDDEN 128)
(define FLAG_WINDOW_MINIMIZED 512)
(define FLAG_WINDOW_MAXIMIZED 1024)
(define FLAG_WINDOW_UNFOCUSED 2048)
(define FLAG_WINDOW_TOPMOST 4096)
(define FLAG_WINDOW_ALWAYS_RUN 256)
(define FLAG_WINDOW_TRANSPARENT 16)
(define FLAG_WINDOW_HIGHDPI 8192)
(define FLAG_WINDOW_MOUSE_PASSTHROUGH 16384)
(define FLAG_BORDERLESS_WINDOWED_MODE 32768)
(define FLAG_MSAA_4X_HINT 32)
(define FLAG_INTERLACED_HINT 65536)
(define LOG_ALL 0)
(define LOG_TRACE 1)
(define LOG_DEBUG 2)
(define LOG_INFO 3)
(define LOG_WARNING 4)
(define LOG_ERROR 5)
(define LOG_FATAL 6)
(define LOG_NONE 7)
(define KEY_NULL 0)
(define KEY_APOSTROPHE 39)
(define KEY_COMMA 44)
(define KEY_MINUS 45)
(define KEY_PERIOD 46)
(define KEY_SLASH 47)
(define KEY_ZERO 48)
(define KEY_ONE 49)
(define KEY_TWO 50)
(define KEY_THREE 51)
(define KEY_FOUR 52)
(define KEY_FIVE 53)
(define KEY_SIX 54)
(define KEY_SEVEN 55)
(define KEY_EIGHT 56)
(define KEY_NINE 57)
(define KEY_SEMICOLON 59)
(define KEY_EQUAL 61)
(define KEY_A 65)
(define KEY_B 66)
(define KEY_C 67)
(define KEY_D 68)
(define KEY_E 69)
(define KEY_F 70)
(define KEY_G 71)
(define KEY_H 72)
(define KEY_I 73)
(define KEY_J 74)
(define KEY_K 75)
(define KEY_L 76)
(define KEY_M 77)
(define KEY_N 78)
(define KEY_O 79)
(define KEY_P 80)
(define KEY_Q 81)
(define KEY_R 82)
(define KEY_S 83)
(define KEY_T 84)
(define KEY_U 85)
(define KEY_V 86)
(define KEY_W 87)
(define KEY_X 88)
(define KEY_Y 89)
(define KEY_Z 90)
(define KEY_LEFT_BRACKET 91)
(define KEY_BACKSLASH 92)
(define KEY_RIGHT_BRACKET 93)
(define KEY_GRAVE 96)
(define KEY_SPACE 32)
(define KEY_ESCAPE 256)
(define KEY_ENTER 257)
(define KEY_TAB 258)
(define KEY_BACKSPACE 259)
(define KEY_INSERT 260)
(define KEY_DELETE 261)
(define KEY_RIGHT 262)
(define KEY_LEFT 263)
(define KEY_DOWN 264)
(define KEY_UP 265)
(define KEY_PAGE_UP 266)
(define KEY_PAGE_DOWN 267)
(define KEY_HOME 268)
(define KEY_END 269)
(define KEY_CAPS_LOCK 280)
(define KEY_SCROLL_LOCK 281)
(define KEY_NUM_LOCK 282)
(define KEY_PRINT_SCREEN 283)
(define KEY_PAUSE 284)
(define KEY_F1 290)
(define KEY_F2 291)
(define KEY_F3 292)
(define KEY_F4 293)
(define KEY_F5 294)
(define KEY_F6 295)
(define KEY_F7 296)
(define KEY_F8 297)
(define KEY_F9 298)
(define KEY_F10 299)
(define KEY_F11 300)
(define KEY_F12 301)
(define KEY_LEFT_SHIFT 340)
(define KEY_LEFT_CONTROL 341)
(define KEY_LEFT_ALT 342)
(define KEY_LEFT_SUPER 343)
(define KEY_RIGHT_SHIFT 344)
(define KEY_RIGHT_CONTROL 345)
(define KEY_RIGHT_ALT 346)
(define KEY_RIGHT_SUPER 347)
(define KEY_KB_MENU 348)
(define KEY_KP_0 320)
(define KEY_KP_1 321)
(define KEY_KP_2 322)
(define KEY_KP_3 323)
(define KEY_KP_4 324)
(define KEY_KP_5 325)
(define KEY_KP_6 326)
(define KEY_KP_7 327)
(define KEY_KP_8 328)
(define KEY_KP_9 329)
(define KEY_KP_DECIMAL 330)
(define KEY_KP_DIVIDE 331)
(define KEY_KP_MULTIPLY 332)
(define KEY_KP_SUBTRACT 333)
(define KEY_KP_ADD 334)
(define KEY_KP_ENTER 335)
(define KEY_KP_EQUAL 336)
(define KEY_BACK 4)
(define KEY_MENU 5)
(define KEY_VOLUME_UP 24)
(define KEY_VOLUME_DOWN 25)
(define MOUSE_BUTTON_LEFT 0)
(define MOUSE_BUTTON_RIGHT 1)
(define MOUSE_BUTTON_MIDDLE 2)
(define MOUSE_BUTTON_SIDE 3)
(define MOUSE_BUTTON_EXTRA 4)
(define MOUSE_BUTTON_FORWARD 5)
(define MOUSE_BUTTON_BACK 6)
(define MOUSE_CURSOR_DEFAULT 0)
(define MOUSE_CURSOR_ARROW 1)
(define MOUSE_CURSOR_IBEAM 2)
(define MOUSE_CURSOR_CROSSHAIR 3)
(define MOUSE_CURSOR_POINTING_HAND 4)
(define MOUSE_CURSOR_RESIZE_EW 5)
(define MOUSE_CURSOR_RESIZE_NS 6)
(define MOUSE_CURSOR_RESIZE_NWSE 7)
(define MOUSE_CURSOR_RESIZE_NESW 8)
(define MOUSE_CURSOR_RESIZE_ALL 9)
(define MOUSE_CURSOR_NOT_ALLOWED 10)
(define GAMEPAD_BUTTON_UNKNOWN 0)
(define GAMEPAD_BUTTON_LEFT_FACE_UP 1)
(define GAMEPAD_BUTTON_LEFT_FACE_RIGHT 2)
(define GAMEPAD_BUTTON_LEFT_FACE_DOWN 3)
(define GAMEPAD_BUTTON_LEFT_FACE_LEFT 4)
(define GAMEPAD_BUTTON_RIGHT_FACE_UP 5)
(define GAMEPAD_BUTTON_RIGHT_FACE_RIGHT 6)
(define GAMEPAD_BUTTON_RIGHT_FACE_DOWN 7)
(define GAMEPAD_BUTTON_RIGHT_FACE_LEFT 8)
(define GAMEPAD_BUTTON_LEFT_TRIGGER_1 9)
(define GAMEPAD_BUTTON_LEFT_TRIGGER_2 10)
(define GAMEPAD_BUTTON_RIGHT_TRIGGER_1 11)
(define GAMEPAD_BUTTON_RIGHT_TRIGGER_2 12)
(define GAMEPAD_BUTTON_MIDDLE_LEFT 13)
(define GAMEPAD_BUTTON_MIDDLE 14)
(define GAMEPAD_BUTTON_MIDDLE_RIGHT 15)
(define GAMEPAD_BUTTON_LEFT_THUMB 16)
(define GAMEPAD_BUTTON_RIGHT_THUMB 17)
(define GAMEPAD_AXIS_LEFT_X 0)
(define GAMEPAD_AXIS_LEFT_Y 1)
(define GAMEPAD_AXIS_RIGHT_X 2)
(define GAMEPAD_AXIS_RIGHT_Y 3)
(define GAMEPAD_AXIS_LEFT_TRIGGER 4)
(define GAMEPAD_AXIS_RIGHT_TRIGGER 5)
(define MATERIAL_MAP_ALBEDO 0)
(define MATERIAL_MAP_METALNESS 1)
(define MATERIAL_MAP_NORMAL 2)
(define MATERIAL_MAP_ROUGHNESS 3)
(define MATERIAL_MAP_OCCLUSION 4)
(define MATERIAL_MAP_EMISSION 5)
(define MATERIAL_MAP_HEIGHT 6)
(define MATERIAL_MAP_CUBEMAP 7)
(define MATERIAL_MAP_IRRADIANCE 8)
(define MATERIAL_MAP_PREFILTER 9)
(define MATERIAL_MAP_BRDF 10)
(define SHADER_LOC_VERTEX_POSITION 0)
(define SHADER_LOC_VERTEX_TEXCOORD01 1)
(define SHADER_LOC_VERTEX_TEXCOORD02 2)
(define SHADER_LOC_VERTEX_NORMAL 3)
(define SHADER_LOC_VERTEX_TANGENT 4)
(define SHADER_LOC_VERTEX_COLOR 5)
(define SHADER_LOC_MATRIX_MVP 6)
(define SHADER_LOC_MATRIX_VIEW 7)
(define SHADER_LOC_MATRIX_PROJECTION 8)
(define SHADER_LOC_MATRIX_MODEL 9)
(define SHADER_LOC_MATRIX_NORMAL 10)
(define SHADER_LOC_VECTOR_VIEW 11)
(define SHADER_LOC_COLOR_DIFFUSE 12)
(define SHADER_LOC_COLOR_SPECULAR 13)
(define SHADER_LOC_COLOR_AMBIENT 14)
(define SHADER_LOC_MAP_ALBEDO 15)
(define SHADER_LOC_MAP_METALNESS 16)
(define SHADER_LOC_MAP_NORMAL 17)
(define SHADER_LOC_MAP_ROUGHNESS 18)
(define SHADER_LOC_MAP_OCCLUSION 19)
(define SHADER_LOC_MAP_EMISSION 20)
(define SHADER_LOC_MAP_HEIGHT 21)
(define SHADER_LOC_MAP_CUBEMAP 22)
(define SHADER_LOC_MAP_IRRADIANCE 23)
(define SHADER_LOC_MAP_PREFILTER 24)
(define SHADER_LOC_MAP_BRDF 25)
(define SHADER_UNIFORM_FLOAT 0)
(define SHADER_UNIFORM_VEC2 1)
(define SHADER_UNIFORM_VEC3 2)
(define SHADER_UNIFORM_VEC4 3)
(define SHADER_UNIFORM_INT 4)
(define SHADER_UNIFORM_IVEC2 5)
(define SHADER_UNIFORM_IVEC3 6)
(define SHADER_UNIFORM_IVEC4 7)
(define SHADER_UNIFORM_SAMPLER2D 8)
(define SHADER_ATTRIB_FLOAT 0)
(define SHADER_ATTRIB_VEC2 1)
(define SHADER_ATTRIB_VEC3 2)
(define SHADER_ATTRIB_VEC4 3)
(define PIXELFORMAT_UNCOMPRESSED_GRAYSCALE 1)
(define PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA 2)
(define PIXELFORMAT_UNCOMPRESSED_R5G6B5 3)
(define PIXELFORMAT_UNCOMPRESSED_R8G8B8 4)
(define PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 5)
(define PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 6)
(define PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 7)
(define PIXELFORMAT_UNCOMPRESSED_R32 8)
(define PIXELFORMAT_UNCOMPRESSED_R32G32B32 9)
(define PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 10)
(define PIXELFORMAT_UNCOMPRESSED_R16 11)
(define PIXELFORMAT_UNCOMPRESSED_R16G16B16 12)
(define PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 13)
(define PIXELFORMAT_COMPRESSED_DXT1_RGB 14)
(define PIXELFORMAT_COMPRESSED_DXT1_RGBA 15)
(define PIXELFORMAT_COMPRESSED_DXT3_RGBA 16)
(define PIXELFORMAT_COMPRESSED_DXT5_RGBA 17)
(define PIXELFORMAT_COMPRESSED_ETC1_RGB 18)
(define PIXELFORMAT_COMPRESSED_ETC2_RGB 19)
(define PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA 20)
(define PIXELFORMAT_COMPRESSED_PVRT_RGB 21)
(define PIXELFORMAT_COMPRESSED_PVRT_RGBA 22)
(define PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA 23)
(define PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA 24)
(define TEXTURE_FILTER_POINT 0)
(define TEXTURE_FILTER_BILINEAR 1)
(define TEXTURE_FILTER_TRILINEAR 2)
(define TEXTURE_FILTER_ANISOTROPIC_4X 3)
(define TEXTURE_FILTER_ANISOTROPIC_8X 4)
(define TEXTURE_FILTER_ANISOTROPIC_16X 5)
(define TEXTURE_WRAP_REPEAT 0)
(define TEXTURE_WRAP_CLAMP 1)
(define TEXTURE_WRAP_MIRROR_REPEAT 2)
(define TEXTURE_WRAP_MIRROR_CLAMP 3)
(define CUBEMAP_LAYOUT_AUTO_DETECT 0)
(define CUBEMAP_LAYOUT_LINE_VERTICAL 1)
(define CUBEMAP_LAYOUT_LINE_HORIZONTAL 2)
(define CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR 3)
(define CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE 4)
(define CUBEMAP_LAYOUT_PANORAMA 5)
(define FONT_DEFAULT 0)
(define FONT_BITMAP 1)
(define FONT_SDF 2)
(define BLEND_ALPHA 0)
(define BLEND_ADDITIVE 1)
(define BLEND_MULTIPLIED 2)
(define BLEND_ADD_COLORS 3)
(define BLEND_SUBTRACT_COLORS 4)
(define BLEND_ALPHA_PREMULTIPLY 5)
(define BLEND_CUSTOM 6)
(define BLEND_CUSTOM_SEPARATE 7)
(define GESTURE_NONE 0)
(define GESTURE_TAP 1)
(define GESTURE_DOUBLETAP 2)
(define GESTURE_HOLD 4)
(define GESTURE_DRAG 8)
(define GESTURE_SWIPE_RIGHT 16)
(define GESTURE_SWIPE_LEFT 32)
(define GESTURE_SWIPE_UP 64)
(define GESTURE_SWIPE_DOWN 128)
(define GESTURE_PINCH_IN 256)
(define GESTURE_PINCH_OUT 512)
(define CAMERA_CUSTOM 0)
(define CAMERA_FREE 1)
(define CAMERA_ORBITAL 2)
(define CAMERA_FIRST_PERSON 3)
(define CAMERA_THIRD_PERSON 4)
(define CAMERA_PERSPECTIVE 0)
(define CAMERA_ORTHOGRAPHIC 1)
(define NPATCH_NINE_PATCH 0)
(define NPATCH_THREE_PATCH_VERTICAL 1)
(define NPATCH_THREE_PATCH_HORIZONTAL 2)
(define LIGHTGRAY (make-Color 200 200 200 255))
(define GRAY (make-Color 130 130 130 255))
(define DARKGRAY (make-Color 80 80 80 255))
(define YELLOW (make-Color 253 249 0 255))
(define GOLD (make-Color 255 203 0 255))
(define ORANGE (make-Color 255 161 0 255))
(define PINK (make-Color 255 109 194 255))
(define RED (make-Color 230 41 55 255))
(define MAROON (make-Color 190 33 55 255))
(define GREEN (make-Color 0 228 48 255))
(define LIME (make-Color 0 158 47 255))
(define DARKGREEN (make-Color 0 117 44 255))
(define SKYBLUE (make-Color 102 191 255 255))
(define BLUE (make-Color 0 121 241 255))
(define DARKBLUE (make-Color 0 82 172 255))
(define PURPLE (make-Color 200 122 255 255))
(define VIOLET (make-Color 135 60 190 255))
(define DARKPURPLE (make-Color 112 31 126 255))
(define BEIGE (make-Color 211 176 131 255))
(define BROWN (make-Color 127 106 79 255))
(define DARKBROWN (make-Color 76 63 47 255))
(define WHITE (make-Color 255 255 255 255))
(define BLACK (make-Color 0 0 0 255))
(define BLANK (make-Color 0 0 0 0))
(define MAGENTA (make-Color 255 0 255 255))
(define RAYWHITE (make-Color 245 245 245 255))
