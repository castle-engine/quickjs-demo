Unit JSWindow;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}

Interface

Uses
  Classes, SysUtils, TypInfo,
  QuickJS,
  CastleWindow, CastleControls, CastleKeysMouse, CastleLog, CastleVectors;

Procedure JS_Window_init(ctx: JSContext; m: JSModuleDef); Cdecl;

Function JS_GetWindowClassData(ctx: JSContext; this_val: JSValue): Pointer;


Type

  TMyWin = Class(TCastleWindow)

  end;


  PJSWindowData = ^TJSWindowData;

  TJSWindowData = Record
    JSWindow: TCastleWindow;
    OnRender: JSValue;
    OnPress: JSValue;
    JSThis: JSValue;
  End;

Implementation

Uses
  QJSUtils, QJS_Engine;

Type
  Window_Exports = Array Of JSCFunctionListEntry;

Var
  Window_Class_id: JSClassID = 0;
  Window_Class_JS: JSClassDef = (class_name: 'Window'; finalizer: nil; gc_mark: nil; call: nil; exotic: nil);
  Window_proto_funcs: Array Of JSCFunctionListEntry;

Const
  WIN_OnRender = 0;
  WIN_OnPress = 1;
  WIN_Height = 100;
  WIN_Width = 200;


Function GetClassID(Value: JSValueConst): JSClassID;
Var
  js_obj: JSObject;
Begin
  js_obj := JS_VALUE_GET_OBJ(Value);
  Result := JSClassID((Pbyte(js_obj) + 6)^);
End;

Function JS_GetWindowClassData(ctx: JSContext; this_val: JSValue): Pointer;
Begin
  Result := JS_GetOpaque2(ctx, this_val, Window_Class_id);
  If Not Assigned(Result) Then
    JS_ThrowInternalError(ctx, 'Error : Window Obj has no data', []);
End;

Procedure Window_JS_finalizer(rt: JSRuntime; val: JSValue); Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque(val, Window_Class_id);
  If Assigned(Data) Then
  Begin
    WritelnLog('[-] Window Finalize');
    JS_FreeValueRT(rt, Data^.OnRender);
    JS_FreeValueRT(rt, Data^.OnPress);
    JS_FreeValueRT(rt, Data^.JSThis);
    js_free_rt(rt, Data);
  End;
End;

Procedure Window_JS_GCMark(rt: JSRuntime; val: JSValueConst; mark_func: PJS_MarkFunc); Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque(val, Window_Class_id);
  If Assigned(Data) Then
  Begin
    WritelnLog('[*] Window GC');
    JS_MarkValue(rt, Data^.JSThis, mark_func);
    JS_MarkValue(rt, Data^.OnRender, mark_func);
    JS_MarkValue(rt, Data^.OnPress, mark_func);
  End;
End;

{==================================================================================================}

Procedure JS_WindowRender(Container: TUIContainer);
Var
  Tag: PtrInt;
  Data: PJSWindowData absolute Tag;
  JSContainer, JSFps: JSValue;
  Args: Array[0..0] Of JSValueConst;
Begin
  Tag := Container.Tag;
  If Assigned(Data) Then
  Begin
    If JS_IsFunction(QJSCtx, Data^.OnRender) Then
    Begin
      JSContainer := JS_NewObject(QJSCtx);
      JSFps := JS_NewObject(QJSCtx);

      JS_SetPropertyStr(QJSCtx, JSFps, 'toString', JS_NewString(QJSCtx, PChar(Container.Fps.toString)));
      JS_SetPropertyStr(QJSCtx, JSContainer, 'Fps', JSFps);

      Args[0] := JSContainer;

      CallJSFunction(Data^.OnRender, Data^.JSThis, 1, @Args);
      JS_FreeValue(QJSCtx, JSContainer);
    End;
  End;
End;

{==================================================================================================}


Procedure JS_KeyPress(Container: TUIContainer; Const Event: TInputPressRelease);
Var
  Tag: PtrInt;
  Data: PJSWindowData absolute Tag;
  JSContainer, JSEvent: JSValue;
  Args: Array[0..1] Of JSValueConst;
  {%H-}KeyEnumStr: String;
Begin
  Tag := Container.Tag;      //  Data := PJSWindowData(Container.Tag);
  If Assigned(Data) Then
  Begin
    If JS_IsFunction(QJSCtx, Data^.OnPress) Then
    Begin
      JSContainer := JS_NewObject(QJSCtx);
      JSEvent := JS_NewObject(QJSCtx);

      JS_SetPropertyStr(QJSCtx, JSEvent, 'EventType', JS_NewInt32(QJSCtx, Integer(Event.EventType)));
      JS_SetPropertyStr(QJSCtx, JSEvent, 'key', JS_NewInt32(QJSCtx, Integer(Event.Key)));

      Args[0] := JSContainer;
      Args[1] := JSEvent;

      CallJSFunction(Data^.OnPress, Data^.JSThis, Length(Args), @Args);
      JS_FreeValue(QJSCtx, JSEvent);
      JS_FreeValue(QJSCtx, JSContainer);
    End;
  End;
End;

{==================================================================================================}

Function Window_Get_Props(ctx: JSContext; this_val: JSValueConst; magic: Integer): JSValue; Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Window_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Case magic Of
    WIN_OnRender: Result := Data^.OnRender;
    WIN_OnPress: Result := Data^.OnPress;
    WIN_Height: Result := JS_NewInt32(ctx, Data^.JSWindow.Height);
    WIN_Width: Result := JS_NewInt32(ctx, Data^.JSWindow.Width);
    Else
      Result := JS_UNDEFINED;
  End;
End;

Function Window_Set_Props(ctx: JSContext; this_val: JSValueConst; val: JSValueConst; magic: Integer): JSValue; Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Window_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Case magic Of
    WIN_OnRender:
    Begin
      { TODO:  Or JS_IsUndefined(val) Or JS_IsNull(val) - Time : 2022/12/10 6:18:59 PM }
      If JS_IsFunction(ctx, val) Then
      Begin
        JS_FreeValue(ctx, Data^.OnRender);
        Data^.OnRender := JS_DupValue(ctx, val);
        Data^.JSWindow.OnRender := @JS_WindowRender;
      End
      Else
      Begin
        JS_ThrowInternalError(ctx, 'Window.OnRender Must be a Function', []);
        Result := JS_EXCEPTION; // Return an Exception so the Engine know that there's an err.
      End;
    End;

    WIN_OnPress:
    Begin
      If JS_IsFunction(ctx, val) Or JS_IsUndefined(val) Or JS_IsNull(val) Then
      Begin
        JS_FreeValue(ctx, Data^.OnPress);
        Data^.OnPress := JS_DupValue(ctx, val);
        Data^.JSWindow.OnPress := @JS_KeyPress;
      End
      Else
      Begin
        JS_ThrowInternalError(ctx, 'Window.OnPress Must be a Function', []);
        Result := JS_EXCEPTION; // Return an Exception so the Engine know that there's an err.
      End;
    End;

    // Set the Height From JS Code
    WIN_Height:
    Begin
      // Check if the Val is Number
      If JS_IsNumber(val) Then
      Begin
        // Set The Window Height
        Data^.JSWindow.Height := JS_VALUE_GET_INT(Val);
      End
      Else
      Begin
        // If not a valid number throwExcption
        JS_ThrowInternalError(ctx, 'Window.Height Must be a Number', []);
        Result := JS_EXCEPTION; // Return an Excp so the Engine know that there's an err.
      End;
    End;
    // Set the Width From JS Code
    WIN_Width:
    Begin
      // Check if the Val is Number
      If JS_IsNumber(val) Then
      Begin
        // Set The Window Width
        Data^.JSWindow.Width := JS_VALUE_GET_INT(Val);
      End
      Else
      Begin
        // If not a valid number throwExcption
        JS_ThrowInternalError(ctx, 'Window.Width Must be a Number', []);
        Result := JS_EXCEPTION; // Return an Excp so the Engine know that there's an err.
      End;
    End;
  End;

  Result := JS_UNDEFINED;
End;

{==================================================================================================}

Function WindowCtor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  Data: PJSWindowData;
  proto, Obj: JSValue;
Begin
  Obj := JS_UNDEFINED;
  Data := js_mallocz(ctx, SizeOf(TJSWindowData));
  If Not Assigned(Data) Then
  Begin
    JS_ThrowInternalError(ctx, 'Can''t Allocate Memory for Window #1', []);
    Exit(JS_EXCEPTION);
  End;
  {
  using new_target to get the prototype is necessary when the
         class is extended.
  }
  proto := JS_GetPropertyStr(ctx, new_target, 'prototype');
  If Not JS_IsException(proto) Then
  Begin
    Obj := JS_NewObjectProtoClass(ctx, proto, Window_Class_id);
    JS_FreeValue(ctx, proto);
    If Not JS_IsException(obj) Then
    Begin
      Data^.JSWindow := nil;

      // TODO: Get argv[1] and check if it's Application then use it if not use nil.
      Data^.JSWindow := TCastleWindow.Create(Application);
      Data^.JSWindow.Container.Tag := PtrInt(Data); // Save Refrence to Data.

      Data^.JSThis := JS_DupValue(ctx, Obj); // To Prevent GC Auto free.
      JS_SetOpaque(Obj, Data); // Save Data to the Object.
      Exit(Obj); // Result.
    End;
  End;
  js_free_rt(QJSRuntime, Data);
  JS_FreeValue(ctx, obj);
  Result := JS_EXCEPTION;
End;


Function JS_WindowOpen(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Window_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Data^.JSWindow.Open;

  Result := JS_UNDEFINED;
End;

Function JS_WindowFree(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  Data: PJSWindowData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Window_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  // TODO : Set All JS CallBacks to JS_UNDEFINED
  JS_SetPropertyStr(ctx, this_val, 'OnRender', JS_UNDEFINED);
  JS_SetPropertyStr(ctx, this_val, 'OnPress', JS_UNDEFINED);
  If Data^.JSWindow <> nil Then
    FreeAndNil(Data^.JSWindow);

  Result := JS_UNDEFINED;
End;

Function object_to_string(ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Const
  ClassName = '[TCastleWindow]';
Begin
  Result := JS_NewStringLen(ctx, PChar(ClassName), strlen(PChar(ClassName)));
End;

Procedure JS_Window_init(ctx: JSContext; m: JSModuleDef); Cdecl;
Var
  Window_proto, Window_class: JSValue;
Begin
  // create the Point class
  JS_NewClassID(@Window_Class_id);

  Window_Class_JS.finalizer := @Window_JS_finalizer; // Class finalizer.
  Window_Class_JS.gc_mark := @Window_JS_GCMark; // Class GC Marker.
  JS_NewClass(JS_GetRuntime(ctx), Window_Class_id, @Window_Class_JS);

  Window_proto := JS_NewObject(ctx);

  Window_proto_funcs := Window_Exports.Create(
    JS_CGETSET_MAGIC_DEF('OnRender', Window_Get_Props, Window_Set_Props, WIN_OnRender),
    JS_CGETSET_MAGIC_DEF('OnPress', Window_Get_Props, Window_Set_Props, WIN_OnPress),
    JS_CGETSET_MAGIC_DEF('Height', Window_Get_Props, Window_Set_Props, WIN_Height),
    JS_CGETSET_MAGIC_DEF('Width', Window_Get_Props, Window_Set_Props, WIN_Width),
    JS_CFUNC_DEF('Open', 0, JS_WindowOpen),
    JS_CFUNC_DEF('Free', 0, JS_WindowFree)
  );

  JS_DefinePropertyValueStr(ctx, Window_proto, 'toString',
      JS_NewCFunction(ctx, @object_to_string, 'toString', 0)
    , JS_PROP_ENUMERABLE Or JS_PROP_CONFIGURABLE);

  JS_SetPropertyFunctionList(ctx, Window_proto, @Window_proto_funcs[0], Length(Window_proto_funcs));
  JS_SetClassProto(ctx, Window_Class_id, Window_proto);

  Window_class := JS_NewCFunction2(ctx, @WindowCtor, 'Window', 0, JS_CFUNC_constructor, 0);
  // set proto.constructor and ctor.prototype
  JS_SetConstructor(ctx, Window_class, Window_proto);

  JS_SetModuleExport(ctx, m, 'Window', Window_class);

End;


End.
