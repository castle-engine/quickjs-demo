Unit QJSUtils;

{$mode Delphi}{$H+}

Interface

Uses
  Classes, SysUtils,
  QuickJS,
  CastleLog, CastleDownload;

Function JS_ObjToStr(ctx: JSContext; val: JSValueConst): String;

Procedure js_dump_error(ctx: JSContext); Cdecl;
Procedure js_dump_obj(ctx: JSContext; val: JSValueConst); Cdecl;


Function eval_buf(ctx: JSContext; Buf: PChar; buf_len: Integer; filename: PChar; is_main: Boolean; eval_flags: Integer = -1): JSValue;
Function LoadScript(ScriptPath: String): String;


function CallJSFunction(MyFunction, ThisObj: JSValue; Argc: Integer; Argv: PJSValueConstArr) : JSValue;
Function CGE_JS_MODULE_LOADER(ctx: JSContext; module_name: PChar; opaque: pointer): JSModuleDef; Cdecl;

Implementation

Uses
  QJS_Engine;

function CallJSFunction(MyFunction, ThisObj: JSValue; Argc: Integer; Argv: PJSValueConstArr) : JSValue;
Var
  JSErr, JSResult, JSFunc, This: JSValue;
Begin
  Result := JS_UNDEFINED; { TODO: Do we need to return a result ?!! }

  If Not Assigned(QJSCtx) Then
  Begin
    WritelnLog('QuickJS', 'QJS Context Is NULL');
    exit;
  End;

  If JS_IsFunction(QJSCtx, MyFunction) Then
  Begin
    If JS_IsObject(ThisObj) Then
      this := ThisObj
    Else
      this := JS_UNDEFINED;

    JSFunc := JS_DupValue(QJSCtx, MyFunction);
    JSResult := JS_Call(QJSCtx, JSFunc, this, Argc, Argv);
    JS_FreeValue(QJSCtx, JSFunc);

    If JS_IsException(JSResult) Then
    Begin
      WritelnLog('QuickJS', '[X] QJS Call Error');
      js_dump_error(QJSCtx);
    End;
    JS_FreeValue(QJSCtx, JSResult);
  End
  Else
  Begin
    WritelnLog('QuickJS', 'CallJSFunction -> JS_IsFunction Error');
    JSErr := JS_GetException(QJSCtx);
    If JS_IsException(JSErr) Then
    Begin
      WritelnLog('QuickJS', '[X] QJS Error');
      js_dump_error(QJSCtx);
    End;
    JS_FreeValue(QJSCtx, JSErr);
  End;
End;

Function JS_ObjToStr(ctx: JSContext; val: JSValueConst): String;
Var
  Str: PChar;
Begin
  Result := '';
  Str := JS_ToCString(ctx, val);
  If Assigned(str) Then
  Begin
    Result := String(str);
    JS_FreeCString(ctx, str);
  End;
End;

Procedure js_dump_obj(ctx: JSContext; val: JSValueConst); Cdecl;
Var
  str: PChar;
Begin
  str := JS_ToCString(ctx, val);
  If Assigned(str) Then
  Begin
    WritelnLog(str);
    JS_FreeCString(ctx, str);
  End
  Else
    WritelnLog('QuickJS', '[QJS] Unknown Exception');
End;

Procedure js_dump_error(ctx: JSContext); Cdecl;
Var
  val, exception_val: JSValue;
  is_error: JS_BOOL;
Begin
  exception_val := JS_GetException(ctx);
  is_error := JS_IsError(ctx, exception_val);
  js_dump_obj(ctx, exception_val);
  If is_error Then
  Begin
    val := JS_GetPropertyStr(ctx, exception_val, 'stack');
    If Not JS_IsUndefined(val) Then
      js_dump_obj(ctx, val);
    JS_FreeValue(ctx, val);
  End;
  JS_FreeValue(ctx, exception_val);
End;


Function eval_buf(ctx: JSContext; Buf: PChar; buf_len: Integer; filename: PChar; is_main: Boolean; eval_flags: Integer = -1): JSValue;
Var
  ret: JSValue;
Begin
  If eval_flags = -1 Then
  Begin
    If JS_DetectModule(Buf, buf_len) Then
      eval_flags := JS_EVAL_TYPE_MODULE
    Else
      eval_flags := JS_EVAL_TYPE_GLOBAL;
  End;

  If (eval_flags And JS_EVAL_TYPE_MASK) = JS_EVAL_TYPE_MODULE Then
  Begin
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags Or JS_EVAL_FLAG_COMPILE_ONLY);
    If Not JS_IsException(ret) Then
    Begin
      js_module_set_import_meta(ctx, ret, True, is_main);
      ret := JS_EvalFunction(ctx, ret);
    End;
  End
  Else
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags);

  If JS_IsException(ret) Then
  Begin
    js_dump_error(ctx);
    Result := JS_NULL;
  End
  Else
    Result := ret;
End;

Function LoadScript(ScriptPath: String): String;
Var
  FileStream: TStream;
  JSCode: TStringList;
Begin
  FileStream := Download('castle-data:/' + ScriptPath);
  JSCode := TStringList.Create;
  JSCode.LoadFromStream(FileStream);
  Result := JSCode.Text;
  FreeAndNil(FileStream);
  FreeAndNil(JSCode);
End;

Function CGE_JS_MODULE_LOADER(ctx: JSContext; module_name: PChar; opaque: pointer): JSModuleDef; Cdecl;
Var
  m: JSModuleDef;
  ScriptName, Ext: String;
  ScriptContent: String;
  func_val: JSValue;
Begin
  Result := nil; // Init Module value.
  ScriptName := String(module_name);
  Begin
    ScriptContent := LoadScript(ScriptName);
    If Not ScriptContent.IsEmpty Then
    Begin
      func_val := JS_Eval(ctx, PChar(ScriptContent), strlen(PChar(ScriptContent)), module_name, JS_EVAL_TYPE_MODULE Or JS_EVAL_FLAG_COMPILE_ONLY);
      ScriptContent := '';
      If Not JS_IsException(func_val) Then
      Begin
        js_module_set_import_meta(ctx, func_val, True, False);
        m := JS_VALUE_GET_PTR(func_val);
        JS_FreeValue(ctx, func_val);
        Result := m;
      End;
    End;
  End;
End;

End.
