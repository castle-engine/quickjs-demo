Unit JSApplication;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}


Interface

Uses
  Classes, SysUtils,
  QuickJS,
  CastleWindow, CastleControls, CastleLog;

Procedure JS_Application_init(ctx: JSContext; m: JSModuleDef); Cdecl;
Function JS_ObjIsApplication(ctx: JSContext; this_val: JSValueConst): JS_BOOL; Cdecl;

Type
  PTApplicationData = ^TJSApplication;

  TJSApplication = Record
    OnInitialize: JSValue;
  End;

Var
  CGEJSApplication: TJSApplication;

Implementation

Uses
  QJSUtils, QJS_Engine, JSWindow;

Const
  APP_OnInitialize = 0;
  APP_MainWindow = 1;

Type
  Application_Exports = Array Of JSCFunctionListEntry;

Var
  Application_proto_funcs: Array Of JSCFunctionListEntry;


Function JS_ObjIsApplication(ctx: JSContext; this_val: JSValueConst): JS_BOOL; Cdecl;
Begin
  Result := JS_ObjToStr(ctx, this_val) = '[TCastleApplication]';
End;

{==================================================================================================}

Procedure JSAppOnInitialize();
Begin
  WriteLn();
  WritelnLog('>> JSAppOnInitialize <<');
  WriteLn();

  If JS_IsFunction(QJSCtx, CGEJSApplication.OnInitialize) Or JS_IsUndefined(CGEJSApplication.OnInitialize) Or JS_IsNull(CGEJSApplication.OnInitialize) Then
  Begin
    CallJSFunction(CGEJSApplication.OnInitialize, JS_UNDEFINED, 0 , nil);
  End
  Else
  Begin
    WriteLn('[JS Engine] Application.OnInitialize is UNDEFINED'); // TODO: return a prober error
  End;
End;

{==================================================================================================}

Function Application_Get_Props(ctx: JSContext; this_val: JSValueConst; magic: Integer): JSValue; Cdecl;
Begin
  Case magic Of
    APP_OnInitialize:
    Begin
      Result := CGEJSApplication.OnInitialize;
    End;
    APP_MainWindow:
    Begin
      // TODO : Create Function in Utils to Create Obj from Base Class.
    End;
    Else
      Result := JS_UNDEFINED;
  End;
End;

Function Application_Set_Props(ctx: JSContext; this_val: JSValueConst; val: JSValueConst; magic: Integer): JSValue; Cdecl;
Var
  Data: PJSWindowData;
Begin
  Case magic Of
    APP_OnInitialize:
    Begin
      If JS_IsFunction(ctx, val) Or JS_IsUndefined(val) Or JS_IsNull(val) Then
      Begin
        JS_FreeValue(ctx, CGEJSApplication.OnInitialize);
        CGEJSApplication.OnInitialize := JS_DupValue(ctx, val);
        Application.OnInitialize := @JSAppOnInitialize;
      End;
    End;
    APP_MainWindow:
    Begin
      // Check if val is Window Object
      Data := JS_GetWindowClassData(ctx, val);
      If Assigned(Data) Then
      Begin
        // Check if the Window is valid
        If Assigned(Data^.JSWindow) Then
        Begin
          // Set The MainWindow with the window class
          Application.MainWindow := Data^.JSWindow;
        End
        Else
        Begin
          JS_ThrowInternalError(ctx, 'Error : Window class is undefined', []);
          Exit(JS_EXCEPTION);
        End;
      End
      Else
        Exit(JS_EXCEPTION);
    End;
  End;
  Result := JS_UNDEFINED;
End;

Function object_to_string(ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
const
  ClassName = '[TCastleApplication]';
Begin
  Result := JS_NewStringLen(ctx, PChar(ClassName), strlen(PChar(ClassName)));
End;

Function JS_ApplicationRun(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Begin
  Application.Run;
  Result := JS_UNDEFINED;
End;

Procedure JS_Application_init(ctx: JSContext; m: JSModuleDef); Cdecl;
Var
  Application_proto: JSValue;
Begin
  CGEJSApplication.OnInitialize := JS_UNDEFINED;

  Application_proto := JS_NewObject(ctx);

  Application_proto_funcs := Application_Exports.Create(
    JS_CGETSET_MAGIC_DEF('OnInitialize', Application_Get_Props, Application_Set_Props, APP_OnInitialize),
    JS_CGETSET_MAGIC_DEF('MainWindow', Application_Get_Props, Application_Set_Props, APP_MainWindow),
    JS_CFUNC_DEF('Run', 0, JS_ApplicationRun),
    JS_PROP_STRING_DEF('name', 'App', JS_PROP_CONFIGURABLE)
  );

  JS_DefinePropertyValueStr(ctx, Application_proto, 'toString',
    JS_NewCFunction(ctx, @object_to_string, 'toString', 0),JS_PROP_ENUMERABLE Or JS_PROP_CONFIGURABLE);

  JS_SetPropertyFunctionList(ctx, Application_proto, @Application_proto_funcs[0], Length(Application_proto_funcs));

  JS_SetModuleExport(ctx, m, 'Application', Application_proto);
End;

End.
