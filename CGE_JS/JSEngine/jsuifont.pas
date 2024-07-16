Unit JSUIFont;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}


Interface

Uses
  Classes, SysUtils,
  QuickJS,
  CastleFonts, CastleControls, CastleLog, CastleVectors;

Procedure JS_UIFont_init(ctx: JSContext; m: JSModuleDef); Cdecl;

Implementation

Uses
  QJSUtils, QJS_Engine;

Type
  UIFont_Exports = Array Of JSCFunctionListEntry;

Var
  UIFont_proto_funcs: Array Of JSCFunctionListEntry;

Function UIFont_Get_Props(ctx: JSContext; this_val: JSValueConst; magic: Integer): JSValue; Cdecl;
Begin
  Case magic Of
    1: ;
    Else
      Result := JS_UNDEFINED;
  End;
End;

Function Application_Set_Props(ctx: JSContext; this_val: JSValueConst; val: JSValueConst; magic: Integer): JSValue; Cdecl;
Begin
  Case magic Of
    1: ;
  End;
  Result := JS_UNDEFINED;
End;

Function object_to_string(ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
const
  ClassName = '[UIFont]';
Begin
  Result := JS_NewStringLen(ctx, PChar(ClassName), strlen(PChar(ClassName)));
End;

Function JS_UIFont_Print(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  UIString: String;
  X, Y: Integer;
Begin
  UIString := '';
  X := 0;
  Y := 0;
  Case argc Of
    4:
    Begin
      If JS_IsNumber(argv[0]) And JS_IsNumber(argv[1]) Then
      Begin
        If JS_IsString(argv[3]) Then
        Begin
          UIString := JS_ObjToStr(ctx, argv[3]);
          X := JS_VALUE_GET_INT(argv[0]);
          Y := JS_VALUE_GET_INT(argv[1]);
          UIFont.Print(X, Y, Vector4(0.4, 0.3, 0.1, 1), UIString);
        End;
      End;

    End
    Else

  End;
  Result := JS_UNDEFINED;
End;

Procedure JS_UIFont_init(ctx: JSContext; m: JSModuleDef); Cdecl;
Var
  UIFont_proto: JSValue;
Begin
  UIFont_proto := JS_NewObject(ctx);

  UIFont_proto_funcs := UIFont_Exports.Create(JS_CFUNC_DEF('Print', 0, JS_UIFont_Print));

  JS_DefinePropertyValueStr(ctx, UIFont_proto, 'toString', JS_NewCFunction(ctx, @object_to_string, 'toString', 0),
    JS_PROP_ENUMERABLE Or JS_PROP_CONFIGURABLE);

  JS_SetPropertyFunctionList(ctx, UIFont_proto, @UIFont_proto_funcs[0], Length(UIFont_proto_funcs));

  JS_SetModuleExport(ctx, m, 'UIFont', UIFont_proto);
End;

End.
