Unit JSTimer;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}


Interface

Uses
  Classes, SysUtils, StrUtils,
  Rtti, RttiUtils, TypInfo,
  QuickJS,
  CastleWindow, CastleControls, CastleLog;

Procedure JS_Timer_init(ctx: JSContext; m: JSModuleDef); Cdecl;

Implementation

Uses
  QJSUtils, QJS_Engine, JSWindow;

Type
  Timer_Exports = Array Of JSCFunctionListEntry;

Var
  Timer_Class_id: JSClassID = 0;
  Timer_Class_JS: JSClassDef = (class_name: 'Timer'; finalizer: nil; gc_mark: nil; call: nil; exotic: nil);
  Timer_proto_funcs: Array Of JSCFunctionListEntry;

Const
  TIMER_INTERVAL = 0;
  TIMER_CADELAYS = 1;


Type
  PTimerData = ^TimerData;

  TimerData = Record
    JSTimer: TCastleTimer;
    OnTick: JSValue;
    JSThis: JSValue;
    Working: Boolean;
  End;


Type
  TEventHandler = Class
    Class Procedure Timer(Sender: TObject);
  End;

Class Procedure TEventHandler.Timer(Sender: TObject);
Var
  tag: PtrInt;
  Data: PTimerData absolute tag;
Begin
  if Sender is TCastleTimer then
  begin
    tag := (Sender As TCastleTimer).Tag;
    If Assigned(Data) Then
    Begin
      CallJSFunction(PTimerData(Data)^.OnTick, PTimerData(Data)^.JSThis, 0, nil); // Call JS Timer OnTick.
    End
    Else
      WritelnLog('Error : OnTick is not set');
  end;
End;

Procedure Timer_JS_finalizer(rt: JSRuntime; val: JSValue); Cdecl;
Var
  Data: PTimerData;
Begin
  Data := JS_GetOpaque(val, Timer_Class_id);
  { Note: 'Data' can be nil in case JS_SetOpaque() was not called }
  If Assigned(Data) Then
  Begin
    WritelnLog('[-] Timer Finalize');
    JS_FreeValueRT(rt, Data^.OnTick);
    JS_FreeValueRT(rt, Data^.JSThis);
    js_free_rt(rt, Data);
  End;
End;

Procedure Timer_JS_GCMark(rt: JSRuntime; val: JSValueConst; mark_func: PJS_MarkFunc); Cdecl;
Var
  Data: PTimerData;
Begin
  Data := JS_GetOpaque(val, Timer_Class_id);
  If Assigned(Data) Then
  Begin
    WritelnLog('[*] Timer GC');
    JS_MarkValue(rt, Data^.JSThis, mark_func);
    JS_MarkValue(rt, Data^.OnTick, mark_func);
  End;
End;

Function TimerCtor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  MyTimer: TCastleTimer;
  Data: PTimerData;
  proto, Obj: JSValue;
Begin
  Obj := JS_UNDEFINED;
  Data := js_mallocz(ctx, SizeOf(TimerData));
  If Not Assigned(Data) Then
  Begin
    JS_ThrowInternalError(ctx, 'Can''t Allocate Memory for Timer #1', []);
    Exit(JS_EXCEPTION);
  End;

  {
  using new_target to get the prototype is necessary when the
         class is extended.
  }
  proto := JS_GetPropertyStr(ctx, new_target, 'prototype');
  If Not JS_IsException(proto) Then
  Begin
    Obj := JS_NewObjectProtoClass(ctx, proto, Timer_Class_id);
    JS_FreeValue(ctx, proto);
    If Not JS_IsException(obj) Then
    Begin
      MyTimer := nil;
      MyTimer := TCastleTimer.Create(Application);
      Data^.JSTimer := MyTimer;
      Data^.Working := False;
      MyTimer.IntervalSeconds := 1.0; // once per second
      MyTimer.OnTimer := TEventHandler(nil).Timer;
      MyTimer.CounteractDelays := True;
      MyTimer.Tag := PtrInt(Data); // Save Refrence to Data.
      Data^.JSThis := JS_DupValue(ctx, Obj); // To Prevent GC Auto free.
      JS_SetOpaque(Obj, Data); // Save Data to the Object.
      Exit(Obj); // Result.
    End;
  End;
  js_free_rt(QJSRuntime, Data);
  JS_FreeValue(ctx, obj);
  Result := JS_EXCEPTION;
End;

Function TimerStart(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  Data: PTimerData;
  WindowsData: PJSWindowData;
  OnTick: JSValue;
Begin
  If argc < 1 Then
  Begin
    JS_ThrowInternalError(ctx, 'Timer Start need one Arg = Window Object', []);
    Exit(JS_EXCEPTION);
  End;

  Data := JS_GetOpaque2(ctx, this_val, Timer_Class_id);
  If Not Assigned(Data) Then
  Begin
    JS_ThrowInternalError(ctx, 'Timer Opaque Data Not Present', []);
    exit(JS_EXCEPTION);
  End;

  OnTick := JS_GetPropertyStr(ctx, this_val, 'OnTick');
  If Not JS_IsFunction(ctx, OnTick) Then
  Begin
    JS_ThrowInternalError(ctx, 'Timer OnTick Function Is Undefined', []);
    Exit(JS_EXCEPTION);
  End;

  If Assigned(Data^.JSTimer) Then
  Begin
    If (Data^.JSTimer Is TCastleTimer) Then
    Begin
      Data^.OnTick := OnTick;
      Data^.Working := True;

      { TODO: Check argc - Time : 2022/12/10 6:31:40 PM }
      WindowsData := JS_GetWindowClassData(ctx, argv[0]);
      If Assigned(WindowsData) And Assigned(WindowsData^.JSWindow) Then
        WindowsData^.JSWindow.Controls.InsertFront(Data^.JSTimer)
      Else
        Exit(JS_EXCEPTION);
    End
    Else
    Begin
      WritelnWarning('Timer is ' + Data^.JSTimer.ClassName);
      JS_ThrowInternalError(ctx, 'Timer Object Is not TCastleTimer ', []);
      Exit(JS_EXCEPTION);
    End;
  End;
  Result := JS_UNDEFINED;
End;

Function TimerFree(ctx: JSContext; {%H-}this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
Var
  Data: PTimerData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Timer_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Data^.Working := False;
  Data^.JSTimer.OnTimer := nil;
  JS_SetPropertyStr(ctx, this_val, 'OnTick', JS_UNDEFINED);
  Result := JS_UNDEFINED;
End;

Function Timer_Props_Get(ctx: JSContext; this_val: JSValueConst; magic: Integer): JSValue; Cdecl;
Var
  Data: PTimerData;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Timer_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Case magic Of
    TIMER_INTERVAL: Result := JS_NewFloat64(ctx, Data^.JSTimer.IntervalSeconds);
    TIMER_CADELAYS: Result := JS_NewBool(ctx, Data^.JSTimer.CounteractDelays);
  End;

End;

Function Timer_Props_Set(ctx: JSContext; this_val: JSValueConst; val: JSValueConst; magic: Integer): JSValue; Cdecl;
Var
  Data: PTimerData;
  Interval: Double;
Begin
  Data := JS_GetOpaque2(ctx, this_val, Timer_Class_id);
  If Not Assigned(Data) Then
    exit(JS_EXCEPTION);

  Case magic Of
    TIMER_INTERVAL:
    Begin
      If JS_IsNumber(val) Then
      Begin
        If JS_ToFloat64(ctx, @Interval, val) = 0 Then
        Begin
          Data^.JSTimer.IntervalSeconds := Interval;
        End
        Else
          Exit(JS_EXCEPTION);
      End
      Else
      Begin
        JS_ThrowInternalError(ctx, '[' + {$I %CURRENTROUTINE%} + '] : ' + 'Timer IntervalSeconds Should be a Number ', []);
        Exit(JS_EXCEPTION);
      End;
    End;
    TIMER_CADELAYS:
    Begin
      If JS_IsBool(val) Then
      Begin
        Data^.JSTimer.CounteractDelays := JS_VALUE_GET_BOOL(val);
      End
      Else
      Begin
        JS_ThrowInternalError(ctx, 'Timer CounteractDelays Should be a Boolean', []);
        Exit(JS_EXCEPTION);
      End;
    End;
  End;

  Result := JS_UNDEFINED;
End;

Function object_to_string(ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; Cdecl;
const
  ClassName = '[TCastleTimer]';
Begin
  Result := JS_NewStringLen(ctx, PChar(ClassName), strlen(PChar(ClassName)));
End;

Procedure JS_Timer_init(ctx: JSContext; m: JSModuleDef); Cdecl;
Var
  Timer_proto, Timer_class: JSValue;
Begin
  // create the Point class
  JS_NewClassID(@Timer_Class_id);

  Timer_Class_JS.finalizer := @Timer_JS_finalizer; // Class finalizer.
  Timer_Class_JS.gc_mark := @Timer_JS_GCMark; // Class GC Marker.
  JS_NewClass(JS_GetRuntime(ctx), Timer_Class_id, @Timer_Class_JS);

  Timer_proto := JS_NewObject(ctx);

  Timer_proto_funcs := Timer_Exports.Create(
    JS_CGETSET_MAGIC_DEF('IntervalSeconds', Timer_Props_Get, Timer_Props_Set, TIMER_INTERVAL),
    JS_CGETSET_MAGIC_DEF('CounteractDelays', Timer_Props_Get, Timer_Props_Set, TIMER_CADELAYS),
    JS_CFUNC_DEF('Start', 0, TimerStart),
    JS_CFUNC_DEF('Free', 0, TimerFree)
  );

  JS_DefinePropertyValueStr(ctx, Timer_proto, 'toString',
    JS_NewCFunction(ctx, @object_to_string, 'toString', 0)
    , JS_PROP_ENUMERABLE Or JS_PROP_CONFIGURABLE);

  JS_SetPropertyFunctionList(ctx, Timer_proto, @Timer_proto_funcs[0], Length(Timer_proto_funcs));
  JS_SetClassProto(ctx, Timer_Class_id, Timer_proto);

  Timer_class := JS_NewCFunction2(ctx, @TimerCtor, 'Timer', 0, JS_CFUNC_constructor, 0);
  // set proto.constructor and ctor.prototype
  JS_SetConstructor(ctx, Timer_class, Timer_proto);

  JS_SetModuleExport(ctx, m, 'Timer', Timer_class);

End;


End.
