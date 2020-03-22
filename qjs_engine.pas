unit QJS_Engine;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}

interface

uses
  QuickJS, SnakeUnit, CastleLog, CastleDownload, Classes, sysutils;

procedure InitQJS(Script: PChar);
procedure UnInitJSEngine();
procedure js_dump_error(ctx: JSContext); cdecl;

var
  // Global Runtime & Context
  rt: JSRuntime;
  QJSCtx: JSContext;

  OnUpdate, JSGlobal: JSValueConst; // Global Obj for the js update function.

implementation


procedure js_dump_obj(ctx: JSContext; val: JSValueConst); cdecl;
var
  str : PChar;
begin
  str := JS_ToCString(ctx, val);
  if Assigned(str) then
  begin
    WritelnLog(str);
    JS_FreeCString(ctx, str);
  end
  else
   WritelnLog('[X] QJS Exception');
end;

procedure js_dump_error(ctx: JSContext); cdecl;
var
  val, exception_val :JSValue;
  is_error : JS_BOOL;
begin
  exception_val := JS_GetException(ctx);
  is_error := JS_IsError(ctx, exception_val);
  js_dump_obj(ctx, exception_val);
  if is_error then
  begin
    val := JS_GetPropertyStr(ctx, exception_val, 'stack');
    if not JS_IsUndefined(val) then
      js_dump_obj(ctx, val);
    JS_FreeValue(ctx, val);
  end;
  JS_FreeValue(ctx, exception_val);
end;


function player_position(ctx: JSContext; {%H-}this_val: JSValueConst;
  argc: integer; argv: PJSValueConstArr): JSValue; cdecl;
var
  Player: JSValue;
begin
  Player := JS_NewObject(ctx);
  JS_DefinePropertyValueStr(ctx, Player, 'x', JS_NewInt32(ctx, Snake.x),JS_PROP_CONFIGURABLE);
  JS_DefinePropertyValueStr(ctx, Player, 'y', JS_NewInt32(ctx, Snake.y),JS_PROP_CONFIGURABLE);
  Result := Player;
end;

function goal_position(ctx: JSContext; {%H-}this_val: JSValueConst;
  argc: integer; argv: PJSValueConstArr): JSValue; cdecl;
var
  JSRabbit: JSValue;
begin
  JSRabbit := JS_NewObject(ctx);
  JS_DefinePropertyValueStr(ctx, JSRabbit, 'x', JS_NewInt32(ctx, Rabbit.x),JS_PROP_CONFIGURABLE);
  JS_DefinePropertyValueStr(ctx, JSRabbit, 'y', JS_NewInt32(ctx, Rabbit.y),JS_PROP_CONFIGURABLE);
  Result := JSRabbit;
end;

function player_move(ctx: JSContext; {%H-}this_val: JSValueConst;
  argc: integer; argv: PJSValueConstArr): JSValue; cdecl;
var
  x, y: Int32;
begin
  x := 0;
  y := x;
  if argc <> 2 then
  begin
    JS_ThrowInternalError(ctx, 'move takes 2 arg', []);
    exit(JS_EXCEPTION);
  end;
  if JS_IsNumber(argv[0]) then
    JS_ToInt32(ctx, @x, argv[0])
  else
  begin
    JS_ThrowInternalError(ctx, 'move First arg must be Number !!!', []);
    Exit(JS_EXCEPTION);
  end;
  if JS_IsNumber(argv[1]) then
    JS_ToInt32(ctx, @y, argv[1])
  else
  begin
    JS_ThrowInternalError(ctx, 'move second arg must be Number !!!', []);
    Exit(JS_EXCEPTION);
  end;
  Snake.setDirection(x, y);
  Result := JS_UNDEFINED;
end;

procedure RegisterNativeFunctions(ctx: JSContext); cdecl;
begin
  // Register update function
  JS_SetPropertyStr(ctx, JSGlobal, 'player_position', JS_NewCFunction(ctx,
    @player_position, 'player_position', 0));
  // Register goal_position .
  JS_SetPropertyStr(ctx, JSGlobal, 'goal_position', JS_NewCFunction(ctx,
    @goal_position, 'goal_position', 0));
  // Register move
  JS_SetPropertyStr(ctx, JSGlobal, 'move', JS_NewCFunction(ctx, @player_move, 'move', 0));
end;


function eval_buf(ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; eval_flags: integer): boolean;
var
  val: JSValue;
begin
  val := JS_Eval(ctx, buf, buf_len, filename, eval_flags);
  if JS_IsException(val) then
  begin
    js_dump_error(ctx);
    //js_dump_error(ctx,val);
    Result := False;
  end
  else
    Result := True;
  JS_FreeValue(ctx, val);
end;

function eval_file(ctx: JSContext; filename: PChar; eval_flags: integer): boolean;
var
  buf_len: size_t;
  Buf: Pointer;
begin
  buf := js_load_file(ctx, @buf_len, filename);
  if not Assigned(buf) then
  begin
    WritelnLog('Error While Loading : ' + filename);
    exit(False);
  end;
  Result := eval_buf(ctx, buf, buf_len, filename, eval_flags);
  js_free(ctx, buf);
end;

procedure InitQJS(Script: PChar);
var
  FileStream: TStream;
  JSCode : TStringList;
  MyCode : String;
begin
 FileStream := Download('castle-data:/movement.js');
 JSCode := TStringList.Create;
 JSCode.LoadFromStream(FileStream);
 MyCode := JSCode.Text;
 FreeAndNil(FileStream);
 FreeAndNil(JSCode);

  WritelnLog('Init QJS Engine');
  rt := JS_NewRuntime;
  if Assigned(rt) then
  begin
    QJSCtx := JS_NewContext(rt);
    if Assigned(rt) then
    begin
      Initialize(OnUpdate);

      // ES6 Module loader.
      JS_SetModuleLoaderFunc(rt, nil, @js_module_loader, nil);

      js_std_add_helpers(QJSCtx, argc, argv);

      JSGlobal := JS_GetGlobalObject(QJSCtx);
      // Register native functions with global object .
      RegisterNativeFunctions(QJSCtx);

      // Execute our JS File.

      WritelnLog('Will Run JS');

      eval_buf(QJSCtx, PChar(MyCode), strlen(PChar(MyCode)), Script,
        JS_EVAL_TYPE_GLOBAL);

      WritelnLog('JS Is Running');
      //if not eval_file(QJSCtx, Script, JS_EVAL_TYPE_GLOBAL) then
        //Halt;

      js_std_loop(QJSCtx);

      OnUpdate := JS_GetPropertyStr(QJSCtx, JSGlobal, 'update');
      if not JS_IsFunction(QJSCtx, OnUpdate) then
      begin
        WritelnLog('update function is not defined !!!');
        Halt();
      end;

    end;
  end;
end;

procedure UnInitJSEngine();
begin
  WritelnLog('Uninit QJS Engine');

  JS_FreeValue(QJSCtx, OnUpdate);
  JS_FreeValue(QJSCtx, JSGlobal);

  js_std_free_handlers(rt);
  JS_FreeContext(QJSCtx);
  JS_FreeRuntime(rt);
end;

end.
