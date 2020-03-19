unit QJS_Engine;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}

interface

uses
  QuickJS,SnakeUnit;




procedure InitQJS(Script : PChar);
procedure UninitJSEngine();

var
  // Global Runtime & Context
  rt  : JSRuntime;
  QJSCtx : JSContext;

  OnUpdate,JSGlobal : JSValue; // Global Obj for the js update function.

implementation

function player_position(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  Player : JSValue;
begin
  Player := JS_NewObject(ctx);
  JS_DefinePropertyValueStr(ctx,Player,'x',JS_NewInt32(ctx,Snake.x),JS_PROP_CONFIGURABLE);
  JS_DefinePropertyValueStr(ctx,Player,'y',JS_NewInt32(ctx,Snake.y),JS_PROP_CONFIGURABLE);
  Result := Player;
end;

function goal_position(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  JSRabbit : JSValue;
begin
  JSRabbit := JS_NewObject(ctx);
  JS_DefinePropertyValueStr(ctx,JSRabbit,'x',JS_NewInt32(ctx,Rabbit.x),JS_PROP_CONFIGURABLE);
  JS_DefinePropertyValueStr(ctx,JSRabbit,'y',JS_NewInt32(ctx,Rabbit.y),JS_PROP_CONFIGURABLE);
  Result := JSRabbit;
end;

function player_move(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  x,y : Int32;
begin
  if argc <> 2 then
  begin
    JS_ThrowInternalError(ctx,'move takes 2 arg',[]);
    exit(JS_EXCEPTION);
  end;
  if JS_IsNumber(argv[0]) then
     JS_ToInt32(ctx,@x,argv[0])
  else
  begin
   JS_ThrowInternalError(ctx,'move First arg must be Number !!!',[]);
   Exit(JS_EXCEPTION);
  end;
  if JS_IsNumber(argv[1]) then
     JS_ToInt32(ctx,@y,argv[1])
  else
  begin
   JS_ThrowInternalError(ctx,'move secod arg must be Number !!!',[]);
   Exit(JS_EXCEPTION);
  end;
  Snake.setDirection(x,y);
  Result := JS_UNDEFINED;
end;

procedure RegisterNativeFunctions(ctx : JSContext); cdecl;
var
  global : JSValue;
begin
  // Register update function
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx,global,'player_position',JS_NewCFunction(ctx, @player_position, 'player_position', 0));
  JS_FreeValue(ctx, global);

  // Register goal_position .
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx,global,'goal_position',JS_NewCFunction(ctx, @goal_position, 'goal_position', 0));
  JS_FreeValue(ctx, global);

  // Register move
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx,global,'move',JS_NewCFunction(ctx, @player_move, 'move', 0));
  JS_FreeValue(ctx, global);
end;


function eval_buf(ctx : JSContext; Buf : PChar; buf_len : Integer; filename : PChar; eval_flags : Integer): Integer;
var
  val : JSValue;
begin
  val := JS_Eval(ctx, buf, buf_len, filename, eval_flags);
  if JS_IsException(val) then
  begin
    js_std_dump_error(ctx);
    Result := -1;
  end
  else
    Result := 0;
    JS_FreeValue(ctx, val);
end;

function eval_file(ctx : JSContext; filename : PChar; eval_flags : Integer): Integer;
var
  buf_len : size_t;
  Buf : Pointer;
begin
  buf := js_load_file(ctx, @buf_len, filename);
  if not Assigned(buf) then
  begin
    Writeln('Error While Loading : ',filename);
    exit(1);
  end;
  Result := eval_buf(ctx, buf, buf_len, filename, eval_flags);
  js_free(ctx, buf);
end;

// Custom Imp of log.
function logme(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  i : Integer;
  str : PChar;
begin
  for i := 0 to Pred(argc) do
  begin
     if i <> 0 then
       write(' ');
     str := JS_ToCString(ctx, argv[i]);
     if not Assigned(str) then
        exit(JS_EXCEPTION);
     Write(str);
     JS_FreeCString(ctx, str);
  end;
  Writeln();
  Result := JS_UNDEFINED;
end;

procedure InitQJS(Script : PChar);
const
  std_hepler : PChar =
    'import * as std from ''std'';'#10+
    'import * as os from ''os'';'#10+
    'globalThis.std = std;'#10+
    'globalThis.os = os;';
begin
  WriteLn('Init QJS Engine');
  rt := JS_NewRuntime;
  if Assigned(rt) then
  begin
    QJSCtx := JS_NewContext(rt);
    if Assigned(rt) then
    begin
      Initialize(OnUpdate);

      // ES6 Module loader.
      JS_SetModuleLoaderFunc(rt, nil, @js_module_loader, nil);

      js_std_add_helpers(QJSCtx,argc,argv);
      js_init_module_std(QJSCtx, 'std');
      js_init_module_os(QJSCtx, 'os');

      JSGlobal := JS_GetGlobalObject(QJSCtx);
      // Register native functions with global object .
      RegisterNativeFunctions(QJSCtx);

      eval_buf(QJSCtx, std_hepler, strlen(std_hepler), '<global_helper>', JS_EVAL_TYPE_MODULE);
      // Execute our JS File.
      eval_file(QJSCtx,Script,JS_EVAL_TYPE_GLOBAL {or JS_EVAL_TYPE_MODULE});
      js_std_loop(QJSCtx);

      OnUpdate := JS_GetPropertyStr(QJSCtx,JSGlobal,'update');
      if not JS_IsFunction(QJSCtx,OnUpdate) then
      begin
        WriteLn('update function is not defined !!!');
        Halt();
      end;

    end;
  end;
end;

procedure UninitJSEngine();
begin
  WriteLn('Uninit QJS Engine');

  JS_FreeValue(QJSCtx,OnUpdate);
  JS_FreeValue(QJSCtx,JSGlobal);

  js_std_free_handlers(rt);
  JS_FreeContext(QJSCtx);
  JS_FreeRuntime(rt);
end;

end.


