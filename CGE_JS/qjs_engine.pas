unit QJS_Engine;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}

interface

uses
  // QuickJS.
  QuickJS, QJSUtils,
  // CGE Units.
  CastleApplicationProperties, CastleLog,
  // System utils.
  Classes, sysutils,
  // JS Proxies.
  JSApplication,
  JSWindow,
  JSTimer,
  JSUIFont;

procedure InitQJS(MainEntryScript: PChar);
procedure UnInitJSEngine();

var
  // JS Global Runtime & Context
  QJSRuntime : JSRuntime;
  QJSCtx     : JSContext;
  JSGlobal   : JSValueConst; // Global Obj for the js Start Update function.

implementation

type
  TCGE_Exports = Array of JSCFunctionListEntry;
var
  CastleEngineExports : TCGE_Exports;

function JS_CastleEngine_init(ctx : JSContext; m : JSModuleDef): Integer; cdecl;
begin
  JS_Application_init(ctx,m);
  JS_Window_init(ctx,m);

  JS_Timer_init(ctx,m);
  JS_UIFont_init(ctx,m);

  Result := 0;
end;

procedure InitQJS(MainEntryScript: PChar);
var
  MainCode : String;
  CGE : JSModuleDef;
begin
  WritelnLog('QuickJS', 'Init QJS Engine');
  QJSRuntime := JS_NewRuntime;
  if Assigned(QJSRuntime) then
  begin
    QJSCtx := JS_NewContext(QJSRuntime);
    if Assigned(QJSRuntime) then
    begin

      // ES6 Module loader.
      JS_SetModuleLoaderFunc(QJSRuntime, nil, @CGE_JS_MODULE_LOADER, nil);

      js_std_add_helpers(QJSCtx, argc-1, @argv[1]);

      // Big Integers.
      JS_AddIntrinsicBigFloat(QJSCtx);
      JS_AddIntrinsicBigDecimal(QJSCtx);
      JS_EnableBignumExt(QJSCtx, True);

      // enable operator overloading.
      JS_AddIntrinsicOperators(QJSCtx);


      JSGlobal := JS_GetGlobalObject(QJSCtx);

      // Register CGE Module.
      CGE := JS_NewCModule(QJSCtx, 'CastleEngine', @JS_CastleEngine_init);
      // Export Names
      CastleEngineExports := TCGE_Exports.Create(
         JS_PROP_UNDEFINED_DEF('Application',JS_PROP_CONFIGURABLE),
         JS_PROP_UNDEFINED_DEF('Window',JS_PROP_CONFIGURABLE),
         JS_PROP_UNDEFINED_DEF('UIFont',JS_PROP_CONFIGURABLE),
         JS_PROP_UNDEFINED_DEF('Timer',JS_PROP_CONFIGURABLE)
      );
      JS_AddModuleExportList(QJSCtx, CGE, @CastleEngineExports[0], Length(CastleEngineExports));

      MainCode := LoadScript(MainEntryScript);
      // Execute our JS File.
      eval_buf(QJSCtx, PChar(MainCode), strlen(PChar(MainCode)), MainEntryScript, False);

      MainCode := '';
      js_std_loop(QJSCtx);

    end;
  end;
end;

procedure UnInitJSEngine();
begin
  WritelnLog('QuickJS','JS Engine Finalization');

  JS_FreeValue(QJSCtx, JSGlobal);
  JS_FreeValue(QJSCtx,CGEJSApplication.OnInitialize);

  js_std_free_handlers(QJSRuntime);

  JS_FreeContext(QJSCtx);
  JS_FreeRuntime(QJSRuntime);
  QJSCtx := nil;
  QJSRuntime := nil;
end;

initialization
  randomize;
  InitializeLog;
  ApplicationProperties.LimitFPS := 0;
  ApplicationProperties._FileAccessSafe := true;
  InitQJS('main.js');
  ApplicationProperties._FileAccessSafe := False;

finalization
  UnInitJSEngine();

end.
