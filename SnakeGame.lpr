program SnakeGame;
uses
  CastleWindow, SnakeGameMain, QJS_Engine,SnakeUnit, sysutils;

begin
  Application.MainWindow.OpenAndRun;

  {don't forget to free everything that is not freed automatically}
  UnInitJSEngine();
  FreeAndNil(SnakeImage);
  FreeAndNil(SnakeFlipImage);
end.
