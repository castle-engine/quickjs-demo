unit SnakeGameMain;

interface



uses
  CastleWindow,CastleApplicationProperties,
  CastleKeysMouse, CastleGLImages, CastleGLUtils,
  CastleVectors, CastleControls, CastleLog,
  SysUtils, Math,
  SnakeUnit, QJS_Engine, QuickJS, QJSUtils;



{ scale of the source sprites and their scaled version size at the screen }
const
  SourceScale = 16;
  DestinationScale = SourceScale*2;

var { basic CastleWindow }
    //Window: TCastleWindowBase;
    { All 16 sprites in one image }
    SnakeImage, SnakeFlipImage: TDrawableImage;
    {create some cheap animation effect}
    flipimage: boolean;

implementation

{ this procedure is called each render }
Procedure WindowRender(Container: TUIContainer);
var i,ix,iy: integer;
    currentSnakeImage: TDrawableImage;
begin
  //UIFont.Print(0,0,Vector4(0.4,0.3,0.1,1),Format('FPS: %s', [Container.Fps.ToString]));

  //draw grassland
  for ix := 0 to maxx do
    for iy := 0 to maxy do
      SnakeFlipImage.Draw(ix*DestinationScale,iy*DestinationScale,DestinationScale,DestinationScale,
                      3*SourceScale,0,SourceScale,SourceScale);
  {we use draw(screenx,screeny,screenwidth,screenheight, sourcex,sourcey,soucrewidth,sourceheight)
   version of TGLImage.draw procedure which allows working with spritesheets}

  //show game score
  If not gameover then begin
    if score<=bestScore then
      UIFont.Print(0,0,Vector4(0.4,0.3,0.1,1),'Score: '+inttostr(score)+' best: '+inttostr(bestscore))
    else
      UIFont.Print(0,0,Vector4(1,0.8,0,1),'SCORE: '+inttostr(score)+'(best!)');
  end;
  {We use UIFont defined in CastleControls unit as a "basic" font}

  {Show music CC-BY-SA credit :)}
  UIFont.Print(0,Window.Height-18,Vector4(0,0.6,0.2,1),LicenseString);

  //draw rabbit
  SnakeImage.draw(rabbit.x*DestinationScale,rabbit.y*DestinationScale,DestinationScale,DestinationScale,
                  2*SourceScale,0,SourceScale,SourceScale);

  //draw snake
  {flipping image once per 300ms gives some cheap animation to the snake.}
  if flipImage then currentSnakeImage:=snakeImage else currentSnakeImage := SnakeFlipImage;
  for i := 0 to snake.Tail.Count-1 do with snake.Tail[i] do begin
    case TailKind of
      tkHead    : currentSnakeImage.draw(x*DestinationScale,y*DestinationScale,DestinationScale,DestinationScale,
                     Direction*SourceScale,3*SourceScale,SourceScale,SourceScale);
      tkTail    : currentSnakeImage.draw(x*DestinationScale,y*DestinationScale,DestinationScale,DestinationScale,
                     Direction*SourceScale,2*SourceScale,SourceScale,SourceScale);
      tkTurn    : currentSnakeImage.draw(x*DestinationScale,y*DestinationScale,DestinationScale,DestinationScale,
                     Direction*SourceScale,1*SourceScale,SourceScale,SourceScale);
      tkStraight: currentSnakeImage.draw(x*DestinationScale,y*DestinationScale,DestinationScale,DestinationScale,
                     Direction*SourceScale,0,SourceScale,SourceScale);
    end;
  end;

  //give a endgame message
  if GameOver then begin
    if score<=bestScore then
    begin
      WriteHighScore;
      UIFont.Print(Window.width div 2 - 80,Window.height div 2+15,Vector4(0.2,0.1,0,1),'GAME OVER!');
      UIFont.Print(Window.width div 2 - 80,Window.height div 2-15,Vector4(0.2,0.1,0,1),'Your score was: '+inttostr(score));
    end
    else
    begin
      WriteHighScore;
      UIFont.Print(Window.width div 2 - 80,Window.height div 2+15,Vector4(1,1,0,1),'GAME OVER!');
      UIFont.Print(Window.width div 2 - 80,Window.height div 2-15,Vector4(1,1,0,1),'BEST SCORE: '+inttostr(score));
    end
  end;
end;


{this procedure is called very 300 miliseconds}
procedure DoTimer;
begin
 if not GameOver then
 begin
   snake.move;
   flipImage := not flipImage;
 end;
end;

{this procedure handles mouse and key presses}
procedure KeyPress({%H-}Container: TUIContainer; const Event: TInputPressRelease);
var dx,dy: integer;
begin
  //if not GameOver then
  //begin
  //  if event.EventType = itKey then
  //    {this event is a keyboard event. Detect which button has been pressed}
  //    case event.key of
  //      k_up    : Snake.setDirection(0,1);
  //      k_down  : Snake.setDirection(0,-1);
  //      k_left  : Snake.setDirection(-1,0);
  //      k_right : Snake.setDirection(1,0);
  //      k_m     : toggleMusic;
  //    else ;
  //    end
  //
  //  else if event.EventType = itMouseButton then begin
  //    {this event is a mouse button or touch event.
  //     get click/touch coordinates.
  //     event.Position[0] is x and event.Position[1] is y}
  //    dx := round(event.Position[0]-(snake.x+0.5)*DestinationScale);
  //    dy := round(event.Position[1]-(snake.y+0.5)*DestinationScale);
  //    {and set the direction accordingly}
  //    if abs(dx)>abs(dy) then begin
  //      if not Snake.setDirection(Sign(dx),0) then Snake.setDirection(0,Sign(dy))
  //    end else begin
  //      if not Snake.setDirection(0,Sign(dy)) then Snake.setDirection(Sign(dx),0)
  //    end;
  //  end;
  //end
  //else
  //  NewGame;
end;

procedure LoadData();
begin
  //SnakeImage := TDrawableImage.Create('castle-data:/Snake.png',false);
  //SnakeFlipImage := TDrawableImage.Create('castle-data:/SnakeFlip.png',false);
  //
  //ReadHighScore;
  {Load music and sound}
  //LoadMusic;

  // Init QuickJS Engine.
end;

initialization
  // Init Logs.
  InitializeLog;
  ApplicationProperties._FileAccessSafe := true;
  InitQJS('main.js');
  ApplicationProperties._FileAccessSafe := False;

  // ReImplementing the Full Game Step by Step

  //
  //{map size is 16x16}
  //maxx := 15;
  //maxy := 15;
  //
  //{set the appropriate window size}
  //Window.Width := (maxx+1)*destinationScale;
  //Window.Height := (maxy+1)*destinationScale;
  //


  (*
  {create window}


  {initialize random sequence}
  randomize;



  flipImage := true;

  // Both of the Snake & Rabbit Classes Can be Implemented in JS
  {create snake}
  Snake := TSnake.create(Application);
  {create rabbit}
  Rabbit := TRabbit.create(Application);

  {set up window events callbacks}
  Window.OnRender := @WindowRender;
  Window.OnPress := @KeyPress;
  Window.ResizeAllowed := raNotAllowed;

  {set up application timer}
  Application.TimerMilisec := 220;
  Application.OnTimer := @doTimer;

  {start a new game}
  score := 0;
  NewGame;
  *)
end.

