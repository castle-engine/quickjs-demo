import { Timer, UIFont, Application, Window } from "CastleEngine";
import { Snake, TailKind } from 'snake.js'

Application.OnInitialize = function () {
  console.log('----- Application OnInitialize -----');
  console.log(JSON.stringify(TailKind));
}

var MyWindow = new Window();

MyWindow.OnRender = function (Container) {
  // console.log(JSON.stringify(Container));
  UIFont.Print(0, this.Height - 50, undefined, 'FPS : ' + Container.Fps.toString);
  UIFont.Print(0, this.Height - 100, undefined, 'Hello From JS :D');
}

MyWindow.OnPress = function (Container, Event) {
  console.log(`Event.EventType : ${Event.EventType} , My key : ${Event.key}`);
}

var MyTimer = new Timer();
MyTimer.myindex = 1;

MyTimer.OnTick = function () {
  console.log(`Timer Tick ${this.myindex}`);
  if (this.myindex == 10) {
    console.log('Free The Timer');
    this.Free();
  }
  this.myindex++;
}
MyTimer.IntervalSeconds = 0.5;
MyTimer.Start(MyWindow);
MyWindow.Open();

Application.MainWindow = MyWindow;
Application.Run();
