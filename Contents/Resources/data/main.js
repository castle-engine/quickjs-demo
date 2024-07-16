import { Timer, Application, Window } from "CastleEngine";

Application.OnInitialize = function(a,b,c){
    for (let i = 0; i < 3; i++)
        console.log('----- Application OnInitialize -----');
}

var MyWindow = new Window(Application);
MyWindow.OnRender = function(Container){
    
    console.log('Hello From JS Window OnRender :D');
}
MyWindow.Open()


var SecondWindow = new Window(Application);
SecondWindow.OnRender = function(Container){
    
    console.log('SecondWindow');
}
SecondWindow.Open();


// TCastleTimer Example :D 
var WindowCloseTimer = new Timer();
WindowCloseTimer.index = 0;

WindowCloseTimer.OnTick = function(){
    if (this.index == 2){
        SecondWindow.Free();
        MyWindow.Free();
        this.Free();
        return;
    }
    this.index++;
}
WindowCloseTimer.Start(MyWindow);

// Application.MainWindow = MyWindow;

Application.Run();
