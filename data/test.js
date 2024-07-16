class NewWindowClass extends Window {
  Box() {
    this.Width = 500;
    this.Height = 500;
  }
}

class NewWinEx extends NewWindowClass {
  Box() {
    super.Box();
  }
}

var newwin = new NewWinEx();

newwin.OnRender = function (Container) {
  UIFont.Print(0, this.Height - 50, undefined, 'FPS : ' + Container.Fps.toString);
  UIFont.Print(0, this.Height - 100, undefined, 'Hello From JS new win :D ');
}

newwin.OnPress = function (Container, Event) {
  console.log(`${this.toString()} : Event.EventType : ${Event.EventType} , My key : ${Event.key}`);
}

newwin.Box();
newwin.Open();