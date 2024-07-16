// Reimplementing the Castel Engine fully in JS

    JS Engine:
      Custom Implementations For JS Engine:
        ✔ Custom Module Loader Compatable with Castle Game Engine @done(20-04-14 08:16)
        ✔ Add Our Own console Object @done
          ✔ log @done
          ☐ warn
          ☐ error

    JSApplication:
      Callbacks:
        ✔ OnInitialize @done(20-04-14 04:43)
      Properties: 
        ✔ MainWindow @done(20-04-14 04:42)
      Methods:
        ✔ Run @done(20-04-14 04:42)
  

    JSWindow:
      Callbacks:
        ✔ OnRender @done(20-04-14 04:45)
        ✔ OnPress @done
      Methods:
        ✔ Open @done(20-04-14 04:43)
        ✔ Free @done(20-04-14 04:43)
      Props:
        ✔ Width @started(20-04-14 04:48) @done(20-04-14 04:55) @lasted(7m50s)
        ✔ Height @started(20-04-14 04:48) @done(20-04-14 04:55) @lasted(7m22s)
        ☐ ResizeAllowed @started(20-04-14 05:02)
    
      TUIContainer:
        Fps (TFramesPerSecond):
          Properties:
            ☐ OnlyRenderFps
            ☐ FrameTime
            ☐ RealFps
            ☐ RealTime
            ☐ WasSleeping
            ☐ SecondsPassed
            ☐ MaxSensibleSecondsPassed
          functions:
            ✔ ToString @done
            ☐ ZeroNextSecondsPassed

      Event (TInputPressRelease):
        ✔ EventType @done
        ✔ Key @done

    JSTimer:
      CallBacks:
        ✔ OnTimer Callback @done(20-04-14 04:46)
        ☐ OnUpdate
      Props:
        ✔ IntervalSeconds @done(20-04-14 04:45)
        ✔ CounteractDelays @started(20-04-14 06:38) @done(20-04-14 07:24) @lasted(10m30s)
    

    JSUIFont:
      ✔ Print @started(20-04-14 06:06) @done(20-04-14 06:33) @lasted(27m)
        ☐ Implement Vec4
  
