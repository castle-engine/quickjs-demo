import * as CGE from 'CastleEngine';

// TCastleTimer Example :D 
var timers = []

for (let i = 0; i < 3; i++) {

    timers[i] = new CGE.Timer();
    timers[i].index = 1;
    
    timers[i].OnTick = function(){
        console.log('this instanceof Timer = ',this instanceof CGE.Timer);
        
        if (this.index == 3){
            console.log('Free > ',this);
            this.Free();
            return
        }
        this.index++;
    }
    timers[i].Start();
}


// Called At Game Init
function Start() {
    console.log('Hello World From Start');
}
// Called Every Frame.
function Update() {
   
}

export { Start, Update };
