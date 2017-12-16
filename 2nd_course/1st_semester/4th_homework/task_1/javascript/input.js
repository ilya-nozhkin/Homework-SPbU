var KeyCode = {
    leftArrow: 37,
    rightArrow: 39,
    upArrow: 38,
    downArrow: 40,
    enter: 13,
    shift: 16 
}

class KeyboardInput {
    constructor() {
        this.keys = []
        for (var i = 0; i < 256; i++) {
            this.keys[i] = false;
        }
    }

    setState(id, state) {
        this.keys[id] = state;
    }

    isPressed(id) {
        return this.keys[id];
    }
}
