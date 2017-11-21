const leftArrow = 37;
const rightArrow = 39;
const upArrow = 38;
const downArrow = 40;
const enter = 13;

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
