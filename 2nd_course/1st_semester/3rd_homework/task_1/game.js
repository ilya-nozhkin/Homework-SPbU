function defaultLandFunction(x) {
    x /= 60;
    return Math.exp(-x * x) * 100;
}

class Game {
    constructor() {
        this.map = new Map(defaultLandFunction);
        this.turn = 0;

        this.bullet = null;
        var game = this;
        var fireCallback = ((bullet) => {game.fire(bullet);});

        this.firstPlayer = new LocalPlayer(-80, this.map, fireCallback);
    }

    fire(bullet) {
        if (!this.bullet) {
            this.bullet = bullet;
        }
    }

    tick(keyboardInput, timeDelta) {
        if (this.turn == 0) {
            this.firstPlayer.tick(keyboardInput, timeDelta);
        }

        if (this.bullet) {
            this.bullet.tick(timeDelta);
            if (!this.bullet.isFlying()) {
                this.bullet = null;
            }
        }
    }

    draw(context) {
        this.map.draw(context);
        this.firstPlayer.draw(context);

        if (this.bullet) {
            this.bullet.draw(context);
        }
    }
}
