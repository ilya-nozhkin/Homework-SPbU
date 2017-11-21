class Bullet {
    constructor(map, x, y, angle, initialSpeed) {
        this.time = 0;
        this.map = map;
        this.flying = true;

        var dx = Math.cos(angle) * initialSpeed;
        var dy = Math.sin(angle) * initialSpeed;

        this.getX = (time) => (x + dx * time);
        this.getY = (time) => (y + dy * time - gravity * time * time / 2);
    }

    draw(context) {}

    tick(timeDelta) {
        this.time += timeDelta;

        var x = this.getX(this.time);
        var y = this.getY(this.time);

        if (y <= this.map.getHeight(x)) {
            this.flying = false;
        }
    }

    isFlying() {
        return this.flying;
    }
}

class BasicBullet extends Bullet {
    constructor(map, x, y, angle) {
        const initialSpeed = 200;
        super(map, x, y, angle, initialSpeed);
    }

    draw(context) {
        const size = 2;
        
        var x = this.getX(this.time);
        var y = this.getY(this.time);

        context.beginPath();
        context.arc(x, y, size, 0, Math.PI * 2, false);
        context.fillStyle = "#000000";
        context.fill();
    }
}
