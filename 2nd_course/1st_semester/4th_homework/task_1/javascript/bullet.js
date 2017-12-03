var BulletType = {
    basicBullet: 0,
    largeBullet: 1
}

var BulletNames = []
BulletNames[BulletType.basicBullet] = "Basic bullet";
BulletNames[BulletType.largeBullet] = "Large bullet";

const bulletTypesCount = 2;

var BulletStatus = {
    flying: 0,
    exploding: 1,
    exploded: 2
}

class Bullet {
    constructor(map, x, y, angle, initialSpeed) {
        this.x0 = x;
        this.y0 = y;
        this.initialAngle = angle;
        this.initialSpeed = initialSpeed;

        this.time = 0;
        this.explosionTime = 0;
        this.map = map;
        this.status = BulletStatus.flying;
    }

    createTimeParametrization(bulletSize) {
        var cos = Math.cos(this.initialAngle);
        var sin = Math.sin(this.initialAngle);

        var x = this.x0 + cos * bulletSize;
        var y = this.y0 + sin * bulletSize;
        var dx = cos * this.initialSpeed;
        var dy = sin * this.initialSpeed;

        this.getX = (time) => (x + dx * time);
        this.getY = (time) => (y + dy * time - gravity * time * time / 2);
    }

    boundingCircle() {
        return [this.getX(this.time), this.getY(this.time), 0];
    }

    collide() {
        if (this.status == BulletStatus.flying) {
            this.status = BulletStatus.exploding;
        }
    }

    draw(context) {}

    explode() {
        this.status = BulletStatus.exploded;
    }

    findIntersectionTime(left, right) {
        const iterations = 50;

        var time = right;

        for (var i = 0; i < iterations; i++) {
            var time = (left + right) / 2;

            var x = this.getX(this.time);
            var y = this.getY(this.time);

            if (y - this.size <= this.map.getHeight(x)) {
                right = time;
            } else {
                left = time;
            }
        }

        return time;
    }

    tick(timeDelta) {
        if (this.status == BulletStatus.flying) {
            var previousTime = this.time;
            this.time += timeDelta;

            var x = this.getX(this.time);
            var y = this.getY(this.time);

            if (y - this.size <= this.map.getHeight(x)) {
                this.time = this.findIntersectionTime(previousTime, this.time);
                this.status = BulletStatus.exploding;
            }
        } else {
            this.explosionTime += timeDelta;
            this.explode();
        }
    }

    getStatus() {
        return this.status;
    }

    serialize() {
        var data = {
            type: this.type,
            x: this.x0,
            y: this.y0,
            angle: this.initialAngle,
            initialSpeed: this.initialSpeed,
            time: this.time
        }
        return data;
    }
}

class BasicBullet extends Bullet {
    constructor(map, x, y, angle) {
        const initialSpeed = 200;
        super(map, x, y, angle, initialSpeed);
        this.type = BulletType.basicBullet;

        this.size = 2;

        this.createTimeParametrization(this.size);
    }

    boundingCircle() {
        return [this.getX(this.time), this.getY(this.time), this.size];
    }

    explode() {
        if (this.explosionTime > 0.1) {
            this.status = BulletStatus.exploded;
        }
    }

    draw(context) {
        var x = this.getX(this.time);
        var y = this.getY(this.time);

        if (this.status == BulletStatus.flying) {
            context.beginPath();
            context.arc(x, y, this.size, 0, Math.PI * 2, false);
            context.fillStyle = "#000000";
            context.fill();
        } else {
            const particles = 5;
            const particleBegin = 100;
            const particleEnd = 200;
            const dx = 6;

            var angle = this.map.angleBetweenPoints(x - dx / 2, x + dx / 2);
            var step = Math.PI / particles;
            angle += step / 2;

            context.beginPath();
            for (var i = 0; i < particles; i++) {
                var currentAngle = angle + step * i;
                var cos = Math.cos(currentAngle);
                var sin = Math.sin(currentAngle);
                
                context.moveTo(x + cos * this.explosionTime * particleBegin, y + sin * this.explosionTime * particleBegin);
                context.lineTo(x + cos * this.explosionTime * particleEnd, y + sin * this.explosionTime * particleEnd);
            }
            context.stroke();
        }
    }
}

class LargeBullet extends Bullet {
    constructor(map, x, y, angle) {
        const initialSpeed = 150;
        super(map, x, y, angle, initialSpeed);
        this.type = BulletType.largeBullet;

        this.size = 4;

        this.createTimeParametrization(this.size);
    }

    boundingCircle() {
        return [this.getX(this.time), this.getY(this.time), this.size];
    }

    explode() {
        this.size = 30;
        if (this.explosionTime > 0.1) {
            this.status = BulletStatus.exploded;
        }
    }

    draw(context) {
        var x = this.getX(this.time);
        var y = this.getY(this.time);

        if (this.status == BulletStatus.flying) {
            context.beginPath();
            context.arc(x, y, this.size, 0, Math.PI * 2, false);
            context.fillStyle = "#000000";
            context.fill();
        } else {
            const particles = 10;
            const particleBegin = 150;
            const particleEnd = 300;
            const dx = 6;

            var angle = this.map.angleBetweenPoints(x - dx / 2, x + dx / 2);
            var step = Math.PI / particles;
            angle += step / 2;

            context.beginPath();
            for (var i = 0; i < particles; i++) {
                var currentAngle = angle + step * i;
                var cos = Math.cos(currentAngle);
                var sin = Math.sin(currentAngle);
                
                context.moveTo(x + cos * this.explosionTime * particleBegin, y + sin * this.explosionTime * particleBegin);
                context.lineTo(x + cos * this.explosionTime * particleEnd, y + sin * this.explosionTime * particleEnd);
            }
            context.stroke();
        }
    }
}

class BulletFactory {
    static createBullet(type, map, x, y, angle) {
        switch(type) {
            case BulletType.basicBullet:
                return new BasicBullet(map, x, y, angle);
            case BulletType.largeBullet:
                return new LargeBullet(map, x, y, angle);
        }
    }

    static createBulletFromSerialized(map, serializedBullet) {
        var x = serializedBullet.x;
        var y = serializedBullet.y;
        var angle = serializedBullet.angle;
        var initialSpeed = serializedBullet.initialSpeed;
        var time = serializedBullet.time;
        var type = serializedBullet.type;

        console.log(serializedBullet);

        var bullet = BulletFactory.createBullet(type, map, x, y, angle);
        //bullet.time = time;

        return bullet;
    }
}
