class Player {
    constructor(x, map, fireCallback) {
        const defaultGunAngle = Math.PI / 4;
        const playerSize = [15, 7];
        const gunSize = [10, 1];

        this.map = map;
        this.fire = fireCallback;
        this.gunAngle = defaultGunAngle;
        this.size = playerSize;
        this.gunSize = gunSize;

        this.updatePosition(x);
    }

    updatePosition(x) {
        this.x = x;
        this.y = this.map.getHeight(this.x);
        this.bodyAngle = this.map.angleBetweenPoints(this.x - this.size[0] / 2, this.x + this.size[0] / 2);
    }

    draw(context) {
        const towerSize = 3;

        context.save();
        context.translate(this.x, this.y);
        context.rotate(this.bodyAngle);

        context.fillStyle = "#000000";
        context.fillRect(-this.size[0] / 2, 0, this.size[0], this.size[1]);

        context.translate(0, this.size[1]);
        context.rotate(this.gunAngle);

        context.beginPath();
        context.arc(0, 0, towerSize, 0, Math.PI * 2, false);
        context.rect(0, -this.gunSize[1] / 2, this.gunSize[0], this.gunSize[1]);
        context.fill();

        context.restore();
    }

    tick(input, timeDelta) {}
}

class LocalPlayer extends Player {
    constructor(x, map, fireCallback) {
        super(x, map, fireCallback);
    }

    tick(keyboardInput, timeDelta) {
        const playerSpeed = 50;
        const gunRotationSpeed = Math.PI / 2;

        var newX = this.x;

        if (keyboardInput.isPressed(leftArrow)) {
            newX -= playerSpeed * timeDelta;
        }
        if (keyboardInput.isPressed(rightArrow)) {
            newX += playerSpeed * timeDelta;
        }

        this.updatePosition(newX);

        if (keyboardInput.isPressed(upArrow)) {
            this.gunAngle += gunRotationSpeed * timeDelta;
        }
        if (keyboardInput.isPressed(downArrow)) {
            this.gunAngle -= gunRotationSpeed * timeDelta;
        }
        this.gunAngle = this.gunAngle < 0 ? 0 : 
                       (this.gunAngle > Math.PI ? Math.PI : this.gunAngle);

        if (keyboardInput.isPressed(enter)) {
            var x = this.x - Math.sin(this.bodyAngle) * this.size[1] + 
                             Math.cos(this.bodyAngle + this.gunAngle) * this.gunSize[0];
            var y = this.y + Math.cos(this.bodyAngle) * this.size[1] + 
                             Math.sin(this.bodyAngle + this.gunAngle) * this.gunSize[0];

            this.fire(new BasicBullet(this.map, x, y, this.bodyAngle + this.gunAngle));
        }
    }
}
