class Player {
    constructor(x, map, fireCallback, isFirst) {
        const defaultGunAngle = Math.PI / 4;
        const playerSize = [15, 7];
        const gunSize = [10, 1];

        this.map = map;
        this.fire = fireCallback;
        this.gunAngle = isFirst ? defaultGunAngle : Math.PI - defaultGunAngle;
        this.size = playerSize;
        this.gunSize = gunSize;
        this.isFirst = isFirst;
        this.selectedBullet = BulletType.basicBullet;

        this.updatePosition(x);
    }

    updatePosition(x) {
        this.x = x;
        this.y = this.map.getHeight(this.x);
        this.bodyAngle = this.map.angleBetweenPoints(this.x - this.size[0] / 2, this.x + this.size[0] / 2);
    }

    clampPosition() {
        var x = this.x;
        var sx = this.size[0] / 2;
        if (this.isFirst) {
            if (x < mapLeft + sx) {
                x = mapLeft + sx;
            }
            if (x > 0) {
                x = 0;
            }
        } else {
            if (x > mapRight - sx) {
                x = mapRight - sx;
            }
            if (x < 0) {
                x = 0;
            }
        }

        this.updatePosition(x);
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

    boundingCircle() {
        var sx = this.size[0] / 2;
        return [this.x, this.y, sx];
    }

    getSelectedBullet() {
        return this.selectedBullet;
    }

    serialize() {
        return [this.x, this.gunAngle];
    }

    deserialize(data) {
        this.updatePosition(data[0]);
        this.gunAngle = data[1];
    }
}

class LocalPlayer extends Player {
    constructor(x, map, fireCallback, isFirst) {
        super(x, map, fireCallback, isFirst);
    }

    tick(keyboardInput, timeDelta) {
        const playerSpeed = 50;
        const gunRotationSpeed = Math.PI / 2;

        var newX = this.x;

        if (keyboardInput.isPressed(KeyCode.leftArrow)) {
            newX -= playerSpeed * timeDelta;
        }
        if (keyboardInput.isPressed(KeyCode.rightArrow)) {
            newX += playerSpeed * timeDelta;
        }

        this.updatePosition(newX);

        var rotationFactor = this.isFirst ? 1 : -1;
        if (keyboardInput.isPressed(KeyCode.upArrow)) {
            this.gunAngle += rotationFactor * gunRotationSpeed * timeDelta;
        }
        if (keyboardInput.isPressed(KeyCode.downArrow)) {
            this.gunAngle -= rotationFactor * gunRotationSpeed * timeDelta;
        }
        this.gunAngle = this.gunAngle < 0 ? 0 : 
                       (this.gunAngle > Math.PI ? Math.PI : this.gunAngle);

        if (keyboardInput.isPressed(KeyCode.enter)) {
            var x = this.x - Math.sin(this.bodyAngle) * this.size[1] + 
                             Math.cos(this.bodyAngle + this.gunAngle) * this.gunSize[0];
            var y = this.y + Math.cos(this.bodyAngle) * this.size[1] + 
                             Math.sin(this.bodyAngle + this.gunAngle) * this.gunSize[0];

            this.fire(BulletFactory.createBullet(this.selectedBullet, this.map, x, y, this.bodyAngle + this.gunAngle));
        }

        if (keyboardInput.isPressed(KeyCode.shift)) {
            if (!this.shiftFlag) {
                this.shiftFlag = true;

                this.selectedBullet++;
                if (this.selectedBullet >= bulletTypesCount) {
                    this.selectedBullet %= bulletTypesCount;
                }
            }
        } else {
            this.shiftFlag = false;
        }
    }
}

class RemotePlayer extends Player {
    constructor(x, map, fireCallback, isFirst) {
        super(x, map, fireCallback, isFirst);
    }

    tick(networkInput, timeDelta) {
        var playerData = networkInput.player;
        this.deserialize(playerData);

        var bulletData = networkInput.bullet;
        if (bulletData) {
            this.fire(BulletFactory.createBulletFromSerialized(this.map, bulletData));
        }
    }
}
