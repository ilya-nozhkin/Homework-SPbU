function defaultLandFunction(x) {
    x /= 30;
    return (1 + Math.cos(x)) * Math.exp(-x * x / 30) * 40;
}

var NetworkMessageType = {
    playerData: 0
}

var GameStatus = {
    training: 0,
    running: 1,
    won: 2,
    lost: 3
}

class Game {
    constructor() {
        this.map = new Map(defaultLandFunction);
        this.turn = 0;

        this.bullet = null;
        this.bulletToSend = null;

        this.status = GameStatus.training;

        var game = this;

        this.localFireCallback = (bullet) => {
            if (game.fire(bullet)) {
                if (game.status == GameStatus.running) {
                    game.bulletToSend = bullet;
                }

                game.changeTurn();
                game.bulletOwnerIsLocal = true;
            }
        };

        this.remoteFireCallback = (bullet) => {
            game.fire(bullet);
            game.bulletOwnerIsLocal = false;
        };

        this.localPlayer = new LocalPlayer(firstPlayerPosition, this.map, this.localFireCallback, true);
        this.remotePlayer = null;
        
        this.network = null;
    }

    setNetwork(network) {
        this.network = network;

        if (this.network.isInitiator()) {
            this.localPlayer = new LocalPlayer(firstPlayerPosition, this.map, this.localFireCallback, true);
        } else {
            this.localPlayer = new LocalPlayer(secondPlayerPosition, this.map, this.localFireCallback, false);
        }

        this.status = GameStatus.running;
    }

    fire(bullet) {
        if (!this.bullet) {
            this.bullet = bullet;
            return true;
        }
        
        return false;
    }

    changeTurn() {
        if (this.status == GameStatus.running) {
            this.turn = (this.turn + 1) % 2;
        }
    }

    processNetworkMessage(message) {
        if (message.type == NetworkMessageType.playerData) {
            if (!this.remotePlayer) {
                this.remotePlayer = new RemotePlayer(0, this.map, this.remoteFireCallback, !this.network.isInitiator());
            }

            this.remotePlayer.tick(message);
        }
    }

    processNetworkMessages() {
        var messages = this.network.getReceivedMessages();
        while (messages.length > 0) {
            var message = messages.shift();
            this.processNetworkMessage(JSON.parse(message));
        }
    }

    sendNetworkMessage(message) {
        this.network.sendMessage(JSON.stringify(message));
    }

    sendNetworkMessages() {
        var playerData = this.localPlayer.serialize();

        
        var message = {
            type: NetworkMessageType.playerData,
            player: playerData,
            bullet: null
        };

        if (this.bulletToSend) {
            message.bullet = this.bulletToSend.serialize();
            this.bulletToSend = null;
        }

        this.sendNetworkMessage(message);
    }

    processNetworking() {
        if (this.network) {
            this.processNetworkMessages();
            this.sendNetworkMessages();
        }
    }

    collide(circle1, circle2) {
        var dx = circle1[0] - circle2[0];
        var dy = circle1[1] - circle2[1];
        return Math.sqrt(dx * dx + dy * dy) < (circle1[2] + circle2[2]);
    }

    checkCollisions() {
        if (this.bullet && this.remotePlayer) {
            var bulletCircle = this.bullet.boundingCircle();
            var localPlayerCircle = this.localPlayer.boundingCircle();
            var remotePlayerCircle = this.remotePlayer.boundingCircle();

            var localCollision = this.collide(bulletCircle, localPlayerCircle);
            var remoteCollision = this.collide(bulletCircle, remotePlayerCircle);

            if (localCollision) {
                this.status = GameStatus.lost;
            } else if (remoteCollision) {
                this.status = GameStatus.won;
            }

            if (localCollision || remoteCollision) {
                this.bullet.collide();
            }
        }
    }

    tick(keyboardInput, timeDelta) {
        if (this.computeTurn()) {
            this.localPlayer.tick(keyboardInput, timeDelta);

            if (this.status == GameStatus.running) {
                this.localPlayer.clampPosition();
            }
        }

        if (this.bullet) {
            this.bullet.tick(timeDelta);

            this.checkCollisions();

            if (this.bullet.getStatus() == BulletStatus.exploded) {
                this.bullet = null;
                if (!this.bulletOwnerIsLocal) {
                    this.changeTurn();
                }
            }
        }
    }
    
    computeTurn() {
        if ((this.status == GameStatus.won) || (this.status == GameStatus.lost)) {
            return false;
        }

        var firstPlayer = true;
        if (this.network) {
            firstPlayer = this.network.isInitiator();
        }

        var yourTurn = firstPlayer == (this.turn == 0);
        return yourTurn;
    }

    createStatusText() {
        var statusText = "";

        switch (this.status) {
            case GameStatus.training:
                statusText += "Training";
                break;
            case GameStatus.running:
                statusText += this.computeTurn() ? "Your turn" : "Opponent's turn";
                break;
            case GameStatus.won:
                statusText += "You have won!";
                break;
            case GameStatus.lost:
                statusText += "You have lost!";
                break;
        }

        return statusText;
    }

    drawStatus(context) {
        const statusShift = 25;

        context.save();
        context.translate(0, statusShift);
        context.scale(1, -1);

        context.textAlign = "center";
        context.font = "12px Verdana";

        context.fillText(this.createStatusText(), 0, 0);

        var ammoText = "Selected ammo: " + BulletNames[this.localPlayer.getSelectedBullet()];
        context.fillText(ammoText, 0, 15);

        context.restore();
    }

    draw(context) {
        const screenShift = 50;

        this.drawStatus(context);

        context.translate(0, screenShift);

        this.map.draw(context);

        this.localPlayer.draw(context);

        if (this.remotePlayer) {
            this.remotePlayer.draw(context);
        }

        if (this.bullet) {
            this.bullet.draw(context);
        }
    }
}
