const mapLeft = -250;
const mapRight = 250;
const firstPlayerPosition = -200;
const secondPlayerPosition = 200;
const mapRenderStep = 5;
const gravity = 9.8 * 10;

class Map {
    constructor(mapFunction) {
        this.mapFunction = mapFunction;
    }

    getHeight(x) {
        return this.mapFunction(x);
    }

    draw(context) {
        context.beginPath();

        var y = this.mapFunction(mapLeft);
        context.moveTo(mapLeft, y);

        for (var x = mapLeft; x <= mapRight; x += mapRenderStep) {
            y = this.mapFunction(x);

            context.lineTo(x, y);
        }

        context.stroke();
    }
    
    angleBetweenPoints(leftX, rightX) {
        var leftY = this.getHeight(leftX);
        var rightY = this.getHeight(rightX);
        return Math.atan2(rightY - leftY, rightX - leftX);
    }
}
