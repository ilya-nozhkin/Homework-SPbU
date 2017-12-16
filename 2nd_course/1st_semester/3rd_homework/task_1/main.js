const drawRate = 16;
const tickRate = 8;

var canvas = document.getElementById("gameCanvas");
canvas.width = window.innerWidth * 0.9;
canvas.height = window.innerHeight * 0.9;

var context = canvas.getContext("2d");

var game = new Game();
var keyboardInput = new KeyboardInput();

function draw() {
    context.clearRect(0, 0, canvas.width, canvas.height);

    context.save();

    scale = canvas.width / (mapRight - mapLeft);

    context.translate(0, canvas.height);
    context.scale(scale, -scale);
    context.translate(-mapLeft, 0)

    game.draw(context);

    context.restore();
}

function tick() {
    game.tick(keyboardInput, tickRate / 1000);
}

document.addEventListener("keydown", (keyboardEvent) => {
    keyboardInput.setState(keyboardEvent.keyCode, true);
});

document.addEventListener("keyup", (keyboardEvent) => {
    keyboardInput.setState(keyboardEvent.keyCode, false);
});

setInterval(draw, drawRate);
setInterval(tick, tickRate);
