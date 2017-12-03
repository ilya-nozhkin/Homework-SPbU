const drawRate = 16;
const tickRate = 8;

var canvas = document.getElementById("gameCanvas");
canvas.width = window.innerWidth * 0.9;
canvas.height = window.innerHeight * 0.9;

var context = canvas.getContext("2d");

var connectButton = document.getElementById("connectButton");
var addressText = document.getElementById("addressText");
var statusText = document.getElementById("statusText");

var NetworkStatusText = []

NetworkStatusText[NetworkStatus.connectionFailed] = "connection failed";
NetworkStatusText[NetworkStatus.idle] = "idle";
NetworkStatusText[NetworkStatus.connectingToServer] = "connecting to server";
NetworkStatusText[NetworkStatus.waitingForPeer] = "waiting for peer";
NetworkStatusText[NetworkStatus.connectingToPeer] = "connecting to peer";
NetworkStatusText[NetworkStatus.connected] = "connected to peer";

var networkStatusChanged = (networkStatus) => {
    statusText.value = "status: " + NetworkStatusText[networkStatus];

    if (networkStatus == NetworkStatus.connected) {
        game.setNetwork(network);
    }

    if (networkStatus == NetworkStatus.idle || networkStatus == NetworkStatus.connectionFailed) {
        connectButton.disabled = false;
    } else {
        connectButton.disabled = true;
    }
}

var network = new Network(networkStatusChanged);
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

var previousTime = Date.now();
function tick() {
    var currentTime = Date.now();
    var delta = currentTime - previousTime;
    if (delta > 200) {
        delta = 10;
    }

    game.tick(keyboardInput, delta / 1000);
    previousTime = currentTime;
}

document.addEventListener("keydown", (keyboardEvent) => {
    keyboardInput.setState(keyboardEvent.keyCode, true);
});

document.addEventListener("keyup", (keyboardEvent) => {
    keyboardInput.setState(keyboardEvent.keyCode, false);
});

connectButton.addEventListener("click", () => network.connect(addressText.value));

setInterval(draw, drawRate);
setInterval(tick, tickRate);
