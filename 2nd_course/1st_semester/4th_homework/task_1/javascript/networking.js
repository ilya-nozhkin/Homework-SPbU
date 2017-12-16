var NetworkStatus = {
    connectionFailed: -1,
    idle: 0,
    connectingToServer: 1,
    waitingForPeer: 2,
    connectingToPeer: 3,
    connected: 4
}

class Network {
    constructor(statusChangedCallback) {
        this.statusChanged = statusChangedCallback;
        this.statusChanged(NetworkStatus.idle);
        this.messageQueue = []
    }

    connect() {
        this.statusChanged(NetworkStatus.connectingToServer);
        
        var thisptr = this;
        var request = new XMLHttpRequest();

        request.open("GET", "/isAnyoneHere", true);
        request.addEventListener("load", 
            () => thisptr.getPeerLookupResponse(request.status, request.responseText));
        request.addEventListener("error",
            () => thisptr.connectionFailed());

        request.send(null);
    }

    connectionFailed() {
        this.statusChanged(NetworkStatus.connectionFailed);
    }

    getPeerLookupResponse(responseStatus, text) {
        if (responseStatus != 200) {
            this.statusChanged(NetworkStatus.connectionFailed);
        } else {
            var thisptr = this
            var messageReceivedCallback = message => thisptr.messageReceived(message);

            if (text == "no") {
                this.peer = new Initiator(this.statusChanged, messageReceivedCallback);
            } else if (text == "yes") {
                this.peer = new Follower(this.statusChanged, messageReceivedCallback);
            }

            this.peer.connect();
        }
    }

    sendMessage(message) {
        this.peer.sendMessage(message);
    }

    messageReceived(message) {
        this.messageQueue.push(message);
    }

    getReceivedMessages() {
        return this.messageQueue;
    }

    isInitiator() {
        return this.peer.isInitiator();
    }
}

class Peer {
    constructor(statusChangedCallback, messageReceivedCallback) {
        this.statusChanged = statusChangedCallback;
        this.messageReceived = messageReceivedCallback;
        this.ready = false;
        this.localReady = false;
        this.remoteReady = false;
    }

    connectionFailed() {
        this.statusChanged(NetworkStatus.connectionFailed);
    }

    connected() {
        this.ready = true;
        this.statusChanged(NetworkStatus.connected);
    }

    createPeerConnection() {
        var thisptr = this;

        var peerConnectionConfiguration = {"iceServers": [{"urls": ["stun:stun.l.google.com:19302"]}]};
        this.peerConnection = new RTCPeerConnection(peerConnectionConfiguration);
        this.localChannel = this.peerConnection.createDataChannel(this.channelName);
        this.localChannel.onopen = event => this.localChannelOpened();

        this.peerConnection.ondatachannel = event => {
            thisptr.remoteChannel = event.channel;
            thisptr.remoteChannel.onmessage = event => thisptr.messageReceived(event.data);
            thisptr.remoteChannelOpened();
        }
    }

    localChannelOpened() {
        this.localReady = true;

        if (this.localReady && this.remoteReady) {
            this.connected();
        }
    }

    remoteChannelOpened() {
        this.remoteReady = true;

        if (this.localReady && this.remoteReady) {
            this.connected();
        }
    }

    sendMessage(message) {
        this.localChannel.send(message);
    }

    connect() {
        this.createPeerConnection();
    }

    isInitiator() {}
}

class Initiator extends Peer {
    constructor(statusChangedCallback, messageReceivedCallback) {
        super(statusChangedCallback, messageReceivedCallback);
        this.channelName = "initiatorChannel";
    }

    isInitiator() {
        return true;
    }

    connect() {
        super.connect();
        this.createOffer();
    }

    createOffer() {
        var thisptr = this;

        this.selfIceCandidates = [];
        this.peerConnection.onicecandidate = event => {
            if (event.candidate != null) {
                thisptr.selfIceCandidates.push(event.candidate);
            } else {
                thisptr.sendOffer()
            }
        }

        this.peerConnection.createOffer().then(offer => 
            thisptr.peerConnection.setLocalDescription(offer));
    }

    sendOffer() {
        var thisptr = this;
        var request = new XMLHttpRequest();

        request.open("POST", "/offer", true);
        request.setRequestHeader("Content-Type", "text/plain");   
        request.addEventListener("load",
            () => thisptr.offerSent(request.status, request.responseText));
        request.addEventListener("error",
            () => thisptr.connectionFailed());

        var offer = {description: this.peerConnection.localDescription,
                     candidates: this.selfIceCandidates};
        request.send(JSON.stringify(offer));
    }

    offerSent(responseStatus, text) {
        if (responseStatus != 200) {
            this.connectionFailed();
        } else {
            if (text == "ok") {
                this.waitForPeer()
                this.statusChanged(NetworkStatus.waitingForPeer);
            }
        }
    }

    waitForPeer() {
        var thisptr = this;

        this.waitingSource = new EventSource("/subscribe");
        this.waitingSource.onerror = () => {
            thisptr.waitingSource.close();
            thisptr.connectionFailed();
        }
        this.waitingSource.addEventListener("answered", event => {
            thisptr.waitingSource.close();
            thisptr.peerAnswered(JSON.parse(event.data));
        });
    }

    peerAnswered(answer) {
        this.statusChanged(NetworkStatus.connectingToPeer);

        var description = answer.description;
        var candidates = answer.candidates;

        var thisptr = this;

        this.peerConnection.setRemoteDescription(description);
        for (var candidate in candidates) {
            this.peerConnection.addIceCandidate(candidates[candidate]);
        }
    }
}

class Follower extends Peer {
    constructor(statusChangedCallback, messageReceivedCallback) {
        super(statusChangedCallback, messageReceivedCallback);
        this.channelName = "followerChannel";
    }

    isInitiator() {
        return false;
    }

    connect() {
        super.connect();
        this.receiveOffer();
    }

    receiveOffer() {
        var thisptr = this;
        var request = new XMLHttpRequest();

        request.open("GET", "/getOffer", true);
        request.addEventListener("load",
            () => thisptr.offerReceived(request.status, request.responseText));
        request.addEventListener("error",
            () => thisptr.connectionFailed());

        request.send(null);
    }

    offerReceived(responseStatus, text) {
        if (responseStatus != 200) {
            this.connectionFailed();
        } else {
            this.statusChanged(NetworkStatus.connectingToPeer);
            this.createAnswer(JSON.parse(text));
        }
    }

    createAnswer(offer) {
        var description = offer.description;
        var candidates = offer.candidates;

        var thisptr = this;

        this.selfIceCandidates = [];
        this.peerConnection.onicecandidate = event => {
            if (event.candidate != null) {
                thisptr.selfIceCandidates.push(event.candidate);
            } else {
                thisptr.sendAnswer()
            }
        }

        this.peerConnection.setRemoteDescription(description);
        for (var candidate in candidates) {
            this.peerConnection.addIceCandidate(candidates[candidate]);
        }

        this.peerConnection.createAnswer().then(answer => 
            thisptr.peerConnection.setLocalDescription(answer));
    }

    sendAnswer() {
        var thisptr = this;
        var request = new XMLHttpRequest();

        request.open("POST", "/answer", true);
        request.setRequestHeader("Content-Type", "text/plain");   
        request.addEventListener("load",
            () => thisptr.answerSent(request.status, request.responseText));
        request.addEventListener("error",
            () => thisptr.connectionFailed());

        var answer = {description: this.peerConnection.localDescription,
                      candidates: this.selfIceCandidates};
        request.send(JSON.stringify(answer));
    }

    answerSent(responseStatus, text) {
        if (responseStatus != 200 || text != "ok") {
            this.connectionFailed();
        }
    } 
}
