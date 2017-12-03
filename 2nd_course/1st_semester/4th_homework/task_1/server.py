from http.server import HTTPServer, BaseHTTPRequestHandler
import sys
import socketserver

firstPlayerInfo = None
firstPlayerSubscribe = None


class TanksHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/isAnyoneHere":
            self.isAnyoneHere()
        if self.path == "/subscribe":
            self.subscribe()
        if self.path == "/getOffer":
            self.getOffer()

    def do_POST(self):
        if self.path == "/offer":
            self.offer()
        if self.path == "/answer":
            self.answer()

    def isAnyoneHere(self):
        global firstPlayerInfo

        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
     
        answer = b"yes" if firstPlayerInfo is not None else b"no"
        
        self.wfile.write(answer)
    
    def offer(self):
        global firstPlayerInfo
        length = int(self.headers['Content-Length'])
        data = self.rfile.read(length)
        firstPlayerInfo = data
    
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(b"ok")
    
    def subscribe(self):
        global firstPlayerSubscribe
        global firstPlayerInfo

        if firstPlayerInfo is None:
            self.send_response(406)
            return
    
        self.send_response(200)
        self.send_header('Content-type', 'text/event-stream')
        self.send_header('Connection', 'keep-alive')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()

        firstPlayerSubscribe = self.wfile

    def getOffer(self):
        global firstPlayerInfo

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
     
        self.wfile.write(firstPlayerInfo)

    def answer(self):
        global firstPlayerInfo
        global firstPlayerSubscribe

        length = int(self.headers['Content-Length'])
        data = self.rfile.read(length)

        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(b"ok")

        firstPlayerSubscribe.write(b"event: answered\n")
        firstPlayerSubscribe.write(b"data: ")
        firstPlayerSubscribe.write(data)
        firstPlayerSubscribe.write(b"\n\n")
        firstPlayerSubscribe.flush()

        firstPlayerInfo = None
        firstPlayerSubscribe = None

class ThreadedServer(socketserver.ThreadingMixIn, HTTPServer):
    pass;

server = ThreadedServer(('', 25535), TanksHandler)
server.serve_forever()
