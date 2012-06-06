#! /usr/bin/env python3

'''
    WebSockets
    [X] handshake with Chrome (Sec-WebSocket-Version: 13)
    [ ] base framing protocol (not implemented yet)
'''

import base64
import hashlib
import os
import socket

from threading import Thread

def start_server():
    ssock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssock.bind(('localhost', 1234))
    ssock.listen(5)
    while True:
        print('listening...')
        try:
            csock, address = ssock.accept()
            Thread(target=handle_client, args=[csock]).start()
        except KeyboardInterrupt:
            os._exit(-1) # quit silently

def handle_client(client):
    handshake(client)
    print('handshaken')
    tick = 0
    while True:
        interact(client, tick)
        tick += 1

def recv_header(client):
    buf = b''
    while not buf.endswith(b'\r\n\r\n'):
        chunk = client.recv(10)
        buf += chunk
    return buf.decode('utf-8')

def recv_data(client, count):
    data = client.recv(count)
    print('recv', data)
    return data.decode('utf-8', 'ignore')

def send_data(client, str):
    str = b'\x00' + str.encode('utf-8') + b'\xff'
    print('send', str)
    return client.send(str)

def handshake(client):
    shake = recv_header(client)
    print('<<' + shake + '>>')
    headers = dict([line.split(': ') for line in shake.strip().split('\r\n') if ': ' in line])
    key = headers['Sec-WebSocket-Key']
    key += '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
    sha1 = hashlib.sha1()
    sha1.update(key.encode('utf-8'))
    sec = base64.encodestring(sha1.digest()).decode('utf-8').strip()
    print(sec)
    response = '\r\n'.join([
        'HTTP/1.1 101 Switching Protocols',
        'Upgrade: websocket',
        'Connection: Upgrade',
        'Sec-WebSocket-Accept: ' + sec,
        ]) + '\r\n\r\n'
    client.send(response.encode('utf-8'))

def interact(client, tick):
    data = recv_data(client, 255)
    print('got', data)
    send_data(client, 'clock ! tick%d' % tick)
    send_data(client, 'out ! ' + data)

if __name__ == '__main__':
    start_server()

