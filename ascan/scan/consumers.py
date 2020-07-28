import json
from logging import info, warning

from asgiref.sync import async_to_sync
from channels.consumer import SyncConsumer

from scan.control import verify_scan_permission

class ScanProgressConsumer(SyncConsumer):

    def websocket_connect(self, event):
        self.send({ 'type': 'websocket.accept' })

    def websocket_disconnect(self, event):
        self.send({ 'type': 'websocket.disconnected' })

    def websocket_receive(self, event):
        received_obj = json.loads(event['text'])
        if received_obj['subject'] == 'inquire_scan':
            # The client in scope should contain the IP and the port.
            is_authorized = verify_scan_permission(self.scope['user'], self.scope['client'][0])
            if is_authorized:
                # Join the group for that scan.
                new_group = 'scan_{}'.format(received_obj['scan_id'])
                info('Adding to group: {}'.format(new_group))
                #self.scan_groups.append(new_group)
                async_to_sync(self.channel_layer.group_add)(
                    new_group,
                    self.channel_name
                )
                self.send({ 'type': 'websocket.send', 'text': 'inquiry_accepted' })
            else:
                self.send({ 'type': 'websocket.send', 'text': 'inquiry_refused' })
        else:
            self.send({ 'type': 'websocket.send', 'text': 'wut' })
            warning('Received a WebSocket transmission with an unknown subject: {}'.format(
                received_obj))

    def progress_info(self, event):
        """
        This should be called with channel_layer.group_send to the appropriate group.
        """
        self.send({ 'type': 'websocket.send', 'text': json.dumps(event)})
