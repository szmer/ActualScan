import json
from logging import info, warning
import socket

from asgiref.sync import async_to_sync
from django.db.models import F
from channels.consumer import SyncConsumer

from scan.control import verify_scan_permission
from scan.get_results import rules_results
from scan.models import Site, TagSiteLink, FeedbackPermission

class RelevanceFeedbackConsumer(SyncConsumer):

    def websocket_connect(self, event):
        self.send({ 'type': 'websocket.accept' })

    def websocket_disconnect(self, event):
        self.send({ 'type': 'websocket.disconnected' })

    def websocket_receive(self, event):
        received_obj = json.loads(event['text'])
        if received_obj['subject'] == 'tag_feedback':
            link = TagSiteLink.objects.get(tag__name=received_obj['tag'],
                    site__site_name=received_obj['site'])
            level_updated = False
            if link is not None:
                if self.scope['user'].is_authenticated:
                    feedback_perm = FeedbackPermission.objects.get(subject=link,
                            is_used=False, user=self.scope['user'])
                else:
                    feedback_perm = FeedbackPermission.objects.get(subject=link,
                            is_used=False, user_ip=self.scope['client'][0])
                if feedback_perm is not None:
                    feedback_perm.is_used = True
                    feedback_perm.save()
                    # Do it as an atomic action to avoid race conditions.
                    TagSiteLink.objects.filter(tag__name=received_obj['tag'],
                            site__site_name=received_obj['site']).update(level = (F('level')+1)
                                    if received_obj['is_positive']
                                    else (F('level')-1))
                    level_updated = True
            if not level_updated:
                info('Refused to update the level for transmission {}, not found the tag-site link'
                        ' or the permission'.format(event['text']))
        else:
            warning('Received a WebSocket transmission with an unknown subject: {}'.format(
                received_obj))

class RulesUpdateConsumer(SyncConsumer):

    def websocket_connect(self, event):
        self.send({ 'type': 'websocket.accept' })

    def websocket_disconnect(self, event):
        self.send({ 'type': 'websocket.disconnected' })

    def websocket_receive(self, event):
        received_obj = json.loads(event['text'])
        if received_obj['subject'] == 'rules_results':
            site_names = received_obj['sites']
            site_names += [site.site_name # add the site names from tags
                for site in Site.objects.filter(
                    tag_links__tag__in=received_obj['tags']).all()]
            try:
                search_context = rules_results(received_obj['query_phrase'], received_obj['rules'],
                        query_site_names=site_names)
                self.send({ 'type': 'websocket.send', 'text': json.dumps(search_context['result']) })
            except socket.timeout:
                self.send({ 'type': 'websocket.send', 'text': '' })
            except Exception as e:
                info('Exception {} blocked getting a result from Solr: {}'.format(type(e).__name__, e))
                self.send({ 'type': 'websocket.send', 'text': '' })
        else:
            warning('Received a WebSocket transmission with an unknown subject: {}'.format(
                received_obj))

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
            warning('Received a WebSocket transmission with an unknown subject: {}'.format(
                received_obj))

    def progress_info(self, event):
        """
        This should be called with channel_layer.group_send to the appropriate group.
        """
        self.send({ 'type': 'websocket.send', 'text': json.dumps(event)})
