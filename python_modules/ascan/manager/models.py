from django.contrib.auth.models import User
from django.contrib.postgres.fields import JSONField
from django.db import models

EDIT_SUGGESTION_STATUSES = [(x, x) for x in [
    'received',
    'accepted',
    'partially accepted',
    'applied earlier',
    'rejected',
    'saved for later'
    ]]
EDIT_SUGGESTION_RECORD_TYPES = [(x, x) for x in [
    'site',
    'tag'
    ]]

class EditSuggestion(models.Model):
    """
    A user-submitted suggestion for changing a Tag or a Site, or for adding a Tag-Site link.
    """
    creator = models.ForeignKey(User, on_delete=models.SET_NULL, related_name='suggestions',
            null=True)
    date_submitted = models.DateTimeField(auto_now_add=True)
    record_type = models.CharField(max_length=16, choices=EDIT_SUGGESTION_RECORD_TYPES)
    target = models.IntegerField() # id of the object in question
    target_name = models.CharField(max_length=1024)
    # fields related to what should be changed
    suggestion = JSONField()
    date_responded = models.DateTimeField(null=True, blank=True)
    status = models.CharField(max_length=32, choices=EDIT_SUGGESTION_STATUSES, default='received')
    comment = models.CharField(max_length=2048, null=True, blank=True)
    mod_comment = models.CharField(max_length=2048, null=True, blank=True)
    mod_activity = JSONField(default=dict, null=True, blank=True)

    def __repr__(self):
        return 'suggestion for {} from {} ({})'.format(
                self.target_name, self.creator, self.id)

    def __str__(self):
        return 'suggestion for {} from {} ({})'.format(
                self.target_name, self.creator, self.id)

class BlockedSite(models.Model):
    """
    A site to be blocked for adding to the index.
    """
    address = models.CharField(max_length=8192)
    kind = models.CharField(max_length=32)
    time_added = models.DateTimeField(auto_now_add=True)

    def __repr__(self):
        return 'blocked: {} ({})'.format(self.address, self.kind)

    def __str__(self):
        return 'blocked: {} ({})'.format(self.address, self.kind)
