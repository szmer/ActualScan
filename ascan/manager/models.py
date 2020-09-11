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

class EditSuggestion(models.Model):
    """
    A user-submitted suggestion for changing a Tag or a Site, or for adding a Tag-Site link.
    """
    creator = models.ForeignKey(User, on_delete=models.SET_NULL, related_name='sites', null=True)
    date_submitted = models.DateTimeField(auto_now_add=True)
    # Fields: record_type (site, tag, tag-site link), target, target2, fields related to what should
    # be changed preceded by fld_ (such as fld_descritpion for Tag description)
    suggestion = JSONField()
    date_responded = models.DateTimeField(null=True, blank=True)
    status = models.CharField(max_length=32, choices=EDIT_SUGGESTION_STATUSES, default='received')
