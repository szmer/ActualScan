from django.db import models
from django.dispatch import receiver
from django.db.models.signals import post_save
from django.contrib.auth.models import User
from django.contrib.postgres.fields import ArrayField

from scan.models import Site, Tag

class QueryRecord(models.Model):
    query_phrase = models.CharField(max_length=204800)
    person = models.ForeignKey(User, on_delete=models.CASCADE, related_name='queries',
            null=True)
    date_submitted = models.DateTimeField(auto_now_add=True)
    query_tags = ArrayField(
            models.CharField(max_length=Tag._meta.get_field('name').max_length),
            blank=True)
    query_site_names = ArrayField(
            models.CharField(max_length=Site._meta.get_field('site_name').max_length),
            blank=True)
    minimal_level = models.CharField(max_length=32)

    def __repr__(self):
        return '({})job {}/{}/{} by {}'.format(self.pk, self.query_phrase, self.query_tags,
                self.query_site_names, self.person)

    def __str__(self):
        return '({})job {}/{}/{} by {}'.format(self.pk, self.query_phrase, self.query_tags,
                self.query_site_names, self.person)

# TODO enforce email uniqueness on users
class UserProfile(models.Model):
    person = models.OneToOneField(User, on_delete=models.CASCADE, primary_key=True)
    wants_query_records = models.BooleanField(default=True)
    # The time of last token sent for account activation.
    last_token_time = models.DateTimeField(auto_now_add=True)

@receiver(post_save, sender=User)
def update_user_profile(sender, instance, created, **kwargs):
    """
    Create a UserProfile for the newly joined User.
    """
    if created:
        UserProfile.objects.create(person=instance)
    instance.userprofile.save()
