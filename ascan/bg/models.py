from django.db import models
from django.contrib.postgres.fields import JSONField

class AutocompleteTerm(models.Model):
    term = models.CharField(max_length=512, unique=True)
    suggest_data = JSONField()

    def __repr__(self):
        return 'AutocompleteTerm:"{}"'.format(self.term)

    def __str__(self):
        return 'AutocompleteTerm:"{}"'.format(self.term)

#    term = SearchVectorField() # fill it with space-separated characters
#    weight = models.IntegerField()
#
#    class Meta:
#        indexes = [GinIndex(fields=['term'])]
