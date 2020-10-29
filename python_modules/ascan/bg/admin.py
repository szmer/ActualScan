from django.contrib import admin

from .models import QueryRecord, UserProfile

admin.site.register(QueryRecord)
admin.site.register(UserProfile)
