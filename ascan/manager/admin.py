from django.contrib import admin

from .models import EditSuggestion

class EditSuggestionAdmin(admin.ModelAdmin):
    readonly_fields = ('date_submitted',)

admin.site.register(EditSuggestion)
