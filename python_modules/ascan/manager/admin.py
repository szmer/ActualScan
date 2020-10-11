from django.contrib import admin

from .models import BlockedSite, EditSuggestion

class BlockedSiteAdmin(admin.ModelAdmin):
    readonly_fields = ('time_added',)

class EditSuggestionAdmin(admin.ModelAdmin):
    readonly_fields = ('date_submitted',)

admin.site.register(BlockedSite, BlockedSiteAdmin)
admin.site.register(EditSuggestion, EditSuggestionAdmin)
