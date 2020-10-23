from django.contrib import admin

from .models import (
        Site, Tag, TagSiteLink, ScrapeRequest, ScanJob, ScanPermission, FeedbackPermission,
        ResultRule
        )

class PermissionAdmin(admin.ModelAdmin):
    readonly_fields = ('time_issued',)

class ScanJobAdmin(admin.ModelAdmin):
    readonly_fields = ('status_changed', 'last_checked')

class ScrapeRequestAdmin(admin.ModelAdmin):
    readonly_fields = ('status_changed',)
    list_filter = ('is_search', 'status', 'job_id', 'site_type', 'site_name')

admin.site.register(Site)
admin.site.register(Tag)
admin.site.register(TagSiteLink)
admin.site.register(FeedbackPermission, PermissionAdmin)
admin.site.register(ScanPermission, PermissionAdmin)
admin.site.register(ScrapeRequest, ScrapeRequestAdmin)
admin.site.register(ScanJob, ScanJobAdmin)
admin.site.register(ResultRule)
