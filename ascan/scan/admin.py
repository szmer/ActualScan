from django.contrib import admin

from .models import Site, Tag, ScrapeRequest, ScanJob, ScanPermission

class ScanPermissionAdmin(admin.ModelAdmin):
    readonly_fields = ('time_issued',)

class ScanJobAdmin(admin.ModelAdmin):
    readonly_fields = ('status_changed', 'last_checked')

class ScrapeRequestAdmin(admin.ModelAdmin):
    readonly_fields = ('status_changed',)

admin.site.register(Site)
admin.site.register(Tag)
admin.site.register(ScanPermission, ScanPermissionAdmin)
admin.site.register(ScrapeRequest, ScrapeRequestAdmin)
admin.site.register(ScanJob, ScanJobAdmin)
