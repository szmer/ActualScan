from django.shortcuts import render
from ipware import get_client_ip

from scan.control import maybe_issue_guest_scan_permission
from scan.forms import PublicScanForm

def index(request):
    form = PublicScanForm()
    # TODO KLUDGE currently unlimited scan permissions for registered
    if request.user.is_authenticated:
        can_scan = True
    else:
        can_scan = maybe_issue_guest_scan_permission(get_client_ip(request))
    return render(request, 'scan/home.html', context={ 'can_scan': can_scan, 'form': form })
