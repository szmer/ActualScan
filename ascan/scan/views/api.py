from django.http import JsonResponse
from ipware import get_client_ip

from scan.models import ScanJob

def myscans(request):
    """
    A JSON API call for getting a list of job ids being ran for the caller.
    """
    if request.user.is_authenticated:
        return JsonResponse({'jobs': [job.id for job in ScanJob.objects.filter(user=request.user)]})
    else:
        return JsonResponse({'jobs': [job.id for job in ScanJob.objects.filter(
            user_ip=get_client_ip(request))]})
