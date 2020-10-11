from django.contrib.auth.decorators import login_required
from django.shortcuts import render

@login_required
def accountinfo(request):
    return render(request, 'registration/info.html')
