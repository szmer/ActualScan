from django.urls import path

from . import views

urlpatterns = [
    path('join/', views.join, name='join'),
    path('activate/<uidb64>/<token>/', views.activate, name='activate'),
    path('activation_resend/', views.activation_resend, name='activation_resend'),
    path('sug/', views.autosuggest, name='sug'),
    path('mysearches/', views.SearchesList.as_view(), name='mysearches'),
    path('userprofile/', views.userprofile, name='userprofile'),
    path('delete_record/', views.deleteRecord, name='deleterecord'),
]
