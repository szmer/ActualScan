from django.urls import path

from . import views

urlpatterns = [
    path('sug/', views.autosuggest, name='sug'),
    path('mysearches/', views.SearchesList.as_view(), name='mysearches'),
    path('userprofile/', views.userprofile, name='userprofile'),
    path('delete_record/', views.deleteRecord, name='deleterecord'),
]
