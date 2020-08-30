from django.urls import path

from . import views

urlpatterns = [
    path('sug/', views.autosuggest, name='sug'),
]
