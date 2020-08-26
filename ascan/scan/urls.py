from django.urls import path

from . import views

urlpatterns = [
    path('scaninfo/', views.scaninfo, name='scaninfo'),
    path('myscans/', views.myscans, name='myscans'),
    #path('testscanenv/', views.testscanenv, name='testscanenv'),
    #path('testscanweb/', views.testscanweb, name='testscanweb'),
    #path('testscanreddit/', views.testscanreddit, name='testscanreddit'),
]
